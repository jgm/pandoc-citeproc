{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances,
    ScopedTypeVariables #-}
import System.Exit
import Data.Char (isSpace)
import System.Process
import System.IO.Temp (withSystemTempDirectory)
import Text.Pandoc.Definition (Inline(Span, Str))
import Text.Pandoc.Generic
import Text.Pandoc.Walk
import Data.Maybe (mapMaybe)
import Data.Aeson
import Data.Aeson.Types (Parser)
import System.FilePath
import System.Directory
import Data.List (intercalate, sort)
import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Text.CSL.Style hiding (Number)
import Text.CSL.Reference
import Text.CSL
import Text.CSL.Input.Pandoc (inlinesToString)
import Control.Monad
import Control.Applicative
import qualified Data.ByteString.Lazy as BL

data TestCase = TestCase{
    testMode          :: Mode        -- mode
  , testBibopts       :: BibOpts     -- bibsection
  , testCitations     :: [CiteObject] -- citations
  , testCitationItems :: Citations   -- citation-items
  , testCsl           :: Style       -- csl
  , testAbbreviations :: Abbreviations -- abbreviations
  , testReferences    :: [Reference] -- input
  , testResult        :: String      -- result
  } deriving (Show)

data Mode = CitationMode
          | BibliographyMode
          | BibliographyHeaderMode
          | BibliographyNoSortMode
          deriving Show

instance FromJSON Mode where
  parseJSON (String "citation")     = return CitationMode
  parseJSON (String "bibliography") = return BibliographyMode
  parseJSON (String "bibliography-header") = return BibliographyHeaderMode
  parseJSON (String "bibliography-nosort") = return BibliographyNoSortMode
  parseJSON _                       = fail "Unknown mode"

instance FromJSON TestCase where
  parseJSON (Object v) = TestCase <$>
              v .:  "mode" <*>
              v .:? "bibsection" .!= Select [] [] <*>
              v .:? "citations" .!= [] <*>
              v .:? "citation_items" .!= [] <*>
              v .:  "csl" <*>
              v .:? "abbreviations" .!= (Abbreviations M.empty) <*>
              v .:  "input" <*>
              v .:  "result"
  parseJSON _ = fail "Could not parse test case"

newtype CiteObject =
        CiteObject { unCiteObject :: [Cite] } deriving Show

instance FromJSON CiteObject where
  parseJSON (Array v) =
    case fromJSON (Array v) of
         Success [Object x, Array _, Array _] ->
                            CiteObject <$> x .: "citationItems"
         Error e         -> fail $ "Could not parse CiteObject: " ++ e
         x               -> fail $ "Could not parse CiteObject" ++ show x
  parseJSON x = fail $ "Could not parse CiteObject " ++ show x

instance FromJSON [CiteObject] where
  parseJSON (Array v) = mapM parseJSON $ V.toList v
  parseJSON _ = return []

data TestResult =
    Passed
  | Skipped
  | Failed
  deriving (Show, Eq)

testDir :: FilePath
testDir = "citeproc-test" </> "processor-tests" </> "machines"

escapeHtml :: String -> String
escapeHtml [] = []
escapeHtml ('&':xs) = "&#38;" ++ escapeHtml xs
escapeHtml ('<':xs) = "&#60;" ++ escapeHtml xs
escapeHtml (x:xs)   = x : escapeHtml xs

runTest :: FilePath -> IO TestResult
runTest path = do
  raw <- BL.readFile path
  let testCase = either error id $ eitherDecode raw
  let procOpts = ProcOpts (testBibopts testCase)
  let style    = (testCsl testCase) {
                        styleAbbrevs = testAbbreviations testCase }
  let refs     = testReferences testCase
  let cites    = map unCiteObject (testCitations testCase) ++ testCitationItems testCase
  let cites'   = if null cites
                    then [map (\ref -> emptyCite{ citeId = refId ref}) refs]
                    else cites
  let expected = trimEnd $ testResult testCase
  let mode     = testMode testCase
  let assemble BibliographyMode xs =
         "<div class=\"csl-bib-body\">\n" ++
         unlines (map (\x -> "  <div class=\"csl-entry\">" ++ x ++
                               "</div>") xs) ++ "</div>\n"
      assemble _ xs = unlines xs
  case mode of
       BibliographyHeaderMode  -> do
          putStrLn $ "[SKIPPED] " ++ path ++ "\n"
          return Skipped
       BibliographyNoSortMode  -> do
          putStrLn $ "[SKIPPED] " ++ path ++ "\n"
          return Skipped
       _ -> do
         let result   = assemble mode
              $ mapMaybe (inlinesToString .
                          walk escapeStr .
                          bottomUp (concatMap removeNocaseSpans) .
                          renderPandoc style) $
                (case mode of {CitationMode -> citations; _ -> bibliography})
                $ citeproc procOpts style refs cites'
         if result == expected
            then do
              putStrLn $ "[PASSED] " ++ path ++ "\n"
              return Passed
            else do
              putStrLn $ "[FAILED] " ++ path
              showDiff expected result
              putStrLn ""
              return Failed

trimEnd :: String -> String
trimEnd = reverse . ('\n':) . dropWhile isSpace . reverse

removeNocaseSpans :: Inline -> [Inline]
removeNocaseSpans (Span ("",["nocase"],[]) xs) = xs
removeNocaseSpans x = [x]

escapeStr :: Inline -> Inline
escapeStr (Str xs) = Str $ escapeHtml xs
escapeStr x = x

showDiff :: String -> String -> IO ()
showDiff expected' result' =
  withSystemTempDirectory "test-pandoc-citeproc-XXX" $ \fp -> do
    let expectedf = fp </> "expected"
    let actualf   = fp </> "actual"
    writeFile expectedf expected'
    writeFile actualf result'
    withDirectory fp $ void $ rawSystem "diff" ["-u","expected","actual"]

withDirectory :: FilePath -> IO a -> IO a
withDirectory fp action = do
    oldDir <- getCurrentDirectory
    setCurrentDirectory fp
    result <- action
    setCurrentDirectory oldDir
    return result

main :: IO ()
main = do
  exists <- doesDirectoryExist testDir
  unless exists $ do
    putStrLn "Downloading test suite"
    rawSystem "hg" ["clone", "https://bitbucket.org/bdarcus/citeproc-test"]
    withDirectory "citeproc-test" $
      void $ rawSystem "python" ["processor.py", "--grind"]

  testFiles <- (map (testDir </>) . sort .
                filter (\f -> takeExtension f == ".json"))
              <$> getDirectoryContents testDir
  results <- mapM runTest testFiles
  let numpasses  = length $ filter (== Passed) results
  let numskipped = length $ filter (== Skipped) results
  let numfailures = length $ filter (== Failed) results
  putStrLn $ show numpasses ++ " passed; " ++ show numfailures ++
              " failed; " ++ show numskipped ++ " skipped."
  exitWith $ if numfailures == 0
                then ExitSuccess
                else ExitFailure numfailures
