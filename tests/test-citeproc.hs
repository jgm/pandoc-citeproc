{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances,
    ScopedTypeVariables #-}
import JSON
import Text.Printf
import System.Exit
import qualified Control.Exception as E
import Text.Pandoc
import Data.Char (isSpace, toLower)
import System.Environment (getArgs)
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
import Data.List (intercalate, sort, isInfixOf)
import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Text.CSL.Style hiding (Number)
import Text.CSL.Reference
import Text.CSL
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
  | Errored
  deriving (Show, Eq)

testDir :: FilePath
testDir = "citeproc-test" </> "processor-tests" </> "machines"

handleError :: FilePath -> E.SomeException -> IO TestResult
handleError path e = do
  putStrLn $ "[ERROR] " ++ path ++ "\n" ++ show e
  return Errored

runTest :: FilePath -> IO TestResult
runTest path = E.handle (handleError path) $ do
  raw <- BL.readFile path
  let testCase = either error id $ eitherDecode raw
  let procOpts = ProcOpts (testBibopts testCase) False
  style <- localizeCSL Nothing
           $ (testCsl testCase) { styleAbbrevs = testAbbreviations testCase }
  let refs     = testReferences testCase
  let cites    = map unCiteObject (testCitations testCase) ++ testCitationItems testCase
  let cites'   = if null cites
                    then [map (\ref -> emptyCite{ citeId = unLiteral $ refId ref}) refs]
                    else cites
  let expected = adjustEntities $ fixBegins $ trimEnd $ testResult testCase
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
              $ map (inlinesToString . renderPandoc style) $
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

-- this is designed to mimic the test suite's output:
inlinesToString  :: [Inline]  -> String
inlinesToString ils =
  writeHtmlString def{ writerWrapText = False }
    $ bottomUp (concatMap adjustSpans)
    $ Pandoc nullMeta [Plain ils]

-- We want &amp; instead of &#38; etc.
adjustEntities :: String -> String
adjustEntities ('&':'#':'3':'8':';':xs) = "&amp;" ++ adjustEntities xs
adjustEntities (x:xs) = x : adjustEntities xs
adjustEntities []     = []

-- citeproc-js test suite expects "citations" to be formatted like
-- .. [0] Smith (2007)
-- >> [1] Jones (2008)
-- To get a meaningful comparison, we remove this.
fixBegins :: String -> String
fixBegins = unlines . map fixLine . lines
  where fixLine ('.':'.':'[':xs) = dropWhile isSpace $ dropWhile (not . isSpace) xs
        fixLine ('>':'>':'[':xs) = dropWhile isSpace $ dropWhile (not . isSpace) xs
        fixLine xs = xs

-- adjust the spans so we fit what the test suite expects.
adjustSpans :: Inline -> [Inline]
adjustSpans (Note [Para xs]) = xs
adjustSpans (Link ils _) = ils
adjustSpans (Span ("",[],[]) xs) = xs
adjustSpans (Span ("",["nocase"],[]) xs) = xs
adjustSpans (Span ("",["citeproc-no-output"],[]) _) =
  [Str "[CSL STYLE ERROR: reference with no printed form.]"]
adjustSpans (Span (id',classes,kvs) ils) =
  [Span (id',classes',kvs') ils]
  where classes' = filter (`notElem` ["csl-no-emph","csl-no-strong","csl-no-smallcaps"]) classes
        kvs' = if null styles then kvs else (("style", concat styles) : kvs)
        styles = ["font-style:normal;" | "csl-no-emph" `elem` classes]
              ++ ["font-weight:normal;" | "csl-no-strong" `elem` classes]
              ++ ["font-variant:normal;" | "csl-no-smallcaps" `elem` classes]
adjustSpans (Emph xs) =
  RawInline (Format "html") "<i>" : xs ++ [RawInline (Format "html") "</i>"]
adjustSpans (Strong xs) =
  RawInline (Format "html") "<b>" : xs ++ [RawInline (Format "html") "</b>"]
adjustSpans (SmallCaps xs) =
  RawInline (Format "html") "<span style=\"font-variant:small-caps;\">" : xs ++ [RawInline (Format "html") "</span>"]
adjustSpans x = [x]

showDiff :: String -> String -> IO ()
showDiff expected' result' =
  withSystemTempDirectory "test-pandoc-citeproc-XXX" $ \fp -> do
    let expectedf = fp </> "expected"
    let actualf   = fp </> "actual"
    UTF8.writeFile expectedf expected'
    UTF8.writeFile actualf result'
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
  args <- getArgs
  let matchesPattern x
        | null args = True
        | otherwise = any (`isInfixOf` (map toLower x))
                        (map (map toLower . takeBaseName) args)
  exists <- doesDirectoryExist testDir
  unless exists $ do
    putStrLn "Downloading test suite"
    rawSystem "hg" ["clone", "https://bitbucket.org/bdarcus/citeproc-test"]
    withDirectory "citeproc-test" $
      void $ rawSystem "python" ["processor.py", "--grind"]

  testFiles <- if any ('/' `elem`) args
               then return args
               else (map (testDir </>) . sort .
                  filter matchesPattern .
                  filter (\f -> takeExtension f == ".json"))
                 <$> getDirectoryContents testDir
  results <- mapM runTest testFiles
  let numpasses  = length $ filter (== Passed) results
  let numskipped = length $ filter (== Skipped) results
  let numfailures = length $ filter (== Failed) results
  let numerrors = length $ filter (== Errored) results
  putStrLn $ show numpasses ++ " passed; " ++ show numfailures ++
              " failed; " ++ show numskipped ++ " skipped; " ++
              show numerrors ++ " errored."
  let summary = unlines $ zipWith (\fp res -> printf "%-10s %s" (show res) fp) testFiles results
  when (null args) $ do -- write log if complete test suite run
    ex <- doesFileExist "test-citeproc.log"
    when ex $ do
      putStrLn "Copying existing test-citeproc.log to test-citeproc.log.old"
      copyFile "test-citeproc.log" "test-citeproc.log.old"
    putStrLn "Writing test-citeproc.log."
    UTF8.writeFile "test-citeproc.log" summary
  exitWith $ if numfailures == 0
                then ExitSuccess
                else ExitFailure $ numfailures + numerrors
