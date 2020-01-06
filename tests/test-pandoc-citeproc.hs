{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import Prelude
import qualified Data.Aeson             as Aeson
import qualified Data.ByteString        as B
import           Data.List              (isSuffixOf)
import           Data.Text              (Text)
import           Data.Text.Encoding     (encodeUtf8)
import qualified Data.Text              as T
import           Data.Maybe             (fromMaybe)
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO
import           System.IO.Temp         (withSystemTempDirectory)
import           System.Process         (rawSystem)
import           Text.CSL.Compat.Pandoc (pipeProcess, writeNative)
import           Text.Pandoc.Definition
import qualified Text.Pandoc.UTF8       as UTF8
#if MIN_VERSION_pandoc(2,0,0)
import qualified Control.Exception      as E
#endif

main :: IO ()
main = do
  args <- getArgs
  let regenerate = "--accept" `elem` args
  testnames <- (map (dropExtension . takeBaseName) .
                     filter (".in.native" `isSuffixOf`)) <$>
               getDirectoryContents "tests"
  citeprocTests <- mapM (testCase regenerate) testnames
  fs <- filter (\f -> takeExtension f `elem` [".bibtex",".biblatex"])
           `fmap` getDirectoryContents "tests/biblio2yaml"
  biblio2yamlTests <- mapM (biblio2yamlTest regenerate) fs
  let allTests = citeprocTests ++ biblio2yamlTests
  let numpasses  = length $ filter (== Passed) allTests
  let numskipped = length $ filter (== Skipped) allTests
  let numfailures = length $ filter (== Failed) allTests
  let numerrors = length $ filter (== Errored) allTests
  putStrLn $ show numpasses ++ " passed; " ++ show numfailures ++
              " failed; " ++ show numskipped ++ " skipped; " ++
              show numerrors ++ " errored."
  exitWith $ if numfailures == 0 && numerrors == 0
                then ExitSuccess
                else ExitFailure $ numfailures + numerrors

err :: String -> IO ()
err = hPutStrLn stderr

data TestResult =
    Passed
  | Skipped
  | Failed
  | Errored
  deriving (Show, Eq)

testCase :: Bool -> String -> IO TestResult
testCase regenerate csl = do
  hPutStr stderr $ "[" ++ csl ++ ".in.native] "
  indataNative <- UTF8.readFile $ "tests/" ++ csl ++ ".in.native"
  expectedNative <- UTF8.readFile $ "tests/" ++ csl ++ ".expected.native"
  let jsonIn = Aeson.encode (read indataNative :: Pandoc)
  let expectedDoc = read expectedNative
  testProgPath <- getExecutablePath
  let pandocCiteprocPath = takeDirectory testProgPath </> ".." </>
        "pandoc-citeproc" </> "pandoc-citeproc"
  (ec, jsonOut) <- pipeProcess
                     (Just [("LANG","en_US.UTF-8"),("HOME",".")])
                     pandocCiteprocPath
                     [] jsonIn
  if ec == ExitSuccess
     then do
       let outDoc = fromMaybe mempty $Aeson.decode jsonOut
       if outDoc == expectedDoc
          then err "PASSED" >> return Passed
          else
            if regenerate
               then do
                 B.writeFile ("tests/" ++ csl ++ ".expected.native") $
                   encodeUtf8 (writeNative outDoc)
                 err "PASSED (accepted)"
                 return Passed
               else do
                 err "FAILED"
                 showDiff (writeNative expectedDoc) (writeNative outDoc)
                 return Failed
     else do
       err "ERROR"
       err $ "Error status " ++ show ec
       return Errored

showDiff :: Text -> Text -> IO ()
showDiff expected result =
  withSystemTempDirectory "test-pandoc-citeproc-XXX" $ \fp -> do
    let expectedf = fp </> "expected"
    let actualf   = fp </> "actual"
    UTF8.writeFile expectedf $ T.unpack expected
    UTF8.writeFile actualf $ T.unpack result
    oldDir <- getCurrentDirectory
    setCurrentDirectory fp
    _ <- rawSystem "diff" ["-U1","expected","actual"]
    setCurrentDirectory oldDir

biblio2yamlTest :: Bool -> String -> IO TestResult
biblio2yamlTest regenerate fp = do
  hPutStr stderr $ "[biblio2yaml/" ++ fp ++ "] "
  let yamld = "tests/biblio2yaml/"
#if MIN_VERSION_pandoc(2,0,0)
  -- in a few cases we need different test output for pandoc >= 2
  -- because smallcaps render differently, for example.
  raw <- E.catch (UTF8.readFile (yamld ++ "/pandoc-2/" ++ fp))
         (\(_ :: E.SomeException) ->
           (UTF8.readFile (yamld ++ fp)))
#else
  raw <- UTF8.readFile (yamld ++ fp)
#endif
  let yamlStart = "---"
  let (biblines, yamllines) = break (== yamlStart) $ lines raw
  let bib = unlines biblines
  let expected = unlines yamllines
  testProgPath <- getExecutablePath
  let pandocCiteprocPath = takeDirectory testProgPath </> ".." </>
        "pandoc-citeproc" </> "pandoc-citeproc"
  (ec, result') <- pipeProcess
                     (Just [("LANG","en_US.UTF-8"),("HOME",".")])
                     pandocCiteprocPath
                     ["--bib2yaml", "-f", drop 1 $ takeExtension fp,
                      "--columns=0"]
                     (UTF8.fromStringLazy bib)
  let result = UTF8.toStringLazy result'
  if ec == ExitSuccess
     then do
       if expected == result
          then err "PASSED" >> return Passed
          else
            if regenerate
               then do
                 let accepted = bib ++ result
#if MIN_VERSION_pandoc(2,0,0)
                 p2version <- doesFileExist (yamld ++ "/pandoc-2/" ++ fp)
                 UTF8.writeFile (if p2version
                                    then (yamld ++ "/pandoc-2/" ++ fp)
                                    else (yamld ++ fp)) accepted
#else
                 UTF8.writeFile (yamld ++ fp) accepted
#endif
                 err "PASSED (accepted)"
                 return Passed
               else do
                 err "FAILED"
                 showDiff (T.pack expected) (T.pack result)
                 return Failed
     else do
       err "ERROR"
       err $ "Error status " ++ show ec
       return Errored
