{-# LANGUAGE CPP #-}
module Main where
import System.Exit
import System.Directory
import System.FilePath
import Data.Maybe (fromMaybe)
import System.IO
import Data.Monoid (mempty)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (rawSystem)
import qualified Data.Aeson as Aeson
import Text.Pandoc.Definition
import qualified Text.Pandoc.UTF8 as UTF8
import Text.Pandoc.Options (WriterOptions(..))
import Text.CSL.Compat.Pandoc (writeNative, pipeProcess)
import Text.Pandoc (def)
import Data.List (isSuffixOf)
import System.Environment
import Control.Monad (when)

main :: IO ()
main = do
  args <- getArgs
  let regenerate = args == ["--regenerate"]
  testnames <- fmap (map (dropExtension . takeBaseName) .
                     filter (".in.native" `isSuffixOf`)) $
               getDirectoryContents "tests"
  citeprocTests <- mapM (testCase regenerate) testnames
  fs <- filter (\f -> takeExtension f `elem` [".bibtex",".biblatex"])
           `fmap` getDirectoryContents "tests/biblio2yaml"
  biblio2yamlTests <- mapM biblio2yamlTest fs
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
  let jsonIn = Aeson.encode $ (read indataNative :: Pandoc)
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
       let outDoc = fromMaybe mempty $ Aeson.decode $ jsonOut
       if outDoc == expectedDoc
          then err "PASSED" >> return Passed
          else do
             err $ "FAILED"
             showDiff (writeNative def expectedDoc) (writeNative def outDoc)
             when regenerate $
               UTF8.writeFile ("tests/" ++ csl ++ ".expected.native") $
#if MIN_VERSION_pandoc(1,19,0)
                  writeNative def{ writerTemplate = Just "" } outDoc
#else
                  writeNative def{ writerStandalone = True }  outDoc
#endif
             return Failed
     else do
       err "ERROR"
       err $ "Error status " ++ show ec
       return Errored

showDiff :: String -> String -> IO ()
showDiff expected result =
  withSystemTempDirectory "test-pandoc-citeproc-XXX" $ \fp -> do
    let expectedf = fp </> "expected"
    let actualf   = fp </> "actual"
    UTF8.writeFile expectedf expected
    UTF8.writeFile actualf result
    oldDir <- getCurrentDirectory
    setCurrentDirectory fp
    rawSystem "diff" ["-U1","expected","actual"]
    setCurrentDirectory oldDir

biblio2yamlTest :: String -> IO TestResult
biblio2yamlTest fp = do
  hPutStr stderr $ "[biblio2yaml/" ++ fp ++ "] "
  let yamlf = "tests/biblio2yaml/" ++ fp
  raw <- UTF8.readFile yamlf
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
                     ["--bib2yaml", "-f", drop 1 $ takeExtension fp]
                     (UTF8.fromStringLazy bib)
  let result = UTF8.toStringLazy result'
  if ec == ExitSuccess
     then do
       if expected == result
          then err "PASSED" >> return Passed
          else do
            err $ "FAILED"
            showDiff expected result
            return Failed
     else do
       err "ERROR"
       err $ "Error status " ++ show ec
       return Errored
