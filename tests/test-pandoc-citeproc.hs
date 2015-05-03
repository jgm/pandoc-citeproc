module Main where
import JSON
import System.Process
import System.Exit
import System.Directory
import System.FilePath
import Data.Maybe (fromMaybe)
import Text.Printf
import System.IO
import Data.Monoid (mempty)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (rawSystem)
import Text.Printf
import qualified Data.Aeson as Aeson
import Text.Pandoc.Definition
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B
import qualified Text.Pandoc.UTF8 as UTF8
import Text.Pandoc.Shared (normalize)
import Text.Pandoc.Process (pipeProcess)
import qualified Data.Yaml as Yaml
import Text.Pandoc (writeNative, writeHtmlString, readNative, def)
import Text.CSL.Pandoc (processCites')
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
  indataNative <- readFile $ "tests/" ++ csl ++ ".in.native"
  expectedNative <- readFile $ "tests/" ++ csl ++ ".expected.native"
  let jsonIn = Aeson.encode $ (read indataNative :: Pandoc)
  let expectedDoc = normalize $ read expectedNative

  (ec, jsonOut, errout) <- pipeProcess
                     (Just [("LANG","en_US.UTF-8"),("HOME",".")])
                     "dist/build/pandoc-citeproc/pandoc-citeproc"
                     [] jsonIn
  if ec == ExitSuccess
     then do
       let outDoc = normalize $ fromMaybe mempty $ Aeson.decode $ jsonOut
       if outDoc == expectedDoc
          then err "PASSED" >> return Passed
          else do
             err $ "FAILED"
             showDiff (UTF8.fromStringLazy $ writeNative def expectedDoc)
                      (UTF8.fromStringLazy $ writeNative def outDoc)
             when regenerate $
               UTF8.writeFile ("tests/" ++ csl ++ ".expected.native") $ show outDoc
             return Failed
     else do
       err "ERROR"
       err $ "Error status " ++ show ec
       err $ UTF8.toStringLazy errout
       return Errored

showDiff :: BL.ByteString -> BL.ByteString -> IO ()
showDiff expected' result' =
  withSystemTempDirectory "test-pandoc-citeproc-XXX" $ \fp -> do
    let expectedf = fp </> "expected"
    let actualf   = fp </> "actual"
    BL.writeFile expectedf expected'
    BL.writeFile actualf result'
    oldDir <- getCurrentDirectory
    setCurrentDirectory fp
    rawSystem "diff" ["-U1","expected","actual"]
    setCurrentDirectory oldDir

biblio2yamlTest :: String -> IO TestResult
biblio2yamlTest fp = do
  hPutStr stderr $ "[biblio2yaml/" ++ fp ++ "] "
  let yamlf = "tests/biblio2yaml/" ++ fp
  raw <- BL.readFile yamlf
  let yamlStart = BL.pack "---"
  let (biblines, yamllines) = break (== yamlStart) $ BL.lines raw
  let bib = BL.unlines biblines
  let expected = BL.unlines yamllines
  (ec, result, errout) <- pipeProcess
                     (Just [("LANG","en_US.UTF-8"),("HOME",".")])
                     "dist/build/pandoc-citeproc/pandoc-citeproc"
                     ["--bib2yaml", "-f", drop 1 $ takeExtension fp] bib
  if ec == ExitSuccess
     then do
       let expectedDoc :: Maybe Aeson.Value
           expectedDoc = Yaml.decode $ B.concat $ BL.toChunks expected
       let resultDoc   :: Maybe Aeson.Value
           resultDoc   = Yaml.decode $ B.concat $ BL.toChunks result
       let result'   = BL.fromChunks [Yaml.encode resultDoc]
       let expected' = BL.fromChunks [Yaml.encode expectedDoc]
       if expected' == result'
          then err "PASSED" >> return Passed
          else do
            err $ "FAILED"
            showDiff expected' result'
            return Failed
     else do
       err "ERROR"
       err $ "Error status " ++ show ec
       err $ UTF8.toStringLazy errout
       return Errored
