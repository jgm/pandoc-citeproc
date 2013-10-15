module Main where
import System.Process
import System.Exit
import System.Directory
import System.FilePath
import Text.Printf
import System.IO
import Data.Monoid (mempty)
import Data.Algorithm.Diff
import Text.Printf
import qualified Data.Aeson as Aeson
import Data.Aeson.Encode.Pretty
import Text.Pandoc.Definition
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B
import qualified Text.Pandoc.UTF8 as UTF8
import Text.Pandoc.Process (pipeProcess)
import qualified Data.Yaml as Yaml (decode)

main = do
  citeprocTests <- mapM testCase ["chicago-author-date", "ieee", "mhra"]
  fs <- filter (\f -> takeExtension f `elem` ["bibtex","biblatex"])
           `fmap` getDirectoryContents "tests/biblio2yaml"
  biblio2yamlTests <- mapM biblio2yamlTest fs
  if all id citeprocTests && all id biblio2yamlTests
     then exitWith ExitSuccess
     else exitWith $ ExitFailure 1

err :: String -> IO ()
err = hPutStrLn stderr

testCase :: String -> IO Bool
testCase csl = do
  err $ "TEST: " ++ csl
  indata <- BL.readFile $ "tests/" ++ csl ++ ".in.json"
  expected <- BL.readFile $ "tests/" ++ csl ++ ".expected.json"
  (ec, result, errout) <- pipeProcess
                     (Just [("LANG","en_US.UTF-8"),("HOME",".")])
                     "dist/build/pandoc-citeproc/pandoc-citeproc"
                     [] indata
  if ec == ExitSuccess
     then do
       let resultDoc :: Maybe Pandoc
           resultDoc = Aeson.decode result
       let expectedDoc :: Maybe Pandoc
           expectedDoc = Aeson.decode expected
       let result' = maybe [] (BL.lines
                     . encodePretty' Config{ confIndent = 2
                                           , confCompare = compare } )
                     resultDoc
       let expected' = maybe [] (BL.lines
                     . encodePretty' Config{ confIndent = 2
                                           , confCompare = compare } )
                     expectedDoc
       if result' == expected'
          then return True
          else do
            let diff = getDiff expected' result'
            err $ showDiff (1,1) diff
            return False
     else do
       err $ "Error status " ++ show ec
       err $ UTF8.toStringLazy errout
       return False

showDiff :: (Int,Int) -> [Diff BL.ByteString] -> String
showDiff _ []             = ""
showDiff (l,r) (First ln : ds) =
  printf "+%4d " l ++ (UTF8.toStringLazy ln) ++ "\n" ++ showDiff (l+1,r) ds
showDiff (l,r) (Second ln : ds) =
  printf "-%4d " r ++ (UTF8.toStringLazy ln) ++ "\n" ++ showDiff (l,r+1) ds
showDiff (l,r) (Both _ _ : ds) =
  showDiff (l+1,r+1) ds

biblio2yamlTest :: String -> IO Bool
biblio2yamlTest fp = do
  let yamlf = "tests/biblio2yaml/" ++ fp
  raw <- BL.readFile yamlf
  let yamlStart = BL.pack "---"
  let (biblines, yamllines) = break (== yamlStart) $ BL.lines raw
  let bib = BL.unlines biblines
  let expected = BL.unlines yamllines
  (ec, result, errout) <- pipeProcess
                     (Just [("LANG","en_US.UTF-8"),("HOME",".")])
                     "dist/build/biblio2yaml/biblio2yaml"
                     ["-f", takeExtension fp] bib
  if ec == ExitSuccess
     then do
       let expected' :: Maybe Aeson.Value
           expected' = Yaml.decode $ B.concat $ BL.toChunks expected
       let actual'   :: Maybe Aeson.Value
           actual' = Yaml.decode $ B.concat $ BL.toChunks result
       if expected' == actual'
          then return True
          else do
            err $ "FAILED: " ++ yamlf
            let diff = getDiff (BL.words expected) (BL.words result)
            err $ showDiff (1,1) diff
            return False
     else do
       err $ "Error status " ++ show ec
       err $ UTF8.toStringLazy errout
       return False
