module Main where
import System.Process
import System.Exit
import Text.Printf
import System.IO
import Data.Monoid (mempty)
import Data.Algorithm.Diff
import Text.Printf
import Data.ByteString.Lazy.UTF8 (fromString, toString)
import Data.Aeson (decode)
import Data.Aeson.Encode.Pretty
import Text.Pandoc.Definition

main = do
  testCase "chicago-author-date"
  testCase "ieee"
  testCase "mhra"
  exitWith ExitSuccess

err :: String -> IO ()
err = hPutStrLn stderr

testCase :: String -> IO ()
testCase csl = do
  err $ "TEST: " ++ csl
  indata <- readFile $ "tests/" ++ csl ++ ".in.json"
  expected <- readFile $ "tests/" ++ csl ++ ".expected.json"
  (ec, result, errout) <- readProcessWithExitCode
                     "dist/build/pandoc-citeproc/pandoc-citeproc"
                     [] indata
  if ec == ExitSuccess
     then do
       let resultDoc :: Maybe Pandoc
           resultDoc = decode $ fromString result
       let expectedDoc :: Maybe Pandoc
           expectedDoc = decode $ fromString expected
       let result' = maybe [] (lines . toString
                     . encodePretty' Config{ confIndent = 2, confCompare = compare } )
                     resultDoc
       let expected' = maybe [] (lines . toString
                     . encodePretty' Config{ confIndent = 2, confCompare = compare } )
                     expectedDoc
       if result' == expected'
          then return ()
          else do
            let diff = getDiff expected' result'
            err $ showDiff (1,1) diff
            exitWith $ ExitFailure 1
     else do
       err $ "Error status " ++ show ec
       err errout
       exitWith ec

showDiff :: (Int,Int) -> [Diff String] -> String
showDiff _ []             = ""
showDiff (l,r) (First ln : ds) =
  printf "+%4d " l ++ ln ++ "\n" ++ showDiff (l+1,r) ds
showDiff (l,r) (Second ln : ds) =
  printf "-%4d " r ++ ln ++ "\n" ++ showDiff (l,r+1) ds
showDiff (l,r) (Both _ _ : ds) =
  showDiff (l+1,r+1) ds

