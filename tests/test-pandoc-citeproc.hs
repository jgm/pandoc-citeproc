module Main where
import System.Process
import System.Exit
import Text.Printf
import System.IO
import Data.Monoid (mempty)
import Data.Algorithm.Diff
import Text.Printf
import Data.Aeson (decode)
import Data.Aeson.Encode.Pretty
import Text.Pandoc.Definition
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Text.Pandoc.UTF8 as UTF8
import Text.Pandoc.Process (pipeProcess)

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
  indata <- BL.readFile $ "tests/" ++ csl ++ ".in.json"
  expected <- BL.readFile $ "tests/" ++ csl ++ ".expected.json"
  (ec, result, errout) <- pipeProcess
                     (Just [("LANG","en_US.UTF-8"),("HOME",".")])
                     "dist/build/pandoc-citeproc/pandoc-citeproc"
                     [] indata
  if ec == ExitSuccess
     then do
       let resultDoc :: Maybe Pandoc
           resultDoc = decode result
       let expectedDoc :: Maybe Pandoc
           expectedDoc = decode expected
       let result' = maybe [] (BL.lines
                     . encodePretty' Config{ confIndent = 2
                                           , confCompare = compare } )
                     resultDoc
       let expected' = maybe [] (BL.lines
                     . encodePretty' Config{ confIndent = 2
                                           , confCompare = compare } )
                     expectedDoc
       if result' == expected'
          then return ()
          else do
            let diff = getDiff expected' result'
            err $ showDiff (1,1) diff
            exitWith $ ExitFailure 1
     else do
       err $ "Error status " ++ show ec
       err $ UTF8.toStringLazy errout
       exitWith ec

showDiff :: (Int,Int) -> [Diff BL.ByteString] -> String
showDiff _ []             = ""
showDiff (l,r) (First ln : ds) =
  printf "+%4d " l ++ (UTF8.toStringLazy ln) ++ "\n" ++ showDiff (l+1,r) ds
showDiff (l,r) (Second ln : ds) =
  printf "-%4d " r ++ (UTF8.toStringLazy ln) ++ "\n" ++ showDiff (l,r+1) ds
showDiff (l,r) (Both _ _ : ds) =
  showDiff (l+1,r+1) ds

