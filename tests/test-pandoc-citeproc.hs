module Main where
import System.Process
import System.Exit
import Text.Printf
import Data.Algorithm.Diff

main = do
  putStr "done"

testCase :: String -> IO Bool
testCase csl = do
  let infile = "tests/" ++ csl ++ ".in.json"
  let expected = "tests/" ++ csl ++ ".expected.json"
  let outfile = "tests/" ++ csl ++ ".out.json"
  indata <- readFile infile
  (ec, result, err) <- readProcessWithExitCode
                        "dist/build/pandoc-citeproc" [] indata
  expected' <- readFile expected
  if expected' == result
     then exitWith ExitSuccess
     else exitWith $ ExitFailure 1

data TestResult = TestPassed
                | TestError ExitCode
                | TestFailed String FilePath [Diff String]
     deriving (Eq)

instance Show TestResult where
  show TestPassed     = "PASSED"
  show (TestError ec) = "ERROR " ++ show ec
  show (TestFailed cmd file d) = '\n' : dash ++
                                 "\n--- " ++ file ++
                                 "\n+++ " ++ cmd ++ "\n" ++ showDiff (1,1) d ++
                                 dash
    where dash = replicate 72 '-'

showDiff :: (Int,Int) -> [Diff String] -> String
showDiff _ []             = ""
showDiff (l,r) (First ln : ds) =
  printf "+%4d " l ++ ln ++ "\n" ++ showDiff (l+1,r) ds
showDiff (l,r) (Second ln : ds) =
  printf "-%4d " r ++ ln ++ "\n" ++ showDiff (l,r+1) ds
showDiff (l,r) (Both _ _ : ds) =
  showDiff (l+1,r+1) ds


