module Main where
import Text.CSL.Pandoc (processCites')
import Text.Pandoc.JSON
import Text.Pandoc.Walk
import System.IO (stderr, hPutStrLn)
import System.Console.GetOpt
import System.Environment (getArgs)
import Control.Monad
import System.Exit
import Data.Version (showVersion)
import Paths_pandoc_citeproc (version)

main :: IO ()
main = do
  argv <- getArgs
  let (flags, _, errs) = getOpt Permute options argv
  let header = "Usage: pandoc-citeproc"
  unless (null errs) $ do
    hPutStrLn stderr $ usageInfo (unlines $ errs ++ [header]) options
    exitWith $ ExitFailure 1
  when (Version `elem` flags) $ do
    putStrLn $ "pandoc-citeproc " ++ showVersion version
    exitWith ExitSuccess
  when (Help `elem` flags) $ do
    putStrLn $ usageInfo header options
    exitWith ExitSuccess
  toJSONFilter doCites

doCites :: Pandoc -> IO Pandoc
doCites doc = do
  doc' <- processCites' doc
  let warnings = query findWarnings doc'
  mapM_ (hPutStrLn stderr) warnings
  return doc'

findWarnings :: Inline -> [String]
findWarnings (Span (_,["citeproc-not-found"],[("data-reference-id",ref)]) _) =
  ["pandoc-citeproc: reference " ++ ref ++ " not found"]
findWarnings (Span (_,["citeproc-no-output"],_) _) =
  ["pandoc-citeproc: reference with no printed form"]
findWarnings _ = []

data Option =
    Help | Version
  deriving (Ord, Eq, Show)

options :: [OptDescr Option]
options =
  [ Option ['h'] ["help"] (NoArg Help) "show usage information"
  , Option ['v'] ["version"] (NoArg Version) "show program version"
  ]


