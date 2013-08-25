module Main where
import Text.CSL.Input.Bibutils (readBiblioFile)
import Text.CSL.Pandoc ()
import System.IO
import System.Environment
import System.Exit
import Data.Yaml
import qualified Data.ByteString as B

main :: IO ()
main = do
  args <- getArgs
  refs <- case args of
               [x] | x /= "-h" && x /= "--help" ->
                 readBiblioFile x
               _    -> do
                 hPutStrLn stderr $ "Usage: mods2yaml MODSFILE"
                 exitWith (ExitFailure 1)
  B.putStr $ encode refs
