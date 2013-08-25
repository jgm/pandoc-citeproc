module Main where
import Text.CSL.Input.MODS (readModsCollection, readModsCollectionFile)
import Text.CSL.Pandoc ()
import System.IO
import System.Environment
import System.Exit
import Data.Yaml
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = do
  args <- getArgs
  refs <- case args of
               [] -> readModsCollection `fmap` BL.getContents
               [x] | x /= "-h" && x /= "--help" ->
                 readModsCollectionFile x
               _    -> do
                 hPutStrLn stderr $ "Usage: mods2yaml MODSFILE"
                 exitWith (ExitFailure 1)
  B.putStr $ encode refs
