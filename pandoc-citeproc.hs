module Main where
import Text.CSL.Pandoc (processCites')
import Text.Pandoc.JSON
import Text.Pandoc.Walk
import System.IO (stderr, hPutStrLn)

main :: IO ()
main = toJSONFilter go
  where go :: Pandoc -> IO Pandoc
        go doc = do
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
