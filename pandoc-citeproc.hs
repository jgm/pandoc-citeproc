module Main where
import Text.CSL.Pandoc (processCites')
import Text.Pandoc.JSON

main :: IO ()
main = toJSONFilter processCites'

