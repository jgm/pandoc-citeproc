module Main where
import Text.CSL.Pandoc (processCites')
import Text.Pandoc.Definition
import Text.Pandoc.JSON
import Text.Pandoc.Walk

main :: IO ()
main = toJSONFilter processCites'

