-- Experimental:
-- goes from bibtex to yaml directly, without bibutils
-- properly parses LaTeX bibtex fields, including math
-- does not yet support biblatex fields
-- probably does not support bibtex accurately

import Text.BibTeX.Entry
import Text.BibTeX.Parse hiding (identifier)
import Text.Parsec.String
import Text.Parsec
import Text.Pandoc
import qualified Data.Map as M
import Data.List.Split (splitOn)
import Data.Maybe
import Data.Char (toLower)

main = do
  inp <- getContents
  let items = case parse (skippingLeadingSpace file) "stdin" inp of
                   Left err -> error (show err)
                   Right xs -> xs
  print items
  putStrLn
    $ writeMarkdown def{ writerTemplate = "$titleblock$"
                       , writerStandalone = True }
    $ Pandoc (Meta $ M.fromList [
                     ("references" , MetaList $ map itemToMetaValue items)]
             ) []

itemToMetaValue :: T -> MetaValue
itemToMetaValue entry = MetaMap $ M.fromList fs'
  where getField f = maybeToList $ lookup f fs
        fs = map (\(k,v) -> (map toLower k, v)) $ fields entry
        f --> f' = [(f', MetaString x) | x <- getField f]
        f ==> f' = [(f', latex x) | x <- getField f]
        f *=> f' = [(f', toAuthorList $ latex as) | as <- getField f]
        fs' =
          [("id", MetaString $ identifier entry)
          ,("type", MetaString $ readType $ map toLower $ entryType entry)
          ] ++
          (case entryType entry of
                "phdthesis"     -> "genre" --> "Ph.D. thesis"
                "mastersthesis" -> "genre" --> "Masters thesis"
                _               -> []) ++
          "title" ==> "title" ++
          "booktitle" ==> "container-title" ++
          "series" ==> "collection-title" ++
          "pages" ==> "page" ++
          "volume" ==> "volume" ++
          "number" ==> "number" ++
          "chapter" ==> "chapter-number" ++
          "edition" ==> "edition" ++
          "note" ==> "note" ++
          "url" --> "url" ++
          "journal" ==> "container-title" ++
          "school" ==> "publisher" ++
          "institution" ==> "publisher" ++
          "publisher" ==> "publisher" ++
          "address" ==> "publisher-place" ++
          "author" *=> "author" ++
          "editor" *=> "editor" ++
          [("issued", MetaMap $ M.fromList $
             "year" ==> "year" ++
             "month" ==> "month")
          ]

toAuthorList :: MetaValue -> MetaValue
toAuthorList (MetaBlocks [Para xs]) =
  MetaList $ map toAuthor $ splitOn [Space, Str "and", Space] xs
toAuthorList (MetaBlocks []) = MetaList []
toAuthorList x = error $ "toAuthorList: " ++ show x

toAuthor :: [Inline] -> MetaValue
toAuthor ils = MetaMap $ M.fromList
  [ ("given", MetaList givens), ("family", family) ]
  where endsWithComma (Str xs) = not (null xs) && last xs == ','
        endsWithComma _ = False
        stripComma xs = case reverse xs of
                             (',':ys) -> reverse ys
                             _ -> xs
        (xs, ys) = break endsWithComma ils
        (family, givens) =
           case ys of
              (Str ws : rest) ->
                  (MetaInlines $ xs ++ [Str (stripComma ws)], map MetaInlines
                                                              $ splitOn [Space]
                                                              $ dropWhile (== Space) rest)
              _ -> case reverse $ splitOn [Space] xs of
                        []     -> (MetaInlines [Str ""], [])
                        (z:zs) -> (MetaInlines z, map MetaInlines $ reverse zs)

latex :: String -> MetaValue
latex s = MetaBlocks bs
  where Pandoc _ bs = readLaTeX def s

readType :: String -> String
readType "article" = "article-journal"
readType "book"    = "book"
readType "booklet" = "pamphlet"
readType "inbook"  = "chapter"
readType "incollection" = "chapter"
readType "inproceedings" = "paper-conference"
readType "manual" = "book"
readType "mastersthesis" = "thesis"
readType "misc" = "no-type"
readType "phdthesis" = "thesis"
readType "proceedings" = "book"
readType "techreport" = "report"
readType "unpublished" = "manuscript"
readType _ = "no-type"

