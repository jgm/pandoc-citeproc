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
import Data.List (intersperse)
import Data.Maybe
import Data.Char (toLower, isUpper)
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO (stderr, hPutStrLn)
import Control.Monad

main :: IO ()
main = do
  argv <- getArgs
  let (flags, args, errs) = getOpt Permute options argv
  let header = "Usage: bibtex2pandoc [OPTION..] [FILE]"
  unless (null errs && length args < 2) $ do
    hPutStrLn stderr $ usageInfo (unlines $ errs ++ [header]) options
    exitWith $ ExitFailure 1
  when (Version `elem` flags) $ do
    putStrLn $ "bibtex2pandoc " ++ "0.0" -- TODO: showVersion version
    exitWith ExitSuccess
  when (Help `elem` flags) $ do
    putStrLn $ usageInfo header options
    exitWith ExitSuccess
  bibstring <- case args of
                    (x:_) -> readFile x
                    []    -> getContents
  let items = case parse (skippingLeadingSpace file) "stdin" bibstring of
                   Left err -> error (show err)
                   Right xs -> xs
  putStrLn
    $ writeMarkdown def{ writerTemplate = "$titleblock$"
                       , writerStandalone = True }
    $ Pandoc (Meta $ M.fromList [
                     ("references" , MetaList $ map itemToMetaValue items)]
             ) []

data Option =
    Help | Version
  deriving (Ord, Eq, Show)

options :: [OptDescr Option]
options =
  [ Option ['h'] ["help"] (NoArg Help) "show usage information"
  , Option ['V'] ["version"] (NoArg Version) "show program version"
  ]


itemToMetaValue :: T -> MetaValue
itemToMetaValue entry = MetaMap $ M.fromList fs'
  where getField f = maybeToList $ lookup f fs
        fs = map (\(k,v) -> (map toLower k, v)) $ fields entry
        f --> f' = [(f', MetaString x) | x <- getField f]
        f ==> f' = [(f', latex x) | x <- getField f]
        f *=> f' = [(f', toAuthorList $ latex as) | as <- getField f]
        f !=> xs = if null xs
                      then xs
                      else [(f, MetaMap $ M.fromList xs)]
        fs' =
          [("id", MetaString $ trim $ identifier entry)
          ,("type", MetaString
                    $ case map toLower (entryType entry) of
                           "article"       -> "article-journal"
                           "book"          -> "book"
                           "booklet"       -> "pamphlet"
                           "bookinbook"    -> "book"
                           "collection"    -> "book"
                           "electronic"    -> "webpage"
                           "inbook"        -> "chapter"
                           "incollection"  -> "chapter"
                           "inreference "  -> "chapter"
                           "inproceedings" -> "paper-conference"
                           "manual"        -> "book"
                           "mastersthesis" -> "thesis"
                           "misc"          -> "no-type"
                           "mvbook"        -> "book"
                           "mvcollection"  -> "book"
                           "mvproceedings" -> "book"
                           "mvreference"   -> "book"
                           "online"        -> "webpage"
                           "patent"        -> "patent"
                           "periodical"    -> "article-journal"
                           "phdthesis"     -> "thesis"
                           "proceedings"   -> "book"
                           "reference"     -> "book"
                           "report"        -> "report"
                           "suppbook"      -> "chapter"
                           "suppcollection" -> "chapter"
                           "suppperiodical" -> "article-journal"
                           "techreport"    -> "report"
                           "thesis"        -> "thesis"
                           "unpublished"   -> "manuscript"
                           "www"           -> "webpage"
                           _               -> "no-type")
          ] ++ concat
          [ case entryType entry of
                 "phdthesis"     -> "genre" --> "Ph.D. thesis"
                 "mastersthesis" -> "genre" --> "Masters thesis"
                 _               -> []
          , "type" ==> "genre"
          , "title" ==> "title"
          , "booktitle" ==> "container-title"
          , "series" ==> "collection-title"
          , "pages" ==> "page"
          , "volume" ==> "volume"
          , "number" ==> "number"
          , "chapter" ==> "chapter-number"
          , "edition" ==> "edition"
          , "note" ==> "note"
          , "url" --> "url"
          , "journal" ==> "container-title"
          , "school" ==> "publisher"
          , "institution" ==> "publisher"
          , "publisher" ==> "publisher"
          , "address" ==> "publisher-place"
          , "author" *=> "author"
          , "editor" *=> "editor"
          , "howpublished" ==> "note"
          , "issued" !=> concat
             [ "year" ==> "year"
             , "month" ==> "month"
             ]
          ]

toAuthorList :: MetaValue -> MetaValue
toAuthorList (MetaBlocks [Para xs]) =
  MetaList $ map toAuthor $ splitOn [Space, Str "and", Space] xs
toAuthorList (MetaBlocks []) = MetaList []
toAuthorList x = error $ "toAuthorList: " ++ show x

toAuthor :: [Inline] -> MetaValue
toAuthor ils = MetaMap $ M.fromList
  [ ("given", MetaList givens)
  , ("family", family)
  , ("non-dropping-particle", particle) ]
  where endsWithComma (Str zs) = not (null zs) && last zs == ','
        endsWithComma _ = False
        stripComma xs = case reverse xs of
                             (',':ys) -> reverse ys
                             _ -> xs
        (xs, ys) = break endsWithComma ils
        (family, givens, particle) =
           case splitOn [Space] ys of
              ((Str w:ws) : rest) ->
                  ( MetaInlines [Str (stripComma w)]
                  , map MetaInlines $ if null ws then rest else (ws : rest)
                  , MetaInlines xs
                  )
              _ -> case reverse xs of
                        []     -> (MetaInlines [], [], MetaInlines [])
                        (z:zs) -> let (us,vs) = break startsWithCapital zs
                                  in  ( MetaInlines [z]
                                      , map MetaInlines $ splitOn [Space] $ reverse vs
                                      , MetaInlines $ reverse us
                                      )

startsWithCapital :: Inline -> Bool
startsWithCapital (Str (x:_)) = isUpper x
startsWithCapital _           = False

latex :: String -> MetaValue
latex s = MetaBlocks bs
  where Pandoc _ bs = readLaTeX def s

trim :: String -> String
trim = unwords . words
