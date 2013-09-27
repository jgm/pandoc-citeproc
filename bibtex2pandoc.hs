-- Experimental:
-- goes from bibtex to yaml directly, without bibutils
-- properly parses LaTeX bibtex fields, including math
-- does not yet support biblatex fields
-- probably does not support bibtex accurately
module Main where
import Text.BibTeX.Entry
import Text.BibTeX.Parse hiding (identifier, entry)
import Text.Parsec.String
import Text.Parsec hiding (optional)
import Text.Pandoc.Walk (query)
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
import Control.Monad.RWS.Strict

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
  let isBibtex = Bibtex `elem` flags
  bibstring <- case args of
                    (x:_) -> readFile x
                    []    -> getContents
  let items = case parse (skippingLeadingSpace file) "stdin" bibstring of
                   Left err -> error (show err)
                   Right xs -> resolveCrossRefs isBibtex
                                  $ map lowercaseFieldNames xs
  putStrLn
    $ writeMarkdown def{ writerTemplate = "$titleblock$"
                       , writerStandalone = True }
    $ Pandoc (Meta $ M.fromList [
                     ("references" , MetaList
                                    $ map (itemToMetaValue isBibtex) items)]
             ) []

data Option =
    Help | Version | Bibtex
  deriving (Ord, Eq, Show)

options :: [OptDescr Option]
options =
  [ Option ['b'] ["bibtex"] (NoArg Bibtex) "parse as BibTeX, not BibLaTeX"
  , Option ['h'] ["help"] (NoArg Help) "show usage information"
  , Option ['V'] ["version"] (NoArg Version) "show program version"
  ]

lowercaseFieldNames :: T -> T
lowercaseFieldNames e = e{ fields = [(map toLower f, v) | (f,v) <- fields e] }

resolveCrossRefs :: Bool -> [T] -> [T]
resolveCrossRefs isBibtex entries =
  map (resolveCrossRef isBibtex entries) entries

resolveCrossRef :: Bool -> [T] -> T -> T
resolveCrossRef isBibtex entries entry =
  case lookup "crossref" (fields entry) of
       Just xref -> case [e | e <- entries, identifier e == xref] of
                         []     -> entry
                         (e':_)
                          | isBibtex -> entry{ fields = fields entry ++
                                           [(k,v) | (k,v) <- fields e',
                                            isNothing (lookup k $ fields entry)]
                                        }
                          | otherwise -> entry{ fields = fields entry ++
                                           [(k',v) | (k,v) <- fields e',
                                            isNothing (lookup k $ fields entry),
                                            k' <- transformKey
                                                   (entryType entry)
                                                   (entryType e') k]
                                              }
       Nothing   -> entry

-- transformKey source target key
transformKey :: String -> String -> String -> [String]
transformKey _ _ "crossref"       = []
transformKey _ _ "xref"           = []
transformKey _ _ "entryset"       = []
transformKey _ _ "entrysubtype"   = []
transformKey _ _ "execute"        = []
transformKey _ _ "label"          = []
transformKey _ _ "options"        = []
transformKey _ _ "presort"        = []
transformKey _ _ "related"        = []
transformKey _ _ "relatedstring"  = []
transformKey _ _ "relatedtype"    = []
transformKey _ _ "shorthand"      = []
transformKey _ _ "shorthandintro" = []
transformKey _ _ "sortkey"        = []
transformKey _ _ x                = [x]

type BibM = RWST T () (M.Map String MetaValue) Maybe

opt :: BibM () -> BibM ()
opt m = m `mplus` return ()

getField :: String -> BibM MetaValue
getField f = do
  fs <- asks fields
  case lookup f fs of
       Just x  -> return $ latex x
       Nothing -> fail "not found"

setField :: String -> MetaValue -> BibM ()
setField f x = modify $ M.insert f x

appendField :: String -> ([Inline] -> [Inline]) -> MetaValue -> BibM ()
appendField f fn x = modify $ M.insertWith combine f x
  where combine new old = MetaInlines $ query (:[]) old ++ fn (query (:[]) new)

notFound :: String -> BibM a
notFound f = fail $ f ++ " not found"

getId :: BibM String
getId = asks identifier

getRawField :: String -> BibM String
getRawField f = do
  fs <- asks fields
  case lookup f fs of
       Just x  -> return x
       Nothing -> notFound f

setRawField :: String -> String -> BibM ()
setRawField f x = modify $ M.insert f (MetaString x)

getAuthorList :: String -> BibM [MetaValue]
getAuthorList f = do
  fs <- asks fields
  case lookup f fs of
       Just x  -> return $ toAuthorList $ latex x
       Nothing -> notFound f

getLiteralList :: String -> BibM [MetaValue]
getLiteralList f = do
  fs <- asks fields
  case lookup f fs of
       Just x  -> return $ toLiteralList $ latex x
       Nothing -> notFound f

setList :: String -> [MetaValue] -> BibM ()
setList f xs = modify $ M.insert f $ MetaList xs

setSubField :: String -> String -> MetaValue -> BibM ()
setSubField f k v = do
  fs <- get
  case M.lookup f fs of
       Just (MetaMap m) -> modify $ M.insert f (MetaMap $ M.insert k v m)
       _ -> modify $ M.insert f (MetaMap $ M.singleton k v)

bibItem :: BibM a -> T -> MetaValue
bibItem m entry = MetaMap $ maybe M.empty fst $ execRWST m entry M.empty

getEntryType :: BibM String
getEntryType = asks entryType

infix 7 ==>

(==>) :: BibM a -> (a -> BibM ()) -> BibM ()
x ==> y = (x >>= y) `mplus` return ()

itemToMetaValue bibtex = bibItem $ do
  getId ==> setRawField "id"
  et <- getEntryType
  let setType = setRawField "type"
  case map toLower et of
       "article"         -> setType "article-journal"
       "book"            -> setType "book"
       "booklet"         -> setType "pamphlet"
       "bookinbook"      -> setType "book"
       "collection"      -> setType "book"
       "electronic"      -> setType "webpage"
       "inbook"          -> setType "chapter"
       "incollection"    -> setType "chapter"
       "inreference "    -> setType "chapter"
       "inproceedings"   -> setType "paper-conference"
       "manual"          -> setType "book"
       "mastersthesis"   -> setType "thesis" >>
                             setRawField "genre" "Ph.D. thesis"
       "misc"            -> setType "no-type"
       "mvbook"          -> setType "book"
       "mvcollection"    -> setType "book"
       "mvproceedings"   -> setType "book"
       "mvreference"     -> setType "book"
       "online"          -> setType "webpage"
       "patent"          -> setType "patent"
       "periodical"      -> setType "article-journal"
       "phdthesis"       -> setType "thesis" >>
                             setRawField "genre" "Ph.D. thesis"
       "proceedings"     -> setType "book"
       "reference"       -> setType "book"
       "report"          -> setType "report"
       "suppbook"        -> setType "chapter"
       "suppcollection"  -> setType "chapter"
       "suppperiodical"  -> setType "article-journal"
       "techreport"      -> setType "report"
       "thesis"          -> setType "thesis"
       "unpublished"     -> setType "manuscript"
       "www"             -> setType "webpage"
       _                 -> setType "no-type"
  getRawField "type" ==> setRawField "genre"
  getField "title" ==> setField "title"
  getField "subtitle" ==> appendField "title" ([Str ":",Space] ++)
  getField "titleaddon" ==> appendField "title"
                                 (\x -> [Space,Str "("] ++ x ++ [Str ")"])
  getField "booktitle" ==> setField "container-title"
  getField "series" ==> setField "collection-title"
  getField "pages" ==> setField "page"
  getField "volume" ==> setField "volume"
  getField "number" ==> setField "number"
  getField "chapter" ==> setField "chapter-number"
  getField "edition" ==> setField "edition"
  getField "note" ==> setField "note"
  getRawField "url" ==> setRawField "url"
  getField "journal" ==> setField "container-title"
  getField "school" ==> setField "publisher"
  getLiteralList "institution" ==> setList "publisher"
  getField "address" ==> setField "publisher-place"
  getAuthorList "author" ==> setList "author"
  getAuthorList "editor" ==> setList "editor"
  getField "howpublished" ==> setField "note"
  getField "abstract" ==> setField "abstract"
  unless bibtex $ do
    getField "addendum" ==> appendField "note" (Space:)
  getField "annotation" ==> setField "annote"
  getField "year" ==> setSubField "issued" "year"
  getField "month" ==> setSubField "issued" "month"



splitByAnd :: [Inline] -> [[Inline]]
splitByAnd = splitOn [Space, Str "and", Space]

toLiteralList :: MetaValue -> [MetaValue]
toLiteralList (MetaBlocks [Para xs]) =
  map MetaInlines $ splitByAnd xs
toLiteralList (MetaBlocks []) = []
toLiteralList x = error $ "toLiteralList: " ++ show x

toAuthorList :: MetaValue -> [MetaValue]
toAuthorList (MetaBlocks [Para xs]) =
  map toAuthor $ splitByAnd xs
toAuthorList (MetaBlocks []) = []
toAuthorList x = error $ "toAuthorList: " ++ show x

toAuthor :: [Inline] -> MetaValue
toAuthor ils = MetaMap $ M.fromList $
  [ ("given", MetaList givens)
  , ("family", family)
  ] ++ case particle of
            MetaInlines [] -> []
            _              -> [("non-dropping-particle", particle)]
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
                                      , MetaInlines $ dropWhile (==Space) $ reverse us
                                      )

startsWithCapital :: Inline -> Bool
startsWithCapital (Str (x:_)) = isUpper x
startsWithCapital _           = False

latex :: String -> MetaValue
latex s = MetaBlocks bs
  where Pandoc _ bs = readLaTeX def s

trim :: String -> String
trim = unwords . words
