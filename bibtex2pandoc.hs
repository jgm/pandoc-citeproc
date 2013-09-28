-- Experimental:
-- goes from bibtex to yaml directly, without bibutils
-- properly parses LaTeX bibtex fields, including math
-- does not yet support biblatex fields
-- probably does not support bibtex accurately
module Main where
import Text.BibTeX.Entry
import Text.BibTeX.Parse hiding (identifier, entry)
import Text.Parsec.String
import Text.Parsec hiding (optional, (<|>))
import Control.Applicative
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
                                            k' <- transformKey (entryType e')
                                                   (entryType entry) k,
                                           isNothing (lookup k' (fields entry))]
                                              }
       Nothing   -> entry

-- transformKey source target key
-- derived from Appendix C of bibtex manual
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
transformKey x y "author"
  | x `elem` ["mvbook", "book"] &&
    y `elem` ["inbook", "bookinbook", "suppbook"] = ["bookauthor"]
transformKey "mvbook" y z
  | y `elem` ["book", "inbook", "bookinbook", "suppbook"] = standardTrans z
transformKey x y z
  | x `elem` ["mvcollection", "mvreference"] &&
    y `elem` ["collection", "reference", "incollection", "suppbook"] =
    standardTrans z
transformKey "mvproceedings" y z
  | y `elem` ["proceedings", "inproceedings"] = standardTrans z
transformKey "book" y z
  | y `elem` ["inbook", "bookinbook", "suppbook"] = standardTrans z
transformKey x y z
  | x `elem` ["collection", "reference"] &&
    y `elem` ["incollection", "inreference", "suppcollection"] = standardTrans z
transformKey "proceedings" "inproceedings" z = standardTrans z
transformKey "periodical" y z
  | y `elem` ["article", "suppperiodical"] =
  case z of
       "title"          -> ["journaltitle"]
       "subtitle"       -> ["journalsubtitle"]
       "shorttitle"     -> []
       "sorttitle"      -> []
       "indextitle"     -> []
       "indexsorttitle" -> []
transformKey _ _ x                = [x]

standardTrans :: String -> [String]
standardTrans z =
  case z of
       "title"          -> ["maintitle"]
       "subtitle"       -> ["mainsubtitle"]
       "titleaddon"     -> ["maintitleaddon"]
       "shorttitle"     -> []
       "sorttitle"      -> []
       "indextitle"     -> []
       "indexsorttitle" -> []
       _                -> [z]

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
  where combine new old = MetaInlines $ toInlines old ++ fn (toInlines new)
        toInlines (MetaInlines ils) = ils
        toInlines (MetaBlocks [Para ils]) = ils
        toInlines (MetaBlocks [Plain ils]) = ils
        toInlines _ = []

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

isPresent :: String -> BibM Bool
isPresent f = do
  fs <- asks fields
  case lookup f fs of
       Just _   -> return True
       Nothing  -> return False

unTitlecase :: MetaValue -> MetaValue
unTitlecase (MetaInlines ils) = MetaInlines $ untc ils
unTitlecase (MetaBlocks [Para ils]) = MetaBlocks [Para $ untc ils]
unTitlecase (MetaBlocks [Plain ils]) = MetaBlocks [Para $ untc ils]

untc :: [Inline] -> [Inline]
untc [] = []
untc (x:xs) = x : map go xs
  where go (Str ys)     = Str $ map toLower ys
        go z            = z

itemToMetaValue bibtex = bibItem $ do
  getId >>= setRawField "id"
  et <- map toLower `fmap` getEntryType
  let setType = setRawField "type"
  case et of
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
                             setRawField "genre" "Master’s thesis"
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
  opt $ getRawField "type" >>= setRawField "genre"
  hyphenation <- getRawField "hyphenation" <|> return "english"
  let processTitle = if (map toLower hyphenation) `elem`
                        ["american","british","canadian","english",
                         "australian","newzealand","usenglish","ukenglish"]
                     then unTitlecase
                     else id
  opt $ getField "title" >>= setField "title" . processTitle
  opt $ getField "subtitle" >>= appendField "title" addColon . processTitle
  opt $ getField "titleaddon" >>= appendField "title" addPeriod . processTitle
  opt $ getField "maintitle" >>= setField "container-title" . processTitle
  opt $ getField "mainsubtitle" >>=
        appendField "container-title" addColon . processTitle
  opt $ getField "maintitleaddon" >>=
             appendField "container-title" addPeriod . processTitle
  hasMaintitle <- isPresent "maintitle"
  opt $ getField "booktitle" >>=
             setField (if hasMaintitle &&
                          et `elem` ["inbook","incollection","inproceedings"]
                       then "volume-title"
                       else "container-title") . processTitle
  opt $ getField "booksubtitle" >>=
             appendField (if hasMaintitle &&
                             et `elem` ["inbook","incollection","inproceedings"]
                          then "volume-title"
                          else "container-title") addColon . processTitle
  opt $ getField "booktitleaddon" >>=
             appendField (if hasMaintitle &&
                             et `elem` ["inbook","incollection","inproceedings"]
                          then "volume-title"
                          else "container-title") addPeriod . processTitle
  opt $ getField "shorttitle" >>= setField "title-short" . processTitle
  opt $ getField "series" >>= setField "collection-title" . processTitle
  opt $ getField "pages" >>= setField "page"
  opt $ getField "volume" >>= setField "volume"
  opt $ getField "number" >>= setField "number"
  opt $ getField "chapter" >>= setField "chapter-number"
  opt $ getField "edition" >>= setField "edition"
  opt $ getField "note" >>= setField "note"
  opt $ getRawField "url" >>= setRawField "url"
  opt $ getField "journal" >>= setField "container-title"
  opt $ getField "journaltitle" >>= setField "container-title"
  opt $ getField "journalsubtitle" >>= appendField "container-title" addColon
  opt $ getField "shortjournal" >>= setField "container-title-short"
  opt $ getField "howpublished" >>= setField "publisher"
  opt $ getField "school" >>= setField "publisher"
  unless bibtex $ do
    opt $ getLiteralList "institution" >>= setList "publisher"
  opt $ getField "publisher" >>= setField "publisher"
  opt $ getField "address" >>= setField "publisher-place"
  unless bibtex $ do
    opt $ getField "location" >>= setField "publisher-place"
  opt $ getAuthorList "author" >>= setList "author"
  opt $ getAuthorList "editor" >>= setList "editor"
  opt $ getField "abstract" >>= setField "abstract"
  unless bibtex $ do
    opt $ getField "addendum" >>= appendField "note" (Space:)
  opt $ getField "annotation" >>= setField "annote"
  opt $ getField "annote" >>= setField "annote"
  opt $ getField "year" >>= setSubField "issued" "year"
  opt $ getField "month" >>= setSubField "issued" "month"
  opt $ getAuthorList "translator" >>= setList "translator"
  opt $ getAuthorList "bookauthor" >>= setList "container-author"
  opt $ getField "abstract" >>= setField "abstract"
  opt $ getField "keywords" >>= setField "keyword"
  opt $ getField "eventdate" >>= setField "event-date"
  opt $ getField "eventtitle" >>= setField "event"
  opt $ getLiteralList "venue" >>= setList "event-place"
  opt $ getRawField "doi" >>= setRawField "DOI"
  opt $ getRawField "isbn" >>= setRawField "ISBN"
  opt $ getRawField "issn" >>= setRawField "ISSN"
  opt $ getField "origdate" >>= setField "original-date"
  opt $ getLiteralList "origlocation" >>=
             setList "original-publisher-place"
  opt $ getLiteralList "origpublisher" >>= setList "original-publisher"
  opt $ getField "origtitle" >>= setField "original-title"
  opt $ getField "pagetotal" >>= setField "number-of-pages"
  opt $ getField "pubstate" >>= setField "status"
  opt $ getField "urldate" >>= setField "accessed"
  opt $ getField "version" >>= setField "version"
  opt $ getField "volumes" >>= setField "number-of-volumes"
  opt $ getRawField "hyphenation" >>= setRawField "language"

addColon :: [Inline] -> [Inline]
addColon xs = [Str ":",Space] ++ xs

addPeriod :: [Inline] -> [Inline]
addPeriod xs = [Str ".",Space] ++ xs

inParens :: [Inline] -> [Inline]
inParens xs = [Space, Str "("] ++ xs ++ [Str ")"]

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
toAuthor [Span ("",[],[]) ils] = -- corporate author
  MetaMap $ M.singleton "literal" $ MetaInlines ils
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
