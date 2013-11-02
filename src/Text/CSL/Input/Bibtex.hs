{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.CSL.Input.Bibtex
-- Copyright   :  (c) John MacFarlane
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  John MacFarlane <fiddlosopher@gmail.com>
-- Stability   :  unstable-- Portability :  unportable
--
-----------------------------------------------------------------------------

module Text.CSL.Input.Bibtex
    ( readBibtexInput
    , readBibtexInputString
    )
    where

import Text.Parsec hiding (optional, (<|>), many, State)
import Control.Applicative
import Text.Pandoc
import Text.Pandoc.Shared (stringify)
import Data.List.Split (splitOn, splitWhen, wordsBy)
import Data.List (intercalate)
import Data.Maybe
import Data.Char (toLower, isUpper, toUpper, isDigit, isAlphaNum)
import Control.Monad
import Control.Monad.RWS
import System.Environment (getEnvironment)
import Text.CSL.Reference
import Text.CSL.Util (trim, onBlocks, unTitlecase, protectCase, splitStrWhen)
import qualified Text.Pandoc.Walk as Walk

blocksToFormatted  :: [Block]  -> Bib Formatted
blocksToFormatted bs =
  case bs of
       [Plain xs]  -> inlinesToFormatted xs
       [Para  xs]  -> inlinesToFormatted xs
       _           -> inlinesToFormatted $ Walk.query (:[]) bs

adjustSpans :: Lang -> Inline -> [Inline]
adjustSpans _ (Span ("",[],[]) xs) = xs
adjustSpans lang (RawInline (Format "latex") s)
  | s == "\\hyphen" = [Str "-"]
  | otherwise = bottomUp (concatMap (adjustSpans lang)) $ parseRawLaTeX lang s
adjustSpans _ (SmallCaps xs) =
  [Span ("",[],[("style","font-variant:small-caps;")]) xs]
adjustSpans _ x = [x]

parseRawLaTeX :: Lang -> String -> [Inline]
parseRawLaTeX lang ('\\':xs) =
  case readLaTeX def{readerParseRaw = True} contents of
       Pandoc _ [Para ys]  -> f command ys
       Pandoc _ [Plain ys] -> f command ys
       _                   -> []
   where (command', contents') = break (=='{') xs
         command  = trim command'
         contents = drop 1 $ reverse $ drop 1 $ reverse contents'
         f "mkbibquote" ils = [Quoted DoubleQuote ils]
         f "textnormal" ils = [Span ("",[],[("style","font-style:normal;")]) ils]
         f "bibstring" [Str s] = [Str $ resolveKey' lang s]
         f _            ils = [Span nullAttr ils]
parseRawLaTeX _ _ = []

inlinesToFormatted :: [Inline] -> Bib Formatted
inlinesToFormatted ils = do
  lang <- gets localeLang
  return $ Formatted $ bottomUp (concatMap (adjustSpans lang)) ils

inlinesToString :: [Inline] -> Bib String
inlinesToString ils = (stringify . unFormatted) <$> inlinesToFormatted ils

data Item = Item{ identifier :: String
                , entryType  :: String
                , fields     :: [(String, String)]
                }

readBibtexInput :: Bool -> FilePath -> IO [Reference]
readBibtexInput isBibtex f = readFile f >>= readBibtexInputString isBibtex

readBibtexInputString :: Bool -> String -> IO [Reference]
readBibtexInputString isBibtex bibstring = do
  env <- getEnvironment
  let lang = case lookup "LANG" env of
                  Just x  -> case splitWhen (\c -> c == '.' || c == '_') x of
                                   (w:z:_) -> Lang w z
                                   [w]     -> Lang w mempty
                                   _       -> Lang "en" "US"
                  Nothing -> Lang "en" "US"
  let items = case runParser (bibEntries <* eof) [] "stdin" bibstring of
                   Left err -> error (show err)
                   Right xs -> resolveCrossRefs isBibtex xs
  return $ mapMaybe (itemToReference lang isBibtex) items

type BibParser = Parsec [Char] [(String, String)]

bibEntries :: BibParser [Item]
bibEntries = many (try (skipMany nonEntry >> bibItem)) <* skipMany nonEntry
  where nonEntry = bibSkip <|> bibComment <|> bibPreamble <|> bibString

bibSkip :: BibParser ()
bibSkip = skipMany1 (satisfy (/='@'))

bibComment :: BibParser ()
bibComment = try $ do
  char '@'
  cistring "comment"
  skipMany (satisfy (/='\n'))

bibPreamble :: BibParser ()
bibPreamble = try $ do
  char '@'
  cistring "preamble"
  spaces
  void inBraces
  return ()

bibString :: BibParser ()
bibString = try $ do
  char '@'
  cistring "string"
  spaces
  char '{'
  spaces
  f <- entField
  spaces
  char '}'
  updateState $ (f:)
  return ()

inBraces :: BibParser String
inBraces = try $ do
  char '{'
  res <- manyTill
         (  many1 (noneOf "{}\\")
        <|> (char '\\' >> (  (char '{' >> return "\\{")
                         <|> (char '}' >> return "\\}")
                         <|> return "\\"))
        <|> (braced <$> inBraces)
         ) (char '}')
  return $ concat res

braced :: String -> String
braced s = "{" ++ s ++ "}"

inQuotes :: BibParser String
inQuotes = do
  char '"'
  concat <$> manyTill (try (string "\\\"")
                     <|> many1 (noneOf "\"\\")
                     <|> count 1 anyChar) (char '"')

fieldName :: BibParser String
fieldName = (map toLower) <$> many1 (letter <|> digit <|> oneOf "-_")

isBibtexKeyChar :: Char -> Bool
isBibtexKeyChar c = isAlphaNum c || c `elem` ".:;?!`'()/*@_+=-[]*"

bibItem :: BibParser Item
bibItem = do
  char '@'
  enttype <- map toLower <$> many1 letter
  spaces
  char '{'
  spaces
  entid <- many (satisfy isBibtexKeyChar)
  spaces
  char ','
  spaces
  entfields <- entField `sepEndBy` (char ',')
  spaces
  char '}'
  return $ Item entid enttype entfields

entField :: BibParser (String, String)
entField = try $ do
  spaces
  k <- fieldName
  spaces
  char '='
  spaces
  vs <- (expandString <|> inQuotes <|> inBraces <|> rawWord) `sepBy`
            (try $ spaces >> char '#' >> spaces)
  spaces
  return (k, concat vs)

rawWord :: BibParser String
rawWord = many1 alphaNum

expandString :: BibParser String
expandString = do
  k <- fieldName
  strs <- getState
  case lookup k strs of
       Just v  -> return v
       Nothing -> return k -- return raw key if not found

cistring :: String -> BibParser String
cistring [] = return []
cistring (c:cs) = do
  x <- (char (toLower c) <|> char (toUpper c))
  xs <- cistring cs
  return (x:xs)

resolveCrossRefs :: Bool -> [Item] -> [Item]
resolveCrossRefs isBibtex entries =
  map (resolveCrossRef isBibtex entries) entries

splitKeys :: String -> [String]
splitKeys = wordsBy (\c -> c == ' ' || c == ',')

getXrefFields :: Bool -> Item -> [Item] -> String -> [(String, String)]
getXrefFields isBibtex baseEntry entries keys = do
  let keys' = splitKeys keys
  xrefEntry <- [e | e <- entries, identifier e `elem` keys']
  (k, v) <- fields xrefEntry
  if k == "crossref" || k == "xdata"
     then do
       xs <- mapM (getXrefFields isBibtex baseEntry entries)
                   (splitKeys v)
       (x, y) <- xs
       guard $ isNothing $ lookup x $ fields xrefEntry
       return (x, y)
     else do
       k' <- if isBibtex
                then return k
                else transformKey (entryType xrefEntry) (entryType baseEntry) k
       guard $ isNothing $ lookup k' $ fields baseEntry
       return (k',v)

resolveCrossRef :: Bool -> [Item] -> Item -> Item
resolveCrossRef isBibtex entries entry = foldl go entry (fields entry)
  where go entry' (key, val) =
          if key == "crossref" || key == "xdata"
          then entry'{ fields = fields entry' ++
                                    getXrefFields isBibtex entry entries val }
          else entry'

-- transformKey source target key
-- derived from Appendix C of bibtex manual
transformKey :: String -> String -> String -> [String]
transformKey _ _ "ids"            = []
transformKey _ _ "crossref"       = []
transformKey _ _ "xref"           = []
transformKey _ _ "entryset"       = []
transformKey _ _ "entrysubtype"   = []
transformKey _ _ "execute"        = []
transformKey _ _ "label"          = []
transformKey _ _ "options"        = []
transformKey _ _ "presort"        = []
transformKey _ _ "related"        = []
transformKey _ _ "relatedoptions" = []
transformKey _ _ "relatedstring"  = []
transformKey _ _ "relatedtype"    = []
transformKey _ _ "shorthand"      = []
transformKey _ _ "shorthandintro" = []
transformKey _ _ "sortkey"        = []
transformKey x y "author"
  | x `elem` ["mvbook", "book"] &&
    y `elem` ["inbook", "bookinbook", "suppbook"] = ["bookauthor", "author"]
-- note: this next clause is not in the biblatex manual, but it makes
-- sense in the context of CSL conversion:
transformKey x y "author"
  | x == "mvbook" && y == "book" = ["bookauthor", "author"]
transformKey "mvbook" y z
  | y `elem` ["book", "inbook", "bookinbook", "suppbook"] = standardTrans z
transformKey x y z
  | x `elem` ["mvcollection", "mvreference"] &&
    y `elem` ["collection", "reference", "incollection", "inreference",
               "suppcollection"] = standardTrans z
transformKey "mvproceedings" y z
  | y `elem` ["proceedings", "inproceedings"] = standardTrans z
transformKey "book" y z
  | y `elem` ["inbook", "bookinbook", "suppbook"] = bookTrans z
transformKey x y z
  | x `elem` ["collection", "reference"] &&
    y `elem` ["incollection", "inreference", "suppcollection"] = bookTrans z
transformKey "proceedings" "inproceedings" z = bookTrans z
transformKey "periodical" y z
  | y `elem` ["article", "suppperiodical"] =
  case z of
       "title"          -> ["journaltitle"]
       "subtitle"       -> ["journalsubtitle"]
       "shorttitle"     -> []
       "sorttitle"      -> []
       "indextitle"     -> []
       "indexsorttitle" -> []
       _                -> [z]
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

bookTrans :: String -> [String]
bookTrans z =
  case z of
       "title"          -> ["booktitle"]
       "subtitle"       -> ["booksubtitle"]
       "titleaddon"     -> ["booktitleaddon"]
       "shorttitle"     -> []
       "sorttitle"      -> []
       "indextitle"     -> []
       "indexsorttitle" -> []
       _                -> [z]

data Lang = Lang String String  -- e.g. "en" "US"

resolveKey :: Lang -> Formatted -> Formatted
resolveKey lang (Formatted ils) = Formatted (Walk.walk go ils)
  where go (Str s) = Str $ resolveKey' lang s
        go x       = x

resolveKey' :: Lang -> String -> String
resolveKey' (Lang "en" "US") k =
  case k of
       "inpreparation" -> "in preparation"
       "submitted"     -> "submitted"
       "forthcoming"   -> "forthcoming"
       "inpress"       -> "in press"
       "prepublished"  -> "pre-published"
       "mathesis"      -> "Master’s thesis"
       "phdthesis"     -> "PhD thesis"
       "candthesis"    -> "Candidate thesis"
       "techreport"    -> "technical report"
       "resreport"     -> "research report"
       "software"      -> "computer software"
       "datacd"        -> "data CD"
       "audiocd"       -> "audio CD"
       "patent"        -> "patent"
       "patentde"      -> "German patent"
       "patenteu"      -> "European patent"
       "patentfr"      -> "French patent"
       "patentuk"      -> "British patent"
       "patentus"      -> "U.S. patent"
       "patreq"        -> "patent request"
       "patreqde"      -> "German patent request"
       "patreqeu"      -> "European patent request"
       "patreqfr"      -> "French patent request"
       "patrequk"      -> "British patent request"
       "patrequs"      -> "U.S. patent request"
       "countryde"     -> "Germany"
       "countryeu"     -> "European Union"
       "countryep"     -> "European Union"
       "countryfr"     -> "France"
       "countryuk"     -> "United Kingdom"
       "countryus"     -> "United States of America"
       "newseries"     -> "new series"
       "oldseries"     -> "old series"
       _               -> k
resolveKey' _ k = resolveKey' (Lang "en" "US") k

parseMonth :: String -> String
parseMonth "jan" = "1"
parseMonth "feb" = "2"
parseMonth "mar" = "3"
parseMonth "apr" = "4"
parseMonth "may" = "5"
parseMonth "jun" = "6"
parseMonth "jul" = "7"
parseMonth "aug" = "8"
parseMonth "sep" = "9"
parseMonth "oct" = "10"
parseMonth "nov" = "11"
parseMonth "dec" = "12"
parseMonth x     = x

data BibState = BibState{
           untitlecase  :: Bool
         , localeLang   :: Lang
         }

type Bib = RWST Item () BibState Maybe

notFound :: String -> Bib a
notFound f = fail $ f ++ " not found"

getField :: String -> Bib Formatted
getField f = do
  fs <- asks fields
  case lookup f fs of
       Just x  -> latex x
       Nothing -> notFound f

getPeriodicalTitle :: String -> Bib Formatted
getPeriodicalTitle f = do
  fs <- asks fields
  case lookup f fs of
       Just x  -> blocksToFormatted $ onBlocks protectCase $ latex' $ trim x
       Nothing -> notFound f

getTitle :: String -> Bib Formatted
getTitle f = do
  fs <- asks fields
  case lookup f fs of
       Just x  -> latexTitle x
       Nothing -> notFound f

getShortTitle :: Bool -> String -> Bib Formatted
getShortTitle requireColon f = do
  fs <- asks fields
  utc <- gets untitlecase
  let processTitle = if utc then onBlocks unTitlecase else id
  case lookup f fs of
       Just x  -> case processTitle $ latex' x of
                       bs | not requireColon || containsColon bs ->
                                  blocksToFormatted $ upToColon bs
                          | otherwise -> return mempty
       Nothing -> notFound f

containsColon :: [Block] -> Bool
containsColon [Para  xs] = (Str ":") `elem` xs
containsColon [Plain xs] = containsColon [Para xs]
containsColon _          = False

upToColon :: [Block] -> [Block]
upToColon [Para  xs] = [Para $ takeWhile (/= (Str ":")) xs]
upToColon [Plain xs] = upToColon [Para xs]
upToColon bs         = bs

getDates :: String -> Bib [RefDate]
getDates f = getRawField f >>= parseDates

parseDates :: Monad m => String-> m [RefDate]
parseDates = mapM parseDate . splitWhen (== '/')

parseDate :: Monad m => String -> m RefDate
parseDate s = do
  let (year', month', day') =
        case splitWhen (== '-') s of
             [y]     -> (y, mempty, mempty)
             [y,m]   -> (y, m, mempty)
             [y,m,d] -> (y, m, d)
             _       -> (mempty, mempty, mempty)
  return RefDate { year   = dropWhile (=='0') year'
                 , month  = dropWhile (=='0') month'
                 , season = mempty
                 , day    = dropWhile (=='0') day'
                 , other  = mempty
                 , circa  = mempty
                 }

isNumber :: String -> Bool
isNumber ('-':d:ds) = all isDigit (d:ds)
isNumber (d:ds)     = all isDigit (d:ds)
isNumber _          = False

-- A negative (BC) year might be written with -- or --- in bibtex:
fixLeadingDash :: String -> String
fixLeadingDash (c:d:ds)
  | (c == '–' || c == '—') && isDigit d = '-':d:ds
fixLeadingDash xs = xs

getOldDates :: String -> Bib [RefDate]
getOldDates prefix = do
  year' <- fixLeadingDash <$> getRawField (prefix ++ "year")
  month' <- (parseMonth <$> getRawField (prefix ++ "month")) <|> return ""
  day' <- getRawField (prefix ++ "day") <|> return mempty
  endyear' <- fixLeadingDash <$> getRawField (prefix ++ "endyear") <|> return ""
  endmonth' <- getRawField (prefix ++ "endmonth") <|> return ""
  endday' <- getRawField (prefix ++ "endday") <|> return ""
  let start' = RefDate { year   = if isNumber year' then year' else ""
                       , month  = month'
                       , season = ""
                       , day    = day'
                       , other  = if isNumber year' then "" else year'
                       , circa  = ""
                       }
  let end' = if null endyear'
                then []
                else [RefDate { year   = if isNumber endyear' then endyear' else ""
                              , month  = endmonth'
                              , day    = endday'
                              , season = mempty
                              , other  = if isNumber endyear' then "" else endyear'
                              , circa  = mempty
                              }]
  return (start':end')

getRawField :: String -> Bib String
getRawField f = (stringify . unFormatted) <$> getField f

getAuthorList :: Options -> String -> Bib [Agent]
getAuthorList opts  f = do
  fs <- asks fields
  case lookup f fs of
       Just x  -> latexAuthors opts x
       Nothing -> notFound f

getLiteralList :: String -> Bib [Formatted]
getLiteralList f = do
  fs <- asks fields
  case lookup f fs of
       Just x  -> toLiteralList $ latex' x
       Nothing -> notFound f

-- separates items with semicolons
getLiteralList' :: String -> Bib Formatted
getLiteralList' f = (Formatted . intercalate [Str ";", Space] . map unFormatted)
  <$> getLiteralList f

splitByAnd :: [Inline] -> [[Inline]]
splitByAnd = splitOn [Space, Str "and", Space]

toLiteralList :: [Block] -> Bib [Formatted]
toLiteralList [Para xs] =
  mapM inlinesToFormatted $ splitByAnd xs
toLiteralList [Plain xs] = toLiteralList [Para xs]
toLiteralList _ = mzero

toAuthorList :: Options -> [Block] -> Bib [Agent]
toAuthorList opts [Para xs] =
  mapM (toAuthor opts) $ splitByAnd xs
toAuthorList opts [Plain xs] = toAuthorList opts [Para xs]
toAuthorList _ _ = mzero

toAuthor :: Options -> [Inline] -> Bib Agent
toAuthor _ [Str "others"] = return $
    Agent { givenName       = []
          , droppingPart    = mempty
          , nonDroppingPart = mempty
          , familyName      = mempty
          , nameSuffix      = mempty
          , literal         = "others"
          , commaSuffix     = False
          }
toAuthor _ [Span ("",[],[]) ils] = do
  literal' <- inlinesToString ils
  return $ -- corporate author
    Agent { givenName       = []
          , droppingPart    = mempty
          , nonDroppingPart = mempty
          , familyName      = mempty
          , nameSuffix      = mempty
          , literal         = literal'
          , commaSuffix     = False
          }
-- First von Last
-- von Last, First
-- von Last, Jr ,First
toAuthor opts ils = do
  let useprefix = isSet "useprefix" opts
  let usecomma  = isSet "juniorcomma" opts
  let words' = wordsBy (\x -> x == Space || x == Str "\160")
  let commaParts = map words' $ splitWhen (== Str ",")
                              $ splitStrWhen (\c -> c == ',' || c == '\160') ils
  let (first, vonlast, jr) =
          case commaParts of
               --- First is the longest sequence of white-space separated
               -- words starting with an uppercase and that is not the
               -- whole string. von is the longest sequence of whitespace
               -- separated words whose last word starts with lower case
               -- and that is not the whole string.
               [fvl]      -> let (caps', rest') = span isCapitalized fvl
                             in  if null rest' && not (null caps')
                                 then (init caps', [last caps'], [])
                                 else (caps', rest', [])
               [vl,f]     -> (f, vl, [])
               (vl:j:f:_) -> (f, vl, j )
               []         -> ([], [], [])
  let (rlast, rvon) = span isCapitalized $ reverse vonlast
  let (von, lastname) = case (reverse rvon, reverse rlast) of
                             (ws@(_:_),[]) -> (init ws, [last ws])
                             (ws, vs)      -> (ws, vs)
  prefix <- inlinesToString $ intercalate [Space] von
  family <- inlinesToString $ intercalate [Space] lastname
  suffix <- inlinesToString $ intercalate [Space] jr
  givens <- mapM inlinesToString first
  return $
    Agent { givenName       = givens
          , droppingPart    = if useprefix then "" else prefix
          , nonDroppingPart = if useprefix then prefix else ""
          , familyName      = family
          , nameSuffix      = suffix
          , literal         = ""
          , commaSuffix     = usecomma
          }

isCapitalized :: [Inline] -> Bool
isCapitalized (Str (c:cs) : rest)
  | isUpper c = True
  | isDigit c = isCapitalized (Str cs : rest)
  | otherwise = False
isCapitalized (_:rest) = isCapitalized rest
isCapitalized [] = True

isSet :: String -> Options -> Bool
isSet key opts = case lookup key opts of
                      Just "true" -> True
                      Just s      -> s == mempty
                      _           -> False

latex' :: String -> [Block]
latex' s = bs
  where Pandoc _ bs = readLaTeX def{readerParseRaw = True} s

latex :: String -> Bib Formatted
latex s = blocksToFormatted $ latex' $ trim s

latexTitle :: String -> Bib Formatted
latexTitle s = do
  utc <- gets untitlecase
  let processTitle = if utc then onBlocks unTitlecase else id
  blocksToFormatted $ processTitle $ latex' s

latexAuthors :: Options -> String -> Bib [Agent]
latexAuthors opts = toAuthorList opts . latex' . trim

bib :: Bib Reference -> Item -> Maybe Reference
bib m entry = fmap fst $ evalRWST m entry (BibState True (Lang "en" "US"))

toLocale :: String -> String
toLocale "english"    = "en-US" -- "en-EN" unavailable in CSL
toLocale "usenglish"  = "en-US"
toLocale "american"   = "en-US"
toLocale "british"    = "en-GB"
toLocale "ukenglish"  = "en-GB"
toLocale "canadian"   = "en-US" -- "en-CA" unavailable in CSL
toLocale "australian" = "en-GB" -- "en-AU" unavailable in CSL
toLocale "newzealand" = "en-GB" -- "en-NZ" unavailable in CSL
toLocale "afrikaans"  = "af-ZA"
toLocale "arabic"     = "ar"
toLocale "basque"     = "eu"
toLocale "bulgarian"  = "bg-BG"
toLocale "catalan"    = "ca-AD"
toLocale "croatian"   = "hr-HR"
toLocale "czech"      = "cs-CZ"
toLocale "danish"     = "da-DK"
toLocale "dutch"      = "nl-NL"
toLocale "estonian"   = "et-EE"
toLocale "finnish"    = "fi-FI"
toLocale "canadien"   = "fr-CA"
toLocale "acadian"    = "fr-CA"
toLocale "french"     = "fr-FR"
toLocale "francais"   = "fr-FR"
toLocale "austrian"   = "de-AT"
toLocale "naustrian"  = "de-AT"
toLocale "german"     = "de-DE"
toLocale "germanb"    = "de-DE"
toLocale "ngerman"    = "de-DE"
toLocale "greek"      = "el-GR"
toLocale "polutonikogreek" = "el-GR"
toLocale "hebrew"     = "he-IL"
toLocale "hungarian"  = "hu-HU"
toLocale "icelandic"  = "is-IS"
toLocale "italian"    = "it-IT"
toLocale "japanese"   = "ja-JP"
toLocale "latvian"    = "lv-LV"
toLocale "lithuanian" = "lt-LT"
toLocale "magyar"     = "hu-HU"
toLocale "mongolian"  = "mn-MN"
toLocale "norsk"      = "nb-NO"
toLocale "nynorsk"    = "nn-NO"
toLocale "farsi"      = "fa-IR"
toLocale "polish"     = "pl-PL"
toLocale "brazil"     = "pt-BR"
toLocale "brazilian"  = "pt-BR"
toLocale "portugues"  = "pt-PT"
toLocale "portuguese" = "pt-PT"
toLocale "romanian"   = "ro-RO"
toLocale "russian"    = "ru-RU"
toLocale "serbian"    = "sr-RS"
toLocale "serbianc"   = "sr-RS"
toLocale "slovak"     = "sk-SK"
toLocale "slovene"    = "sl-SL"
toLocale "spanish"    = "es-ES"
toLocale "swedish"    = "sv-SE"
toLocale "thai"       = "th-TH"
toLocale "turkish"    = "tr-TR"
toLocale "ukrainian"  = "uk-UA"
toLocale "vietnamese" = "vi-VN"
toLocale "latin"      = "la"
toLocale x            = x

concatWith :: Char -> [Formatted] -> Formatted
concatWith sep = Formatted . foldl go mempty . map unFormatted
  where go :: [Inline] -> [Inline] -> [Inline]
        go accum [] = accum
        go accum s  = case reverse accum of
                           []    -> s
                           (Str x:_)
                             | not (null x) && last x `elem` "!?.,:;"
                                          -> accum ++ (Space : s)
                             | otherwise  -> accum ++ (Str [sep] : Space : s)
                           xs    -> accum ++ xs

type Options = [(String, String)]

parseOptions :: String -> Options
parseOptions = map breakOpt . splitWhen (==',')
  where breakOpt x = case break (=='=') x of
                          (w,v) -> (map toLower $ trim w,
                                    map toLower $ trim $ drop 1 v)

itemToReference :: Lang -> Bool -> Item -> Maybe Reference
itemToReference lang bibtex = bib $ do
  modify $ \st -> st{ localeLang = lang,
                      untitlecase = case lang of
                                         Lang "en" _ -> True
                                         _           -> False }
  id' <- asks identifier
  et <- asks entryType
  guard $ et /= "xdata"
  opts <- (parseOptions <$> getRawField "options") <|> return []
  let getAuthorList' = getAuthorList opts
  st <- getRawField "entrysubtype" <|> return mempty
  let (reftype, refgenre) = case et of
       "article"
         | st == "magazine"  -> (ArticleMagazine,mempty)
         | st == "newspaper" -> (ArticleNewspaper,mempty)
         | otherwise         -> (ArticleJournal,mempty)
       "book"            -> (Book,mempty)
       "booklet"         -> (Pamphlet,mempty)
       "bookinbook"      -> (Book,mempty)
       "collection"      -> (Book,mempty)
       "electronic"      -> (Webpage,mempty)
       "inbook"          -> (Chapter,mempty)
       "incollection"    -> (Chapter,mempty)
       "inreference "    -> (Chapter,mempty)
       "inproceedings"   -> (PaperConference,mempty)
       "manual"          -> (Book,mempty)
       "mastersthesis"   -> (Thesis, Formatted [Str $ resolveKey' lang "mathesis"])
       "misc"            -> (NoType,mempty)
       "mvbook"          -> (Book,mempty)
       "mvcollection"    -> (Book,mempty)
       "mvproceedings"   -> (Book,mempty)
       "mvreference"     -> (Book,mempty)
       "online"          -> (Webpage,mempty)
       "patent"          -> (Patent,mempty)
       "periodical"
         | st == "magazine"  -> (ArticleMagazine,mempty)
         | st == "newspaper" -> (ArticleNewspaper,mempty)
         | otherwise         -> (ArticleJournal,mempty)
       "phdthesis"       -> (Thesis, Formatted [Str $ resolveKey' lang "phdthesis"])
       "proceedings"     -> (Book,mempty)
       "reference"       -> (Book,mempty)
       "report"          -> (Report,mempty)
       "suppbook"        -> (Chapter,mempty)
       "suppcollection"  -> (Chapter,mempty)
       "suppperiodical"
         | st == "magazine"  -> (ArticleMagazine,mempty)
         | st == "newspaper" -> (ArticleNewspaper,mempty)
         | otherwise         -> (ArticleJournal,mempty)
       "techreport"      -> (Report,mempty)
       "thesis"          -> (Thesis,mempty)
       "unpublished"     -> (Manuscript,mempty)
       "www"             -> (Webpage,mempty)
       -- biblatex, "unsupporEd"
       "artwork"         -> (Graphic,mempty)
       "audio"           -> (Song,mempty)         -- for audio *recordings*
       "commentary"      -> (Book,mempty)
       "image"           -> (Graphic,mempty)      -- or "figure" ?
       "jurisdiction"    -> (LegalCase,mempty)
       "legislation"     -> (Legislation,mempty)  -- or "bill" ?
       "legal"           -> (Treaty,mempty)
       "letter"          -> (PersonalCommunication,mempty)
       "movie"           -> (MotionPicture,mempty)
       "music"           -> (Song,mempty)         -- for musical *recordings*
       "performance"     -> (Speech,mempty)
       "review"          -> (Review,mempty)       -- or "review-book" ?
       "software"        -> (Book,mempty)         -- for lack of any better match
       "standard"        -> (Legislation,mempty)
       "video"           -> (MotionPicture,mempty)
       -- biblatex-apa:
       "data"            -> (Dataset,mempty)
       "letters"         -> (PersonalCommunication,mempty)
       "newsarticle"     -> (ArticleNewspaper,mempty)
       _                 -> (NoType,mempty)
  reftype' <- resolveKey lang <$> getField "type" <|> return mempty

  let isContainer = et `elem` ["book","collection","proceedings","reference",
                     "mvbook","mvcollection","mvproceedings", "mvreference",
                     "suppbook","suppcollection"]

  -- hyphenation:
  let defaultHyphenation = case lang of
                                Lang x y -> x ++ "-" ++ y
  hyphenation <- ((toLocale . map toLower) <$> getRawField "hyphenation")
                <|> return mempty

  -- authors:
  author' <- getAuthorList' "author" <|> return []
  containerAuthor' <- getAuthorList' "bookauthor" <|> return []
  translator' <- getAuthorList' "translator" <|> return []
  editortype <- getRawField "editortype" <|> return mempty
  editor'' <- getAuthorList' "editor" <|> return []
  director'' <- getAuthorList' "director" <|> return []
  let (editor', director') = case editortype of
                                  "director"  -> ([], editor'')
                                  _           -> (editor'', director'')
  -- FIXME: add same for editora, editorb, editorc

  -- titles
  let isArticle = et `elem` ["article", "periodical", "suppperiodical"]
  let isPeriodical = et == "periodical"
  let hasVolumes = et `elem`
         ["inbook","incollection","inproceedings","bookinbook"]
  let hyphenation' = if null hyphenation
                     then defaultHyphenation
                     else hyphenation
  let la = case splitWhen (== '-') hyphenation' of
                      (x:_) -> x
                      []    -> mempty
  modify $ \s -> s{ untitlecase = la == "en" }
  title' <- getTitle (if isPeriodical then "issuetitle" else "title")
           <|> return mempty
  subtitle' <- getTitle (if isPeriodical then "issuesubtitle" else "subtitle")
              <|> return mempty
  titleaddon' <- getTitle "titleaddon"
               <|> return mempty
  volumeTitle' <- (getTitle "maintitle" >> guard hasVolumes
                    >> getTitle "booktitle")
                  <|> return mempty
  volumeSubtitle' <- (getTitle "maintitle" >> guard hasVolumes
                      >> getTitle "booksubtitle")
                     <|> return mempty
  volumeTitleAddon' <- (getTitle "maintitle" >> guard hasVolumes
                                   >> getTitle "booktitleaddon")
                       <|> return mempty
  containerTitle' <- (guard isPeriodical >> getPeriodicalTitle "title")
                  <|> getTitle "maintitle"
                  <|> (guard (not isContainer) >>
                       guard (volumeTitle' == mempty) >> getTitle "booktitle")
                  <|> getPeriodicalTitle "journaltitle"
                  <|> getPeriodicalTitle "journal"
                  <|> return mempty
  containerSubtitle' <- (guard isPeriodical >> getPeriodicalTitle "subtitle")
                       <|> getTitle "mainsubtitle"
                       <|> (guard (not isContainer) >>
                            guard (volumeSubtitle' == mempty) >>
                             getTitle "booksubtitle")
                       <|> getPeriodicalTitle "journalsubtitle"
                       <|> return mempty
  containerTitleAddon' <- (guard isPeriodical >> getPeriodicalTitle "titleaddon")
                       <|> getTitle "maintitleaddon"
                       <|> (guard (not isContainer) >>
                            guard (volumeTitleAddon' == mempty) >>
                             getTitle "booktitleaddon")
                       <|> return mempty
  containerTitleShort' <- (guard isPeriodical >> getField "shorttitle")
                        <|> (guard (not isContainer) >>
                             getTitle "booktitleshort")
                        <|> getPeriodicalTitle "journaltitleshort"
                        <|> getPeriodicalTitle "shortjournal"
                        <|> return mempty
  seriesTitle' <- resolveKey lang <$> getTitle "series" <|> return mempty
  shortTitle' <- getTitle "shorttitle"
               <|> if subtitle' /= mempty
                      then getShortTitle False "title"
                      else getShortTitle True  "title"

  eventTitle' <- getTitle "eventtitle" <|> return mempty
  origTitle' <- getTitle "origtitle" <|> return mempty

  -- publisher
  pubfields <- mapM (\f -> Just `fmap`
                       (if bibtex || f == "howpublished"
                        then getField f
                        else getLiteralList' f)
                      <|> return Nothing)
         ["school","institution","organization", "howpublished","publisher"]
  let publisher' = concatWith ';' [p | Just p <- pubfields]
  origpublisher' <- getField "origpublisher" <|> return mempty

-- places
  venue' <- getField "venue" <|> return mempty
  address' <- (if bibtex
               then getField "address"
               else getLiteralList' "address"
                     <|> (guard (et /= "patent") >>
                          getLiteralList' "location"))
              <|> return mempty
  origLocation' <- (if bibtex
                    then getField "origlocation"
                    else getLiteralList' "origlocation")
                  <|> return mempty
  jurisdiction' <- if et == "patent"
                   then ((concatWith ';' . map (resolveKey lang)) <$>
                           getLiteralList "location") <|> return mempty
                   else return mempty

  -- locators
  pages' <- getField "pages" <|> return mempty
  volume' <- getField "volume" <|> return mempty
  part' <- getField "part" <|> return mempty
  volumes' <- getField "volumes" <|> return mempty
  pagetotal' <- getField "pagetotal" <|> return mempty
  chapter' <- getField "chapter" <|> return mempty
  edition' <- getField "edition" <|> return mempty
  version' <- getField "version" <|> return mempty
  (number', collectionNumber', issue') <-
     (getField "number" <|> return mempty) >>= \x ->
       if et `elem` ["book","collection","proceedings","reference",
                     "mvbook","mvcollection","mvproceedings", "mvreference",
                     "bookinbook","inbook", "incollection","inproceedings",
                     "inreference", "suppbook","suppcollection"]
       then return (mempty,x,mempty)
       else if isArticle
            then (getField "issue" >>= \y ->
                                    return (mempty,mempty,concatWith ',' [x,y]))
               <|> return (mempty,mempty,x)
            else return (x,mempty,mempty)

  -- dates
  issued' <- getDates "date" <|> getOldDates mempty <|> return []
  eventDate' <- getDates "eventdate" <|> getOldDates "event"
              <|> return []
  origDate' <- getDates "origdate" <|> getOldDates "orig"
              <|> return []
  accessed' <- getDates "urldate" <|> getOldDates "url" <|> return []

  -- url, doi, isbn, etc.:
  -- note that with eprinttype = arxiv, we take eprint to be a partial url
  url' <- (guard (et == "online" || lookup "url" opts /= Just "false")
           >> getRawField "url")
       <|> (do etype <- getRawField "eprinttype"
               eprint <- getRawField "eprint"
               case map toLower etype of
                    "arxiv"       -> return $ "http://arxiv.org/abs/" ++ eprint
                    "googlebooks" -> return $ "http://books.google.com?id=" ++
                                        eprint
                    _             -> mzero)
       <|> return mempty
  doi' <- (guard (lookup "doi" opts /= Just "false") >> getRawField "doi")
         <|> return mempty
  isbn' <- getRawField "isbn" <|> return mempty
  issn' <- getRawField "issn" <|> return mempty
  callNumber' <- getRawField "library" <|> return mempty

  -- notes
  annotation' <- getField "annotation" <|> getField "annote"
                   <|> return mempty
  abstract' <- getField "abstract" <|> return mempty
  keywords' <- getField "keywords" <|> return mempty
  note' <- if et == "periodical"
           then return mempty
           else (getField "note" <|> return mempty)
  addendum' <- if bibtex
               then return mempty
               else getField "addendum"
                 <|> return mempty
  pubstate' <- resolveKey lang `fmap`
                 (  getField "pubstate"
                <|> case issued' of
                         (x:_) | other x == "forthcoming" ->
                                     return (Formatted [Str "forthcoming"])
                         _ -> return mempty
                 )

  let convertEnDash (Str s) = Str (map (\c -> if c == '–' then '-' else c) s)
      convertEnDash x       = x

  return $ emptyReference
         { refId               = id'
         , refType             = reftype
         , author              = author'
         , editor              = editor'
         , translator          = translator'
         -- , recipient           = undefined -- :: [Agent]
         -- , interviewer         = undefined -- :: [Agent]
         -- , composer            = undefined -- :: [Agent]
         , director            = director'
         -- , illustrator         = undefined -- :: [Agent]
         -- , originalAuthor      = undefined -- :: [Agent]
         , containerAuthor     = containerAuthor'
         -- , collectionEditor    = undefined -- :: [Agent]
         -- , editorialDirector   = undefined -- :: [Agent]
         -- , reviewedAuthor      = undefined -- :: [Agent]

         , issued              = issued'
         , eventDate           = eventDate'
         , accessed            = accessed'
         -- , container           = undefined -- :: [RefDate]
         , originalDate        = origDate'
         -- , submitted           = undefined -- :: [RefDate]
         , title               = concatWith '.' [
                                    concatWith ':' [title', subtitle']
                                  , titleaddon' ]
         , titleShort          = shortTitle'
         -- , reviewedTitle       = undefined -- :: String
         , containerTitle      = concatWith '.' [
                                      concatWith ':' [ containerTitle'
                                                     , containerSubtitle']
                                    , containerTitleAddon' ]
                                   `mappend`
                                   if isArticle && seriesTitle' /= mempty
                                      then if containerTitle' == mempty
                                              then seriesTitle'
                                              else (Formatted [Str ",",Space])
                                                    `mappend` seriesTitle'
                                      else mempty
         , collectionTitle     = if isArticle then mempty else seriesTitle'
         , volumeTitle         = concatWith '.' [
                                      concatWith ':' [ volumeTitle'
                                                     , volumeSubtitle']
                                    , volumeTitleAddon' ]
         , containerTitleShort = containerTitleShort'
         , collectionNumber    = collectionNumber'
         , originalTitle       = origTitle'
         , publisher           = publisher'
         , originalPublisher   = origpublisher'
         , publisherPlace      = address'
         , originalPublisherPlace = origLocation'
         , jurisdiction        = jurisdiction'
         , event               = eventTitle'
         , eventPlace          = venue'
         , page                = Formatted $
                                 Walk.walk convertEnDash $ unFormatted pages'
         -- , pageFirst           = undefined -- :: String
         , numberOfPages       = pagetotal'
         , version             = version'
         , volume              = Formatted $ intercalate [Str "."]
                                 $ filter (not . null)
                                 [unFormatted volume', unFormatted part']
         , numberOfVolumes     = volumes'
         , issue               = issue'
         , chapterNumber       = chapter'
         -- , medium              = undefined -- :: String
         , status              = pubstate'
         , edition             = edition'
         -- , section             = undefined -- :: String
         -- , source              = undefined -- :: String
         , genre               = if refgenre == mempty
                                    then reftype'
                                    else refgenre
         , note                = concatWith '.' [note', addendum']
         , annote              = annotation'
         , abstract            = abstract'
         , keyword             = keywords'
         , number              = number'
         , url                 = url'
         , doi                 = doi'
         , isbn                = isbn'
         , issn                = issn'
         , language            = hyphenation
         , callNumber          = callNumber'
         }
