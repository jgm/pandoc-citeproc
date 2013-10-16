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

import Text.Parsec hiding (optional, (<|>), many)
import Control.Applicative
import Text.Pandoc
import Data.List.Split (splitOn, splitWhen, wordsBy, whenElt,
                           dropBlanks, split)
import Data.List (intercalate)
import Data.Maybe
import Data.Char (toLower, isUpper, toUpper, isDigit)
import Control.Monad
import Control.Monad.Reader
import System.Environment (getEnvironment)
import Text.CSL.Reference
import Text.CSL.Input.Pandoc (blocksToString, inlinesToString)

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
                                   [w]     -> Lang w ""
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
fieldName = do
  c <- letter
  cs <- many1 (letter <|> digit <|> oneOf "-_")
  return $ map toLower (c:cs)

bibItem :: BibParser Item
bibItem = do
  char '@'
  enttype <- map toLower <$> many1 letter
  spaces
  char '{'
  spaces
  entid <- many1 (noneOf " \t\n\r,")
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

ident :: BibParser String
ident = do
  c <- letter
  cs <- many (letter <|> digit <|> char '_')
  return (c:cs)

expandString :: BibParser String
expandString = do
  k <- ident
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
          then entry'{ fields = fields entry ++
                                    getXrefFields isBibtex entry entries val }
          else entry'

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

trim :: String -> String
trim = unwords . words

data Lang = Lang String String  -- e.g. "en" "US"

resolveKey :: Lang -> String -> String
resolveKey (Lang "en" "US") k =
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
       _               -> k
resolveKey _ k = resolveKey (Lang "en" "US") k

type Bib = ReaderT Item Maybe

notFound :: String -> Bib a
notFound f = fail $ f ++ " not found"

getField :: String -> Bib String
getField f = do
  fs <- asks fields
  case lookup f fs >>= latex of
       Just x  -> return x
       Nothing -> notFound f

getTitle :: Lang -> String -> Bib String
getTitle lang f = do
  fs <- asks fields
  case lookup f fs >>= latexTitle lang of
       Just x  -> return x
       Nothing -> notFound f

getDates :: String -> Bib [RefDate]
getDates f = do
  fs <- asks fields
  case lookup f fs >>= parseDates of
       Just x  -> return x
       Nothing -> notFound f

parseDates :: String -> Maybe [RefDate]
parseDates s = mapM parseDate $ splitOn "/" s

parseDate :: String -> Maybe RefDate
parseDate s = do
  let (year', month', day') =
        case splitOn "-" s of
             [y]     -> (y, "", "")
             [y,m]   -> (y, m, "")
             [y,m,d] -> (y, m, d)
             _       -> ("", "", "")
  return RefDate { year   = year'
                 , month  = month'
                 , season = ""
                 , day    = day'
                 , other  = ""
                 , circa  = ""
                 }

getOldDates :: String -> Bib [RefDate]
getOldDates prefix = do
  year' <- getField (prefix ++ "year")
  month' <- getField (prefix ++ "month") <|> return ""
  day' <- getField (prefix ++ "day") <|> return ""
  endyear' <- getField (prefix ++ "endyear") <|> return ""
  endmonth' <- getField (prefix ++ "endmonth") <|> return ""
  endday' <- getField (prefix ++ "endday") <|> return ""
  let start' = RefDate { year   = year'
                       , month  = month'
                       , season = ""
                       , day    = day'
                       , other  = ""
                       , circa  = ""
                       }
  let end' = if null endyear'
                then []
                else [RefDate { year   = endyear'
                              , month  = endmonth'
                              , day    = endday'
                              , season = ""
                              , other  = ""
                              , circa  = ""
                              }]
  return (start':end')

getRawField :: String -> Bib String
getRawField f = do
  fs <- asks fields
  case lookup f fs of
       Just x  -> return x
       Nothing -> notFound f

getAuthorList :: Bool -> String -> Bib [Agent]
getAuthorList useprefix f = do
  fs <- asks fields
  case lookup f fs >>= latexAuthors useprefix of
       Just xs -> return xs
       Nothing -> notFound f

getLiteralList :: String -> Bib [String]
getLiteralList f = do
  fs <- asks fields
  case lookup f fs of
       Just x  -> latex' x >>= toLiteralList
       Nothing -> notFound f

-- separates items with semicolons
getLiteralList' :: String -> Bib String
getLiteralList' f = intercalate "; " <$> getLiteralList f

splitByAnd :: [Inline] -> [[Inline]]
splitByAnd = splitOn [Space, Str "and", Space]

toLiteralList :: (Functor m, MonadPlus m) => [Block] -> m [String]
toLiteralList [Para xs] =
  mapM inlinesToString $ splitByAnd xs
toLiteralList [Plain xs] = toLiteralList [Para xs]
toLiteralList _ = mzero

toAuthorList :: MonadPlus m => Bool -> [Block] -> m [Agent]
toAuthorList useprefix [Para xs] =
  return $ map (toAuthor useprefix) $ splitByAnd xs
toAuthorList useprefix [Plain xs] = toAuthorList useprefix [Para xs]
toAuthorList _ _ = mzero

toAuthor :: Bool -> [Inline] -> Agent
toAuthor _ [Str "others"] =
    Agent { givenName       = []
          , droppingPart    = ""
          , nonDroppingPart = ""
          , familyName      = ""
          , nameSuffix      = ""
          , literal         = "others"
          , commaSuffix     = False
          }
toAuthor _ [Span ("",[],[]) ils] = -- corporate author
    Agent { givenName       = []
          , droppingPart    = ""
          , nonDroppingPart = ""
          , familyName      = ""
          , nameSuffix      = ""
          , literal         = maybe "" id $ inlinesToString ils
          , commaSuffix     = False
          }
-- First von Last
-- von Last, First
-- von Last, Jr ,First
toAuthor useprefix ils =
    Agent { givenName       = givens
          , droppingPart    = if useprefix then "" else prefix
          , nonDroppingPart = if useprefix then prefix else ""
          , familyName      = family
          , nameSuffix      = suffix
          , literal         = ""
          , commaSuffix     = not (null suffix)
          }
  where commaParts = map words' $ splitWhen (== Str ",") $ separateCommas ils
        words' = wordsBy (== Space)
        isCapitalized (Str (c:cs) : rest)
          | isUpper c = True
          | isDigit c = isCapitalized (Str cs : rest)
          | otherwise = False
        isCapitalized (_:rest) = isCapitalized rest
        isCapitalized [] = True
        inlinesToString' = maybe "" id . inlinesToString
        prefix = inlinesToString' $ intercalate [Space] von
        family = inlinesToString' $ intercalate [Space] lastname
        suffix = inlinesToString' $ intercalate [Space] jr
        givens = map inlinesToString' first
        (first, vonlast, jr) =
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
        (rlast, rvon) = span isCapitalized $ reverse vonlast
        (von, lastname) = case (reverse rvon, reverse rlast) of
                               (ws@(_:_),[]) -> (init ws, [last ws])
                               (ws, vs)      -> (ws, vs)

separateCommas :: [Inline] -> [Inline]
separateCommas [] = []
separateCommas (Str xs : ys)
  | ',' `elem` xs = map Str ((split . dropBlanks) (whenElt (==',')) xs) ++ separateCommas ys
separateCommas (x : ys) = x : separateCommas ys

latex' :: (MonadPlus m, Functor m) => String -> m [Block]
latex' s = return bs
  where Pandoc _ bs = readLaTeX def s

latex :: (MonadPlus m, Functor m) => String -> m String
latex s = latex' (trim s) >>= blocksToString

latexTitle :: (MonadPlus m, Functor m) => Lang -> String -> m String
latexTitle (Lang l _) s = trim `fmap` blocksToString (processTitle bs)
  where Pandoc _ bs = readLaTeX def s
        processTitle = case l of
                          'e':'n':_ -> unTitlecase
                          _         -> id

latexAuthors :: MonadPlus m => Bool -> String -> m [Agent]
latexAuthors useprefix s = toAuthorList useprefix bs
  where Pandoc _ bs = readLaTeX def s

bib :: Bib Reference -> Item -> Maybe Reference
bib m entry = runReaderT m entry

unTitlecase :: [Block] -> [Block]
unTitlecase [Para ils]  = [Para $ untc ils]
unTitlecase [Plain ils] = [Para $ untc ils]
unTitlecase xs          = xs

untc :: [Inline] -> [Inline]
untc [] = []
untc (x:xs) = x : map go xs
  where go (Str ys)     = Str $ map toLower ys
        go z            = z

toLocale :: String -> String
toLocale "english"    = "en-US" -- "en-EN" unavailable in CSL
toLocale "USenglish"  = "en-US"
toLocale "american"   = "en-US"
toLocale "british"    = "en-GB"
toLocale "UKenglish"  = "en-GB"
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
toLocale _            = ""

concatWith :: Char -> [String] -> String
concatWith sep xs = foldl go "" xs
  where go :: String -> String -> String
        go accum "" = accum
        go accum s  = case reverse accum of
                           []    -> s
                           (x:_) | x `elem` "!?.,:;" -> accum ++ " " ++ s
                                 | otherwise         -> accum ++ [sep, ' '] ++ s

parseOptions :: String -> [(String, String)]
parseOptions = map breakOpt . splitWhen (==',')
  where breakOpt x = case break (=='=') x of
                          (w,v) -> (map toLower $ trim w,
                                    map toLower $ trim $ drop 1 v)

itemToReference :: Lang -> Bool -> Item -> Maybe Reference
itemToReference lang bibtex = bib $ do
  id' <- asks identifier
  et <- asks entryType
  guard $ et /= "xdata"
  opts <- (parseOptions <$> getRawField "options") <|> return []
  let useprefix = maybe False (=="true") $ lookup "useprefix" opts
  let getAuthorList' = getAuthorList useprefix
  st <- getRawField "entrysubtype" <|> return ""
  let (reftype, refgenre) = case et of
       "article"
         | st == "magazine"  -> (ArticleMagazine,"")
         | st == "newspaper" -> (ArticleNewspaper,"")
         | otherwise         -> (ArticleJournal,"")
       "book"            -> (Book,"")
       "booklet"         -> (Pamphlet,"")
       "bookinbook"      -> (Book,"")
       "collection"      -> (Book,"")
       "electronic"      -> (Webpage,"")
       "inbook"          -> (Chapter,"")
       "incollection"    -> (Chapter,"")
       "inreference "    -> (Chapter,"")
       "inproceedings"   -> (PaperConference,"")
       "manual"          -> (Book,"")
       "mastersthesis"   -> (Thesis, resolveKey lang "mathesis")
       "misc"            -> (NoType,"")
       "mvbook"          -> (Book,"")
       "mvcollection"    -> (Book,"")
       "mvproceedings"   -> (Book,"")
       "mvreference"     -> (Book,"")
       "online"          -> (Webpage,"")
       "patent"          -> (Patent,"")
       "periodical"
         | st == "magazine"  -> (ArticleMagazine,"")
         | st == "newspaper" -> (ArticleNewspaper,"")
         | otherwise         -> (ArticleJournal,"")
       "phdthesis"       -> (Thesis, resolveKey lang "phdthesis")
       "proceedings"     -> (Book,"")
       "reference"       -> (Book,"")
       "report"          -> (Report,"")
       "suppbook"        -> (Chapter,"")
       "suppcollection"  -> (Chapter,"")
       "suppperiodical"
         | st == "magazine"  -> (ArticleMagazine,"")
         | st == "newspaper" -> (ArticleNewspaper,"")
         | otherwise         -> (ArticleJournal,"")
       "techreport"      -> (Report,"")
       "thesis"          -> (Thesis,"")
       "unpublished"     -> (Manuscript,"")
       "www"             -> (Webpage,"")
       -- biblatex, "unsupporEd"
       "artwork"         -> (Graphic,"")
       "audio"           -> (Song,"")         -- for audio *recordings*
       "commentary"      -> (Book,"")
       "image"           -> (Graphic,"")      -- or "figure" ?
       "jurisdiction"    -> (LegalCase,"")
       "legislation"     -> (Legislation,"")  -- or "bill" ?
       "legal"           -> (Treaty,"")
       "letter"          -> (PersonalCommunication,"")
       "movie"           -> (MotionPicture,"")
       "music"           -> (Song,"")         -- for musical *recordings*
       "performance"     -> (Speech,"")
       "review"          -> (Review,"")       -- or "review-book" ?
       "software"        -> (Book,"")         -- for lack of any better match
       "standard"        -> (Legislation,"")
       "video"           -> (MotionPicture,"")
       -- biblatex-apa:
       "data"            -> (Dataset,"")
       "letters"         -> (PersonalCommunication,"")
       "newsarticle"     -> (ArticleNewspaper,"")
       _                 -> (NoType,"")
  reftype' <- resolveKey lang <$> getField "type" <|> return ""

  -- hyphenation:
  let defaultHyphenation = case lang of
                                Lang x y -> x ++ "-" ++ y
  hyphenation <- (toLocale <$> getRawField "hyphenation")
                <|> return defaultHyphenation

  -- authors:
  author' <- getAuthorList' "author" <|> return []
  containerAuthor' <- getAuthorList' "bookauthor" <|> return []
  translator' <- getAuthorList' "translator" <|> return []
  editortype <- getRawField "editortype" <|> return ""
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
  let (la, co) = case splitOn "-" hyphenation of
                      [x]     -> (x, "")
                      (x:y:_) -> (x, y)
                      []      -> ("", "")
  let getTitle' = getTitle (Lang la co)
  title' <- getTitle' (if isPeriodical then "issuetitle" else "title")
           <|> return ""
  subtitle' <- getTitle' (if isPeriodical then "issuesubtitle" else "subtitle")
              <|> return ""
  titleaddon' <- getTitle' "titleaddon"
               <|> return ""
  containerTitle' <- (guard isPeriodical >> getField "title")
                  <|> getTitle' "maintitle"
                  <|> getTitle' "booktitle"
                  <|> getField "journal"
                  <|> getField "journaltitle"
                  <|> return ""
  containerSubtitle' <- (guard isPeriodical >> getField "subtitle")
                       <|> getTitle' "mainsubtitle"
                       <|> getTitle' "booksubtitle"
                       <|> getField "journalsubtitle"
                       <|> return ""
  containerTitleAddon' <- (guard isPeriodical >> getField "titleaddon")
                       <|> getTitle' "maintitleaddon"
                       <|> getTitle' "booktitleaddon"
                       <|> return ""
  containerTitleShort' <- (guard isPeriodical >> getField "shorttitle")
                        <|> getTitle' "booktitleshort"
                        <|> getField "journaltitleshort"
                        <|> getField "shortjournal"
                        <|> return ""
  seriesTitle' <- getTitle' "series" <|> return ""
  volumeTitle' <- (getTitle' "maintitle" >> guard hasVolumes
                    >> getTitle' "booktitle")
                  <|> return ""
  volumeSubtitle' <- (getTitle' "maintitle" >> guard hasVolumes
                      >> getTitle' "booksubtitle")
                     <|> return ""
  volumeTitleAddon' <- (getTitle' "maintitle" >> guard hasVolumes
                                   >> getTitle' "booktitleaddon")
                       <|> return ""
  shortTitle' <- getTitle' "shorttitle"
               <|> if ':' `elem` title'
                   then return (takeWhile (/=':') title')
                   else return ""

  eventTitle' <- getTitle' "eventtitle" <|> return ""
  origTitle' <- getTitle' "origtitle" <|> return ""

  -- publisher
  pubfields <- mapM (\f -> Just `fmap`
                       (if bibtex || f == "howpublished"
                        then getField f
                        else getLiteralList' f)
                      <|> return Nothing)
         ["school","institution","organization", "howpublished","publisher"]
  let publisher' = intercalate "; " [p | Just p <- pubfields]
  origpublisher' <- getField "origpublisher" <|> return ""

-- places
  venue' <- getField "venue" <|> return ""
  address' <- (if bibtex
               then getField "address"
               else getLiteralList' "address"
                     <|> (guard (et /= "patent") >>
                          getLiteralList' "location"))
              <|> return ""
  origLocation' <- (if bibtex
                    then getField "origlocation"
                    else getLiteralList' "origlocation")
                  <|> return ""
  jurisdiction' <- if et == "patent"
                   then resolveKey lang <$> getLiteralList' "location" <|> return ""
                   else return ""

  -- locators
  pages' <- getField "pages" <|> return ""
  volume' <- getField "volume" <|> return ""
  volumes' <- getField "volumes" <|> return ""
  pagetotal' <- getField "pagetotal" <|> return ""
  chapter' <- getField "chapter" <|> return ""
  edition' <- getField "edition" <|> return ""
  version' <- getField "version" <|> return ""
  (number', collectionNumber', issue') <-
     (getField "number" <|> return "") >>= \x ->
       if et `elem` ["book","collection","proceedings","reference",
                     "mvbook","mvcollection","mvproceedings", "mvreference",
                     "bookinbook","inbook", "incollection","inproceedings",
                     "inreference", "suppbook","suppcollection"]
       then return ("",x,"")
       else if isArticle
            then (getField "issue" >>= \y ->
                                    return ("","",concatWith ',' [x,y]))
               <|> return ("","",x)
            else return (x,"","")

  -- dates
  issued' <- getDates "date" <|> getOldDates "" <|> return []
  eventDate' <- getDates "eventdate" <|> getOldDates "event"
              <|> return []
  origDate' <- getDates "origdate" <|> getOldDates "orig"
              <|> return []
  accessed' <- getDates "urldate" <|> getOldDates "url" <|> return []

  -- url, doi, isbn, etc.:
  url' <- getRawField "url" <|> return ""
  doi' <- getRawField "doi" <|> return ""
  isbn' <- getRawField "isbn" <|> return ""
  issn' <- getRawField "issn" <|> return ""
  callNumber' <- getRawField "library" <|> return ""

  -- notes
  annotation' <- getField "annotation" <|> getField "annote"
                   <|> return ""
  abstract' <- getField "abstract" <|> return ""
  keywords' <- getField "keywords" <|> return ""
  note' <- if et == "periodical"
           then return ""
           else (getField "note" <|> return "")
  addendum' <- if bibtex
               then return ""
               else getField "addendum"
                 <|> return ""
  pubstate' <- resolveKey lang `fmap`
                   getRawField "pubstate" <|> return ""

  let convertEnDash = map (\c -> if c == '–' then '-' else c)

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
                                   ++ if isArticle && not (null seriesTitle')
                                      then if null containerTitle'
                                              then seriesTitle'
                                              else ", " ++ seriesTitle'
                                      else ""
         , collectionTitle     = if isArticle then "" else seriesTitle'
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
         , page                = convertEnDash pages'
         -- , pageFirst           = undefined -- :: String
         , numberOfPages       = pagetotal'
         , version             = version'
         , volume              = volume'
         , numberOfVolumes     = volumes'
         , issue               = issue'
         , chapterNumber       = chapter'
         -- , medium              = undefined -- :: String
         , status              = pubstate'
         , edition             = edition'
         -- , section             = undefined -- :: String
         -- , source              = undefined -- :: String
         , genre               = if null refgenre
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
         , language            = if hyphenation == defaultHyphenation
                                    then ""
                                    else hyphenation
         , callNumber          = callNumber'
         }
