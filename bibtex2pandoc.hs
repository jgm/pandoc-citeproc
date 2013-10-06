-- TODO:
-- make inlinesToString switchable
-- when writing JSON we want current behavior
-- when writing pandoc YAML we want markdown

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
import Data.Yaml
import Data.List.Split (splitOn, splitWhen)
import Data.List (intersperse, intercalate)
import Data.Maybe
import Data.Char (toLower, isUpper, isLower)
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO (stderr, hPutStrLn)
import Control.Monad
import Control.Monad.RWS.Strict
import Control.Monad.Reader
import System.Environment (getEnvironment)
import qualified Data.Text as T
import Text.CSL.Reference
import Text.CSL.Pandoc (blocksToString, inlinesToString)
import qualified Data.ByteString as B

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
  env <- getEnvironment
  let lang = case lookup "LANG" env of
                  Just x  -> case splitWhen (\c -> c == '.' || c == '_') x of
                                   (w:z:_) -> Lang w z
                                   [w]     -> Lang w ""
                                   _       -> Lang "en" "US"
                  Nothing -> Lang "en" "US"
  bibstring <- case args of
                    (x:_) -> readFile x
                    []    -> getContents
  let items = case parse (skippingLeadingSpace file) "stdin" bibstring of
                   Left err -> error (show err)
                   Right xs -> resolveCrossRefs isBibtex
                                  $ map lowercaseFieldNames xs
  putStrLn "---\nreferences:"
  B.putStr $ encode
           $ mapMaybe (itemToReference lang isBibtex) items
  putStrLn "..."

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

type Bib = ReaderT T Maybe

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

getAuthorList :: String -> Bib [Agent]
getAuthorList f = do
  fs <- asks fields
  case lookup f fs >>= latexAuthors of
       Just xs -> return xs
       Nothing -> notFound f

getLiteralList :: String -> Bib [String]
getLiteralList f = do
  fs <- asks fields
  case lookup f fs of
       Just x  -> return $ map trim $ splitOn " and " x
       Nothing -> notFound f

splitByAnd :: [Inline] -> [[Inline]]
splitByAnd = splitOn [Space, Str "and", Space]

toAuthorList :: [Block] -> Maybe [Agent]
toAuthorList [Para xs] =
  Just $ map toAuthor $ splitByAnd xs
toAuthorList [Plain xs] = toAuthorList [Para xs]
toAuthorList x = Nothing

toAuthor :: [Inline] -> Agent
toAuthor [Str "others"] =
    Agent { givenName       = []
          , droppingPart    = ""
          , nonDroppingPart = ""
          , familyName      = ""
          , nameSuffix      = ""
          , literal         = "others"
          , commaSuffix     = False
          }
toAuthor [Span ("",[],[]) ils] = -- corporate author
    Agent { givenName       = []
          , droppingPart    = ""
          , nonDroppingPart = ""
          , familyName      = ""
          , nameSuffix      = ""
          , literal         = maybe "" id $ inlinesToString ils
          , commaSuffix     = False
          }
toAuthor ils =
    Agent { givenName       = givens
          , droppingPart    = dropping
          , nonDroppingPart = nondropping
          , familyName      = family
          , nameSuffix      = suffix
          , literal         = ""
          , commaSuffix     = isCommaSuffix
          }
  where inlinesToString' = maybe "" id . inlinesToString
        isCommaSuffix = False -- TODO
        suffix = "" -- TODO
        dropping = "" -- TODO
        endsWithComma (Str zs) = not (null zs) && last zs == ','
        endsWithComma _ = False
        stripComma xs = case reverse xs of
                             (',':ys) -> reverse ys
                             _ -> xs
        (xs, ys) = break endsWithComma ils
        (family, givens, nondropping) =
           case splitOn [Space] ys of
              ((Str w:ws) : rest) ->
                  ( inlinesToString' [Str (stripComma w)]
                  , map inlinesToString' $ if null ws then rest else (ws : rest)
                  , trim $ inlinesToString' xs
                  )
              _ -> case reverse xs of
                        []     -> ("", [], "")
                        (z:zs) -> let (us,vs) = break startsWithCapital zs
                                  in  ( inlinesToString' [z]
                                      , map inlinesToString' $ splitOn [Space] $ reverse vs
                                      , trim $ inlinesToString' $ dropWhile (==Space) $ reverse us
                                      )

startsWithCapital :: Inline -> Bool
startsWithCapital (Str (x:_)) = isUpper x
startsWithCapital _           = False

latex :: String -> Maybe String
latex s = trim `fmap` blocksToString bs
  where Pandoc _ bs = readLaTeX def s

latexTitle :: Lang -> String -> Maybe String
latexTitle (Lang l _) s = trim `fmap` blocksToString (processTitle bs)
  where Pandoc _ bs = readLaTeX def s
        processTitle = case l of
                          'e':'n':_ -> unTitlecase
                          _         -> id

latexAuthors :: String -> Maybe [Agent]
latexAuthors s = toAuthorList bs
  where Pandoc _ bs = readLaTeX def s

bib :: Bib Reference -> T -> Maybe Reference
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

itemToReference :: Lang -> Bool -> T -> Maybe Reference
itemToReference lang bibtex = bib $ do
  id' <- asks identifier
  et <- map toLower `fmap` asks entryType
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
  reftype' <- resolveKey lang <$> getRawField "type" <|> return ""

  -- hyphenation:
  hyphenation <- getRawField "hyphenation" <|> return ""

  -- authors:
  author' <- getAuthorList "author" <|> return []
  containerAuthor' <- getAuthorList "bookauthor" <|> return []
  translator' <- getAuthorList "translator" <|> return []
  editortype <- getRawField "editortype" <|> return ""
  editor'' <- getAuthorList "editor" <|> return []
  director'' <- getAuthorList "director" <|> return []
  let (editor', director') = case editortype of
                                  "director"  -> ([], editor'')
                                  _           -> (editor'', director'')
  -- FIXME: add same for editora, editorb, editorc

  -- titles
  let isArticle = et `elem` ["article", "periodical", "suppperiodical"]
  let isPeriodical = et == "periodical"
  let hasVolumes = et `elem`
         ["inbook","incollection","inproceedings","bookinbook"]
  let addColon = fmap (": " ++)
  let addPeriod = fmap (". " ++)
  title' <- (guard isPeriodical >> getTitle lang "issuetitle")
         <|> getTitle lang "title"
         <|> return ""
  subtitle' <- addColon ((guard isPeriodical >> getTitle lang "issuesubtitle")
                <|> getTitle lang "subtitle")
              <|> return ""
  titleaddon' <- addPeriod (getTitle lang "titleaddon")
               <|> return ""
  containerTitle' <- (guard isPeriodical >> getTitle lang "title")
                  <|> getTitle lang "maintitle"
                  <|> getTitle lang "booktitle"
                  <|> getTitle lang "journal"
                  <|> getTitle lang "journaltitle"
                  <|> (guard isArticle >> getTitle lang "series")
                  <|> return ""
  containerSubtitle' <- addColon (getTitle lang "mainsubtitle"
                       <|> getTitle lang "booksubtitle"
                       <|> getTitle lang "journalsubtitle")
                       <|> return ""
  containerTitleAddon' <- addPeriod (getTitle lang "maintitleaddon"
                       <|> getTitle lang "booktitleaddon")
                       <|> return ""
  containerTitleShort' <- getTitle lang "booktitleshort"
                        <|> getTitle lang "journaltitleshort"
                        <|> getTitle lang "shortjournal"
                        <|> return ""
  volumeTitle' <- (getTitle lang "maintitle" >> guard hasVolumes
                    >> getTitle lang "booktitle")
                  <|> (guard (not isArticle) >> getTitle lang "series")
                  <|> return ""
  volumeSubtitle' <- addColon (getTitle lang "maintitle"
                      >> guard hasVolumes
                      >> getTitle lang "booksubtitle")
                     <|> return ""
  volumeTitleAddon' <- addPeriod (getTitle lang "maintitle"
                                   >> guard hasVolumes
                                   >> getTitle lang "booktitleaddon")
                       <|> return ""
  shortTitle' <- getTitle lang "shorttitle"
               <|> if ':' `elem` title'
                   then return (takeWhile (/=':') title')
                   else return ""

  eventTitle' <- getTitle lang "eventtitle" <|> return ""
  origTitle' <- getTitle lang "origtitle" <|> return ""

  -- publisher
  pubfields <- mapM (\f -> Just `fmap` getField f <|> return Nothing)
               ["school","institution","organization",
                "howpublished","publisher"]
  let publisher' = intercalate ", " [p | Just p <- pubfields]
  origpublisher' <- getField "origpublisher" <|> return ""

-- places
  venue' <- getField "venue" <|> return ""
  address' <- getField "address" <|> if bibtex then return "" else getField "location"
            <|> return ""
  origLocation' <- getField "origlocation" <|> return ""
  jurisdiction' <- getField "jurisdiction" <|> return ""

  -- locators
  pages' <- getField "pages" <|> return ""
  volume' <- getField "volume" <|> return ""
  volumes' <- getField "volumes" <|> return ""
  pagetotal' <- getField "pagetotal" <|> return ""
  chapter' <- getField "chapter" <|> return ""
  edition' <- getField "edition" <|> return ""
  version' <- getField "version" <|> return ""
  (number', collectionNumber') <- (getField "number" <|> return "") >>= \x ->
       if et `elem` ["book","collection","proceedings","reference",
                     "mvbook","mvcollection","mvproceedings", "mvreference",
                     "bookinbook","inbook", "incollection","inproceedings",
                     "inreference", "suppbook","suppcollection"]
       then return ("",x)
       else return (x,"")

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
               else ((" "++) `fmap` getField "addendum")
                 <|> return ""
  pubstate' <- resolveKey lang `fmap`
                   getRawField "pubstate" <|> return ""

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
         , title               = title' ++ subtitle' ++ titleaddon'
         , titleShort          = shortTitle'
         -- , reviewedTitle       = undefined -- :: String
         , containerTitle      = containerTitle' ++ containerSubtitle' ++ containerTitleAddon'
         , collectionTitle     = volumeTitle' ++ volumeSubtitle' ++ volumeTitleAddon'
         , containerTitleShort = containerTitleShort'
         -- , collectionNumber    = undefined -- :: String --Int
         , originalTitle       = origTitle'
         , publisher           = publisher'
         , originalPublisher   = origpublisher'
         , publisherPlace      = address'
         , originalPublisherPlace = origLocation'
         , jurisdiction        = jurisdiction'
         , event               = eventTitle'
         , eventPlace          = venue'
         , page                = pages'
         -- , pageFirst           = undefined -- :: String
         , numberOfPages       = pagetotal'
         , version             = version'
         , volume              = volume'
         , numberOfVolumes     = volumes'
         -- , issue               = undefined -- :: String
         , chapterNumber       = chapter'
         -- , medium              = undefined -- :: String
         , status              = pubstate'
         , edition             = edition'
         -- , section             = undefined -- :: String
         -- , source              = undefined -- :: String
         , genre               = if null refgenre
                                    then reftype'
                                    else refgenre
         , note                = note' ++ addendum'
         , annote              = annotation'
         , abstract            = abstract'
         , keyword             = keywords'
         , number              = number'
         , url                 = url'
         , doi                 = doi'
         , isbn                = isbn'
         , issn                = issn'
         , language            = toLocale hyphenation
         }
