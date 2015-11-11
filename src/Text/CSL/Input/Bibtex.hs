{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE CPP #-}
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
import Data.List.Split (splitOn, splitWhen, wordsBy)
import Data.List (intercalate)
import Data.Maybe
import Data.Char (toLower, isUpper, toUpper, isDigit, isAlphaNum)
import Control.Monad
import Control.Monad.RWS
import System.Environment (getEnvironment)
import Text.CSL.Reference
import Text.CSL.Style (Formatted(..), Locale(..), CslTerm(..), Agent(..))
import Text.CSL.Util (trim, onBlocks, unTitlecase, protectCase, splitStrWhen)
import Text.CSL.Parser (parseLocale)
import qualified Text.Pandoc.Walk as Walk
import qualified Text.Pandoc.UTF8 as UTF8

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
adjustSpans _ x = [x]

parseRawLaTeX :: Lang -> String -> [Inline]
parseRawLaTeX lang ('\\':xs) =
#if MIN_VERSION_pandoc(1,14,0)
  case readLaTeX def{readerParseRaw = True} contents of
       Right (Pandoc _ [Para ys])  -> f command ys
       Right (Pandoc _ [Plain ys]) -> f command ys
       _                           -> []
#else
  case readLaTeX def{readerParseRaw = True} contents of
       Pandoc _ [Para ys]  -> f command ys
       Pandoc _ [Plain ys] -> f command ys
       _                   -> []
#endif
   where (command', contents') = break (=='{') xs
         command  = trim command'
         contents = drop 1 $ reverse $ drop 1 $ reverse contents'
         f "mkbibquote"    ils = [Quoted DoubleQuote ils]
         f "mkbibemph"     ils = [Emph ils]
         f "mkbibitalic"   ils = [Emph ils] -- TODO: italic/=emph
         f "mkbibbold"     ils = [Strong ils]
         f "mkbibparens"   ils = [Str "("] ++ ils ++ [Str ")"] -- TODO: ...
         f "mkbibbrackets" ils = [Str "["] ++ ils ++ [Str "]"] -- TODO: ...
         -- ... both should be nestable & should work in year fields
         f "autocap"    ils = ils  -- TODO: should work in year fields
         f "textnormal" ils = [Span ("",["nodecor"],[]) ils]
         f "bibstring" [Str s] = [Str $ resolveKey' lang s]
         f _            ils = [Span nullAttr ils]
parseRawLaTeX _ _ = []

inlinesToFormatted :: [Inline] -> Bib Formatted
inlinesToFormatted ils = do
  lang <- gets localeLanguage
  return $ Formatted $ bottomUp (concatMap (adjustSpans lang)) ils

data Item = Item{ identifier :: String
                , entryType  :: String
                , fields     :: [(String, String)]
                }

readBibtexInput :: Bool -> FilePath -> IO [Reference]
readBibtexInput isBibtex f = UTF8.readFile f >>= readBibtexInputString isBibtex

readBibtexInputString :: Bool -> String -> IO [Reference]
readBibtexInputString isBibtex bibstring = do
  env <- getEnvironment
  let lang = case lookup "LANG" env of
                  Just x  -> case splitWhen (\c -> c == '.' || c == '_' || c == '-') x of
                                   (w:z:_)            -> Lang w z
                                   [w] | not (null w) -> Lang w mempty
                                   _                  -> Lang "en" "US"
                  Nothing -> Lang "en" "US"
  let items = case runParser (bibEntries <* eof) [] "stdin" bibstring of
                   Left err -> error (show err)
                   Right xs -> resolveCrossRefs isBibtex xs
  locale <- parseLocale (langToLocale lang)
  return $ mapMaybe (itemToReference lang locale isBibtex) items

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
  concat <$> manyTill (  many1 (noneOf "\"\\{")
                     <|> (char '\\' >> (\c -> ['\\',c]) <$> anyChar)
                     <|> braced <$> inBraces
                      ) (char '"')

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
resolveCrossRef isBibtex entries entry = foldr go entry (fields entry)
  where go (key, val) entry' =
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

langToLocale :: Lang -> String
langToLocale (Lang x y) = x ++ ('-':y)

-- Biblatex Localization Keys (see Biblatex manual)
-- Currently we only map a subset likely to be used in Biblatex *databases*
-- (in fields such as `type`, and via `\bibstring{}` commands).

resolveKey :: Lang -> Formatted -> Formatted
resolveKey lang (Formatted ils) = Formatted (Walk.walk go ils)
  where go (Str s) = Str $ resolveKey' lang s
        go x       = x

-- biblatex localization keys, from files at
-- http://github.com/plk/biblatex/tree/master/tex/latex/biblatex/lbx
-- Some keys missing in these were added from csl locale files at
-- http://github.com/citation-style-language/locales -- labeled "csl"
resolveKey' :: Lang -> String -> String
resolveKey' (Lang "ca" "AD") k =
    case map toLower k of
       "inpreparation" -> "en preparació"
       "submitted"     -> "enviat"
       "forthcoming"   -> "disponible en breu"
       "inpress"       -> "a impremta"
       "prepublished"  -> "pre-publicat"
       "mathesis"      -> "tesi de màster"
       "phdthesis"     -> "tesi doctoral"
       "candthesis"    -> "tesi de candidatura"
       "techreport"    -> "informe tècnic"
       "resreport"     -> "informe de recerca"
       "software"      -> "programari"
       "datacd"        -> "CD de dades"
       "audiocd"       -> "CD d’àudio"
       "patent"        -> "patent"
       "patentde"      -> "patent alemana"
       "patenteu"      -> "patent europea"
       "patentfr"      -> "patent francesa"
       "patentuk"      -> "patent britànica"
       "patentus"      -> "patent estatunidenca"
       "patreq"        -> "soŀlicitud de patent"
       "patreqde"      -> "soŀlicitud de patent alemana"
       "patreqeu"      -> "soŀlicitud de patent europea"
       "patreqfr"      -> "soŀlicitud de patent francesa"
       "patrequk"      -> "soŀlicitud de patent britànica"
       "patrequs"      -> "soŀlicitud de patent estatunidenca"
       "countryde"     -> "Alemanya"
       "countryeu"     -> "Unió Europea"
       "countryep"     -> "Unió Europea"
       "countryfr"     -> "França"
       "countryuk"     -> "Regne Unit"
       "countryus"     -> "Estats Units d’Amèrica"
       "newseries"     -> "sèrie nova"
       "oldseries"     -> "sèrie antiga"
       _               -> k
resolveKey' (Lang "da" "DK") k =
    case map toLower k of
       -- "inpreparation" -> "" -- missing
       -- "submitted"     -> "" -- missing
       "forthcoming"   -> "kommende" -- csl
       "inpress"       -> "i tryk"   -- csl
       -- "prepublished"  -> "" -- missing
       "mathesis"      -> "speciale"
       "phdthesis"     -> "ph.d.-afhandling"
       "candthesis"    -> "kandidatafhandling"
       "techreport"    -> "teknisk rapport"
       "resreport"     -> "forskningsrapport"
       "software"      -> "software"
       "datacd"        -> "data-cd"
       "audiocd"       -> "lyd-cd"
       "patent"        -> "patent"
       "patentde"      -> "tysk patent"
       "patenteu"      -> "europæisk patent"
       "patentfr"      -> "fransk patent"
       "patentuk"      -> "britisk patent"
       "patentus"      -> "amerikansk patent"
       "patreq"        -> "ansøgning om patent"
       "patreqde"      -> "ansøgning om tysk patent"
       "patreqeu"      -> "ansøgning om europæisk patent"
       "patreqfr"      -> "ansøgning om fransk patent"
       "patrequk"      -> "ansøgning om britisk patent"
       "patrequs"      -> "ansøgning om amerikansk patent"
       "countryde"     -> "Tyskland"
       "countryeu"     -> "Europæiske Union"
       "countryep"     -> "Europæiske Union"
       "countryfr"     -> "Frankrig"
       "countryuk"     -> "Storbritanien"
       "countryus"     -> "USA"
       "newseries"     -> "ny serie"
       "oldseries"     -> "gammel serie"
       _               -> k
resolveKey' (Lang "de" "DE") k =
    case map toLower k of
       "inpreparation" -> "in Vorbereitung"
       "submitted"     -> "eingereicht"
       "forthcoming"   -> "im Erscheinen"
       "inpress"       -> "im Druck"
       "prepublished"  -> "Vorveröffentlichung"
       "mathesis"      -> "Magisterarbeit"
       "phdthesis"     -> "Dissertation"
       -- "candthesis" -> "" -- missing
       "techreport"    -> "Technischer Bericht"
       "resreport"     -> "Forschungsbericht"
       "software"      -> "Computer-Software"
       "datacd"        -> "CD-ROM"
       "audiocd"       -> "Audio-CD"
       "patent"        -> "Patent"
       "patentde"      -> "deutsches Patent"
       "patenteu"      -> "europäisches Patent"
       "patentfr"      -> "französisches Patent"
       "patentuk"      -> "britisches Patent"
       "patentus"      -> "US-Patent"
       "patreq"        -> "Patentanmeldung"
       "patreqde"      -> "deutsche Patentanmeldung"
       "patreqeu"      -> "europäische Patentanmeldung"
       "patreqfr"      -> "französische Patentanmeldung"
       "patrequk"      -> "britische Patentanmeldung"
       "patrequs"      -> "US-Patentanmeldung"
       "countryde"     -> "Deutschland"
       "countryeu"     -> "Europäische Union"
       "countryep"     -> "Europäische Union"
       "countryfr"     -> "Frankreich"
       "countryuk"     -> "Großbritannien"
       "countryus"     -> "USA"
       "newseries"     -> "neue Folge"
       "oldseries"     -> "alte Folge"
       _               -> k
resolveKey' (Lang "en" "US") k =
  case map toLower k of
       "audiocd"        -> "audio CD"
       "by"             -> "by"
       "candthesis"     -> "Candidate thesis"
       "countryde"      -> "Germany"
       "countryep"      -> "European Union"
       "countryeu"      -> "European Union"
       "countryfr"      -> "France"
       "countryuk"      -> "United Kingdom"
       "countryus"      -> "United States of America"
       "datacd"         -> "data CD"
       "edition"        -> "ed."
       "forthcoming"    -> "forthcoming"
       "inpreparation"  -> "in preparation"
       "inpress"        -> "in press"
       "introduction"   -> "introduction"
       "jourser"        -> "ser."
       "mathesis"       -> "Master’s thesis"
       "newseries"      -> "new series"
       "nodate"         -> "n. d."
       "number"         -> "no."
       "numbers"        -> "nos."
       "oldseries"      -> "old series"
       "patent"         -> "patent"
       "patentde"       -> "German patent"
       "patenteu"       -> "European patent"
       "patentfr"       -> "French patent"
       "patentuk"       -> "British patent"
       "patentus"       -> "U.S. patent"
       "patreq"         -> "patent request"
       "patreqde"       -> "German patent request"
       "patreqeu"       -> "European patent request"
       "patreqfr"       -> "French patent request"
       "patrequk"       -> "British patent request"
       "patrequs"       -> "U.S. patent request"
       "phdthesis"      -> "PhD thesis"
       "prepublished"   -> "pre-published"
       "pseudonym"      -> "pseud."
       "recorded"       -> "recorded"
       "resreport"      -> "research report"
       "reviewof"       -> "Review of"
       "revisededition" -> "rev. ed."
       "software"       -> "computer software"
       "submitted"      -> "submitted"
       "techreport"     -> "technical report"
       "volume"         -> "vol."
       _               -> k
resolveKey' (Lang "es" "ES") k =
    case map toLower k of
       -- "inpreparation" -> "" -- missing
       -- "submitted"     -> "" -- missing
       "forthcoming"   -> "previsto"    -- csl
       "inpress"       -> "en imprenta" -- csl
       -- "prepublished"  -> "" -- missing
       "mathesis"      -> "Tesis de licenciatura"
       "phdthesis"     -> "Tesis doctoral"
       -- "candthesis" -> "" -- missing
       "techreport"    -> "informe técnico"
       -- "resreport"  -> "" -- missing
       -- "software"   -> "" -- missing
       -- "datacd"     -> "" -- missing
       -- "audiocd"    -> "" -- missing
       "patent"        -> "patente"
       "patentde"      -> "patente alemana"
       "patenteu"      -> "patente europea"
       "patentfr"      -> "patente francesa"
       "patentuk"      -> "patente británica"
       "patentus"      -> "patente americana"
       "patreq"        -> "solicitud de patente"
       "patreqde"      -> "solicitud de patente alemana"
       "patreqeu"      -> "solicitud de patente europea"
       "patreqfr"      -> "solicitud de patente francesa"
       "patrequk"      -> "solicitud de patente británica"
       "patrequs"      -> "solicitud de patente americana"
       "countryde"     -> "Alemania"
       "countryeu"     -> "Unión Europea"
       "countryep"     -> "Unión Europea"
       "countryfr"     -> "Francia"
       "countryuk"     -> "Reino Unido"
       "countryus"     -> "Estados Unidos de América"
       "newseries"     -> "nueva época"
       "oldseries"     -> "antigua época"
       _               -> k
resolveKey' (Lang "fi" "FI") k =
    case map toLower k of
       -- "inpreparation" -> ""      -- missing
       -- "submitted"     -> ""      -- missing
       "forthcoming"   -> "tulossa"  -- csl
       "inpress"       -> "painossa" -- csl
       -- "prepublished"  -> ""      -- missing
       "mathesis"      -> "tutkielma"
       "phdthesis"     -> "tohtorinväitöskirja"
       "candthesis"    -> "kandidat"
       "techreport"    -> "tekninen raportti"
       "resreport"     -> "tutkimusraportti"
       "software"      -> "ohjelmisto"
       "datacd"        -> "data-CD"
       "audiocd"       -> "ääni-CD"
       "patent"        -> "patentti"
       "patentde"      -> "saksalainen patentti"
       "patenteu"      -> "Euroopan Unionin patentti"
       "patentfr"      -> "ranskalainen patentti"
       "patentuk"      -> "englantilainen patentti"
       "patentus"      -> "yhdysvaltalainen patentti"
       "patreq"        -> "patenttihakemus"
       "patreqde"      -> "saksalainen patenttihakemus"
       "patreqeu"      -> "Euroopan Unionin patenttihakemus"
       "patreqfr"      -> "ranskalainen patenttihakemus"
       "patrequk"      -> "englantilainen patenttihakemus"
       "patrequs"      -> "yhdysvaltalainen patenttihakemus"
       "countryde"     -> "Saksa"
       "countryeu"     -> "Euroopan Unioni"
       "countryep"     -> "Euroopan Unioni"
       "countryfr"     -> "Ranska"
       "countryuk"     -> "Iso-Britannia"
       "countryus"     -> "Yhdysvallat"
       "newseries"     -> "uusi sarja"
       "oldseries"     -> "vanha sarja"
       _               -> k

resolveKey' (Lang "fr" "FR") k =
    case map toLower k of
       "inpreparation" -> "en préparation"
       "submitted"     -> "soumis"
       "forthcoming"   -> "à paraître"
       "inpress"       -> "sous presse"
       "prepublished"  -> "prépublié"
       "mathesis"      -> "mémoire de master"
       "phdthesis"     -> "thèse de doctorat"
       "candthesis"    -> "thèse de candidature"
       "techreport"    -> "rapport technique"
       "resreport"     -> "rapport scientifique"
       "software"      -> "logiciel"
       "datacd"        -> "cédérom"
       "audiocd"       -> "disque compact audio"
       "patent"        -> "brevet"
       "patentde"      -> "brevet allemand"
       "patenteu"      -> "brevet européen"
       "patentfr"      -> "brevet français"
       "patentuk"      -> "brevet britannique"
       "patentus"      -> "brevet américain"
       "patreq"        -> "demande de brevet"
       "patreqde"      -> "demande de brevet allemand"
       "patreqeu"      -> "demande de brevet européen"
       "patreqfr"      -> "demande de brevet français"
       "patrequk"      -> "demande de brevet britannique"
       "patrequs"      -> "demande de brevet américain"
       "countryde"     -> "Allemagne"
       "countryeu"     -> "Union européenne"
       "countryep"     -> "Union européenne"
       "countryfr"     -> "France"
       "countryuk"     -> "Royaume-Uni"
       "countryus"     -> "États-Unis"
       "newseries"     -> "nouvelle série"
       "oldseries"     -> "ancienne série"
       _               -> k
resolveKey' (Lang "it" "IT") k =
    case map toLower k of
       -- "inpreparation" -> "" -- missing
       -- "submitted"     -> "" -- missing
       "forthcoming"   -> "futuro" -- csl
       "inpress"       -> "in stampa"
       -- "prepublished"  -> "" -- missing
       "mathesis"      -> "tesi di laurea magistrale"
       "phdthesis"     -> "tesi di dottorato"
       -- "candthesis" -> "" -- missing
       "techreport"    -> "rapporto tecnico"
       "resreport"     -> "rapporto di ricerca"
       -- "software"   -> "" -- missing
       -- "datacd"     -> "" -- missing
       -- "audiocd"    -> "" -- missing
       "patent"        -> "brevetto"
       "patentde"      -> "brevetto tedesco"
       "patenteu"      -> "brevetto europeo"
       "patentfr"      -> "brevetto francese"
       "patentuk"      -> "brevetto britannico"
       "patentus"      -> "brevetto americano"
       "patreq"        -> "brevetto richiesto"
       "patreqde"      -> "brevetto tedesco richiesto"
       "patreqeu"      -> "brevetto europeo richiesto"
       "patreqfr"      -> "brevetto francese richiesto"
       "patrequk"      -> "brevetto britannico richiesto"
       "patrequs"      -> "brevetto U.S.A. richiesto"
       "countryde"     -> "Germania"
       "countryeu"     -> "Unione Europea"
       "countryep"     -> "Unione Europea"
       "countryfr"     -> "Francia"
       "countryuk"     -> "Regno Unito"
       "countryus"     -> "Stati Uniti d’America"
       "newseries"     -> "nuova serie"
       "oldseries"     -> "vecchia serie"
       _               -> k
resolveKey' (Lang "nl" "NL") k =
    case map toLower k of
       "inpreparation" -> "in voorbereiding"
       "submitted"     -> "ingediend"
       "forthcoming"   -> "onderweg"
       "inpress"       -> "in druk"
       "prepublished"  -> "voorpublicatie"
       "mathesis"      -> "masterscriptie"
       "phdthesis"     -> "proefschrift"
       -- "candthesis" -> "" -- missing
       "techreport"    -> "technisch rapport"
       "resreport"     -> "onderzoeksrapport"
       "software"      -> "computersoftware"
       "datacd"        -> "cd-rom"
       "audiocd"       -> "audio-cd"
       "patent"        -> "patent"
       "patentde"      -> "Duits patent"
       "patenteu"      -> "Europees patent"
       "patentfr"      -> "Frans patent"
       "patentuk"      -> "Brits patent"
       "patentus"      -> "Amerikaans patent"
       "patreq"        -> "patentaanvraag"
       "patreqde"      -> "Duitse patentaanvraag"
       "patreqeu"      -> "Europese patentaanvraag"
       "patreqfr"      -> "Franse patentaanvraag"
       "patrequk"      -> "Britse patentaanvraag"
       "patrequs"      -> "Amerikaanse patentaanvraag"
       "countryde"     -> "Duitsland"
       "countryeu"     -> "Europese Unie"
       "countryep"     -> "Europese Unie"
       "countryfr"     -> "Frankrijk"
       "countryuk"     -> "Verenigd Koninkrijk"
       "countryus"     -> "Verenigde Staten van Amerika"
       "newseries"     -> "nieuwe reeks"
       "oldseries"     -> "oude reeks"
       _               -> k
resolveKey' (Lang "pl" "PL") k =
    case map toLower k of
       "inpreparation" -> "przygotowanie"
       "submitted"     -> "prezentacja"
       "forthcoming"   -> "przygotowanie"
       "inpress"       -> "wydrukowane"
       "prepublished"  -> "przedwydanie"
       "mathesis"      -> "praca magisterska"
       "phdthesis"     -> "praca doktorska"
       "techreport"    -> "sprawozdanie techniczne"
       "resreport"     -> "sprawozdanie naukowe"
       "software"      -> "oprogramowanie"
       "datacd"        -> "CD-ROM"
       "audiocd"       -> "audio CD"
       "patent"        -> "patent"
       "patentde"      -> "patent Niemiec"
       "patenteu"      -> "patent Europy"
       "patentfr"      -> "patent Francji"
       "patentuk"      -> "patent Wielkiej Brytanji"
       "patentus"      -> "patent USA"
       "patreq"        -> "podanie na patent"
       "patreqeu"      -> "podanie na patent Europy"
       "patrequs"      -> "podanie na patent USA"
       "countryde"     -> "Niemcy"
       "countryeu"     -> "Unia Europejska"
       "countryep"     -> "Unia Europejska"
       "countryfr"     -> "Francja"
       "countryuk"     -> "Wielka Brytania"
       "countryus"     -> "Stany Zjednoczone Ameryki"
       "newseries"     -> "nowa serja"
       "oldseries"     -> "stara serja"
       _               -> k
resolveKey' (Lang "pt" "PT") k =
    case map toLower k of
       -- "candthesis" -> "" -- missing
       "techreport"    -> "relatório técnico"
       "resreport"     -> "relatório de pesquisa"
       "software"      -> "software"
       "datacd"        -> "CD-ROM"
       "patent"        -> "patente"
       "patentde"      -> "patente alemã"
       "patenteu"      -> "patente européia"
       "patentfr"      -> "patente francesa"
       "patentuk"      -> "patente britânica"
       "patentus"      -> "patente americana"
       "patreq"        -> "pedido de patente"
       "patreqde"      -> "pedido de patente alemã"
       "patreqeu"      -> "pedido de patente européia"
       "patreqfr"      -> "pedido de patente francesa"
       "patrequk"      -> "pedido de patente britânica"
       "patrequs"      -> "pedido de patente americana"
       "countryde"     -> "Alemanha"
       "countryeu"     -> "União Europeia"
       "countryep"     -> "União Europeia"
       "countryfr"     -> "França"
       "countryuk"     -> "Reino Unido"
       "countryus"     -> "Estados Unidos da América"
       "newseries"     -> "nova série"
       "oldseries"     -> "série antiga"
       -- "inpreparation" -> "" -- missing
       "forthcoming"   -> "a publicar" -- csl
       "inpress"       -> "na imprensa"
       -- "prepublished"  -> "" -- missing
       "mathesis"      -> "tese de mestrado"
       "phdthesis"     -> "tese de doutoramento"
       "audiocd"       -> "CD áudio"
       _               -> k
resolveKey' (Lang "pt" "BR") k =
    case map toLower k of       
       -- "candthesis" -> "" -- missing
       "techreport"    -> "relatório técnico"
       "resreport"     -> "relatório de pesquisa"
       "software"      -> "software"
       "datacd"        -> "CD-ROM"
       "patent"        -> "patente"
       "patentde"      -> "patente alemã"
       "patenteu"      -> "patente européia"
       "patentfr"      -> "patente francesa"
       "patentuk"      -> "patente britânica"
       "patentus"      -> "patente americana"
       "patreq"        -> "pedido de patente"
       "patreqde"      -> "pedido de patente alemã"
       "patreqeu"      -> "pedido de patente européia"
       "patreqfr"      -> "pedido de patente francesa"
       "patrequk"      -> "pedido de patente britânica"
       "patrequs"      -> "pedido de patente americana"
       "countryde"     -> "Alemanha"
       "countryeu"     -> "União Europeia"
       "countryep"     -> "União Europeia"
       "countryfr"     -> "França"
       "countryuk"     -> "Reino Unido"
       "countryus"     -> "Estados Unidos da América"
       "newseries"     -> "nova série"
       "oldseries"     -> "série antiga"
       "inpreparation" -> "em preparação"
       "forthcoming"   -> "aceito para publicação"
       "inpress"       -> "no prelo"
       "prepublished"  -> "pré-publicado"
       "mathesis"      -> "dissertação de mestrado"
       "phdthesis"     -> "tese de doutorado"
       "audiocd"       -> "CD de áudio"
       _               -> k
resolveKey' (Lang "sv" "SE") k =
    case map toLower k of
       -- "inpreparation" -> "" -- missing
       -- "submitted"     -> "" -- missing
       "forthcoming"   -> "kommande" -- csl
       "inpress"       -> "i tryck"  -- csl
       -- "prepublished"  -> "" -- missing
       "mathesis"      -> "examensarbete"
       "phdthesis"     -> "doktorsavhandling"
       "candthesis"    -> "kandidatavhandling"
       "techreport"    -> "teknisk rapport"
       "resreport"     -> "forskningsrapport"
       "software"      -> "datorprogram"
       "datacd"        -> "data-cd"
       "audiocd"       -> "ljud-cd"
       "patent"        -> "patent"
       "patentde"      -> "tyskt patent"
       "patenteu"      -> "europeiskt patent"
       "patentfr"      -> "franskt patent"
       "patentuk"      -> "brittiskt patent"
       "patentus"      -> "amerikanskt patent"
       "patreq"        -> "patentansökan"
       "patreqde"      -> "ansökan om tyskt patent"
       "patreqeu"      -> "ansökan om europeiskt patent"
       "patreqfr"      -> "ansökan om franskt patent"
       "patrequk"      -> "ansökan om brittiskt patent"
       "patrequs"      -> "ansökan om amerikanskt patent"
       "countryde"     -> "Tyskland"
       "countryeu"     -> "Europeiska unionen"
       "countryep"     -> "Europeiska unionen"
       "countryfr"     -> "Frankrike"
       "countryuk"     -> "Storbritannien"
       "countryus"     -> "USA"
       "newseries"     -> "ny följd"
       "oldseries"     -> "gammal följd"
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
           untitlecase     :: Bool
         , localeLanguage  :: Lang
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
  return RefDate { year   = Literal $ dropWhile (=='0') year'
                 , month  = Literal $ dropWhile (=='0') month'
                 , season = mempty
                 , day    = Literal $ dropWhile (=='0') day'
                 , other  = mempty
                 , circa  = False
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
  let start' = RefDate { year   = Literal $ if isNumber year' then year' else ""
                       , month  = Literal $ month'
                       , season = mempty
                       , day    = Literal day'
                       , other  = Literal $ if isNumber year' then "" else year'
                       , circa  = False
                       }
  let end' = if null endyear'
                then []
                else [RefDate { year   = Literal $ if isNumber endyear' then endyear' else ""
                              , month  = Literal $ endmonth'
                              , day    = Literal $ endday'
                              , season = mempty
                              , other  = Literal $ if isNumber endyear' then "" else endyear'
                              , circa  = False
                              }]
  return (start':end')

getRawField :: String -> Bib String
getRawField f = do
  fs <- asks fields
  case lookup f fs of
       Just x  -> return x
       Nothing -> notFound f

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
          , literal         = Formatted [Str "others"]
          , commaSuffix     = False
          , parseNames      = False
          }
toAuthor _ [Span ("",[],[]) ils] =
  return $ -- corporate author
    Agent { givenName       = []
          , droppingPart    = mempty
          , nonDroppingPart = mempty
          , familyName      = mempty
          , nameSuffix      = mempty
          , literal         = Formatted ils
          , commaSuffix     = False
          , parseNames      = False
          }
-- First von Last
-- von Last, First
-- von Last, Jr ,First
-- NOTE: biblatex and bibtex differ on:
-- Drummond de Andrade, Carlos
-- bibtex takes "Drummond de" as the von;
-- biblatex takes the whole as a last name.
-- See https://github.com/plk/biblatex/issues/236
-- Here we implement the more sensible biblatex behavior.
toAuthor opts ils = do
  let useprefix = optionSet "useprefix" opts
  let usecomma  = optionSet "juniorcomma" opts
  let bibtex    = optionSet "bibtex" opts
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

  let (von, lastname) =
         if bibtex
            then case span isCapitalized $ reverse vonlast of
                        ([],(w:ws))    -> (reverse ws, [w])
                        (vs, ws)       -> (reverse ws, reverse vs)
            else case span (not . isCapitalized) vonlast of
                        (vs@(_:_), []) -> (init vs, [last vs])
                        (vs, ws)       -> (vs, ws)
  let prefix = Formatted $ intercalate [Space] von
  let family = Formatted $ intercalate [Space] lastname
  let suffix = Formatted $ intercalate [Space] jr
  let givens = map Formatted first
  return $
    Agent { givenName       = givens
          , droppingPart    = if useprefix then mempty else prefix
          , nonDroppingPart = if useprefix then prefix else mempty
          , familyName      = family
          , nameSuffix      = suffix
          , literal         = mempty
          , commaSuffix     = usecomma
          , parseNames      = False
          }

isCapitalized :: [Inline] -> Bool
isCapitalized (Str (c:cs) : rest)
  | isUpper c = True
  | isDigit c = isCapitalized (Str cs : rest)
  | otherwise = False
isCapitalized (_:rest) = isCapitalized rest
isCapitalized [] = True

optionSet :: String -> Options -> Bool
optionSet key opts = case lookup key opts of
                      Just "true" -> True
                      Just s      -> s == mempty
                      _           -> False

latex' :: String -> [Block]
latex' s =
#if MIN_VERSION_pandoc(1,14,0)
  case readLaTeX def{readerParseRaw = True} s of
                Right (Pandoc _ bs) -> bs
                _                   -> []
#else
  case readLaTeX def{readerParseRaw = True} s of
                Pandoc _ bs         -> bs
#endif

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
                           _     -> accum ++ (Str [sep] : Space : s)

type Options = [(String, String)]

parseOptions :: String -> Options
parseOptions = map breakOpt . splitWhen (==',')
  where breakOpt x = case break (=='=') x of
                          (w,v) -> (map toLower $ trim w,
                                    map toLower $ trim $ drop 1 v)

ordinalize :: Locale -> String -> String
ordinalize locale n =
  case [termSingular c | c <- terms, cslTerm c == ("ordinal-" ++ pad0 n)] ++
       [termSingular c | c <- terms, cslTerm c == "ordinal"] of
       (suff:_) -> n ++ suff
       []       -> n
    where pad0 [c] = ['0',c]
          pad0 s   = s
          terms = localeTerms locale

itemToReference :: Lang -> Locale -> Bool -> Item -> Maybe Reference
itemToReference lang locale bibtex = bib $ do
  modify $ \st -> st{ localeLanguage = lang,
                      untitlecase = case lang of
                                         Lang "en" _ -> True
                                         _           -> False }
  id' <- asks identifier
  et <- asks entryType
  guard $ et /= "xdata"
  opts <- (parseOptions <$> getRawField "options") <|> return []
  let getAuthorList' = getAuthorList
         (("bibtex", map toLower $ show bibtex):opts)
  st <- getRawField "entrysubtype" <|> return mempty
  isEvent <- (True <$ (getRawField "eventdate"
                     <|> getRawField "eventtitle"
                     <|> getRawField "venue")) <|> return False
  reftype' <- resolveKey lang <$> getField "type" <|> return mempty
  let (reftype, refgenre) = case et of
       "article"
         | st == "magazine"  -> (ArticleMagazine,mempty)
         | st == "newspaper" -> (ArticleNewspaper,mempty)
         | otherwise         -> (ArticleJournal,mempty)
       "book"            -> (Book,mempty)
       "booklet"         -> (Pamphlet,mempty)
       "bookinbook"      -> (Chapter,mempty)
       "collection"      -> (Book,mempty)
       "electronic"      -> (Webpage,mempty)
       "inbook"          -> (Chapter,mempty)
       "incollection"    -> (Chapter,mempty)
       "inreference"     -> (EntryEncyclopedia,mempty)
       "inproceedings"   -> (PaperConference,mempty)
       "manual"          -> (Book,mempty)
       "mastersthesis"   -> (Thesis, if reftype' == mempty
                                        then Formatted [Str $ resolveKey' lang "mathesis"]
                                        else reftype')
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
       "phdthesis"       -> (Thesis, if reftype' == mempty
                                        then Formatted [Str $ resolveKey' lang "phdthesis"]
                                        else reftype')
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
       "unpublished"     -> (if isEvent then Speech else Manuscript,mempty)
       "www"             -> (Webpage,mempty)
       -- biblatex, "unsupported"
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

  -- hyphenation:
  let defaultHyphenation = case lang of
                                Lang x y -> x ++ "-" ++ y
  let getLangId = do
           langid <- (trim . map toLower) <$> getRawField "langid"
           idopts <- (trim . map toLower) <$>
                         getRawField "langidopts" <|> return ""
           case (langid, idopts) of
                ("english","variant=british")    -> return "british"
                ("english","variant=american")   -> return "american"
                ("english","variant=us")         -> return "american"
                ("english","variant=usmax")      -> return "american"
                ("english","variant=uk")         -> return "british"
                ("english","variant=australian") -> return "australian"
                ("english","variant=newzealand") -> return "newzealand"
                (x,_)                            -> return x
  hyphenation <- ((toLocale . map toLower) <$>
                   (getLangId <|> getRawField "hyphenation"))
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
  let isArticle = et `elem` ["article", "periodical", "suppperiodical", "review"]
  let isPeriodical = et == "periodical"
  let isChapterlike = et `elem`
         ["inbook","incollection","inproceedings","inreference","bookinbook"]
  hasMaintitle <- (True <$ (getRawField "maintitle")) <|> return False
  let hyphenation' = if null hyphenation
                     then defaultHyphenation
                     else hyphenation
  let la = case splitWhen (== '-') hyphenation' of
                      (x:_) -> x
                      []    -> mempty
  modify $ \s -> s{ untitlecase = la == "en" }

  title' <- (guard isPeriodical >> getTitle "issuetitle")
            <|> (guard hasMaintitle >> guard (not isChapterlike) >> getTitle "maintitle")
            <|> getTitle "title"
            <|> return mempty
  subtitle' <- (guard isPeriodical >> getTitle "issuesubtitle")
               <|> (guard hasMaintitle >> guard (not isChapterlike) >> getTitle "mainsubtitle")
               <|> getTitle "subtitle"
               <|> return mempty
  titleaddon' <- (guard hasMaintitle >> guard (not isChapterlike) >> getTitle "maintitleaddon")
                 <|> getTitle "titleaddon"
                 <|> return mempty
  volumeTitle' <- (guard hasMaintitle >> guard (not isChapterlike) >> getTitle "title")
                  <|> (guard hasMaintitle >> guard isChapterlike >> getTitle "booktitle")
                  <|> return mempty
  volumeSubtitle' <- (guard hasMaintitle >> guard (not isChapterlike) >> getTitle "subtitle")
                     <|> (guard hasMaintitle >> guard isChapterlike >> getTitle "booksubtitle")
                     <|> return mempty
  volumeTitleAddon' <- (guard hasMaintitle >> guard (not isChapterlike) >> getTitle "titleaddon")
                       <|> (guard hasMaintitle >> guard isChapterlike >> getTitle "booktitleaddon")
                       <|> return mempty
  containerTitle' <- (guard isPeriodical >> getPeriodicalTitle "title")
                     <|> (guard isChapterlike >> getTitle "maintitle")
                     <|> (guard isChapterlike >> getTitle "booktitle")
                     <|> getPeriodicalTitle "journaltitle"
                     <|> getPeriodicalTitle "journal"
                     <|> return mempty
  containerSubtitle' <- (guard isPeriodical >> getPeriodicalTitle "subtitle")
                        <|> (guard isChapterlike >> getTitle "mainsubtitle")
                        <|> (guard isChapterlike >> getTitle "booksubtitle")
                        <|> getPeriodicalTitle "journalsubtitle"
                        <|> return mempty
  containerTitleAddon' <- (guard isPeriodical >> getPeriodicalTitle "titleaddon")
                          <|> (guard isChapterlike >> getTitle "maintitleaddon")
                          <|> (guard isChapterlike >> getTitle "booktitleaddon")
                          <|> return mempty
  containerTitleShort' <- (guard isPeriodical >> guard (not hasMaintitle)
                       >> getField "shorttitle")
                        <|> getPeriodicalTitle "shortjournal"
                        <|> return mempty
  -- change numerical series title to e.g. 'series 3'
  let fixSeriesTitle (Formatted [Str xs]) | all isDigit xs =
         Formatted [Str (ordinalize locale xs),
                    Space, Str (resolveKey' lang "ser.")]
      fixSeriesTitle x = x
  seriesTitle' <- (fixSeriesTitle . resolveKey lang) <$>
                      getTitle "series" <|> return mempty
  shortTitle' <- (guard (not hasMaintitle || isChapterlike) >>
                        getTitle "shorttitle")
               <|> if (subtitle' /= mempty || titleaddon' /= mempty) &&
                      (not hasMaintitle)
                      then getShortTitle False "title"
                      else getShortTitle True  "title"
               <|> return mempty

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
  pmid' <- getRawField "pmid" <|> return mempty
  pmcid' <- getRawField "pmcid" <|> return mempty
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
                         (x:_) | other x == Literal "forthcoming" ->
                                     return (Formatted [Str "forthcoming"])
                         _ -> return mempty
                 )

  let convertEnDash (Str s) = Str (map (\c -> if c == '–' then '-' else c) s)
      convertEnDash x       = x

  let takeDigits (Str xs : _) =
         case takeWhile isDigit xs of
              []               -> []
              ds               -> [Str ds]
      takeDigits x             = x

  return $ emptyReference
         { refId               = Literal id'
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
         , collectionTitle     = seriesTitle'
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
         , pageFirst           = Formatted $ takeDigits $ unFormatted pages'
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
         , url                 = Literal url'
         , doi                 = Literal doi'
         , isbn                = Literal isbn'
         , issn                = Literal issn'
         , pmcid               = Literal pmcid'
         , pmid                = Literal pmid'
         , language            = Literal hyphenation
         , callNumber          = Literal callNumber'
         }
