{-# LANGUAGE OverloadedStrings, TupleSections #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.CSL.Parser
-- Copyright   :  (C) 2014 John MacFarlane
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  John MacFarlane <jgm@berkeley.edu>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Parser for CSL XML files.
-----------------------------------------------------------------------------

module Text.CSL.Parser (readCSLFile, parseCSL, parseCSL',
                        parseLocale, localizeCSL)
where
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import qualified Control.Exception as E
import Control.Monad (when)
import Data.Either (lefts, rights)
import Data.Text (Text, unpack)
import Text.CSL.Style hiding (parseNames)
import Text.CSL.Util (toRead, findFile)
import System.Directory (getAppUserDataDirectory)
import qualified Text.XML as X
import Data.Default
import Text.Pandoc.Shared (safeRead)
import Text.XML.Cursor
import Data.Maybe (listToMaybe, fromMaybe)
import Text.Pandoc.UTF8 (fromStringLazy)
import Text.Pandoc.Shared (fetchItem)
import Text.CSL.Data (getLocale)

-- | Parse a 'String' into a 'Style' (with default locale).
parseCSL :: String -> Style
parseCSL = parseCSL' . fromStringLazy

-- | Parse locale.
parseLocale :: String -> IO Locale
parseLocale locale =
  parseLocaleElement . fromDocument . X.parseLBS_ def <$> getLocale locale

-- | Merge locale into a CSL style.
localizeCSL :: Maybe String -> Style -> IO Style
localizeCSL mbLocale s = do
  let locale = fromMaybe (styleDefaultLocale s) mbLocale
  l <- parseLocale locale
  return s { styleLocale = mergeLocales locale l (styleLocale s) }

-- | Read and parse a CSL style file into a localized sytle.
readCSLFile :: Maybe String -> FilePath -> IO Style
readCSLFile mbLocale src = do
  csldir <- getAppUserDataDirectory "csl"
  mbSrc <- findFile [".", csldir] src
  fetchRes <- fetchItem Nothing (fromMaybe src mbSrc)
  f <- case fetchRes of
            Left err         -> E.throwIO err
            Right (rawbs, _) -> return $ L.fromChunks [rawbs]
  let cur = fromDocument $ X.parseLBS_ def f
  -- see if it's a dependent style, and if so, try to fetch its parent:
  let pickParentCur = get "link" >=> attributeIs (X.Name "rel" Nothing Nothing) "independent-parent"
  let parentCur = cur $/ get "info" &/ pickParentCur
  let parent' = concatMap (stringAttr "href") parentCur
  when (parent' == src) $ do
    error $ "Dependent CSL style " ++ src ++ " specifies itself as parent."
  case parent' of
       ""  -> localizeCSL mbLocale $ parseCSLCursor cur
       y   -> do
           -- note, we insert locale from the dependent style:
           let mbLocale' = case stringAttr "default-locale" cur of
                                  ""   -> mbLocale
                                  x    -> Just x
           readCSLFile mbLocale' y

parseCSL' :: L.ByteString -> Style
parseCSL' = parseCSLCursor . fromDocument . X.parseLBS_ def

parseCSLCursor :: Cursor -> Style
parseCSLCursor cur =
  Style{ styleVersion = version
       , styleClass = class_
       , styleInfo = Just info
       , styleDefaultLocale = defaultLocale
       , styleLocale = locales
       , styleAbbrevs = Abbreviations M.empty
       , csOptions = filter (\(k,_) -> k `notElem`
                                       ["class",
                                        "xmlns",
                                        "version",
                                        "default-locale"]) $ parseOptions cur
       , csMacros = macros
       , citation = fromMaybe (Citation [] [] Layout{ layFormat = emptyFormatting
                                                    , layDelim = ""
                                                    , elements = [] }) $ listToMaybe $
                    cur $/ get "citation" &| parseCitation
       , biblio = listToMaybe $ cur $/ get "bibliography" &| parseBiblio
       }
  where version = unpack . T.concat $ cur $| laxAttribute "version"
        class_ = unpack . T.concat $ cur $| laxAttribute "class"
        defaultLocale = case cur $| laxAttribute "default-locale" of
                             (x:_) -> unpack x
                             []    -> "en-US"
        author = case (cur $// get "info" &/ get "author") of
                      (x:_) -> CSAuthor (x $/ get "name" &/ string)
                                 (x $/ get "email" &/ string)
                                 (x $/ get "uri"   &/ string)
                      _     -> CSAuthor "" "" ""
        info = CSInfo
          { csiTitle      = cur $/ get "info" &/ get "title" &/ string
          , csiAuthor     = author
          , csiCategories = []  -- TODO we don't really use this, and the type
                                -- in Style doesn't match current CSL at all
          , csiId         = cur $/ get "info" &/ get "id" &/ string
          , csiUpdated    = cur $/ get "info" &/ get "updated" &/ string
          }
        locales = cur $/ get "locale" &| parseLocaleElement
        macros  = cur $/ get "macro" &| parseMacroMap

get :: Text -> Axis
get name =
  element (X.Name name (Just "http://purl.org/net/xbiblio/csl") Nothing)

string :: Cursor -> String
string = unpack . T.concat . content

attrWithDefault :: Read a => Text -> a -> Cursor -> a
attrWithDefault t d cur =
  case safeRead (toRead $ stringAttr t cur) of
       Just x   -> x
       Nothing  -> d

stringAttr :: Text -> Cursor -> String
stringAttr t cur =
  case node cur of
    X.NodeElement e ->
      case M.lookup (X.Name t Nothing Nothing)
           (X.elementAttributes e) of
           Just x  -> unpack x
           Nothing -> ""
    _ -> ""

parseCslTerm :: Cursor -> CslTerm
parseCslTerm cur =
    let body = unpack $ T.dropAround (`elem` (" \t\r\n" :: String)) $
                  T.concat $ cur $/ content
    in CT
      { cslTerm        = stringAttr "name" cur
      , termForm       = attrWithDefault "form" Long cur
      , termGender     = attrWithDefault "gender" Neuter cur
      , termGenderForm = attrWithDefault "gender-form" Neuter cur
      , termSingular   = if null body
                            then cur $/ get "single" &/ string
                            else body
      , termPlural     = if null body
                            then cur $/ get "multiple" &/ string
                            else body
      , termMatch      = stringAttr "match" cur
      }

parseLocaleElement :: Cursor -> Locale
parseLocaleElement cur = Locale
      { localeVersion = unpack $ T.concat version
      , localeLang    = unpack $ T.concat lang
      , localeOptions = concat $ cur $/ get "style-options" &| parseOptions
      , localeTerms   = terms
      , localeDate    = concat $ cur $/ get "date" &| parseElement
      }
  where version = cur $| laxAttribute "version"
        lang    = cur $| laxAttribute "lang"
        terms   = cur $/ get "terms" &/ get "term" &| parseCslTerm

parseElement :: Cursor -> [Element]
parseElement cur =
  case node cur of
       X.NodeElement e ->
         case X.nameLocalName $ X.elementName e of
              "term" -> parseTerm cur
              "text" -> parseText cur
              "choose" -> parseChoose cur
              "group" -> parseGroup cur
              "label" -> parseLabel cur
              "number" -> parseNumber cur
              "substitute" -> parseSubstitute cur
              "names" -> parseNames cur
              "date" -> parseDate cur
              _ -> []
       _ -> []

getFormatting :: Cursor -> Formatting
getFormatting cur =
  emptyFormatting{
    prefix  = stringAttr "prefix" cur
  , suffix  = stringAttr "suffix" cur
  , fontFamily = stringAttr "font-family" cur
  , fontStyle = stringAttr "font-style" cur
  , fontVariant = stringAttr "font-variant" cur
  , fontWeight = stringAttr "font-weight" cur
  , textDecoration = stringAttr "text-decoration" cur
  , verticalAlign = stringAttr "vertical-align" cur
  , textCase = stringAttr "text-case" cur
  , display = stringAttr "display" cur
  , quotes = if attrWithDefault "quotes" False cur
                then NativeQuote
                else NoQuote
  , stripPeriods = attrWithDefault "strip-periods" False cur
  , noCase = attrWithDefault "no-case" False cur
  , noDecor = attrWithDefault "no-decor" False cur
  }

parseDate :: Cursor -> [Element]
parseDate cur = [Date (words variable) form format delim parts partsAttr]
  where variable   = stringAttr "variable" cur
        form       = case stringAttr "form" cur of
                           "text"    -> TextDate
                           "numeric" -> NumericDate
                           _         -> NoFormDate
        format     = getFormatting cur
        delim      = stringAttr "delimiter" cur
        parts      = cur $/ get "date-part" &| (parseDatePart form)
        partsAttr  = stringAttr "date-parts" cur

parseDatePart :: DateForm -> Cursor -> DatePart
parseDatePart defaultForm cur =
  DatePart { dpName       = stringAttr "name" cur
           , dpForm       = case stringAttr "form" cur of
                                  ""  -> case defaultForm of
                                              TextDate -> "long"
                                              NumericDate -> "numeric"
                                              _ -> "long"
                                  x    -> x
           , dpRangeDelim = case stringAttr "range-delimiter" cur of
                                  ""  -> "-"
                                  x   -> x
           , dpFormatting = getFormatting cur
           }

parseNames :: Cursor -> [Element]
parseNames cur = [Names (words variable) names formatting delim others]
  where variable   = stringAttr "variable" cur
        formatting = getFormatting cur
        delim      = stringAttr "delimiter" cur
        elts       = cur $/ parseName
        names      = case rights elts of
                          [] -> [Name NotSet emptyFormatting [] [] []]
                          xs -> xs
        others     = lefts elts

parseName :: Cursor -> [Either Element Name]
parseName cur =
  case node cur of
       X.NodeElement e ->
         case X.nameLocalName $ X.elementName e of
              "name"   -> [Right $ Name (attrWithDefault "form" NotSet cur)
                              format (nameAttrs e) delim nameParts]
              "label"  -> [Right $ NameLabel (attrWithDefault "form" Long cur)
                              format plural]
              "et-al"  -> [Right $ EtAl format $ stringAttr "term" cur]
              _        -> map Left $ parseElement cur
       _ -> map Left $ parseElement cur
   where format    = getFormatting cur
         plural    = attrWithDefault "plural" Contextual cur
         delim     = stringAttr "delimiter" cur
         nameParts = cur $/ get "name-part" &| parseNamePart
         nameAttrs x = [(T.unpack n, T.unpack v) |
                 (X.Name n _ _, v) <- M.toList (X.elementAttributes x),
                 n `elem` nameAttrKeys]
         nameAttrKeys =  [ "et-al-min"
                         , "et-al-use-first"
                         , "et-al-subsequent-min"
                         , "et-al-subsequent-use-first"
                         , "et-al-use-last"
                         , "delimiter-precedes-et-al"
                         , "and"
                         , "delimiter-precedes-last"
                         , "sort-separator"
                         , "initialize"
                         , "initialize-with"
                         , "name-as-sort-order" ]


parseNamePart :: Cursor -> NamePart
parseNamePart cur = NamePart s format
   where format    = getFormatting cur
         s         = stringAttr "name" cur

parseSubstitute :: Cursor -> [Element]
parseSubstitute cur = [Substitute (cur $/ parseElement)]

parseTerm :: Cursor -> [Element]
parseTerm cur =
  let termForm'      = attrWithDefault "form" Long cur
      formatting     = getFormatting cur
      plural         = attrWithDefault "plural" True cur
      name           = stringAttr "name" cur
  in  [Term name termForm' formatting plural]

parseText :: Cursor -> [Element]
parseText cur =
  let term           = stringAttr "term" cur
      variable       = stringAttr "variable" cur
      macro          = stringAttr "macro" cur
      value          = stringAttr "value" cur
      delim          = stringAttr "delimiter" cur
      formatting     = getFormatting cur
      plural         = attrWithDefault "plural" True cur
      textForm       = attrWithDefault "form" Long cur
  in  if not (null term)
         then [Term term textForm formatting plural]
         else if not (null macro)
              then [Macro macro formatting]
              else if not (null variable)
                      then [Variable (words variable) textForm formatting delim]
                      else if not (null value)
                           then [Const value formatting]
                           else []

parseChoose :: Cursor -> [Element]
parseChoose cur =
  let ifPart         = cur $/ get "if" &| parseIf
      elseIfPart     = cur $/ get "else-if" &| parseIf
      elsePart       = cur $/ get "else" &/ parseElement
  in  [Choose (head ifPart) elseIfPart elsePart]

parseIf :: Cursor -> IfThen
parseIf cur = IfThen cond mat elts
  where cond = Condition {
                 isType          = go "type"
               , isSet           = go "variable"
               , isNumeric       = go "is-numeric"
               , isUncertainDate = go "is-uncertain-date"
               , isPosition      = go "position"
               , disambiguation  = go "disambiguate"
               , isLocator       = go "locator"
               }
        mat  = attrWithDefault "match" All cur
        elts = cur $/ parseElement
        go x = words $ stringAttr x cur

parseLabel :: Cursor -> [Element]
parseLabel cur = [Label variable form formatting plural]
  where variable   = stringAttr "variable" cur
        form       = attrWithDefault "form" Long cur
        formatting = getFormatting cur
        plural     = attrWithDefault "plural" Contextual cur

parseNumber :: Cursor -> [Element]
parseNumber cur = [Number variable numForm formatting]
  where variable   = stringAttr "variable" cur
        numForm    = attrWithDefault "form" Numeric cur
        formatting = getFormatting cur

parseGroup :: Cursor -> [Element]
parseGroup cur =
  let elts           = cur $/ parseElement
      delim          = stringAttr "delimiter" cur
      formatting     = getFormatting cur
  in  [Group formatting delim elts]

parseMacroMap :: Cursor -> MacroMap
parseMacroMap cur = (name, elts)
  where name = cur $| stringAttr "name"
        elts = cur $/ parseElement

parseCitation :: Cursor -> Citation
parseCitation cur =  Citation{ citOptions = parseOptions cur
                             , citSort = concat $ cur $/ get "sort" &| parseSort
                             , citLayout = case cur $/ get "layout" &| parseLayout of
                                            (x:_) -> x
                                            []    -> Layout
                                                      { layFormat = emptyFormatting
                                                      , layDelim = ""
                                                      , elements = [] }
                             }

parseSort :: Cursor -> [Sort]
parseSort cur = concat $ cur $/ get "key" &| parseKey

parseKey :: Cursor -> [Sort]
parseKey cur =
  case stringAttr "variable" cur of
       "" ->
         case stringAttr "macro" cur of
           "" -> []
           x  -> [SortMacro x sorting (attrWithDefault "names-min" 0 cur)
                       (attrWithDefault "names-use-first" 0 cur)
                       (stringAttr "names-use-last" cur)]
       x  -> [SortVariable x sorting]
  where sorting = case stringAttr "sort" cur of
                       "descending"  -> Descending ""
                       _             -> Ascending ""

parseBiblio :: Cursor -> Bibliography
parseBiblio cur =
  Bibliography{
    bibOptions = parseOptions cur,
    bibSort = concat $ cur $/ get "sort" &| parseSort,
    bibLayout = case cur $/ get "layout" &| parseLayout of
                       (x:_) -> x
                       []    -> Layout
                                 { layFormat = emptyFormatting
                                 , layDelim = ""
                                 , elements = [] }
    }

parseOptions :: Cursor -> [Option]
parseOptions cur =
  case node cur of
    X.NodeElement e ->
     [(T.unpack n, T.unpack v) |
      (X.Name n _ _, v) <- M.toList (X.elementAttributes e)]
    _ -> []

parseLayout :: Cursor -> Layout
parseLayout cur =
  Layout
    { layFormat = getFormatting cur
    , layDelim = stringAttr "delimiter" cur
    , elements = cur $/ parseElement
    }
