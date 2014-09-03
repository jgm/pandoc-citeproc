{-# LANGUAGE OverloadedStrings, TupleSections #-}
import Control.Monad.Trans.Resource
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Conduit (($$), Sink)
import Data.Either (lefts, rights)
import Data.Text (Text, unpack)
-- import Text.XML.Stream.Parse
import Text.CSL.Style hiding (parseNames)
-- TODO, replace toRead below with: import Text.CSL.Util (toRead)
import Data.XML.Types (Event)
import Control.Applicative hiding (many, Const)
import qualified Text.XML as X
import Data.Default
import Data.Char (toUpper)
import Data.String (fromString)
import Text.Pandoc.Shared (safeRead)
import Text.XML.Cursor
import Data.Maybe (listToMaybe, fromMaybe)

main :: IO ()
main = readCSLFile "chicago-author-date.csl" >>= print

readCSLFile :: FilePath -> IO Style
readCSLFile fn = do
  doc <- X.readFile def (fromString fn)
  let cur = fromDocument doc
  let version = unpack . T.concat $ cur $| laxAttribute "version"
  let class_ = unpack . T.concat $ cur $| laxAttribute "class"
  let defaultLocale = case cur $| laxAttribute "default-locale" of
                           (x:_) -> unpack x
                           []    -> "en-US"
  let author = head (cur $// get "info" &/ get "author") -- TODO
  let info = CSInfo
        { csiTitle      = cur $/ get "info" &/ get "title" &/ string
        , csiAuthor     = CSAuthor (author $/ get "name"  &/ string)
                                   (author $/ get "email" &/ string)
                                   (author $/ get "uri"   &/ string)
        , csiCategories = []  -- TODO we don't really use this, and the type
                              -- in Style doesn't match current CSL at all
        , csiId         = cur $/ get "info" &/ get "id" &/ string
        , csiUpdated    = cur $/ get "info" &/ get "updated" &/ string
        }
  let locales = cur $/ get "locale" &/ parseLocale
  let macros  = cur $/ get "macro" &| parseMacroMap
  return Style{ styleVersion = version
              , styleClass = class_
              , styleInfo = Just info
              , styleDefaultLocale = defaultLocale
              , styleLocale = locales
              , styleAbbrevs = Abbreviations M.empty
              , csOptions = []
              , csMacros = macros
              , citation = fromMaybe (Citation [] [] Layout{ layFormat = emptyFormatting
                                                            , layDelim = ""
                                                            , elements = [] }) $ listToMaybe $
                           cur $/ get "citation" &| parseCitation
              , biblio = listToMaybe $ cur $/ get "bibliography" &| parseBiblio
              }

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
stringAttr t cur = unpack $ T.concat $ laxAttribute t cur

parseCslTerm :: Cursor -> CslTerm
parseCslTerm cur =
    let body = unpack $ T.strip $ T.concat $ cur $/ content
    in CT
      { cslTerm        = stringAttr "name" cur
      , termForm       = attrWithDefault "form" NotSet cur
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

parseLocale :: Cursor -> [Locale]
parseLocale cur = [Locale
      { localeVersion = unpack $ T.concat version
      , localeLang    = unpack $ T.concat lang
      , localeOptions = options
      , localeTerms   = terms
      , localeDate    = date
      }]
  where version = cur $| laxAttribute "version"
        lang    = cur $| laxAttribute "lang"
        options = [] -- TODO
        terms   = cur $/ get "term" &| parseCslTerm
        date    = [] -- TODO

parseElement :: Cursor -> [Element]
parseElement cur =
  case node cur of
       X.NodeElement e ->
         case X.nameLocalName $ X.elementName e of
              "const" -> [Const (stringAttr "value" cur) (getFormatting cur)]
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
        form       = attrWithDefault "form" NoFormDate cur
        format     = getFormatting cur
        delim      = stringAttr "delimiter" cur
        parts      = cur $/ get "date-part" &| parseDatePart
        partsAttr  = stringAttr "date-parts" cur

parseDatePart :: Cursor -> DatePart
parseDatePart cur =
  DatePart { dpName       = stringAttr "name" cur
           , dpForm       = attrWithDefault "form" "long" cur
           , dpRangeDelim = attrWithDefault "range-delimiter" "-" cur
           , dpFormatting = getFormatting cur
           }

parseNames :: Cursor -> [Element]
parseNames cur = [Names (words variable) names formatting delim others]
  where variable   = stringAttr "variable" cur
        form       = attrWithDefault "form" NotSet cur
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
              "name"   -> [Right $ Name form format (nameAttrs e) delim nameParts]
              "label"  -> [Right $ NameLabel form format plural]
              "et-al"  -> [Right $ EtAl format ""]
              x        -> map Left $ parseElement cur
       _ -> map Left $ parseElement cur
   where form      = attrWithDefault "form" NotSet cur
         format    = getFormatting cur
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
  let termForm       = attrWithDefault "form" Long cur
      formatting     = getFormatting cur
      plural         = attrWithDefault "plural" True cur
      name           = stringAttr "name" cur
  in  [Term name termForm formatting plural]

parseText :: Cursor -> [Element]
parseText cur =
  let term           = stringAttr "term" cur
      variable       = stringAttr "variable" cur
      macro          = stringAttr "macro" cur
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
                      else []

parseChoose :: Cursor -> [Element]
parseChoose cur =
  let ifPart         = cur $/ get "if" &| parseIf
      elseIfPart     = cur $/ get "else-if" &| parseIf
      elsePart       = cur $/ get "else" &/ parseElement
  in  [Choose (head ifPart) elseIfPart elsePart]

parseIf :: Cursor -> IfThen
parseIf cur = IfThen cond match elts
  where cond = Condition {
                 isType          = go "type"
               , isSet           = go "variable"
               , isNumeric       = go "is-numeric"
               , isUncertainDate = go "is-uncertain-date"
               , isPosition      = go "position"
               , disambiguation  = go "disambiguate"
               , isLocator       = go "locator"
               }
        match = attrWithDefault "match" All cur
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
  let termForm       = attrWithDefault "form" Long cur
      elts           = cur $/ parseElement
      delim          = stringAttr "delimiter" cur
      formatting     = getFormatting cur
  in  [Group formatting delim elts]

parseMacroMap :: Cursor -> MacroMap
parseMacroMap cur = (name, elts)
  where name = cur $| stringAttr "name"
        elts = cur $/ parseElement

toRead :: String -> String
toRead    []  = []
toRead (s:ss) = toUpper s : camel ss
    where
      camel x
          | '-':y:ys <- x = toUpper y : camel ys
          | '_':y:ys <- x = toUpper y : camel ys
          |     y:ys <- x =         y : camel ys
          | otherwise     = []

parseCitation :: Cursor -> Citation
parseCitation cur =  Citation{ citOptions = []
                             , citSort = cur $/ parseSort
                             , citLayout = Layout{
                                  layFormat = getFormatting cur
                                , layDelim = stringAttr "delimiter" cur
                                , elements = cur $/ parseElement } }
   where citOpt x = [(T.unpack n, T.unpack v) |
                 (X.Name n _ _, v) <- M.toList (X.elementAttributes x),
                 n `elem` citOptKeys]
         citOptKeys  =  [ "disambiguate-add-names"
                        , "disambiguate-add-givenname"
                        , "disambiguate-add-year-suffix"
                        , "givenname-disambiguation-rule"
                        , "collapse"
                        , "cite-group-delimiter"
                        , "year-suffix-delimiter"
                        , "after-collapse-delimiter"
                        , "near-note-distance" ]

parseSort :: Cursor -> [Sort]
parseSort _ = [] -- TODO

{-
data Sort
    = SortVariable String Sorting
    | SortMacro    String Sorting Int Int String
      deriving ( Eq, Show, Read, Typeable, Data, Generic )

data Sorting
    = Ascending  String
    | Descending String
      deriving ( Read, Show, Eq, Typeable, Data, Generic )

data Layout
    = Layout
      { layFormat ::  Formatting
      , layDelim  ::  Delimiter
      , elements  :: [Element]
      } deriving ( Show, Read, Typeable, Data, Generic )
-}




parseBiblio :: Cursor -> Bibliography
parseBiblio cur = Bibliography{}
