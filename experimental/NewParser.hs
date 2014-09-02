{-# LANGUAGE OverloadedStrings, TupleSections #-}
import Control.Monad.Trans.Resource
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Conduit (($$), Sink)
import Data.Text (Text, unpack)
-- import Text.XML.Stream.Parse
import Text.CSL.Style
import Data.XML.Types (Event)
import Control.Applicative hiding (many, Const)
import qualified Text.XML as X
import Data.Default
import Text.Pandoc.Shared (safeRead)
import Text.XML.Cursor

get :: Text -> Axis
get name =
  element (X.Name name (Just "http://purl.org/net/xbiblio/csl") Nothing)

string :: Cursor -> String
string = unpack . T.concat . content

main :: IO ()
main = do
  doc <- X.readFile def "chicago-author-date.csl"
  let cur = fromDocument doc
  print $ cur $| laxAttribute "version"
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
  let locales = cur $// get "locale" &/ parseLocale
  let macros  = cur $// get "macro" &| parseMacro
  print  Style{ styleVersion = version
              , styleClass = class_
              , styleInfo = Just info
              , styleDefaultLocale = defaultLocale
              , styleLocale = locales
              , styleAbbrevs = Abbreviations M.empty
              , csOptions = []
              , csMacros = macros
              , citation = Citation{ citOptions = []
                                   , citSort = []
                                   , citLayout = Layout{
                                         layFormat = emptyFormatting
                                       , layDelim = ""
                                       , elements = [] } }
              , biblio = Nothing
              }

attrWithDefault :: Read a => Text -> a -> Cursor -> a
attrWithDefault t d cur =
  case safeRead (stringAttr t cur) of
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
              "choose" -> parseChoose cur
              "group" -> parseGroup cur
              x -> [Const ("UNDEFINED " ++ T.unpack x) (getFormatting cur)]
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
  , quotes = if attrWithDefault "quote" False cur
                then NativeQuote
                else NoQuote
  , stripPeriods = attrWithDefault "strip-periods" False cur
  , noCase = attrWithDefault "no-case" False cur
  , noDecor = attrWithDefault "no-decor" False cur
  }

parseElements :: Cursor -> [Element]
parseElements cur =
  let es = cur $/ parseElement
  in  [Elements emptyFormatting es]

parseTerm :: Cursor -> [Element]
parseTerm cur =
  let termForm       = attrWithDefault "form" NotSet cur
      formatting     = getFormatting cur
      plural         = attrWithDefault "plural" True cur
      name           = stringAttr "name" cur
  in  [Term name termForm formatting plural]

parseChoose :: Cursor -> [Element]
parseChoose cur =
  let ifPart         = cur $/ get "if" &| parseIf
      elseIfPart     = cur $/ get "else-if" &| parseIf
      elsePart       = cur $/ get "else" &/ parseElements
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

parseGroup :: Cursor -> [Element]
parseGroup cur =
  let termForm       = attrWithDefault "form" NotSet cur
      elts           = cur $/ parseElement
      delim          = stringAttr "delimiter" cur
      formatting     = getFormatting cur
  in  [Group formatting delim elts]

parseMacro :: Cursor -> MacroMap
parseMacro cur = (name, elts)
  where name = cur $| stringAttr "name"
        elts = cur $/ parseElement
