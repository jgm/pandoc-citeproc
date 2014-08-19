{-# LANGUAGE OverloadedStrings, TupleSections #-}
import Control.Monad.Trans.Resource
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Conduit (($$), Sink)
import Data.Text (Text, unpack)
-- import Text.XML.Stream.Parse
import Text.CSL.Style
import Data.XML.Types (Event)
import Control.Applicative hiding (many)
import qualified Text.XML as X
import Data.Default
import Text.XML.Cursor

get name =
  element (X.Name name (Just "http://purl.org/net/xbiblio/csl") Nothing)

string = unpack . T.concat . content

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
  print  Style{ styleVersion = version
              , styleClass = class_
              , styleInfo = Just info
              , styleDefaultLocale = defaultLocale
              , styleLocale = locales
              , styleAbbrevs = Abbreviations M.empty
              , csOptions = []
              , csMacros = []
              , citation = Citation{ citOptions = []
                                   , citSort = []
                                   , citLayout = Layout{
                                         layFormat = emptyFormatting
                                       , layDelim = ""
                                       , elements = [] } }
              , biblio = Nothing
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
        terms   = [] -- TODO
        date    = [] -- TODO
