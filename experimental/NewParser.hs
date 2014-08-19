{-# LANGUAGE OverloadedStrings, TupleSections #-}
import Control.Monad.Trans.Resource
import qualified Data.Map as M
import Data.Conduit (($$), Sink)
import Data.Text (Text, unpack)
import Text.XML.Stream.Parse
import Text.CSL.Style
import Data.XML.Types (Event)
import Control.Applicative hiding (many)
import qualified Text.XML as X

munpack = maybe "" unpack

inTag name p =
  tagName (X.Name name (Just "http://purl.org/net/xbiblio/csl") Nothing)
    ignoreAttrs $ \_ -> p

parseInfo = inTag "info" $ do
  title <- inTag "title" content
  return CSInfo
      { csiTitle      = munpack title
      , csiAuthor     = CSAuthor "" "" ""
      , csiCategories = [] -- [CSCategory]
      , csiId         = ""
      , csiUpdated    = ""
      }

parseStyle = tagName "{http://purl.org/net/xbiblio/csl}style"
  ((,) <$> requireAttr "version" <*> requireAttr "class" <* ignoreAttrs) $
  \(version, class_) -> do
    info <- parseInfo
    return Style{ styleVersion = unpack version
                , styleClass = unpack class_
                , styleInfo = info
                , styleDefaultLocale = ""
                , styleLocale = []
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

main = do
     style <- runResourceT $
             parseFile def "chicago-author-date.csl" $$
               force "style required" parseStyle
     print style
