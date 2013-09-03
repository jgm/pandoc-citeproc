{-# LANGUAGE CPP #-}
module Text.CSL.Data (getLocale, getDefaultCSL) where

import System.FilePath ()
import qualified Data.ByteString.Lazy as L
#ifdef EMBED_DATA_FILES
import Data.FileEmbed
import qualified Data.ByteString as S
import Text.CSL.Parser (defaultCSL)
import Data.ByteString.UTF8 (toString)
#else
import Paths_pandoc_citeproc (getDataFileName)
import System.Directory  (doesFileExist)
#endif

defaultCSLPath :: FilePath
defaultCSLPath = "chicago-author-date.csl"

#ifdef EMBED_DATA_FILES
localeFiles :: [(FilePath, S.ByteString)]
localeFiles = $(embedDir "locales/")

defaultCSL :: S.ByteString
defaultCSL = $(embedFile defaultCSLPath)
#endif

getLocale :: String -> IO L.ByteString
getLocale s = do
#ifdef EMBED_DATA_FILES
  f <- case length s of
         0 -> maybe (return S.empty) return
              $ lookup "locales-en-US.xml" localeFiles
         2 -> let fn = ("locales-" ++ maybe "en-US"
                                      id (lookup s langBase) ++ ".xml")
              in case lookup fn localeFiles of
                   Just x' -> return x'
                   _       -> error "could not load the locale file"
         _ -> case lookup ("locales-" ++ take 5 s ++ ".xml") localeFiles of
                    Just x' -> return x'
                    _       -> error "could not load the locale file"
  return $ L.fromChunks [f]
#else
  f <- case length s of
             0 -> return "locales/locales-en-US.xml"
             2 -> getDataFileName ("locales/locales-" ++
                                maybe "en-US" id (lookup s langBase) ++ ".xml")
             _ -> getDataFileName ("locales/locales-" ++ take 5 s ++ ".xml")
  exists <- doesFileExist f
  if not exists && length s > 2
     then getLocale $ take 2 s  -- try again with base locale
     else L.readFile f
#endif

getDefaultCSL :: IO L.ByteString
getDefaultCSL =
#ifdef EMBED_DATA_FILES
  return $ L.toChunks [defaultCSL]
#else
  getDataFileName defaultCSLPath >>= L.readFile
#endif

langBase :: [(String, String)]
langBase
    = [("af", "af-ZA")
      ,("ar", "ar-AR")
      ,("bg", "bg-BG")
      ,("ca", "ca-AD")
      ,("cs", "cs-CZ")
      ,("da", "da-DK")
      ,("de", "de-DE")
      ,("el", "el-GR")
      ,("en", "en-US")
      ,("es", "es-ES")
      ,("et", "et-EE")
      ,("fa", "fa-IR")
      ,("fi", "fi-FI")
      ,("fr", "fr-FR")
      ,("he", "he-IL")
      ,("hr", "hr-HR")
      ,("hu", "hu-HU")
      ,("is", "is-IS")
      ,("it", "it-IT")
      ,("ja", "ja-JP")
      ,("km", "km-KH")
      ,("ko", "ko-KR")
      ,("lt", "lt-LT")
      ,("lv", "lv-LV")
      ,("mn", "mn-MN")
      ,("nb", "nb-NO")
      ,("nl", "nl-NL")
      ,("nn", "nn-NO")
      ,("pl", "pl-PL")
      ,("pt", "pt-PT")
      ,("ro", "ro-RO")
      ,("ru", "ru-RU")
      ,("sk", "sk-SK")
      ,("sl", "sl-SI")
      ,("sr", "sr-RS")
      ,("sv", "sv-SE")
      ,("th", "th-TH")
      ,("tr", "tr-TR")
      ,("uk", "uk-UA")
      ,("vi", "vi-VN")
      ,("zh", "zh-CN")
      ]
