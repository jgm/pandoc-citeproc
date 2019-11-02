{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.CSL.Data
-- Copyright   :  (c) John MacFarlane
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  John MacFarlane <fiddlosopher@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-----------------------------------------------------------------------------

module Text.CSL.Data
    ( getLocale
    , CSLLocaleException(..)
    , getDefaultCSL
    , getManPage
    , getLicense
    , langBase
    ) where

import Prelude
import qualified Control.Exception      as E
import qualified Data.ByteString.Lazy   as L
import           Data.Typeable
import           System.FilePath        ()
import           Data.Maybe             (fromMaybe)
#ifdef EMBED_DATA_FILES
import           Text.CSL.Data.Embedded (defaultCSL, license, localeFiles,
                                         manpage)
#else
import           Paths_pandoc_citeproc  (getDataFileName)
import           System.Directory       (doesFileExist)
#endif

data CSLLocaleException =
    CSLLocaleNotFound String
  | CSLLocaleReadError E.IOException
  deriving Typeable
instance Show CSLLocaleException where
  show (CSLLocaleNotFound s)  = "Could not find locale data for " ++ s
  show (CSLLocaleReadError e) = show e
instance E.Exception CSLLocaleException

-- | Raises 'CSLLocaleException' on error.
getLocale :: String -> IO L.ByteString
getLocale s = do
  let baseLocale = takeWhile (/='.') s
#ifdef EMBED_DATA_FILES
  let toLazy x = L.fromChunks [x]
  let returnDefaultLocale =
        maybe (E.throwIO $ CSLLocaleNotFound "en-US") (return . toLazy)
           $ lookup "locales-en-US.xml" localeFiles
  case length baseLocale of
      0 -> returnDefaultLocale
      1 | baseLocale == "C" -> returnDefaultLocale
      _ -> case lookup ("locales-" ++ baseLocale ++ ".xml") localeFiles of
                 Just x' -> return $ toLazy x'
                 Nothing ->
                       -- try again with 2-letter locale (lang only)
                       let shortLocale = takeWhile (/='-') baseLocale in
                       case lookup ("locales-" ++ fromMaybe shortLocale
                              (lookup shortLocale langBase) ++ ".xml")
                              localeFiles of
                             Just x'' -> return $ toLazy x''
                             _        -> E.throwIO $ CSLLocaleNotFound s
#else
  f <- getDataFileName $
         case length baseLocale of
             0 -> "locales/locales-en-US.xml"
             1 | baseLocale == "C" -> "locales/locales-en-US.xml"
             2 -> "locales/locales-" ++
                    fromMaybe s (lookup s langBase) ++ ".xml"
             _ -> "locales/locales-" ++ take 5 s ++ ".xml"
  exists <- doesFileExist f
  if not exists && length baseLocale > 2
     then getLocale $ dropWhile (/='-') baseLocale
          -- try again with lang only
     else E.handle (E.throwIO . CSLLocaleReadError) $ L.readFile f
#endif

getDefaultCSL :: IO L.ByteString
getDefaultCSL =
#ifdef EMBED_DATA_FILES
  return $ L.fromChunks [defaultCSL]
#else
  getDataFileName "chicago-author-date.csl" >>= L.readFile
#endif

getManPage :: IO L.ByteString
getManPage =
#ifdef EMBED_DATA_FILES
  return $ L.fromChunks [manpage]
#else
  getDataFileName "man/man1/pandoc-citeproc.1" >>= L.readFile
#endif

getLicense :: IO L.ByteString
getLicense =
#ifdef EMBED_DATA_FILES
  return $ L.fromChunks [license]
#else
  getDataFileName "LICENSE" >>= L.readFile
#endif

langBase :: [(String, String)]
langBase
    = [("af", "af-ZA")
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

