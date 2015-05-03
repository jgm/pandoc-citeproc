{-# LANGUAGE CPP, ForeignFunctionInterface, PatternGuards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.CSL.Input.Bibutils
-- Copyright   :  (C) 2008 Andrea Rossato
-- License     :  BSD3
--
-- Maintainer  :  andrea.rossato@unitn.it
-- Stability   :  unstable
-- Portability :  unportable
--
-----------------------------------------------------------------------------

module Text.CSL.Input.Bibutils
    ( readBiblioFile
    , readBiblioString
    , BibFormat (..)
    , convertRefs
    ) where

import qualified Text.Pandoc.UTF8 as UTF8
import Text.Pandoc
import Data.Char
import System.FilePath ( takeExtension )
import Text.CSL.Reference hiding ( Value )
import Text.CSL.Input.Bibtex
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import Data.Aeson

#ifdef USE_BIBUTILS
import qualified Control.Exception as E
import Control.Exception ( bracket, catch )
import Control.Monad.Trans ( liftIO )
import System.FilePath ( (</>), (<.>) )
import System.IO.Error ( isAlreadyExistsError )
import System.Directory
import Text.Bibutils
#endif

-- | Read a file with a bibliographic database. The database format
-- is recognized by the file extension.
--
-- Supported formats are: @json@, @mods@, @bibtex@, @biblatex@, @ris@,
-- @endnote@, @endnotexml@, @isi@, @medline@, and @copac@.
readBiblioFile :: FilePath -> IO [Reference]
readBiblioFile f
    = case getExt f of
        ".json"     -> BL.readFile f >>= either error return . eitherDecode
        ".yaml"     -> UTF8.readFile f >>= either error return . readYamlBib
        ".bib"      -> readBibtexInput False f
        ".bibtex"   -> readBibtexInput True f
        ".biblatex" -> readBibtexInput False f
#ifdef USE_BIBUTILS
        ".mods"     -> readBiblioFile' f mods_in
        ".ris"      -> readBiblioFile' f ris_in
        ".enl"      -> readBiblioFile' f endnote_in
        ".xml"      -> readBiblioFile' f endnotexml_in
        ".wos"      -> readBiblioFile' f isi_in
        ".medline"  -> readBiblioFile' f medline_in
        ".copac"    -> readBiblioFile' f copac_in
        _           -> error $ "citeproc: the format of the bibliographic database could not be recognized\n" ++
                              "using the file extension."
#else
        _           -> error $ "citeproc: Bibliography format not supported.\n" ++
#endif

data BibFormat
    = Json
    | Yaml
    | Bibtex
    | BibLatex
#ifdef USE_BIBUTILS
    | Ris
    | Endnote
    | EndnotXml
    | Isi
    | Medline
    | Copac
    | Mods
#endif

readBiblioString :: BibFormat -> String -> IO [Reference]
readBiblioString b s
    | Json      <- b = either error return $ eitherDecode $ UTF8.fromStringLazy s
    | Yaml      <- b = either error return $ readYamlBib s
    | Bibtex    <- b = readBibtexInputString True s
    | BibLatex  <- b = readBibtexInputString False s
#ifdef USE_BIBUTILS
    | Ris       <- b = go ris_in
    | Endnote   <- b = go endnote_in
    | EndnotXml <- b = go endnotexml_in
    | Isi       <- b = go isi_in
    | Medline   <- b = go medline_in
    | Copac     <- b = go copac_in
    | Mods      <- b = go mods_in
#endif
    | otherwise      = error "in readBiblioString"
#ifdef USE_BIBUTILS
    where
      go f = withTempDir "citeproc" $ \tdir -> do
               let tfile = tdir </> "bibutils-tmp.biblio"
               UTF8.writeFile tfile s
               readBiblioFile' tfile f
#endif

#ifdef USE_BIBUTILS
readBiblioFile' :: FilePath -> BiblioIn -> IO [Reference]
readBiblioFile' fin bin
    | bin == biblatex_in = readBibtexInput False fin
    | otherwise      = E.handle handleBibfileError
                       $ withTempDir "citeproc"
                       $ \tdir -> do
                            let tfile = tdir </> "bibutils-tmp"
                            param <- bibl_initparams bin bibtex_out "hs-bibutils"
                            bibl  <- bibl_init
                            unsetBOM        param
                            setCharsetIn    param bibl_charset_unicode
                            setCharsetOut   param bibl_charset_unicode
                            _ <- bibl_read  param bibl fin
                            _ <- bibl_write param bibl tfile
                            bibl_free bibl
                            bibl_freeparams param
                            refs <- readBibtexInput True tfile
                            return $! refs
  where handleBibfileError :: E.SomeException -> IO [Reference]
        handleBibfileError e = error $ "Error reading " ++ fin ++ "\n" ++ show e

-- | Perform a function in a temporary directory and clean up.
withTempDir :: FilePath -> (FilePath -> IO a) -> IO a
withTempDir baseName = bracket (createTempDir 0 baseName)
  (removeDirectoryRecursive)

-- | Create a temporary directory with a unique name.
createTempDir :: Integer -> FilePath -> IO FilePath
createTempDir num baseName = do
  sysTempDir <- getTemporaryDirectory
  let dirName = sysTempDir </> baseName <.> show num
  liftIO $ Control.Exception.catch (createDirectory dirName >> return dirName) $
      \e -> if isAlreadyExistsError e
            then createTempDir (num + 1) baseName
            else ioError e
#endif

getExt :: String -> String
getExt = takeExtension . map toLower

readYamlBib :: String -> Either String [Reference]
readYamlBib s =
#if MIN_VERSION_pandoc(1,14,0)
  case readMarkdown def{readerStandalone = True} s of
       Right (Pandoc meta _) -> convertRefs (lookupMeta "references" meta)
       Left e                -> Left (show e)
#else
  case readMarkdown def{readerStandalone = True} s of
       Pandoc meta _ -> convertRefs (lookupMeta "references" meta)
#endif

convertRefs :: Maybe MetaValue -> Either String [Reference]
convertRefs Nothing = Right []
convertRefs (Just v) =
  case fromJSON (metaValueToJSON v) of
       Data.Aeson.Error s   ->
         -- check for empty string and treat it as empty list:
         -- ---
         -- references:
         -- ...
         case fromJSON (metaValueToJSON v) of
               Success ""   -> Right []
               _            -> Left s
       Success x            -> Right x

metaValueToJSON :: MetaValue -> Value
metaValueToJSON (MetaMap m) = toJSON $ M.map metaValueToJSON m
metaValueToJSON (MetaList xs) = toJSON $ map metaValueToJSON xs
metaValueToJSON (MetaString t) = toJSON t
metaValueToJSON (MetaBool b) = toJSON b
metaValueToJSON (MetaInlines ils) = toJSON ils
metaValueToJSON (MetaBlocks bs) = toJSON bs
