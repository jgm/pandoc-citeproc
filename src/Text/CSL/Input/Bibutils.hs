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
    ) where

import Text.Pandoc.UTF8 ( fromStringLazy, fromString )
import Data.Char
import System.FilePath ( takeExtension )
import Text.CSL.Reference
import Text.CSL.Input.Bibtex
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Aeson
import qualified Data.Map as M
import qualified Data.Yaml as Yaml

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
        ".yaml"     -> BL.readFile f >>=
         (either error return . Yaml.decodeEither .  B.concat . BL.toChunks) >>=
         (maybe (return []) return . M.lookup "references")
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
#endif

readBiblioString :: BibFormat -> String -> IO [Reference]
readBiblioString b s
    | Json      <- b = either error return $ eitherDecode $ fromStringLazy s
    | Yaml      <- b = (maybe [] id . M.lookup "references") `fmap`
                       (either error return $ Yaml.decodeEither $ fromString s)
    | Bibtex    <- b = readBibtexInputString True s
    | BibLatex  <- b = readBibtexInputString False s
#ifdef USE_BIBUTILS
    | Ris       <- b = go ris_in
    | Endnote   <- b = go endnote_in
    | EndnotXml <- b = go endnotexml_in
    | Isi       <- b = go isi_in
    | Medline   <- b = go medline_in
    | Copac     <- b = go copac_in
#endif
    | otherwise      = error "in readBiblioString"
#ifdef USE_BIBUTILS
    where
      go f = withTempDir "citeproc" $ \tdir -> do
               let tfile = tdir </> "bibutils-tmp.biblio"
               writeFile tfile s
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
