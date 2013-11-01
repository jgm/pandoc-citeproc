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
import Text.CSL.Pickle
import Text.CSL.Reference
import Text.CSL.Input.Bibtex
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Aeson
import qualified Data.Map as M
import qualified Data.Yaml as Yaml

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
        _           -> error $ "citeproc: Bibliography format not supported.\n" ++

data BibFormat
    = Json
    | Yaml
    | Bibtex
    | BibLatex

readBiblioString :: BibFormat -> String -> IO [Reference]
readBiblioString b s
    | Json      <- b = either error return $ eitherDecode $ fromStringLazy s
    | Yaml      <- b = (maybe [] id . M.lookup "references") `fmap`
                       (either error return $ Yaml.decodeEither $ fromString s)
    | Bibtex    <- b = readBibtexInputString True s
    | BibLatex  <- b = readBibtexInputString False s
    where
      go f = withTempDir "citeproc" $ \tdir -> do
               let tfile = tdir </> "bibutils-tmp.biblio"
               writeFile tfile s
               readBiblioFile' tfile f

getExt :: String -> String
getExt = takeExtension . map toLower
