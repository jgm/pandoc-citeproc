{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE PatternGuards            #-}
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

import Prelude
import qualified Control.Exception      as E
import           Data.Aeson
import qualified Data.ByteString.Lazy   as BL
import qualified Data.ByteString        as BS
import qualified Data.HashMap.Strict    as HM
import qualified Data.Text              as T
import qualified Data.Yaml              as Yaml
import qualified Data.Vector            as V
import           Data.Char
import qualified Data.Map               as M
import           System.FilePath        (takeExtension)
import           Text.CSL.Compat.Pandoc (readMarkdown)
import           Text.CSL.Exception
import           Text.CSL.Input.Bibtex
import           Text.CSL.Reference     hiding (Value)
import           Text.CSL.Util          (parseString)
import           Text.Pandoc            hiding (readMarkdown)
import qualified Text.Pandoc.UTF8       as UTF8

#ifdef USE_BIBUTILS
import           Control.Exception      (bracket, catch)
import           Control.Monad.Trans    (liftIO)
import           System.Directory
import           System.FilePath        ((<.>), (</>))
import           System.IO.Error        (isAlreadyExistsError)
import           Text.Bibutils
#endif

-- | Read a file with a bibliographic database. The database format
-- is recognized by the file extension.  The first argument is
-- a predicate to filter citation identifiers.
--
-- Supported formats are: @json@, @mods@, @bibtex@, @biblatex@, @ris@,
-- @endnote@, @endnotexml@, @isi@, @medline@, @copac@, and @nbib@.
readBiblioFile :: (String -> Bool) -> FilePath -> IO [Reference]
readBiblioFile idpred f
    = case getExt f of
        ".json"     -> BL.readFile f >>= either
                       (E.throwIO . ErrorReadingBibFile f)
                       (return . filterEntries idpred) . eitherDecode
        ".yaml"     -> UTF8.readFile f >>= either
                       (E.throwIO . ErrorReadingBibFile f) return .
                       readYamlBib idpred
        ".bib"      -> readBibtex idpred False True f
        ".bibtex"   -> readBibtex idpred True True f
        ".biblatex" -> readBibtex idpred False True f
#ifdef USE_BIBUTILS
        ".mods"     -> readBiblioFile' idpred f mods_in
        ".ris"      -> readBiblioFile' idpred f ris_in
        ".enl"      -> readBiblioFile' idpred f endnote_in
        ".xml"      -> readBiblioFile' idpred f endnotexml_in
        ".wos"      -> readBiblioFile' idpred f isi_in
        ".medline"  -> readBiblioFile' idpred f medline_in
        ".copac"    -> readBiblioFile' idpred f copac_in
        ".nbib"     -> readBiblioFile' idpred f nbib_in
        _           -> E.throwIO $ ErrorReadingBibFile f "the format of the bibliographic database could not be recognized from the file extension"
#else
        _           -> E.throwIO $ ErrorReadingBibFile f "bibliography format not supported"
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
    | Nbib
#endif
    deriving Show

readBiblioString :: (String -> Bool) -> BibFormat -> String -> IO [Reference]
readBiblioString idpred b s
    | Json      <- b = either (E.throwIO . ErrorReadingBib)
                         return $ eitherDecode $ UTF8.fromStringLazy s
    | Yaml      <- b = either (E.throwIO . ErrorReadingBib)
                         return $ readYamlBib idpred s
    | Bibtex    <- b = readBibtexString idpred True True s
    | BibLatex  <- b = readBibtexString idpred False True s
#ifdef USE_BIBUTILS
    | Ris       <- b = go ris_in
    | Endnote   <- b = go endnote_in
    | EndnotXml <- b = go endnotexml_in
    | Isi       <- b = go isi_in
    | Medline   <- b = go medline_in
    | Copac     <- b = go copac_in
    | Mods      <- b = go mods_in
    | Nbib      <- b = go nbib_in
#endif
    | otherwise      = E.throwIO $ ErrorReadingBib $
                          "unsupported format " ++ show b
#ifdef USE_BIBUTILS
    where
      go f = withTempDir "citeproc" $ \tdir -> do
               let tfile = tdir </> "bibutils-tmp.biblio"
               UTF8.writeFile tfile s
               readBiblioFile' idpred tfile f
#endif

#ifdef USE_BIBUTILS
readBiblioFile' :: (String -> Bool) -> FilePath -> BiblioIn -> IO [Reference]
readBiblioFile' idpred fin bin
    | bin == biblatex_in = readBibtex idpred False True fin
    | otherwise      = withTempDir "citeproc"
                       $ \tdir -> do
                            let tfile = tdir </> "bibutils-tmp"
                            E.handle handleBibfileError $ do
                              param <- bibl_initparams bin bibtex_out "hs-bibutils"
                              bibl  <- bibl_init
                              unsetBOM        param
                              setCharsetIn    param bibl_charset_unicode
                              setCharsetOut   param bibl_charset_unicode
                              _ <- bibl_read  param bibl fin
                              _ <- bibl_write param bibl tfile
                              bibl_free bibl
                              bibl_freeparams param
                            refs <- readBibtex idpred True False tfile
                            return $! refs
  where handleBibfileError :: E.SomeException -> IO ()
        handleBibfileError e = E.throwIO $ ErrorReadingBibFile fin (show e)

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

readYamlBib :: (String -> Bool) -> String -> Either String [Reference]
readYamlBib idpred s =
  case readMarkdown s' of
         (Pandoc meta _) -> convertRefs (lookupMeta "references" meta)
  where s' = addTop $ addBottom
                    $ UTF8.toString
                    $ selectEntries idpred
                    $ UTF8.fromString
                    $ s
        addTop = ("---\n" ++)
        addBottom = (++ "...\n")

selectEntries :: (String -> Bool) -> BS.ByteString -> BS.ByteString
selectEntries idpred bs =
  case Yaml.decodeEither' bs of
       Right (Array vs) -> Yaml.encode (filterObjects $ V.toList vs)
       Right (Object o) ->
              case HM.lookup (T.pack "references") o of
                   Just (Array vs) ->
                     Yaml.encode (HM.insert (T.pack "references")
                                    (filterObjects $ V.toList vs) mempty)
                   _ -> BS.empty
       Right _ -> BS.empty
       Left e  -> E.throw $ ErrorParsingReferences
                              (Yaml.prettyPrintParseException e)
    where filterObjects = filter
               (\x -> case x of
                        Object o ->
                            case HM.lookup (T.pack "id") o of
                                 Just i ->
                                  case Yaml.parseMaybe parseString i of
                                       Just s -> idpred s
                                       Nothing -> False
                                 _ -> False
                        _ -> False)

filterEntries :: (String -> Bool) -> [Reference] -> [Reference]
filterEntries idpred = filter (\r -> idpred (unLiteral (refId r)))

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
               Success "" -> Right []
               _          -> Left s
       Success x            -> Right x

metaValueToJSON :: MetaValue -> Value
metaValueToJSON (MetaMap m)       = toJSON $ M.map metaValueToJSON m
metaValueToJSON (MetaList xs)     = toJSON $ map metaValueToJSON xs
metaValueToJSON (MetaString t)    = toJSON t
metaValueToJSON (MetaBool b)      = toJSON b
metaValueToJSON (MetaInlines ils) = toJSON ils
metaValueToJSON (MetaBlocks bs)   = toJSON bs

