{-# LANGUAGE CPP, ScopedTypeVariables #-}
-- | Compatibility module to work around differences in the
-- types of functions between pandoc < 2.0 and pandoc >= 2.0.
module Text.CSL.Compat.Pandoc (
  writeMarkdown,
  writePlain,
  writeNative,
  writeHtmlString,
  readNative,
  readHtml,
  readMarkdown,
  readLaTeX,
  fetchItem,
  pipeProcess ) where

import qualified Control.Exception as E
import System.Exit (ExitCode)
import Data.ByteString.Lazy as BL
import Data.ByteString as B
import Text.Pandoc (Pandoc, ReaderOptions(..), def, WrapOption(..),
        WriterOptions(..))
import qualified Text.Pandoc as Pandoc
import qualified Text.Pandoc.Process
#if MIN_VERSION_pandoc(2,0,0)
import Text.Pandoc.MIME (MimeType)
import Text.Pandoc.Error (PandocError)
import Text.Pandoc.Class (runPure, runIO)
import qualified Text.Pandoc.Class (fetchItem)
import Control.Monad.Except (runExceptT, lift)
import Text.Pandoc.Extensions (extensionsFromList, Extension(..),
          pandocExtensions, disableExtension)
#else
import System.IO (stderr)
import qualified Text.Pandoc.Shared (fetchItem)

type MimeType = String
#endif

readHtml, readLaTeX, readMarkdown, readNative :: String -> Pandoc
writeMarkdown, writePlain, writeNative, writeHtmlString :: Pandoc -> String

#if MIN_VERSION_pandoc(2,0,0)
readHtml = either mempty id . runPure . Pandoc.readHtml
   def{ readerExtensions = extensionsFromList [Ext_native_divs,
                           Ext_native_spans, Ext_raw_html, Ext_smart] }

readMarkdown = either mempty id . runPure . Pandoc.readMarkdown
   def{ readerExtensions = pandocExtensions, readerStandalone = True }

readLaTeX = either mempty id . runPure . Pandoc.readLaTeX
   def{ readerExtensions = extensionsFromList [Ext_raw_tex, Ext_smart] }

readNative = either mempty id . runPure . Pandoc.readNative def

writeMarkdown = either mempty id . runPure . Pandoc.writeMarkdown
   def{ writerExtensions = disableExtension Ext_smart pandocExtensions,
        writerWrapText = WrapNone }

writePlain = either mempty id . runPure . Pandoc.writePlain def

writeNative = either mempty id . runPure . Pandoc.writeNative def

writeHtmlString = either mempty id . runPure . Pandoc.writeHtml4String
   def{ writerExtensions = extensionsFromList
       [Ext_native_divs, Ext_native_spans, Ext_raw_html] }

#else
readHtml = either mempty id . Pandoc.readHtml
   def{ readerSmart = True, readerParseRaw = True }

readMarkdown = either mempty id . Pandoc.readMarkdown
   def{ readerSmart = True, readerStandalone = True}

readLaTeX = either mempty id . Pandoc.readLaTeX
   def{ readerSmart = True, readerParseRaw = True }

readNative = either mempty id . Pandoc.readNative

writeMarkdown = Pandoc.writeMarkdown def{ writerWrapText = WrapNone }

writePlain = Pandoc.writePlain def

writeNative = Pandoc.writeNative def

writeHtmlString = Pandoc.writeHtmlString def
#endif

pipeProcess :: Maybe [(String, String)] -> FilePath -> [String]
            -> BL.ByteString -> IO (ExitCode,BL.ByteString)
#if MIN_VERSION_pandoc(2,0,0)
pipeProcess = Text.Pandoc.Process.pipeProcess
#else
pipeProcess e f a b = do
  (ec, out, err) <- Text.Pandoc.Process.pipeProcess e f a b
  BL.hPutStr stderr err
  return (ec, out)
#endif

fetchItem :: Maybe String
          -> String
          -> IO (Either E.SomeException (B.ByteString, Maybe MimeType))
#if MIN_VERSION_pandoc(2,0,0)
fetchItem mbd s = do
  res <- runIO $ runExceptT $ lift $ Text.Pandoc.Class.fetchItem mbd s
  return $ case res of
       Left e          -> Left (E.toException e)
       Right (Left (e :: PandocError))  -> Left (E.toException e)
       Right (Right r) -> Right r
#else
fetchItem = Text.Pandoc.Shared.fetchItem
#endif
