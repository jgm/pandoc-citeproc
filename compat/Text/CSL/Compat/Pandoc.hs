{-# LANGUAGE CPP, ScopedTypeVariables, NoImplicitPrelude #-}
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

import Prelude
import qualified Control.Exception as E
import System.Exit (ExitCode)
import Data.ByteString.Lazy as BL
import Data.ByteString as B
import Text.Pandoc (Pandoc, ReaderOptions(..), def, WrapOption(..),
        WriterOptions(..))
import Text.Pandoc (Extension(..), pandocExtensions)
import qualified Text.Pandoc as Pandoc
import qualified Text.Pandoc.Process
import qualified Data.Text as T
import Text.Pandoc.MIME (MimeType)
import Text.Pandoc.Error (PandocError)
import Text.Pandoc.Class (runPure, runIO)
import qualified Text.Pandoc.Class (fetchItem)
import Control.Monad.Except (runExceptT, lift)
import Text.Pandoc.Extensions (extensionsFromList, disableExtension)

readHtml, readLaTeX, readMarkdown, readNative :: String -> Pandoc
writeMarkdown, writePlain, writeNative, writeHtmlString :: Pandoc -> String

readHtml = either mempty id . runPure . Pandoc.readHtml
   def{ readerExtensions = extensionsFromList [Ext_native_divs,
                           Ext_native_spans, Ext_raw_html, Ext_smart] } .
   T.pack

readMarkdown = either mempty id . runPure . Pandoc.readMarkdown
   def{ readerExtensions = pandocExtensions, readerStandalone = True } .
   T.pack

readLaTeX = either mempty id . runPure . Pandoc.readLaTeX
   def{ readerExtensions = extensionsFromList [Ext_raw_tex, Ext_smart] } .
   T.pack

readNative = either mempty id . runPure . Pandoc.readNative def . T.pack

writeMarkdown = either mempty T.unpack . runPure . Pandoc.writeMarkdown
   def{ writerExtensions = disableExtension Ext_smart $
                           disableExtension Ext_bracketed_spans $
                           disableExtension Ext_raw_attribute $
                           pandocExtensions,
        writerWrapText = WrapNone }

writePlain = either mempty T.unpack . runPure . Pandoc.writePlain def

writeNative = either mempty T.unpack . runPure . Pandoc.writeNative def{ writerTemplate = Just mempty }

writeHtmlString = either mempty T.unpack . runPure . Pandoc.writeHtml4String
   def{ writerExtensions = extensionsFromList
       [Ext_native_divs, Ext_native_spans, Ext_raw_html],
       writerWrapText = WrapPreserve }

pipeProcess :: Maybe [(String, String)] -> FilePath -> [String]
            -> BL.ByteString -> IO (ExitCode,BL.ByteString)
pipeProcess = Text.Pandoc.Process.pipeProcess

fetchItem :: String
          -> IO (Either E.SomeException (B.ByteString, Maybe MimeType))
fetchItem s = do
  res <- runIO $ runExceptT $ lift $ Text.Pandoc.Class.fetchItem $ T.pack s
  return $ case res of
       Left e          -> Left (E.toException e)
       Right (Left (e :: PandocError))  -> Left (E.toException e)
       Right (Right r) -> Right r
