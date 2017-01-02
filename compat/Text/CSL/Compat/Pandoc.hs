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
import Text.Pandoc (Pandoc, ReaderOptions, WriterOptions)
import qualified Text.Pandoc as Pandoc
import qualified Text.Pandoc.Process
#if MIN_VERSION_pandoc(2,0,0)
import Text.Pandoc.MIME (MimeType)
import Text.Pandoc.Error (PandocError)
import Text.Pandoc.Class (runPure, runIO)
import qualified Text.Pandoc.Class (fetchItem)
import Control.Monad.Except (runExceptT, lift)
#else
import System.IO (stderr)
import Text.Pandoc.Shared (fetchItem)

type MimeType = String
#endif

#if MIN_VERSION_pandoc(2,0,0)
#define WRAPREADER(f) f o = either mempty id . runPure . Pandoc.f o
#define WRAPWRITER(f) f o = either mempty id . runPure . Pandoc.f o
#else
#define WRAPREADER(f) f o = either mempty id . Pandoc.f o
#define WRAPWRITER(f) f = Pandoc.f
#endif

readHtml, readLaTeX, readMarkdown, readNative ::
  ReaderOptions -> String -> Pandoc

WRAPREADER(readHtml)
WRAPREADER(readLaTeX)
WRAPREADER(readMarkdown)

#if MIN_VERSION_pandoc(2,0,0)
WRAPREADER(readNative)
#else
readNative _ = either mempty id . Pandoc.readNative
#endif

writeMarkdown, writePlain, writeNative, writeHtmlString ::
  WriterOptions -> Pandoc -> String

WRAPWRITER(writeMarkdown)
WRAPWRITER(writePlain)
WRAPWRITER(writeNative)
WRAPWRITER(writeHtmlString)

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
fetchItem = Text.Pandoc.Class.fetchItem
#endif
