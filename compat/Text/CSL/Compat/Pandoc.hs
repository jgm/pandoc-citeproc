{-# LANGUAGE CPP #-}
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
  pipeProcess ) where

import System.Exit (ExitCode)
import Data.ByteString.Lazy as BL
import Text.Pandoc (Pandoc, ReaderOptions, WriterOptions)
import qualified Text.Pandoc as Pandoc
import qualified Text.Pandoc.Process
#if MIN_VERSION_pandoc(2,0,0)
import Text.Pandoc.Class (runPure)
#else
import System.IO (stderr)
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
