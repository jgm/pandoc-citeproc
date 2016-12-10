{-# LANGUAGE CPP #-}
module Text.CSL.Compat.Pandoc (
  readHtml,
  readMarkdown,
  writeMarkdown,
  readLaTeX ) where

import Text.Pandoc (Pandoc, ReaderOptions, WriterOptions)
import qualified Text.Pandoc (readHtml, writeMarkdown, readLaTeX, readMarkdown)
#if MIN_VERSION_pandoc(2,0,0)
import Text.Pandoc.Class (runPure)
#endif
#if MIN_VERSION_pandoc(1,14,0)
import Text.Pandoc.Error (PandocError)
#endif
import Data.Monoid

readHtml :: ReaderOptions -> String -> Pandoc
readLaTeX :: ReaderOptions -> String -> Pandoc
readMarkdown :: ReaderOptions -> String -> Pandoc
writeMarkdown :: WriterOptions -> Pandoc -> String

#if MIN_VERSION_pandoc(2,0,0)
readHtml o = either mempty id . runPure . Text.Pandoc.readHtml o
readLaTeX o = either mempty id . runPure . Text.Pandoc.readLaTeX o
readMarkdown o = either mempty id . runPure . Text.Pandoc.readMarkdown o
writeMarkdown o = runPure . Text.Pandoc.readHtml o
#elif MIN_VERSION_pandoc(1,14,0)
readHtml o = either mempty id . Text.Pandoc.readHtml o
readLaTeX o = either mempty id . Text.Pandoc.readLaTeX o
readMarkdown o = either mempty id . Text.Pandoc.readMarkdown o
writeMarkdown = Text.Pandoc.writeMarkdown
#else
readHtml o = Text.Pandoc.readHtml o
readLaTeX o = Text.Pandoc.readLaTeX o
readMarkdown o = Text.Pandoc.readMarkdown o
writeMarkdown = Text.Pandoc.writeMarkdown
#endif

