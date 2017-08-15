{-# LANGUAGE TemplateHaskell #-}

module Text.CSL.Data.Embedded (localeFiles, defaultCSL, manpage, license)
where
import Data.FileEmbed
import qualified Data.ByteString.Char8 as S

localeFiles :: [(FilePath, S.ByteString)]
localeFiles = $(embedDir "locales")

defaultCSL :: S.ByteString
defaultCSL = $(embedFile "chicago-author-date.csl")

manpage :: S.ByteString
manpage = $(embedFile "man/man1/pandoc-citeproc.1")

license :: S.ByteString
license = $(embedFile "LICENSE")
