-----------------------------------------------------------------------------
-- |
-- Module      :  Text.CSL
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Andrea Rossato <andrea.rossato@unitn.it>
-- Stability   :  unstable
-- Portability :  unportable
--
-- /citeproc-hs/ is a library for automatically formatting
-- bibliographic reference citations into a variety of styles using a
-- macro language called Citation Style Language (CSL). More details
-- on CSL can be found here: <http://citationstyles.org/>.
--
-- This module documents and exports the library API.
--
-----------------------------------------------------------------------------

module Text.CSL
    ( -- * Introduction
      -- $intro

      -- * Overview: A Simple Example
      -- $overview

      -- * Reading Bibliographic Databases
      readBiblioFile
    , BibFormat (..)
    , readBiblioString

    -- ** Reference Representation
    , Reference (..)
    , getReference
    , setNearNote

    -- * CSL Parser, Representation, and Processing
    , readCSLFile
    , parseCSL
    , parseCSL'
    , localizeCSL

    -- ** The Style Types
    , Style (..)
    , Citation (..)
    , Bibliography (..)
    , Cite (..)
    , Abbreviations (..)
    , emptyCite

    -- ** High Level Processing
    , ProcOpts (..)
    , procOpts
    , BibOpts (..)
    , citeproc
    , processCitations
    , processBibliography
    , BiblioData (..)

    -- * The output and the rendering functions
    , renderPlain
    , renderPandoc
    , renderPandoc'
    ) where

import Text.CSL.Proc
import Text.CSL.Reference
import Text.CSL.Style
import Text.CSL.Parser
import Text.CSL.Input.Bibutils
import Text.CSL.Output.Pandoc
import Text.CSL.Output.Plain


-- $intro
--
-- /citeproc-hs/ provides functions for reading bibliographic
-- databases, for reading and parsing CSL files and for generating
-- citations in an internal format, 'Formatted', that can be
-- easily rendered into different final formats. At the present time
-- only 'Pandoc' and plain text rendering functions are provided by
-- the library.
--
-- The library also provides a wrapper around hs-bibutils, the Haskell
-- bindings to Chris Putnam's bibutils, a library that interconverts
-- between various bibliography formats using a common MODS-format XML
-- intermediate. For more information about hs-bibutils see here:
-- <http://hackage.haskell.org/package/hs-bibutils>.
--
-- /citeproc-hs/ can natively read MODS and JSON formatted
-- bibliographic databases. The JSON format is only partially
-- documented. It is used by citeproc-js, by the CSL processor
-- test-suite and is derived by the CSL scheme. More information can
-- be read here:
-- <http://citationstyles.org/>.
--
-- A (git) repository of styles can be found here:
-- <https://github.com/citation-style-language/styles>.

-- $overview
--
-- The following example assumes you have installed citeproc-hs with
-- hs-bibutils support (which is the default).
--
-- Suppose you have a small bibliographic database, like this one:
--
-- > @Book{Rossato2006,
-- > author="Andrea Rossato",
-- > title="My Second Book",
-- > year="2006"
-- > }
-- >
-- > @Book{Caso2007,
-- > author="Roberto Caso",
-- > title="Roberto's Book",
-- > year="2007"
-- > }
--
-- Save it as @mybibdb.bib@.
--
-- Then you can grab one of the CSL styles that come with the
-- test-suite for CSL processors. Suppose this one:
--
-- <https://bitbucket.org/bdarcus/citeproc-test/raw/18141149d1d3/styles/apa-x.csl>
--
-- saved locally as @apa-x.csl@.
--
-- This would be a simple program that formats a list of citations
-- according to that style:
--
-- > import Text.CSL
-- >
-- > cites :: [Cite]
-- > cites = [emptyCite { citeId = "Caso2007"
-- >                    , citeLabel = "page"
-- >                    , citeLocator = "15"}
-- >         ,emptyCite { citeId = "Rossato2006"
-- >                    , citeLabel = "page"
-- >                    , citeLocator = "10"}
-- >         ]
-- >
-- > main :: IO ()
-- > main = do
-- >   m <- readBiblioFile "mybibdb.bib"
-- >   s <- readCSLFile Nothing "apa-x.csl"
-- >   let result = citeproc procOpts s m $ [cites]
-- >   putStrLn . unlines . map renderPlain . citations $ result
--
-- The result would be:
--
-- > (Caso, 2007, p. 15; Rossato, 2006, p. 10)
