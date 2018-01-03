
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.CSL.Output.Plain
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Andrea Rossato <andrea.rossato@unitn.it>
-- Stability   :  unstable
-- Portability :  unportable
--
-- The plain ascii output formatter for CSL
--
-----------------------------------------------------------------------------

module Text.CSL.Output.Plain
    ( renderPlain
    ) where

import           Text.CSL.Compat.Pandoc (writePlain)
import           Text.CSL.Style
import           Text.Pandoc            (Block (Plain), Pandoc (..), nullMeta)

-- | Render the 'Formatted' into a plain text string.
renderPlain :: Formatted -> String
renderPlain (Formatted ils) = writePlain $ Pandoc nullMeta [Plain ils]

