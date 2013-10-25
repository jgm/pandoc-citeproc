{-# LANGUAGE PatternGuards #-}
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
    , renderPlainStrict
    ) where

import Data.Char
import Text.CSL.Style
import Text.CSL.Util (capitalize, (<^>))

-- | Render the 'FormattedOutput' into a plain text string.
renderPlain :: [FormattedOutput] -> String
renderPlain = concatMap $ render False

-- | Same as 'renderPlain', but will not clean up the produced output.
renderPlainStrict :: [FormattedOutput] -> String
renderPlainStrict = concatMap $ render True

render :: Bool -> FormattedOutput -> String
render _ (FPan i) = show i
render _ (FDel s) = s
render noClean fo =
  case fo of
      (FS str fm   ) -> prefix fm <++> format fm (trim   str    ) <++> suffix fm
      (FErr _ )      -> "???"
      (FN str fm   ) -> prefix fm <++> format fm (trim   str    ) <++> suffix fm
      (FUrl t fm   ) -> prefix fm <++> format fm (trim $ fst  t ) <++> suffix fm
      (FO     fm xs) -> prefix fm <++> format fm (trim $ rest xs) <++> suffix fm
      _              -> []
    where
      rest  xs  = concatM (render noClean) xs

      trim      = if noClean then id   else unwords . words
      (<++>)    = if noClean then (++) else (<^>)
      concatM f = foldr (<++>) [] . map f

      quote  f s = if null s || quotes f == NoQuote
                      then s
                      else "\"" ++ s ++ "\""
      format f s = quote f . text_case f $ s

      text_case fm s =
        case textCase fm of
            "capitalize-first" -> capitalize s
            "capitalize-all"   -> unwords . map capitalize . words $ s
            "lowercase"        -> map toLower s
            "uppercase"        -> map toUpper s
            _                  -> s


