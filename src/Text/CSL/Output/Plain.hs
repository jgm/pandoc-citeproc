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
    , (<+>)
    , (<>)
    , capitalize
    , head'
    , tail'
    ) where

import Data.Char
import Text.CSL.Style

-- | Render the 'FormattedOutput' into a plain text string.
renderPlain :: [FormattedOutput] -> String
renderPlain = concatMap $ render False

-- | Same as 'renderPlain' , but will not clean up the produced
-- output.
renderPlainStrict :: [FormattedOutput] -> String
renderPlainStrict = concatMap $ render True

render :: Bool -> FormattedOutput -> String
render _ (FPan i) = show i
render _ (FDel s) = s
render b fo
    | (FS str fm   ) <- fo = prefix fm <++> format fm (trim   str    ) <++> suffix fm
    | (FErr _ )      <- fo = "???"
    | (FN str fm   ) <- fo = prefix fm <++> format fm (trim   str    ) <++> suffix fm
    | (FUrl t fm   ) <- fo = prefix fm <++> format fm (trim $ fst  t ) <++> suffix fm
    | (FO     fm xs) <- fo = prefix fm <++> format fm (trim $ rest xs) <++> suffix fm
    | otherwise            = []
    where
      rest  xs  = concatM (render b) xs

      trim      = if b then id   else unwords . words
      (<++>)    = if b then (++) else (<>)
      concatM f = foldr (<++>) [] . map f

      quote  f s = if s /= [] && quotes f /= NoQuote then "\"" ++ s ++ "\"" else s
      format f s = quote f . text_case f $ s

      text_case fm s
          | "capitalize-first" <- textCase fm = capitalize s
          | "capitalize-all"   <- textCase fm = unwords . map capitalize . words
                                                $ s
          | "lowercase"        <- textCase fm = map toLower s
          | "uppercase"        <- textCase fm = map toUpper s
          | otherwise = s

(<+>) :: String -> String -> String
[] <+> ss = ss
s  <+> [] = s
s  <+> ss = s ++ " " ++ ss

(<>) :: String -> String -> String
sa <> sb
    | sa /= [], (s:xs) <- sb
    , last sa == s
    , s `elem` ";:,. " = sa ++ xs
    | otherwise        = sa ++ sb

capitalize :: String -> String
capitalize [] = []
capitalize (c:cs) = toUpper c : cs

head' :: [a] -> [a]
head' = take 1

tail' :: Eq a => [a] -> [a]
tail' = drop 1
