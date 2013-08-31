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
    , procList
    , (<+>)
    , (<>)
    , capitalize
    , entityToChar
    , head'
    , tail'
    ) where

import Control.Arrow ( (&&&) )
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
    | (FErr e )      <- fo = "???"
    | (FN str fm   ) <- fo = prefix fm <++> format fm (trim   str    ) <++> suffix fm
    | (FUrl t fm   ) <- fo = prefix fm <++> format fm (trim $ fst  t ) <++> suffix fm
    | (FO     fm xs) <- fo = prefix fm <++> format fm (trim $ rest xs) <++> suffix fm
    | otherwise            = []
    where
      rest  xs  = procList xs $ concatM (render b)

      trim      = if b then id   else unwords . words
      (<++>)    = if b then (++) else (<>)
      concatM f = foldr (<++>) [] . map f

      quote  f s = if s /= [] && quotes f /= NoQuote then "\"" ++ s ++ "\"" else s
      capital  s = toUpper (head s) : (tail s)
      format f s = quote f . text_case f $ s

      text_case fm s
          | "capitalize-first" <- textCase fm = procList s capital
          | "capitalize-all"   <- textCase fm = procList s $ unwords . map capital . words
          | "lowercase"        <- textCase fm = map toLower s
          | "uppercase"        <- textCase fm = map toUpper s
          | otherwise = s

procList :: Eq a => [a] -> ([a] -> [b]) -> [b]
procList  s f = if s /= [] then f s else []

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
capitalize s = if s /= [] then toUpper (head s) : tail s else []

entityToChar :: String -> String
entityToChar s
    | '&':'#':xs <- s = uncurry (:) $ parseEntity xs
    | x      :xs <- s = x : entityToChar xs
    | otherwise       = []
    where
      parseEntity  = chr . readNum . takeWhile (/= ';') &&&
                     entityToChar . tail' . dropWhile (/= ';')

readNum :: String -> Int
readNum ('x': n) = readNum $ "0x" ++ n
readNum       n  = case readsPrec 1 n of
                     [(x,[])] -> x
                     _        -> error $ "Invalid character entity:" ++ n

head' :: [a] -> [a]
head' = foldr (\x _ -> [x]) []

tail' :: Eq a => [a] -> [a]
tail' x = if x /= [] then tail x else []
