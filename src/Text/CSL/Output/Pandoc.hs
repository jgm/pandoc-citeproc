{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

{-# LANGUAGE PatternGuards      #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.CSL.Output.Pandoc
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Andrea Rossato <andrea.rossato@unitn.it>
-- Stability   :  unstable
-- Portability :  unportable
--
-- The pandoc output formatter for CSL
--
-----------------------------------------------------------------------------

module Text.CSL.Output.Pandoc
    ( renderPandoc
    , renderPandoc'
    , headInline
    , initInline
    , lastInline
    , tailInline
    , tailFirstInlineStr
    , toCapital
    ) where

import Prelude
import           Data.List              (dropWhileEnd)
import           Data.Maybe             (fromMaybe)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Text.CSL.Style
import           Text.CSL.Util          (headInline, initInline, lastInline,
                                         proc, proc', tailFirstInlineStr,
                                         tailInline, toCapital)
import           Text.Pandoc.Definition
import           Text.Pandoc.XML        (fromEntities)

renderPandoc :: Style -> Formatted -> [Inline]
renderPandoc sty
    = proc (convertQuoted sty) . proc' (clean' sty) . flipFlop .
      fixBreaks . unFormatted

-- remove leading/trailing LineBreak
fixBreaks :: [Inline] -> [Inline]
fixBreaks = dropWhile (== LineBreak) . dropWhileEnd (== LineBreak)

renderPandoc' :: Style -> (Formatted, Text) -> Block
renderPandoc' sty (form, citId) = Div ("ref-" <> citId, [], []) [Para $ renderPandoc sty form]

clean' :: Style -> [Inline] -> [Inline]
clean' _   []  = []
clean' sty (i:is) =
  case i:is of
      (Str "" : rest) -> clean' sty rest
      (Str xs : Str ys : rest) -> clean' sty $ Str (xs <> ys) : rest
      (Link a1 lab1 (T.uncons -> Just ('#',r1), "")
       : Str "\8211" : Link a2 lab2 (T.uncons -> Just ('#',r2), "") : rest)
        | r1 == r2, a1 == a2 ->
           Link a1 (lab1 ++ [Str "\8211"] ++ lab2) (T.cons '#' r1, "") : clean' sty rest
      (Span ("",[],[]) inls : _) -> inls ++ clean' sty is
      (Span ("",["csl-inquote"],kvs) inls : _) ->
         let isOuter = lookup "position" kvs == Just "outer"
         in  case headInline is of
               Just x -> if x `elem` (".," :: String) && isPunctuationInQuote sty
                         then if lastInline inls `elem` map Just ".,;:!?"
                              then quoted isOuter inls ++
                                   clean' sty (tailInline is)
                              else quoted isOuter (inls ++ [Str (T.singleton x)]) ++
                                     clean' sty (tailInline is)
                         else quoted isOuter inls ++ clean' sty is
               _      ->      quoted isOuter inls ++ clean' sty is
      (Quoted t inls : _) -> quoted (t == DoubleQuote) inls ++ clean' sty is
      _      -> if lastInline [i] == headInline is && isPunct
                   then i : clean' sty (tailInline is)
                   else i : clean' sty is
    where
      isPunct = all (`elem` (".,;:!? " :: String)) $ headInline is
      locale  = case styleLocale sty of
                     (x:_) -> x
                     []    -> Locale "" "" [] [] [] -- should not happen
      getQuote s d     = case [term | term <- localeTerms locale, cslTerm term == s] of
                               (x:_) -> Str $ termSingular x
                               _     -> Str d
      openQuoteOuter   = getQuote "open-quote" "“"
      openQuoteInner   = getQuote "open-inner-quote" "‘"
      closeQuoteOuter  = getQuote "close-quote" "”"
      closeQuoteInner  = getQuote "close-inner-quote" "’"
      quoted True ils  = openQuoteOuter : ils ++ [closeQuoteOuter]
      quoted False ils = openQuoteInner : ils ++ [closeQuoteInner]

convertQuoted :: Style -> [Inline] -> [Inline]
convertQuoted s = convertQuoted'
    where
      locale = let l = styleLocale s in case l of [x] -> x; _   -> Locale "" "" [] [] []
      getQuote  x y = fromEntities . termSingular . fromMaybe newTerm {termSingular = x} .
                      findTerm y Long . localeTerms $ locale
      doubleQuotesO = getQuote "\"" "open-quote"
      doubleQuotesC = getQuote "\"" "close-quote"
      singleQuotesO = getQuote "'"  "open-inner-quote"
      singleQuotesC = getQuote "'"  "close-inner-quote"
      convertQuoted' o
          | (Quoted DoubleQuote t:xs) <- o = Str doubleQuotesO : t ++ Str doubleQuotesC : convertQuoted' xs
          | (Quoted SingleQuote t:xs) <- o = Str singleQuotesO : t ++ Str singleQuotesC : convertQuoted' xs
          | (x                   :xs) <- o = x : convertQuoted' xs
          | otherwise                      = []

-- flip-flop

data FlipFlopState = FlipFlopState
     { inEmph        :: Bool
     , inStrong      :: Bool
     , inSmallCaps   :: Bool
     , inOuterQuotes :: Bool
     }

flipFlop :: [Inline] -> [Inline]
flipFlop = map (flipFlop' $ FlipFlopState False False False False)

flipFlop' :: FlipFlopState -> Inline -> Inline
flipFlop' st (Emph ils) =
  (if inEmph st then Span ("",["csl-no-emph"],[]) else Emph)
  $ map (flipFlop' st{ inEmph = not $ inEmph st }) ils
flipFlop' st (Strong ils) =
  (if inStrong st then Span ("",["csl-no-strong"],[]) else Strong)
  $ map (flipFlop' st{ inStrong = not $ inStrong st }) ils
flipFlop' st (SmallCaps ils) =
  (if inSmallCaps st then Span ("",["csl-no-smallcaps"],[]) else SmallCaps)
  $ map (flipFlop' st{ inSmallCaps = not $ inSmallCaps st }) ils
flipFlop' st (Strikeout ils) =
  Strikeout $ map (flipFlop' st) ils
flipFlop' st (Superscript ils) =
  Superscript $ map (flipFlop' st) ils
flipFlop' st (Subscript ils) =
  Subscript $ map (flipFlop' st) ils
flipFlop' st (Quoted _ ils) =
  Quoted (if inOuterQuotes st then SingleQuote else DoubleQuote)
  $ map (flipFlop' st{ inOuterQuotes = not $ inOuterQuotes st }) ils
flipFlop' st (Span (_, ["csl-inquote"], _) ils) =
  Span ("", ["csl-inquote"], [("position", if inOuterQuotes st then "inner" else "outer")])
  $ map (flipFlop' st{ inOuterQuotes = not $ inOuterQuotes st }) ils
flipFlop' st (Span (id',classes,kvs) ils)
  | "nodecor" `elem` classes = Span (id',classes',kvs) $ map (flipFlop' st) ils
  | otherwise = Span (id',classes,kvs) $ map (flipFlop' st) ils
     where classes' = filter (/= "nodecor") classes ++
                       ["csl-no-emph"      | inEmph st] ++
                       ["csl-no-strong"    | inStrong st] ++
                       ["csl-no-smallcaps" | inSmallCaps st]
flipFlop' st (Link attr ils t) =
  Link attr (map (flipFlop' st) ils) t
flipFlop' st (Note [Para ils]) =
  Note [Para $ map (flipFlop' st) ils]
flipFlop' _ il = il
