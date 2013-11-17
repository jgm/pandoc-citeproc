{-# LANGUAGE PatternGuards, DeriveDataTypeable #-}
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

import Text.CSL.Util ( capitalize, proc, proc', query )
import Data.Maybe ( fromMaybe )
import Text.CSL.Style
import Text.Pandoc.Definition
import Text.Pandoc.XML (fromEntities)
import Text.Pandoc.Shared (stringify)

renderPandoc :: Style -> FormattedOutput -> [Inline]
renderPandoc sty
    = proc (convertQuoted sty) . proc' (clean' $ isPunctuationInQuote sty) . flipFlop

renderPandoc' :: Style -> FormattedOutput -> Block
renderPandoc' sty = Para . renderPandoc sty

clean' :: Bool -> [Inline] -> [Inline]
clean' _   []  = []
clean' punctuationInQuote (i:is) =
  case (i:is) of
      (Span ("",[],kvs) inls : _)
         | lookup "csl-inquote" kvs == Just "true" ->
             case headInline is of
                    [x] -> if x `elem` ".," && punctuationInQuote
                           then if lastInline inls `elem` [".",",",";",":","!","?"]
                                then quote DoubleQuote inls                :
                                     clean' punctuationInQuote (tailInline is)
                                else quote DoubleQuote (inls ++ [Str [x]]) :
                                     clean' punctuationInQuote (tailInline is)
                           else quote DoubleQuote inls : clean' punctuationInQuote is
                    _   ->      quote DoubleQuote inls : clean' punctuationInQuote is
      (Quoted t inls : _) -> quote t inls : clean' punctuationInQuote is
      _      -> if lastInline [i] == headInline is && isPunct
                   then i : clean' punctuationInQuote (tailInline is)
                   else i : clean' punctuationInQuote is
    where
      quote t x = Quoted t (reverseQuoted t x)
      isPunct = and . map (flip elem ".,;:!? ") $ headInline is
      reverseQuoted t = proc reverseQuoted'
          where
            reverseQuoted' q
                | Quoted _ qs <- q
                , DoubleQuote <- t = Quoted SingleQuote (reverseQuoted SingleQuote qs)
                | Quoted _ qs <- q
                , SingleQuote <- t = Quoted DoubleQuote (reverseQuoted DoubleQuote qs)
                | otherwise        = q

isPunctuationInQuote :: Style -> Bool
isPunctuationInQuote = or . query punctIn'
    where
      punctIn' n
          | ("punctuation-in-quote","true") <- n = [True]
          | otherwise                            = [False]

convertQuoted :: Style -> [Inline] -> [Inline]
convertQuoted s = convertQuoted'
    where
      locale = let l = styleLocale s in case l of [x] -> x; _   -> Locale [] [] [] [] []
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

headInline :: [Inline] -> String
headInline = take 1 . stringify

lastInline :: [Inline] -> String
lastInline xs = case stringify xs of
                      [] -> []
                      ys -> [last ys]

initInline :: [Inline] -> [Inline]
initInline [] = []
initInline (i:[])
    | Str          s <- i = return $ Str         (init'       s)
    | Emph        is <- i = return $ Emph        (initInline is)
    | Strong      is <- i = return $ Strong      (initInline is)
    | Superscript is <- i = return $ Superscript (initInline is)
    | Subscript   is <- i = return $ Subscript   (initInline is)
    | Quoted q    is <- i = return $ Quoted q    (initInline is)
    | SmallCaps   is <- i = return $ SmallCaps   (initInline is)
    | Strikeout   is <- i = return $ Strikeout   (initInline is)
    | Link      is t <- i = return $ Link        (initInline is) t
    | Span at     is <- i = return $ Span at     (initInline is)
    | otherwise           = []
    where
      init' s = if s /= [] then init s else []
initInline (i:xs) = i : initInline xs

tailInline :: [Inline] -> [Inline]
tailInline (Space:xs) = xs
tailInline xs         = removeEmpty $ tailFirstInlineStr xs
  where removeEmpty   = dropWhile (== Str "")

tailFirstInlineStr :: [Inline] -> [Inline]
tailFirstInlineStr = mapHeadInline (drop 1)

toCapital :: [Inline] -> [Inline]
toCapital = mapHeadInline capitalize

mapHeadInline :: (String -> String) -> [Inline] -> [Inline]
mapHeadInline _ [] = []
mapHeadInline f (i:xs)
    | Str         [] <- i =                      mapHeadInline f xs
    | Str          s <- i = Str         (f                s)   : xs
    | Emph        is <- i = Emph        (mapHeadInline f is)   : xs
    | Strong      is <- i = Strong      (mapHeadInline f is)   : xs
    | Superscript is <- i = Superscript (mapHeadInline f is)   : xs
    | Subscript   is <- i = Subscript   (mapHeadInline f is)   : xs
    | Quoted q    is <- i = Quoted q    (mapHeadInline f is)   : xs
    | SmallCaps   is <- i = SmallCaps   (mapHeadInline f is)   : xs
    | Strikeout   is <- i = Strikeout   (mapHeadInline f is)   : xs
    | Link      is t <- i = Link        (mapHeadInline f is) t : xs
    | Span     at is <- i = Span at     (mapHeadInline f is)   : xs
    | otherwise           = i : xs

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
-- TODO? Make Quoted flipflop in localized way.
flipFlop' st (Quoted _ ils) =
  Quoted (if inOuterQuotes st then SingleQuote else DoubleQuote)
  $ map (flipFlop' st{ inOuterQuotes = not $ inOuterQuotes st }) ils
flipFlop' st (Link ils t) =
  Link (map (flipFlop' st) ils) t
flipFlop' st (Span (_, ["csl-inquote"], _) ils) =
  flipFlop' st (Quoted DoubleQuote ils)
flipFlop' st (Span (id',classes,kvs) ils)
  | "nodecor" `elem` classes = Span (id',classes',kvs) $ map (flipFlop' st) ils
  | otherwise = Span (id',classes,kvs) $ map (flipFlop' st) ils
     where classes' = filter (/= "nodecor") classes ++
                       ["csl-no-emph"      | inEmph st] ++
                       ["csl-no-strong"    | inStrong st] ++
                       ["csl-no-smallcaps" | inSmallCaps st]
flipFlop' st (Note [Para ils]) =
  Note [Para $ map (flipFlop' st) ils]
flipFlop' _ il = il
