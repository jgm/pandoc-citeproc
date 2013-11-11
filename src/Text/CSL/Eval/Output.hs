{-# LANGUAGE PatternGuards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.CSL.Eval.Output
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Andrea Rossato <andrea.rossato@unitn.it>
-- Stability   :  unstable
-- Portability :  unportable
--
-- The CSL implementation
--
-----------------------------------------------------------------------------

module Text.CSL.Eval.Output where

import Text.CSL.Output.Plain
import Text.CSL.Output.Pandoc (lastInline)
import Text.CSL.Style
import Data.Char (toLower, toUpper)
import Text.CSL.Util (capitalize, titlecase, unTitlecase)
import Text.Pandoc.Definition
import Text.Pandoc.Walk (walk)
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.XML (fromEntities)
import Data.List.Split (splitWhen)
import Data.List (intercalate)

output :: Formatting -> String -> [Output]
output fm s
    | ' ':xs <- s = OSpace : output fm xs
    | []     <- s = []
    | otherwise   = [OStr s fm]

appendOutput :: Formatting -> [Output] -> [Output]
appendOutput fm xs = if xs /= [] then [Output xs fm] else []

outputList :: Formatting -> Delimiter -> [Output] -> [Output]
outputList fm d = appendOutput fm . addDelim d . map cleanOutput'
    where
      cleanOutput' o
          | Output xs f <- o = Output (cleanOutput xs) f
          | otherwise        = rmEmptyOutput o

cleanOutput :: [Output] -> [Output]
cleanOutput = flatten
    where
      flatten [] = []
      flatten (o:os)
          | ONull       <- o     = flatten os
          | Output xs f <- o
          , f == emptyFormatting = flatten xs ++ flatten os
          | otherwise            = rmEmptyOutput o : flatten os

rmEmptyOutput :: Output -> Output
rmEmptyOutput o
    | Output [] _ <- o = ONull
    | OStr []   _ <- o = ONull
    | OUrl t    _ <- o = if null (fst t) then ONull else o
    | otherwise        = o

addDelim :: String -> [Output] -> [Output]
addDelim d = foldr (\x xs -> if null xs then x : xs else check x xs) []
    where
      check ONull xs   = xs
      check x     xs   = let text = renderPlain . formatOutputList
                         in  if not (null d) && text [x] /= [] && text xs /= []
                             then if head d == last (text [x]) && head d `elem` ".,;:!?"
                                  then x : ODel (tail d) : xs
                                  else x : ODel       d  : xs
                             else      x                 : xs

noOutputError :: Output
noOutputError = OErr NoOutput

noBibDataError :: Cite -> Output
noBibDataError c = OErr $ ReferenceNotFound (citeId c)

oStr :: String -> [Output]
oStr s = oStr' s emptyFormatting

oStr' :: String -> Formatting -> [Output]
oStr' [] _ = []
oStr' s  f = [OStr s f]

(<++>) :: [Output] -> [Output] -> [Output]
[] <++> o  = o
o  <++> [] = o
o1 <++> o2 = o1 ++ [OSpace] ++ o2

formatOutputList :: [Output] -> FormattedOutput
formatOutputList = concatMap formatOutput

-- | Convert evaluated 'Output' into 'FormattedOutput', ready for the
-- output filters.
formatOutput :: Output -> FormattedOutput
formatOutput o =
  case o of
      OSpace              -> [Space]
      OPan     i          -> i
      ODel     []         -> []
      ODel     " "        -> [Space]
      ODel     s          -> intercalate [Str "\n"]
                             $ map (B.toList . B.text) $ splitWhen (=='\n') s
      OStr     []      _  -> []
      OStr     s       f  -> addFormatting f $ B.toList $ B.text s
                             -- case formattingToAttr f of
                             --        ("",[],[]) -> [Str s]
                             --        attr       -> [Span attr [Str s]]
      OErr NoOutput       -> [Span ("",["citeproc-no-output"],[])
                                     [Strong [Str "???"]]]
      OErr (ReferenceNotFound r)
                          -> [Span ("",["citeproc-not-found"],
                                            [("data-reference-id",r)])
                                     [Strong [Str "???"]]]
      OLabel   []      _  -> []
      OLabel   s       f  -> formatOutput (OStr s f)
      ODate    os         -> format os
      OYear    s _     f  -> formatOutput (OStr s f)
      OYearSuf s _ _   f  -> formatOutput (OStr s f)
      ONum     i       f  -> formatOutput (OStr (show i) f)
      OCitNum  i       f  -> formatOutput (OStr (add00 i) f)
      OUrl     s       f  -> [Link (formatOutput (OStr (fst s) f)) s]
      OName  _ os _    f  -> formatOutput (Output os f)
      OContrib _ _ os _ _ -> format os
      OLoc     os      f  -> formatOutput (Output os f)
      Output   []      _  -> []
      Output   os      f  -> addFormatting f $ format os
                           -- case formattingToAttr f of
                           --          ("",[],[]) -> format os
                             --        attr       -> [Span attr $ format os]
      _                   -> []
    where
      format = concatMap formatOutput
      add00  = show -- TODO why was this here??  reverse . take 5 . flip (++) (repeat '0') . reverse . show

addFormatting :: Formatting -> FormattedOutput -> FormattedOutput
addFormatting f = addSuffix . pref . quote . font_variant . font . text_case
  where pref = case prefix f of { "" -> id; x -> (Str x :) }
        addSuffix i
          | case suffix f of {(c:_) | c `elem` ".?!" -> True; _ -> False}
          , case lastInline i of {(c:_) | c `elem` ".?!" -> True; _ -> False}
                                  = i ++ toStr (tail $ suffix f)
          | not (null (suffix f)) = i ++ toStr (       suffix f)
          | otherwise             = i
        quote []  = []
        quote ils = case quotes f of
                         NoQuote     -> valign ils
                         NativeQuote ->
                                  [Span ("",[],[("csl-inquote","true")]) ils]
                         _           -> [Quoted DoubleQuote $ valign ils]
        font_variant ils =
          case fontVariant f of
               "small-caps"  -> [SmallCaps ils]
               _             -> ils

        font ils
          | noDecor f                  = [Span ("",[],[("csl-nodecor","true")]) ils]
          | otherwise = case fontStyle f of
                             "italic"  -> [Emph ils]
                             "oblique" -> [Emph ils]
                             "bold"    -> [Strong ils]
                             _         -> ils

        text_case [] = []
        text_case ils@(i:is)
          | noCase f  = [Span ("",["nocase"],[]) ils]
          | otherwise =
              case textCase f of
                   "lowercase"        -> walk lowercaseStr ils
                   "uppercase"        -> walk uppercaseStr ils
                   "capitalize-all"   -> walk capitalizeStr ils
                   "title"            -> titlecase ils
                   "capitalize-first" -> walk capitalizeStr i : is
                   "sentence"         -> unTitlecase ils
                   _                  -> ils

        lowercaseStr (Str xs)  = Str $ map toLower xs
        lowercaseStr x         = x
        uppercaseStr (Str xs)  = Str $ map toUpper xs
        uppercaseStr x         = x
        capitalizeStr (Str xs) = Str $ capitalize xs
        capitalizeStr x        = x

        valign [] = []
        valign ils
          | "sup"      <- verticalAlign f = [Superscript ils]
          | "sub"      <- verticalAlign f = [Subscript   ils]
          | "baseline" <- verticalAlign f =
                              [Span ("",[],[("csl-baseline","true")]) ils]
          | otherwise                     = ils

toStr :: String -> [Inline]
toStr = B.toList . B.text . tweak . fromEntities
    where
      tweak ('«':' ':xs) = "«\8239" ++ tweak xs
      tweak (' ':'»':xs) = "\8239»" ++ tweak xs
      tweak (' ':';':xs) = "\8239;" ++ tweak xs
      tweak (' ':':':xs) = "\8239:" ++ tweak xs
      tweak (' ':'!':xs) = "\8239!" ++ tweak xs
      tweak (' ':'?':xs) = "\8239?" ++ tweak xs
      tweak ( x :xs    ) = x : tweak xs
      tweak []           = []
