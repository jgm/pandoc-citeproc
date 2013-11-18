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

import Text.CSL.Output.Pandoc (lastInline, headInline, tailInline)
import Text.CSL.Style
import Data.Char (toLower, toUpper)
import Text.CSL.Util (capitalize, titlecase, unTitlecase)
import Text.Pandoc.Definition
import Text.Pandoc.Walk (walk)
import Data.Monoid (mappend, mempty, mconcat, (<>))
import Data.String (fromString)

-- import Debug.Trace
-- tr' note' x = Debug.Trace.trace (note' ++ ": " ++ show x) x

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
          , f == emptyFormatting = flatten (map rmEmptyOutput xs) ++ flatten os
          | otherwise            = rmEmptyOutput o : flatten os

rmEmptyOutput :: Output -> Output
rmEmptyOutput o
    | Output [] _ <- o = ONull
    | OStr []   _ <- o = ONull
    | OPan []     <- o = ONull
    | ODel []     <- o = ONull
    | OUrl t    _ <- o = if null (fst t) then ONull else o
    | otherwise        = o

addDelim :: String -> [Output] -> [Output]
addDelim "" = id
addDelim d  = foldr check []
    where
      check ONull xs   = xs
      check x     []   = [x]
      check x (z:zs)   = if formatOutput x == mempty || formatOutput z == mempty
                            then x : z : zs
                            else x : ODel d : z : zs

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

formatOutputList :: [Output] -> Formatted
formatOutputList = foldr appendWithPunct mempty . map formatOutput

appendWithPunct :: Formatted -> Formatted -> Formatted
appendWithPunct (Formatted left) (Formatted right) = Formatted $ left ++
  if isPunct' lastleft && isPunct' firstright ||
     isSp lastleft && isSp firstright
     then tailInline right
     else right
  where isPunct' [c] = isPunct c
        isPunct' _   = False
        isSp " "     = True
        isSp _       = False
        lastleft     = lastInline left
        firstright   = headInline right

-- | Convert evaluated 'Output' into 'Formatted', ready for the
-- output filters.
formatOutput :: Output -> Formatted
formatOutput o =
  case o of
      OSpace              -> Formatted [Space]
      OPan     i          -> Formatted i
      ODel     []         -> Formatted []
      ODel     " "        -> Formatted [Space]
      ODel     s          -> fromString s
      OStr     []      _  -> Formatted []
      OStr     s       f  -> addFormatting f $ fromString s
      OErr NoOutput       -> Formatted [Span ("",["citeproc-no-output"],[])
                                     [Strong [Str "???"]]]
      OErr (ReferenceNotFound r)
                          -> Formatted [Span ("",["citeproc-not-found"],
                                            [("data-reference-id",r)])
                                     [Strong [Str "???"]]]
      OLabel   []      _  -> Formatted []
      OLabel   s       f  -> formatOutput (OStr s f)
      ODate    os         -> format os
      OYear    s _     f  -> formatOutput (OStr s f)
      OYearSuf s _ _   f  -> formatOutput (OStr s f)
      ONum     i       f  -> formatOutput (OStr (show i) f)
      OCitNum  i       f  -> if i == 0
                                then Formatted [Strong [Str "???"]]
                                else formatOutput (OStr (show i) f)
      OUrl     s       f  -> Formatted [Link (unFormatted $ formatOutput (OStr (fst s) f)) s]
      OName  _ os _    f  -> formatOutput (Output os f)
      OContrib _ _ os _ _ -> format os
      OLoc     os      f  -> formatOutput (Output os f)
      Output   []      _  -> Formatted []
      Output   os      f  -> addFormatting f $ format os
      _                   -> Formatted []
    where
      format = mconcat . map formatOutput

isPunct :: Char -> Bool
isPunct c = c `elem` ".;?!"

addFormatting :: Formatting -> Formatted -> Formatted
addFormatting f = addSuffix . pref . quote . font . text_case
  where pref = case prefix f of { "" -> id; x -> ((fromString x) <>) }
        addSuffix i
          | case suffix f of {(c:_) | isPunct c -> True; _ -> False}
          , case lastInline (unFormatted i) of
                             {(c:_) | isPunct c -> True; _ -> False}
                                  = i <> fromString (tail $ suffix f)
          | not (null (suffix f)) = i <> fromString (       suffix f)
          | otherwise             = i
        quote (Formatted [])  = Formatted []
        quote (Formatted ils) =
                    case quotes f of
                         NoQuote     -> Formatted $ valign ils
                         NativeQuote -> Formatted
                                  [Span ("",["csl-inquote"],[]) ils]
                         _           -> Formatted [Quoted DoubleQuote $ valign ils]

        font (Formatted ils)
          | noDecor f    = Formatted [Span ("",["nodecor"],[]) ils]
          | otherwise    = Formatted $ font_variant . font_style .  font_weight $ ils
        font_variant ils =
          case fontVariant f of
               "small-caps"  -> [SmallCaps ils]
               _             -> ils
        font_style ils =
          case fontStyle f of
               "italic"      -> [Emph ils]
               "oblique"     -> [Emph ils]
               _             -> ils
        font_weight ils =
          case fontWeight f of
               "bold"        -> [Strong ils]
               _             -> ils

        text_case (Formatted []) = Formatted []
        text_case (Formatted ils@(i:is))
          | noCase f  = Formatted [Span ("",["nocase"],[]) ils]
          | otherwise = Formatted $
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
                              [Span ("",["csl-baseline"],[]) ils]
          | otherwise                     = ils

