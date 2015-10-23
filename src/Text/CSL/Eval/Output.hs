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

import Text.CSL.Output.Pandoc (lastInline)
import Text.CSL.Style
import Data.Char (toLower, toUpper)
import Text.CSL.Util (capitalize, titlecase, unTitlecase, isPunct)
import Text.Pandoc.Definition
import Text.Pandoc.Walk (walk)
import Data.String (fromString)
import Data.Maybe (mapMaybe)

output :: Formatting -> String -> [Output]
output fm s
    | ' ':xs <- s = OSpace : output fm xs
    | []     <- s = []
    | otherwise   = [OStr s fm]

appendOutput :: Formatting -> [Output] -> [Output]
appendOutput fm xs = if xs /= [] then [Output xs fm] else []

outputList :: Formatting -> Delimiter -> [Output] -> [Output]
outputList fm d = appendOutput fm . addDelim d . mapMaybe cleanOutput'
    where
      cleanOutput' o
          | Output xs f <- o = case cleanOutput xs of
                                 []   -> Nothing
                                 ys   -> Just (Output ys f)
          | otherwise        = rmEmptyOutput o

cleanOutput :: [Output] -> [Output]
cleanOutput = flatten
    where
      flatten [] = []
      flatten (o:os)
          | ONull       <- o     = flatten os
          | Output xs f <- o
          , f == emptyFormatting = flatten (mapMaybe rmEmptyOutput xs) ++ flatten os
          | Output xs f <- o     = Output (flatten $ mapMaybe rmEmptyOutput xs) f : flatten os
          | otherwise            = maybe id (:) (rmEmptyOutput o) $ flatten os

rmEmptyOutput :: Output -> Maybe Output
rmEmptyOutput o
    | Output [] _ <- o = Nothing
    | OStr []   _ <- o = Nothing
    | OPan []     <- o = Nothing
    | ODel []     <- o = Nothing
    | otherwise        = Just o

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

oPan :: [Inline] -> [Output]
oPan []  = []
oPan ils = [OPan ils]

oPan' :: [Inline] -> Formatting -> [Output]
oPan' [] _ = []
oPan' ils f = [Output [OPan ils] f]

formatOutputList :: [Output] -> Formatted
formatOutputList = mconcat . map formatOutput

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
      ODate    os         -> formatOutputList os
      OYear    s _     f  -> formatOutput (OStr s f)
      OYearSuf s _ _   f  -> formatOutput (OStr s f)
      ONum     i       f  -> formatOutput (OStr (show i) f)
      OCitNum  i       f  -> if i == 0
                                then Formatted [Strong [Str "???"]]
                                else formatOutput (OStr (show i) f)
      OName  _ os _    f  -> formatOutput (Output os f)
      OContrib _ _ os _ _ -> formatOutputList os
      OLoc     os      f  -> formatOutput (Output os f)
      Output   []      _  -> Formatted []
      Output   os      f  -> addFormatting f $ formatOutputList os
      _                   -> Formatted []

addFormatting :: Formatting -> Formatted -> Formatted
addFormatting f =
  addLink . addSuffix . pref . quote . font . text_case . strip_periods
  where addLink i = case hyperlink f of
                         ""  -> i
                         url -> Formatted [Link (unFormatted i) (url, "")]
        pref i = case prefix f of
                      "" -> i
                      x  -> fromString x <> i
        addSuffix i
          | null (suffix f)       = i
          | case suffix f of {(c:_) | isPunct c -> True; _ -> False}
          , case lastInline (unFormatted i) of {(c:_) | isPunct c -> True; _ -> False}
                                  = i <> fromString (tail $ suffix f)
          | otherwise             = i <> fromString (suffix f)

        strip_periods (Formatted ils) = Formatted (walk removePeriod ils)
        removePeriod (Str xs) | stripPeriods f = Str (filter (/='.') xs)
        removePeriod x        = x

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

