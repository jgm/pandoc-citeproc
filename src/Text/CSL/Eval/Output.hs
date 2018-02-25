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

import           Data.Maybe             (mapMaybe)
import           Data.String            (fromString)
import           Text.CSL.Output.Pandoc (lastInline)
import           Text.CSL.Style
import           Text.CSL.Util          (isPunct, transformCase,
                                         titleCaseTransform,
                                         sentenceCaseTransform,
                                         uppercaseTransform,
                                         lowercaseTransform,
                                         capitalizeAllTransform,
                                         capitalizeFirstTransform)
import           Text.Pandoc.Definition
import           Text.Pandoc.Walk       (walk)
import           Text.Parsec

-- Parse affix or delimiter into Formatted, splitting out
-- raw components in @{{format}}...{{/format}}@.
formatString :: String -> Formatted
formatString s =
  case parse pAffix s s of
       Left _    -> fromString s
       Right ils -> Formatted ils

pAffix :: Parsec String () [Inline]
pAffix = many (pRaw <|> pString <|> pSpace)

pRaw :: Parsec String () Inline
pRaw = try $ do
  _ <- string "{{"
  format <- many1 letter
  _ <- string "}}"
  contents <- manyTill anyChar (try (string ("{{/" ++ format ++ "}}")))
  return $ RawInline (Format format) contents

pString :: Parsec String () Inline
pString = Str <$> (many1 (noneOf " \t\n\r{}") <|> count 1 (oneOf "{}"))

pSpace :: Parsec String () Inline
pSpace = Space <$ many1 (oneOf " \t\n\r")

output :: Formatting -> String -> [Output]
output fm s
    | ' ':xs <- s = OSpace : output fm xs
    | []     <- s = []
    | otherwise   = [OStr s fm]

appendOutput :: Formatting -> [Output] -> [Output]
appendOutput fm xs = [Output xs fm | xs /= []]

outputList :: Formatting -> Delimiter -> [Output] -> [Output]
outputList fm d = appendOutput fm . addDelim d . mapMaybe cleanOutput'
    where
      cleanOutput' o
          | Output xs f <- o = case cleanOutput xs of
                                 [] -> Nothing
                                 ys -> Just (Output ys f)
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
oPan' [] _  = []
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
      ODel     "\n"       -> Formatted [SoftBreak]
      ODel     s          -> formatString s
      OStr     []      _  -> Formatted []
      OStr     s       f  -> addFormatting f $ formatString s
      OErr NoOutput       -> Formatted [Span ("",["citeproc-no-output"],[])
                                     [Strong [Str "???"]]]
      OErr (ReferenceNotFound r)
                          -> Formatted [Span ("",["citeproc-not-found"],
                                            [("data-reference-id",r)])
                                     [Strong [Str "???"]]]
      OLabel   []      _  -> Formatted []
      OLabel   s       f  -> addFormatting f $ formatString s
      ODate    os         -> formatOutputList os
      OYear    s _     f  -> addFormatting f $ formatString s
      OYearSuf s _ _   f  -> addFormatting f $ formatString s
      ONum     i       f  -> formatOutput (OStr (show i) f)
      OCitNum  i       f  -> if i == 0
                                then Formatted [Strong [Str "???"]]
                                else formatOutput (OStr (show i) f)
      OCitLabel s      f  -> if s == ""
                                then Formatted [Strong [Str "???"]]
                                else formatOutput (OStr s f)
      OName  _ os _    f  -> formatOutput (Output os f)
      OContrib _ _ os _ _ -> formatOutputList os
      OLoc     os      f  -> formatOutput (Output os f)
      Output   []      _  -> Formatted []
      Output   os      f  -> addFormatting f $ formatOutputList os
      _                   -> Formatted []

addFormatting :: Formatting -> Formatted -> Formatted
addFormatting f =
  addDisplay . addLink . addSuffix . pref . quote . font . text_case . strip_periods
  where addLink i = case hyperlink f of
                         ""  -> i
                         url -> Formatted [Link nullAttr (unFormatted i) (url, "")]
        pref i = case prefix f of
                      "" -> i
                      x  -> formatString x <> i
        addSuffix i
          | null (suffix f)       = i
          | case suffix f of {(c:_) | isPunct c -> True; _ -> False}
          , case lastInline (unFormatted i) of {(c:_) | isPunct c -> True; _ -> False}
                                  = i <> formatString (tail $ suffix f)
          | otherwise             = i <> formatString (suffix f)

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

        addDisplay (Formatted []) = Formatted []
        addDisplay (Formatted ils) =
                     case display f of
                          "block"    -> Formatted (LineBreak : ils ++
                                                       [LineBreak])
                          _          -> Formatted ils

        font (Formatted ils)
          | noDecor f    = Formatted [Span ("",["nodecor"],[]) ils]
          | otherwise    = Formatted $ font_variant . font_style .  font_weight $ ils
        font_variant ils =
          case fontVariant f of
               "small-caps" -> [SmallCaps ils]
               _            -> ils
        font_style ils =
          case fontStyle f of
               "italic"  -> [Emph ils]
               "oblique" -> [Emph ils]
               _         -> ils
        font_weight ils =
          case fontWeight f of
               "bold" -> [Strong ils]
               _      -> ils

        text_case (Formatted []) = Formatted []
        text_case (Formatted ils)
          | noCase f  = Formatted [Span ("",["nocase"],[]) ils]
          | otherwise = Formatted $
             (case textCase f of
                   "lowercase"        -> transformCase lowercaseTransform
                   "uppercase"        -> transformCase uppercaseTransform
                   "capitalize-all"   -> transformCase capitalizeAllTransform
                   "title"            -> transformCase titleCaseTransform
                   "capitalize-first" -> transformCase capitalizeFirstTransform
                   "sentence"         -> transformCase sentenceCaseTransform
                   _                  -> id) ils

        valign [] = []
        valign ils
          | "sup"      <- verticalAlign f = [Superscript ils]
          | "sub"      <- verticalAlign f = [Subscript   ils]
          | "baseline" <- verticalAlign f =
                              [Span ("",["csl-baseline"],[]) ils]
          | otherwise                     = ils

