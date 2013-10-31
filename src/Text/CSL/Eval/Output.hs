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
import Text.Pandoc.Definition
import Text.Pandoc.XML (fromEntities)
import Text.ParserCombinators.Parsec hiding ( State (..) )
import Control.Applicative ((<*))
import Data.List.Split (wordsBy)
import Data.List (intersperse)

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
oStr' s  f = rtfParser f s

(<++>) :: [Output] -> [Output] -> [Output]
[] <++> o  = o
o  <++> [] = o
o1 <++> o2 = o1 ++ [OSpace] ++ o2

rtfTags :: [(String, (String,Formatting))]
rtfTags =
    [("b"                      , ("b"   , ef {fontWeight    = "bold"      }))
    ,("i"                      , ("i"   , ef {fontStyle     = "italic"    }))
    ,("sc"                     , ("sc"  , ef {fontVariant   = "small-caps"}))
    ,("sup"                    , ("sup" , ef {verticalAlign = "sup"       }))
    ,("sub"                    , ("sub" , ef {verticalAlign = "sub"       }))
    ,("span class=\"nocase\""  , ("span", ef {noCase        = True        }))
    ,("span class=\"nodecor\"" , ("span", ef {noDecor       = True        }))
    ,("span style=\"font-variant:small-caps;\""
                               , ("span", ef {fontVariant   = "small-caps"}))
    ,("span style=\"font-variant:normal;\""
                               , ("span", ef {fontVariant   = "normal"}))
    ]
    where
      ef = emptyFormatting

rtfParser :: Formatting -> String -> [Output]
rtfParser _ [] = []
rtfParser fm s
    = either (const [OStr s fm]) (return . flip Output fm) $
      parse (manyTill parser eof) "" s
    where
      parser = parseText <|> parseQuotes <|> parseMarkup

      parseText = do
        let amper = try $ char '&' <* notFollowedBy (char '#')
            apos  = char '\''
            regChar = noneOf "<'\"`“‘&"
        many1 (regChar <|> amper <|> apos) >>= \x ->
                        return (OStr x emptyFormatting)

      parseMarkup = do
        m   <- char '<' >> manyTill anyChar (char '>')
        case lookup m rtfTags of
             Just tf -> do let ct = try $ string $ "</" ++ fst tf ++ ">"
                           contents <- manyTill parser ct
                           return (Output contents (snd tf))
             Nothing -> do return (OStr ("<" ++ m ++ ">") emptyFormatting)

      parseQuotes = choice [parseQ "'" "'"
                           ,parseQ "\"" "\""
                           ,parseQ "``" "''"
                           ,parseQ "`" "'"
                           ,parseQ "“" "”"
                           ,parseQ "‘" "’"
                           ,parseQ "&#39;" "&#39;"
                           ,parseQ "&#34;" "&#34;"
                           ,parseQ "&quot;" "&quot;"
                           ,parseQ "&apos;" "&apos;"
                           ]
      parseQ a b = try $ do
        _ <- string a
        contents <- manyTill parser (try $ string b >> notFollowedBy letter)
        return (Output contents (emptyFormatting {quotes = ParsedQuote}))

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
      ODel     s          -> [Str s]
      OStr     []      _  -> []
      OStr     s       f  -> addFormatting f [Str s]
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
      Output   os      f  -> addFormatting f $ format os
                           -- case formattingToAttr f of
                           --          ("",[],[]) -> format os
                             --        attr       -> [Span attr $ format os]
      _                   -> []
    where
      format = concatMap formatOutput
      add00  = reverse . take 5 . flip (++) (repeat '0') . reverse . show

addFormatting :: Formatting -> FormattedOutput -> FormattedOutput
addFormatting f = addSuffix . pref . quote . font_variant . font . text_case
  where pref = case prefix f of { "" -> id; x -> (Str x :) }
        addSuffix i
          | case suffix f of {(c:_) | c `elem` ".?!" -> True; _ -> False}
          , case lastInline i of {(c:_) | c `elem` ".?!" -> True; _ -> False}
                                  = i ++ toStr (tail $ suffix f)
          | not (null (suffix f)) = i ++ toStr (       suffix f)
          | otherwise             = i
        quote = id -- TODO
        font_variant = id -- TODO
        font = id -- TODO
        text_case = id -- TODO

toStr :: String -> [Inline]
toStr = intersperse Space . map Str . wordsBy (==' ') . tweak . fromEntities
    where
      tweak s
          |'«':' ':xs <- s = "«\8239" ++ tweak xs
          |' ':'»':xs <- s = "\8239»" ++ tweak xs
          |' ':';':xs <- s = "\8239;" ++ tweak xs
          |' ':':':xs <- s = "\8239:" ++ tweak xs
          |' ':'!':xs <- s = "\8239!" ++ tweak xs
          |' ':'?':xs <- s = "\8239?" ++ tweak xs
          | x :xs     <- s = x : tweak xs
          | otherwise  = []

formattingToAttr :: Formatting -> Attr
formattingToAttr f = ("", [], kvs)
  where kvs = filter (\(_, v) -> not (null v))
         [ ("csl-prefix", prefix f)
         , ("csl-suffix", suffix f)
         , ("csl-font-family", fontFamily f)
         , ("csl-font-style", fontStyle f)
         , ("csl-font-variant", fontVariant f)
         , ("csl-font-weight", fontWeight f)
         , ("csl-text-decoration", textDecoration f)
         , ("csl-vertical-align", verticalAlign f)
         , ("csl-text-case", textCase f)
         , ("csl-display", display f)
         , ("csl-quotes", case quotes f of{ NativeQuote -> "native-quote";
                                        ParsedQuote -> "parsed-quote";
                                        NoQuote     -> ""})
         , ("csl-strip-periods", if stripPeriods f then "true" else "")
         , ("csl-no-case", if noCase f then "true" else "")
         , ("csl-no-decor", if noDecor f then "true" else "")
         ]
