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

-- TODO - this is a placeholder
renderPandoc :: Style -> FormattedOutput -> [Inline]
renderPandoc sty
    = proc (convertQuoted sty) . proc' (clean' $ isPunctuationInQuote sty)
      -- . flipFlop

renderPandoc' :: Style -> FormattedOutput -> Block
renderPandoc' sty = Para . renderPandoc sty

{-
render :: [FormattedOutput] -> [Inline]
render [] = []
render (x:[])   =   renderFo x
render (x:y:os) =   let a = renderFo x
                        b = renderFo y
                        isPunct = and . map (flip elem ".!?") in
                    if isPunct (lastInline a) && isPunct (headInline b)
                    then a ++ render (tailFO [y] ++ os)
                    else a ++ render (y:os)

tailFO :: [FormattedOutput] -> [FormattedOutput]
tailFO [] = []
tailFO (f:fs)
    | FDel  s  <- f = FDel  (drop 1 s)       : fs
    | FPan is  <- f = FPan  (tailInline is) : fs
    | FN s  fm <- f = if prefix fm /= [] then FN s (tailFm fm)    : fs else FN    (drop 1 s) fm : fs
    | FS s  fm <- f = if prefix fm /= [] then FS s (tailFm fm)    : fs else FS    (drop 1 s) fm : fs
    | FO fm fo <- f = if prefix fm /= [] then FO   (tailFm fm) fo : fs else FO fm (tailFO fo)  : fs
    | otherwise     = f : tailFO fs
    where
      tailFm fm = fm { prefix = tail $ prefix fm }

renderFo ::  FormattedOutput -> [Inline]
renderFo (FPan i) = i
renderFo (FDel s) = toStr s
renderFo fo
    | FS str fm                  <- fo = toPandoc fm $ toStr str
    | FN str fm                  <- fo = toPandoc fm $ toStr $ rmZeros str
    | FO     fm xs               <- fo = toPandoc fm $ rest xs
    | FUrl u fm                  <- fo = toPandoc fm [Link (toStr $ snd u) u]
    | FErr NoOutput              <- fo = [Span ("",["citeproc-no-output"],[])
                                              [Strong [Str "???"]]]
    | FErr (ReferenceNotFound r) <- fo = [Span ("",["citeproc-not-found"],
                                            [("data-reference-id",r)])
                                            [Strong [Str "???"]]]
    | otherwise = []
    where
      addSuffix f i
          | not (null (suffix f))
          , elem (head $ suffix f) ".?!"
          , not (null (lastInline i))
          , last (lastInline i)`elem` ".?!" = i ++ toStr (tail $ suffix f)
          | null (suffix f)                 = i ++ toStr (       suffix f)
          | otherwise                       = i

      toPandoc f i = addSuffix f $ toStr (prefix f) ++
                     (quote f . format f . proc cleanStrict $ i)
      format     f = font_variant f . font f . text_case f
      rest      xs = render xs
      quote    f i = if i /= [] && quotes f /= NoQuote
                     then if quotes f == NativeQuote
                          then [escape "inquote"   . valign f $ i]
                          else [Quoted DoubleQuote . valign f $ i]
                     else valign f i
      setCase f i
          | Str     s <- i = Str $ f s
          | otherwise      = i
      setCase' f i
          | Link s r <- i = Link (map (setCase f) s) r
          | otherwise     = setCase f i

      toCap       [] = []
      toCap   (x:xs)
          | isPunctuation x = x : toCap xs
          | otherwise       = toUpper x : xs
      toTitleCap   s = if isShortWord s then s else toCap s
      isShortWord  s = s `elem` ["a","an","and","as","at","but","by","down","for","from"
                                      ,"in","into","nor","of","on","onto","or","over","so"
                                      ,"the","till","to","up","via","with","yet"]
      text_case _ [] = []
      text_case fm a@(i:is)
          | noCase fm                         = [Span ("",["nocase"],[]) a]
          | "lowercase"        <- textCase fm = map (setCase' $ map toLower) a
          | "uppercase"        <- textCase fm = map (setCase' $ map toUpper) a
          | "capitalize-all"   <- textCase fm = map (setCase  $ unwords . map toCap      . words') a
          | "title"            <- textCase fm = map (setCase  $ unwords . map toTitleCap . words') a
          | "capitalize-first" <- textCase fm = [setCase capitalize i] ++ is
          | "sentence"         <- textCase fm = [setCase toCap      i] ++
                                                map (setCase $ map toLower) is
          | otherwise                         = a

      font_variant fm i
          | "small-caps" <- fontVariant fm = [SmallCaps i]
          | otherwise                      = i

      font fm
          | noDecor fm                 = return . escape "nodecor"
          | "italic"  <- fontStyle  fm = return . Emph
          | "oblique" <- fontStyle  fm = return . Emph
          | "bold"    <- fontWeight fm = return . Strong
          | otherwise                  = id

      valign _ [] = []
      valign fm i
          | "sup"      <- verticalAlign fm = [Superscript i]
          | "sub"      <- verticalAlign fm = [Subscript   i]
          | "baseline" <- verticalAlign fm = [escape "baseline" i]
          | otherwise                      = i

      rmZeros = dropWhile (== '0')
      escape s x = Link x (s,s) -- we use a link to store some data
-}


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

{-
flipFlop :: [Inline] -> [Inline]
flipFlop [] = []
flipFlop (i:is)
    | Emph     inls <- i = Emph   (reverseEmph   True inls) : flipFlop is
    | Strong   inls <- i = Strong (reverseStrong True inls) : flipFlop is
    | otherwise          = i                                : flipFlop is
    where
      reverseEmph bo = map reverseEmph'
          where
            reverseEmph' e
                | bo, Emph inls <- e = Link (reverseEmph False inls) ("emph","emph")
                | Emph     inls <- e = Emph (reverseEmph True  inls)
                | Link ls (x,y) <- e = if x == "nodecor" && x == y
                                       then Link ls ("emph","emph")
                                       else e
                | otherwise          = e
      reverseStrong bo = map reverseStrong'
          where
            reverseStrong' e
                | bo, Strong inls <- e = Link   (reverseStrong False inls) ("strong","strong")
                | Strong     inls <- e = Strong (reverseStrong True  inls)
                | Link   ls (x,y) <- e = if x == "nodecor" && x == y
                                         then Link ls ("strong","strong")
                                         else e
                | otherwise            = e
-}

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
tailInline xs         = tailFirstInlineStr xs

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
