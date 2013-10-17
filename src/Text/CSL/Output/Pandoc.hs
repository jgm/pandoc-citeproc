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
    , renderPandoc_
    , headInline
    , initInline
    , tailFirstInlineStr
    , toCapital
    , startWithPunct
    , endWithPunct
    ) where

import Data.Char ( toUpper, toLower, isPunctuation )
import Data.Maybe ( fromMaybe )

import Text.CSL.Style
import Text.CSL.Output.Plain
import Text.Pandoc.Definition

-- | With a 'Style' and the formatted output generate a 'String' in
-- the native 'Pandoc' formats (i.e. immediately readable by pandoc).
renderPandoc :: Style -> [FormattedOutput] -> [Inline]
renderPandoc s
    = proc (convertQuoted s) . proc' (clean s $ isPunctuationInQuote s) .
      flipFlop . render s

-- | Same as 'renderPandoc', but the output is wrapped in a pandoc
-- paragraph block.
renderPandoc' :: Style -> [FormattedOutput] -> Block
renderPandoc' s
    = Para . proc (convertQuoted s) . proc' (clean s $ isPunctuationInQuote s) .
      flipFlop . render s

-- | For the testsuite: we use 'Link' and 'Strikeout' to store
-- "nocase" and "nodecor" rich text formatting classes.
renderPandoc_ :: Style -> [FormattedOutput] -> [Inline]
renderPandoc_ s
    = proc (convertQuoted s) . proc (clean' s $ isPunctuationInQuote s) .
      flipFlop . render s

render :: Style -> [FormattedOutput] -> [Inline]
render _ [] = []
render s (x:[])   = renderFo s x
render s (x:y:os) = let a = renderFo s x
                        b = renderFo s y
                        isPunct = and . map (flip elem ".!?") in
                    if isPunct (lastInline a) && isPunct (headInline b)
                    then a ++ render s (tailFO [y] ++ os)
                    else a ++ render s (y:os)

tailFO :: [FormattedOutput] -> [FormattedOutput]
tailFO [] = []
tailFO (f:fs)
    | FDel  s  <- f = FDel  (tail' s)       : fs
    | FPan is  <- f = FPan  (tailInline is) : fs
    | FN s  fm <- f = if prefix fm /= [] then FN s (tailFm fm)    : fs else FN    (tail' s) fm : fs
    | FS s  fm <- f = if prefix fm /= [] then FS s (tailFm fm)    : fs else FS    (tail' s) fm : fs
    | FO fm fo <- f = if prefix fm /= [] then FO   (tailFm fm) fo : fs else FO fm (tailFO fo)  : fs
    | otherwise     = f : tailFO fs
    where
      tailFm fm = fm { prefix = tail $ prefix fm }

renderFo :: Style -> FormattedOutput -> [Inline]
renderFo _ (FPan i) = i
renderFo _ (FDel s) = toStr s
renderFo sty fo
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
          | suffix f /= []
          , elem (head $ suffix f) ".?!"
          , lastInline i /= []
          , last (lastInline i)`elem` ".?!" = i ++ toStr (tail $ suffix f)
          | suffix f /= []                  = i ++ toStr (       suffix f)
          | otherwise                       = i

      toPandoc f i = addSuffix f $ toStr (prefix f) ++
                     (quote f . format f . proc cleanStrict $ i)
      format     f = font_variant f . font f . text_case f
      rest      xs = procList xs $ render sty
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
          | "capitalize-all"   <- textCase fm = map (setCase  $ unwords . map toCap      . words) a
          | "title"            <- textCase fm = map (setCase  $ unwords . map toTitleCap . words) a
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

toStr :: String -> [Inline]
toStr = toStr' . entityToChar
    where
      toStr' s
          |'«':' ':xs <- s = toStr' ("«\8239" ++ xs)
          |' ':'»':xs <- s = toStr' ("\8239»" ++ xs)
          |' ':';':xs <- s = toStr' ("\8239;" ++ xs)
          |' ':':':xs <- s = toStr' ("\8239:" ++ xs)
          |' ':'!':xs <- s = toStr' ("\8239!" ++ xs)
          |' ':'?':xs <- s = toStr' ("\8239?" ++ xs)
          |' ':xs <- s = Space   : toStr' xs
          | x :xs <- s = Str [x] : toStr' xs
          | otherwise  = []

cleanStrict :: [Inline] -> [Inline]
cleanStrict []  = []
cleanStrict (i:is)
    | Str []    <- i  =                  cleanStrict is
    | Str " "   <- i  = Space          : cleanStrict is
    | Str sa    <- i
    , Str sb:xs <- is = Str (sa ++ sb) : cleanStrict xs
    | otherwise       =              i : cleanStrict is

clean :: Style -> Bool -> [Inline] -> [Inline]
clean _ _ []  = []
clean s b (i:is)
    | Superscript x <- i = split (isLink  "baseline") (return . Superscript) x ++ clean s b is
    | Subscript   x <- i = split (isLink  "baseline") (return . Subscript  ) x ++ clean s b is
    | SmallCaps   x <- i = split (isLink  "nodecor" ) (return . SmallCaps  ) x ++ clean s b is
    | Emph        x <- i = split (isLink' "emph"    ) (return . Emph       ) x ++ clean s b is
    | Strong      x <- i = split (isLink' "strong"  ) (return . Strong     ) x ++ clean s b is
    | Link      x t <- i = clean' s b (Link x t : clean s b is)
    | otherwise          = clean' s b (i        : clean s b is)
    where
      unwrap f ls
          | Link x _ : _ <- ls = clean' s b x
          |        _ : _ <- ls = f ls
          | otherwise          = []
      isLink l il
          | Link _ (x,y) <- il = x == l && x == y
          | otherwise          = False
      isLink' l il
          | Link _ (x,y) <- il = (x == l || x == "nodecor") && x == y
          | otherwise          = False
      split _ _ [] = []
      split f g xs = let (y, r) = break f xs
                     in concatMap (unwrap g) [y, head' r] ++ split f g (tail' r)

clean' :: Style -> Bool -> [Inline] -> [Inline]
clean' _ _   []  = []
clean' s b (i:is)
    | Link inls (y,z) <- i, y == "inquote"
    , y == z    = case headInline is of
                    [x] -> if x `elem` ".," && b
                           then if lastInline inls `elem` [".",",",";",":","!","?"]
                                then quote DoubleQuote inls                : clean' s b (tailInline is)
                                else quote DoubleQuote (inls ++ [Str [x]]) : clean' s b (tailInline is)
                           else quote DoubleQuote inls : clean' s b is
                    _   ->      quote DoubleQuote inls : clean' s b is
    | Quoted t inls <- i = quote t inls : clean' s b is
    | otherwise = if lastInline [i] == headInline is && isPunct
                  then i : clean' s b (tailInline is)
                  else i : clean' s b is
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

isPunctuationInQuote :: Style -> Bool
isPunctuationInQuote = or . query punctIn'
    where
      punctIn' n
          | ("punctuation-in-quote","true") <- n = [True]
          | otherwise                            = [False]

endWithPunct, startWithPunct :: [Inline] -> Bool
endWithPunct   = and . map (`elem` ".,;:!?") . lastInline
startWithPunct = and . map (`elem` ".,;:!?") . headInline

convertQuoted :: Style -> [Inline] -> [Inline]
convertQuoted s = convertQuoted'
    where
      locale = let l = styleLocale s in case l of [x] -> x; _   -> Locale [] [] [] [] []
      getQuote  x y = entityToChar . termSingular . fromMaybe newTerm {termSingular = x} .
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
headInline [] = []
headInline (i:_)
    | Str s <- i = head' s
    | Space <- i = " "
    | otherwise  = headInline $ getInline i

lastInline :: [Inline] -> String
lastInline [] = []
lastInline (i:[])
    | Str s <- i = last' s
    | Space <- i = " "
    | otherwise  = lastInline $ getInline i
    where
      last' s = if s /= [] then [last s] else []
lastInline (_:xs) = lastInline xs

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
    | otherwise           = []
    where
      init' s = if s /= [] then init s else []
initInline (i:xs) = i : initInline xs

tailInline :: [Inline] -> [Inline]
tailInline inls
    | (i:t) <- inls
    , Space <- i = t
    | otherwise  = tailFirstInlineStr inls

tailFirstInlineStr :: [Inline] -> [Inline]
tailFirstInlineStr = mapHeadInline tail'

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
    | otherwise           = i : xs

getInline :: Inline -> [Inline]
getInline i
    | Emph        is <- i = is
    | Strong      is <- i = is
    | Strikeout   is <- i = is
    | Superscript is <- i = is
    | Subscript   is <- i = is
    | Quoted _    is <- i = is
    | SmallCaps   is <- i = is
    | Link      is _ <- i = is
    | otherwise           = []
