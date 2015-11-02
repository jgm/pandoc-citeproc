{-# LANGUAGE PatternGuards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.CSL.Eval.Date
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

module Text.CSL.Eval.Date where

import Control.Monad.State
import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe

import Text.CSL.Eval.Common
import Text.CSL.Eval.Output
import Text.CSL.Style
import Text.CSL.Reference
import Text.CSL.Util ( readNum, toRead, init', last' )
import Text.Pandoc.Definition ( Inline (Str) )

evalDate :: Element -> State EvalState [Output]
evalDate (Date s f fm dl dp dp') = do
  tm <- gets $ terms . env
  k  <- getStringVar "ref-id"
  em <- gets mode
  let updateFM (Formatting aa ab ac ad ae af ag ah ai aj ak al am an ahl)
               (Formatting _  _  bc bd be bf bg bh _  bj bk _ _ _ _) =
                   Formatting aa ab (updateS ac bc)
                                    (updateS ad bd)
                                    (updateS ae be)
                                    (updateS af bf)
                                    (updateS ag bg)
                                    (updateS ah bh)
                                    ai
                                    (updateS aj bj)
                                    (if bk /= ak then bk else ak)
                                    al am an ahl
      updateS a b = if b /= a && b /= [] then b else a
  case f of
    NoFormDate -> mapM getDateVar s >>= return . outputList fm dl .
                  concatMap (formatDate em k tm dp . concatMap parseRefDate)
    _          -> do Date _ _ lfm ldl ldp _ <- getDate f
                     let go dps = return . outputList (updateFM fm lfm) (if ldl /= [] then ldl else dl) .
                                  concatMap (formatDate em k tm dps . concatMap parseRefDate)
                         update l x@(DatePart a b c d) =
                             case filter ((==) a . dpName) l of
                               (DatePart _ b' c' d':_) -> DatePart a (updateS  b b')
                                                                     (updateS  c c')
                                                                     (updateFM d d')
                               _                       -> x
                         updateDP = map (update dp) ldp
                         date     = mapM getDateVar s
                     case dp' of
                       "year-month" -> go (filter ((/=) "day"  . dpName) updateDP) =<< date
                       "year"       -> go (filter ((==) "year" . dpName) updateDP) =<< date
                       _            -> go                                updateDP  =<< date

evalDate _ = return []

getDate :: DateForm -> State EvalState Element
getDate f = do
  x <- filter (\(Date _ df _ _ _ _) -> df == f) <$> gets (dates . env)
  case x of
    [x'] -> return x'
    _    -> return $ Date [] NoFormDate emptyFormatting [] [] []

formatDate :: EvalMode -> String -> [CslTerm] -> [DatePart] -> [RefDate] -> [Output]
formatDate em k tm dp date
    | [d]     <- date = concatMap (formatDatePart False d) dp
    | (a:b:_) <- date = addODate . concat $ doRange a b
    | otherwise       = []
    where
      addODate []   = []
      addODate xs   = [ODate xs]
      splitDate a b = case split (onSublist $ diff a b dp) dp of
                        [x,y,z] -> (x,y,z)
                        _       -> error "error in splitting date ranges"
      doRange   a b = let (x,y,z) = splitDate a b in
                      map (formatDatePart False  a) x ++
                      map (formatDatePart False  a) (init' y) ++
                      map (formatDatePart True   a) (last' y) ++
                      map (formatDatePart False  b) (rmPrefix y) ++
                      map (formatDatePart False  b) z
      -- the point of rmPrefix is to remove the blank space that otherwise
      -- gets added after the delimiter in a range:  24- 26.
      rmPrefix (dp':rest) = dp'{ dpFormatting = (dpFormatting dp') { prefix = "" } } : rest
      rmPrefix []         = []
      diff  a b = filter (flip elem (diffDate a b) . dpName)
      diffDate (RefDate ya ma sa da _ _)
               (RefDate yb mb sb db _ _) = case () of
                                             _ | ya /= yb  -> ["year","month","day"]
                                               | ma /= mb  ->
                                                 if da == mempty && db == mempty
                                                    then ["month"]
                                                    else ["month","day"]
                                               | da /= db  -> ["day"]
                                               | sa /= sb  -> ["month"]
                                               | otherwise -> ["year","month","day"]

      term f t = let f' = if f `elem` ["verb", "short", "verb-short", "symbol"]
                          then read $ toRead f
                          else Long
                 in maybe [] termPlural $ findTerm t f' tm

      addZero n = if length n == 1 then '0' : n else n
      addZeros  = reverse . take 5 . flip (++) (repeat '0') . reverse
      formatDatePart False (RefDate (Literal y) (Literal m)
        (Literal e) (Literal d) _ _) (DatePart n f _ fm)
          | "year"  <- n, y /= mempty = return $ OYear (formatYear  f    y) k fm
          | "month" <- n, m /= mempty = output fm      (formatMonth f fm m)
          | "day"   <- n, d /= mempty = output fm      (formatDay   f m  d)
          | "month" <- n, m == mempty
                        , e /= mempty = output fm $ term f ("season-0" ++ e)

      formatDatePart True (RefDate (Literal y) (Literal m) (Literal e) (Literal d) _ _) (DatePart n f rd fm)
          | "year"  <- n, y /= mempty = OYear (formatYear  f y) k (fm {suffix = []}) : formatDelim
          | "month" <- n, m /= mempty = output (fm {suffix = []}) (formatMonth f fm m) ++ formatDelim
          | "day"   <- n, d /= mempty = output (fm {suffix = []}) (formatDay   f m  d) ++ formatDelim
          | "month" <- n, m == mempty
                        , e /= mempty = output (fm {suffix = []}) (term f $ "season-0" ++ e) ++ formatDelim
          where
            formatDelim = if rd == "-" then [OPan [Str "\x2013"]] else [OPan [Str rd]]

      formatDatePart _ (RefDate _ _ _ _ (Literal o) _) (DatePart n _ _ fm)
          | "year"  <- n, o /= mempty = output fm o
          | otherwise                 = []

      formatYear f y
          | "short" <- f = drop 2 y
          | isSorting em
          , iy < 0       = '-' : addZeros (tail y)
          | isSorting em = addZeros y
          | iy < 0       = show (abs iy) ++ term [] "bc"
          | length y < 4
          , iy /= 0      = y ++ term [] "ad"
          | iy == 0      = []
          | otherwise    = y
          where
            iy = readNum y
      formatMonth f fm m
          | "short"   <- f = getMonth $ period . termPlural
          | "long"    <- f = getMonth termPlural
          | "numeric" <- f = m
          | otherwise      = addZero m
          where
            period     = if stripPeriods fm then filter (/= '.') else id
            getMonth g = maybe m g $ findTerm ("month-" ++ addZero m) (read $ toRead f) tm
      formatDay f m d
          | "numeric-leading-zeros" <- f = addZero d
          | "ordinal"               <- f = ordinal tm ("month-" ++ addZero m) d
          | otherwise                    = d

ordinal :: [CslTerm] -> String -> String -> String
ordinal _ _ [] = []
ordinal ts v s
    | length s == 1 = let a = termPlural (getWith1 s) in
                      if  a == [] then setOrd (term []) else s ++ a
    | length s == 2 = let a = termPlural (getWith2 s)
                          b = getWith1 [last s] in
                      if  a /= []
                      then s ++ a
                      else if termPlural b == [] || (termMatch b /= [] && termMatch b /= "last-digit")
                           then setOrd (term []) else setOrd b
    | otherwise     = let a = getWith2  last2
                          b = getWith1 [last s] in
                      if termPlural a /= [] && termMatch a /= "whole-number"
                      then setOrd a
                      else if termPlural b == [] || (termMatch b /= [] && termMatch b /= "last-digit")
                           then setOrd (term []) else setOrd b
    where
      setOrd   = (++) s . termPlural
      getWith1 = term . (++) "-0"
      getWith2 = term . (++) "-"
      last2    = reverse . take 2 . reverse $ s
      term   t = getOrdinal v ("ordinal" ++ t) ts

longOrdinal :: [CslTerm] -> String -> String -> String
longOrdinal _ _ [] = []
longOrdinal ts v s
    | num > 10 ||
      num == 0  = ordinal ts v s
    | otherwise = case last s of
                    '1' -> term "01"
                    '2' -> term "02"
                    '3' -> term "03"
                    '4' -> term "04"
                    '5' -> term "05"
                    '6' -> term "06"
                    '7' -> term "07"
                    '8' -> term "08"
                    '9' -> term "09"
                    _   -> term "10"
    where
      num    = readNum s
      term t = termPlural $ getOrdinal v ("long-ordinal-" ++ t) ts

getOrdinal :: String -> String -> [CslTerm] -> CslTerm
getOrdinal v s ts
    = case findTerm' s Long gender ts of
        Just  x -> x
        Nothing -> case findTerm' s Long Neuter ts of
                     Just  x -> x
                     Nothing -> newTerm
    where
      gender = if v `elem` numericVars || "month" `isPrefixOf` v
               then maybe Neuter termGender $ findTerm v Long ts
               else Neuter

parseRefDate :: RefDate -> [RefDate]
parseRefDate r@(RefDate _ _ _ _ (Literal o) c)
    = if null o then return r
      else let (a,b) = break (== '-') o
           in  if null b then return (parseRaw o) else [parseRaw a, parseRaw b]
    where
      parseRaw str =
          case words $ check str of
            [y']       | and (map isDigit y') -> RefDate (Literal y') mempty mempty mempty (Literal o) c
            [s',y']    | and (map isDigit y')
                       , and (map isDigit s') -> RefDate (Literal y') (Literal s') mempty mempty (Literal o) c
            [s',y']    | s' `elem'` seasons   -> RefDate (Literal y') mempty (Literal $ select s' seasons) mempty (Literal o) False
            [s',y']    | s' `elem'` months    -> RefDate (Literal y') (Literal $ select s'  months) mempty mempty (Literal o) c
            [s',d',y'] | and (map isDigit s')
                       , and (map isDigit y')
                       , and (map isDigit d') -> RefDate (Literal y') (Literal s') mempty (Literal d') (Literal o) c
            [s',d',y'] | s' `elem'` months
                       , and (map isDigit y')
                       , and (map isDigit d') -> RefDate (Literal y') (Literal $ select s'  months) mempty (Literal d') (Literal o) c
            [s',d',y'] | s' `elem'` months
                       , and (map isDigit y')
                       , and (map isDigit d') -> RefDate (Literal y') (Literal $ select s'  months) mempty (Literal d') (Literal o) c
            _                                 -> r
      check []     = []
      check (x:xs) = if x `elem` ",/-" then ' ' : check xs else x : check xs
      select     x = show . (+ 1) . fromJust . elemIndex' x
      elem'      x = elem      (map toLower $ take 3 x)
      elemIndex' x = elemIndex (map toLower $ take 3 x)

      months   = ["jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"]
      seasons  = ["spr","sum","fal","win"]
