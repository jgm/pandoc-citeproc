{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
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

import Prelude
import qualified Control.Exception      as E
import           Control.Monad.State

import           Data.List.Split
import           Data.Maybe (fromMaybe, isNothing)
import           Data.Text              (Text)
import qualified Data.Text              as T

import           Text.CSL.Exception
import           Text.CSL.Eval.Common
import           Text.CSL.Eval.Output
import           Text.CSL.Style
import           Text.CSL.Reference
import           Text.CSL.Util ( toRead, last' )
import           Text.Pandoc.Definition ( Inline (Str) )
import           Text.Printf (printf)

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
      updateS a b = if b /= a && b /= "" then b else a
  case f of
    NoFormDate -> outputList fm dl .
                  concatMap (formatDate em k tm dp) <$> mapM getDateVar s
    _          -> do res <- getDate f
                     case res of
                       Date _ _ lfm ldl ldp _ -> do
                         let go dps = return . outputList (updateFM fm lfm) (if ldl /= "" then ldl else dl) .
                                      concatMap (formatDate em k tm dps)
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
                       _ -> return []
evalDate _ = return []

getDate :: DateForm -> State EvalState Element
getDate f = do
  x <- filter (\(Date _ df _ _ _ _) -> df == f) <$> gets (dates . env)
  case x of
    [x'] -> return x'
    _    -> return $ Date [] NoFormDate emptyFormatting "" [] ""

formatDate :: EvalMode -> Text -> [CslTerm] -> [DatePart] -> [RefDate] -> [Output]
formatDate em k tm dp date
    | [d]     <- date = concatMap (formatDatePart d) dp
    | (a:b:_) <- date = addODate . concat $ doRange a b
    | otherwise       = []
    where
      addODate [] = []
      addODate xs = [ODate xs]
      splitDate a b = case split (onSublist $ diff a b dp) dp of
                        [x,y,z] -> (x,y,z)
                        _       -> E.throw ErrorSplittingDate
      doRange   a b = let (x,y,z) = splitDate a b in
                      map (formatDatePart a) x ++
                      withDelim y
                        (map (formatDatePart a) (rmSuffix y))
                        (map (formatDatePart b) (rmPrefix y))
                        ++
                      map (formatDatePart b) z
      -- the point of rmPrefix is to remove the blank space that otherwise
      -- gets added after the delimiter in a range:  24- 26.
      rmPrefix (dp':rest) = dp'{ dpFormatting =
                                 (dpFormatting dp') { prefix = "" } } : rest
      rmPrefix []         = []
      rmSuffix (dp':rest)
         | null rest      = [dp'{ dpFormatting =
                                  (dpFormatting dp') { suffix = "" } }]
         | otherwise      = dp':rmSuffix rest
      rmSuffix []         = []

      diff (RefDate ya ma sa da _ _)
           (RefDate yb mb sb db _ _)
           = filter (\x -> dpName x `elem` ns)
              where ns =
                      case () of
                        _ | ya /= yb  -> ["year","month","day"]
                          | ma /= mb || sa /= sb ->
                            if isNothing da && isNothing db
                               then ["month"]
                               else ["month","day"]
                          | da /= db  -> ["day"]
                          | otherwise -> ["year","month","day"]

      term f t = let f' = if f `elem` ["verb", "short", "verb-short", "symbol"]
                          then read . T.unpack $ toRead f
                          else Long
                 in maybe "" termPlural $ findTerm t f' tm

      formatDatePart (RefDate y m e d o _) (DatePart n f _ fm)
          | "year"  <- n, Just y' <- y = return $ OYear (formatYear  f    y') k fm
          | "month" <- n, Just m' <- m = output fm      (formatMonth f fm m')
          | "month" <- n, Just e' <- e =
               case e' of
                    RawSeason s -> [OStr s fm]
                    _ -> output fm . term f . T.pack $
                         (printf "season-%02d" $ fromMaybe 0 $ seasonToInt e')
          | "day"   <- n, Just d' <- d = output fm      (formatDay   f m  d')
          | "year"  <- n, o /= mempty = output fm (unLiteral o)
          | otherwise                 = []

      withDelim xs o1 o2
        | null (concat o1 ++ concat o2) = []
        | otherwise = o1 ++ (case dpRangeDelim <$> last' xs of
                              ["-"] -> [[OPan [Str "\x2013"]]]
                              [s]   -> [[OPan [Str s]]]
                              _     -> []) ++ o2

      formatYear f y
          | "short" <- f = T.pack $ printf "%02d" y
          | isSorting em
          , y < 0        = T.pack $ printf "-%04d" (abs y)
          | isSorting em = T.pack $ printf "%04d" y
          | y < 0        = (T.pack $ printf "%d" (abs y)) <> term "" "bc"
          | y < 1000
          , y > 0        = (T.pack $ printf "%d" y) <> term "" "ad"
          | y == 0       = ""
          | otherwise    = T.pack $ printf "%d" y

      formatMonth f fm m
          | "short"   <- f = getMonth $ period . termPlural
          | "long"    <- f = getMonth termPlural
          | "numeric" <- f = T.pack $ printf "%d" m
          | otherwise      = T.pack $ printf "%02d" m
          where
            period     = if stripPeriods fm then T.filter (/= '.') else id
            getMonth g = case findTerm ("month-" <> T.pack (printf "%02d" m))
                                       (read . T.unpack $ toRead f) tm of
                           Nothing -> T.pack (show m)
                           Just x  -> g x

      formatDay f m d
          | "numeric-leading-zeros" <- f = T.pack $ printf "%02d" d
          | "ordinal"               <- f = ordinal tm ("month-" <> maybe "0" (T.pack . printf "%02d") m) d
          | otherwise                    = T.pack $ printf "%d" d

ordinal :: [CslTerm] -> Text -> Int -> Text
ordinal ts v s
    | s < 10        = let a = termPlural (getWith1 (show s)) in
                      if T.null a
                      then setOrd (term "")
                      else T.pack (show s) <> a
    | s < 100       = let a = termPlural (getWith2 (show s))
                          b = getWith1 [last (show s)] in
                      if not (T.null a)
                      then T.pack (show s) <> a
                      else if T.null (termPlural b) ||
                              (not (T.null (termMatch b)) &&
                               termMatch b /= "last-digit")
                           then setOrd (term "")
                           else setOrd b
    | otherwise     = let a = getWith2  last2
                          b = getWith1 [last (show s)] in
                      if not (T.null (termPlural a)) &&
                         termMatch a /= "whole-number"
                      then setOrd a
                      else if T.null (termPlural b) ||
                              (not (T.null (termMatch b)) &&
                               termMatch b /= "last-digit")
                           then setOrd (term "")
                           else setOrd b
    where
      setOrd   = T.append (T.pack $ show s) . termPlural
      getWith1 = term . T.append "-0" . T.pack
      getWith2 = term . T.append "-" . T.pack
      last2    = reverse . take 2 . reverse $ show s
      term   t = getOrdinal v ("ordinal" <> t) ts

longOrdinal :: [CslTerm] -> Text -> Int -> Text
longOrdinal ts v s
    | s > 10 ||
      s == 0  = ordinal ts v s
    | otherwise = case s `mod` 10 of
                    1 -> term "01"
                    2 -> term "02"
                    3 -> term "03"
                    4 -> term "04"
                    5 -> term "05"
                    6 -> term "06"
                    7 -> term "07"
                    8 -> term "08"
                    9 -> term "09"
                    _ -> term "10"
    where
      term t = termPlural $ getOrdinal v ("long-ordinal-" <> t) ts

getOrdinal :: Text -> Text -> [CslTerm] -> CslTerm
getOrdinal v s ts
    = fromMaybe newTerm $ findTerm' s Long gender ts `mplus`
                          findTerm' s Long Neuter ts
    where
      gender = if v `elem` numericVars || "month" `T.isPrefixOf` v
               then maybe Neuter termGender $ findTerm v Long ts
               else Neuter
