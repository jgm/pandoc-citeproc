{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.CSL.Proc.Collapse
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Andrea Rossato <andrea.rossato@unitn.it>
-- Stability   :  unstable
-- Portability :  unportable
--
-- This module provides functions for processing the evaluated
-- 'Output' for citation collapsing.
--
-----------------------------------------------------------------------------

module Text.CSL.Proc.Collapse where

import Prelude
import           Control.Arrow          (second, (&&&), (>>>))
import           Data.Char
import           Data.List              (groupBy, sortBy)
import           Data.Monoid            (Any (..))
import           Data.Ord               (comparing)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Text.CSL.Eval
import           Text.CSL.Proc.Disamb
import           Text.CSL.Style         hiding (Any)
import           Text.CSL.Util          (proc, proc', query)
import           Text.Pandoc.Definition (Inline (Str))

-- | Collapse citations according to the style options.
collapseCitGroups :: Style -> [CitationGroup] -> [CitationGroup]
collapseCitGroups s
    = map doCollapse
    where
      doCollapse = case getCollapseOptions s of
                     "year"               : _ -> collapseYear s ""
                     "year-suffix"        : _ -> collapseYear s "year-suffix"
                     "year-suffix-ranged" : _ -> collapseYear s "year-suffix-ranged"
                     "citation-number"    : _ -> collapseNumber
                     _                        -> id

-- | Get the collapse option set in the 'Style' for citations.
getCollapseOptions :: Style -> [Text]
getCollapseOptions
    = map snd . filter ((==) "collapse" . fst) . citOptions . citation

collapseNumber :: CitationGroup -> CitationGroup
collapseNumber (CG _ f d os) = mapCitationGroup process $ CG [] f d os
-- note:  for numerical styles, we treat author-in-text citations just
-- like any others.
    where
      hasLocator = or . query hasLocator'
      hasLocator' o
          | OLoc _ _ <- o = [True]
          | otherwise     = [False]
      citNums (OCitNum i _) = [i]
      citNums (Output xs _) = concatMap citNums xs
      citNums _             = []
      numOf  = foldr const 0 . citNums
      process xs = if hasLocator xs
                      then xs
                      else flip concatMap (groupConsecWith numOf xs)
                           $ \ys ->
                              if length ys > 2
                                 then [ Output [
                                            head ys
                                          , OPan [Str "\x2013"]
                                          , last ys
                                          ] emptyFormatting
                                      ]
                                 else ys

groupCites :: [(Cite, Output)] -> [(Cite, Output)]
groupCites []     = []
groupCites (x:xs) = let equal    = filter (hasSameNamesAs x) xs
                        notequal = filter (not . hasSameNamesAs x) xs
                    in  x : equal ++ groupCites notequal
    where
      hasSameNamesAs w y = namesOf (snd w) == namesOf (snd y)
      contribsQ o
          | OContrib _ _ c _ _ <- o = [c]
          | otherwise               = []
      namesOf y = case query contribsQ y of
                       []    -> []
                       (z:_) -> proc rmHashAndGivenNames z

getYearAndSuf :: Output -> Output
getYearAndSuf x
    = case query getOYear x of
        [] -> noOutputError
        x' -> Output x' emptyFormatting
    where
      getOYear o
          | OYear    {} : _ <- o = [head o]
          | OYearSuf {} : _ <- o = [head o]
          | OLoc     {} : _ <- o = [head o]
          | ODel _ : OLoc {} : _ <- o = [head o]
          | OStatus  {} : _ <- o = [head o]
          | otherwise = []

collapseYear :: Style -> Text -> CitationGroup -> CitationGroup
collapseYear s ranged (CG cs f d os) = CG cs f "" (process os)
    where
      styleYSD    = getOptionVal "year-suffix-delimiter"    . citOptions . citation $ s
      yearSufDel  = if T.null styleYSD
                    then layDelim . citLayout . citation $ s
                    else styleYSD
      afterCD     = getOptionVal "after-collapse-delimiter" . citOptions . citation $ s
      afterColDel = if T.null afterCD then d else afterCD

      format []     = []
      format (x:xs) = x : map getYearAndSuf xs

      isRanged = case ranged of
                   "year-suffix-ranged" -> True
                   _                    -> False

      collapseRange = if T.null ranged then map (uncurry addCiteAffixes)
                      else collapseYearSuf isRanged yearSufDel

      rmAffixes x = x {citePrefix = mempty, citeSuffix = mempty}
      delim = let d' = getOptionVal "cite-group-delimiter" . citOptions . citation $ s
              -- FIXME: see https://bitbucket.org/bdarcus/citeproc-test/issue/15
              -- in  if null d' then if null d then ", " else d else d'
              in  if T.null d' then ", " else d'

      collapsYS a = case a of
                      []  -> (emptyCite, ONull)
                      [x] -> rmAffixes . fst &&& uncurry addCiteAffixes $ x
                      _   -> (,) (rmAffixes $ fst $ head a) . flip Output emptyFormatting .
                             addDelim delim . collapseRange .
                             uncurry zip . second format . unzip $ a

      doCollapse []     = []
      doCollapse [x] = [collapsYS x]
      doCollapse (x:xs) = let (a,b) = collapsYS x
                          in if length x > 1
                             then (a, Output (b : [ODel afterColDel]) emptyFormatting) : doCollapse xs
                             else (a, Output (b : [ODel d          ]) emptyFormatting) : doCollapse xs

      contribsQ o
          | OContrib _ _ c _ _ <- o = [proc' rmHashAndGivenNames c]
          | otherwise               = []
      namesOf = query contribsQ
      hasSameNames a b = not (null (namesOf (snd a))) &&
                         namesOf (snd a) == namesOf (snd b)
      process = doCollapse . groupBy hasSameNames . groupCites

collapseYearSuf :: Bool -> Text -> [(Cite,Output)] -> [Output]
collapseYearSuf ranged ysd = process
    where
      yearOf  = T.concat . query getYear
      getYear o
          | OYear y _ _ <- o = [y]
          | otherwise        = []

      processYS = if ranged then collapseYearSufRanged else id
      process = map (flip Output emptyFormatting . getYS) . groupBy comp

      checkAffix (Formatted  []) = True
      checkAffix _               = False

      comp a b = yearOf (snd a) == yearOf (snd b) &&
                 checkAffix (citePrefix $ fst a) &&
                 checkAffix (citeSuffix $ fst a) &&
                 checkAffix (citePrefix $ fst b) &&
                 checkAffix (citeSuffix $ fst b) &&
                 T.null (citeLocator $ fst a) &&
                 T.null (citeLocator $ fst b)

      getYS []     = []
      getYS [x] = return $ uncurry addCiteAffixes x
      getYS (x:xs) = if ranged
                     then proc rmOYearSuf (snd x) : addDelim ysd (processYS $ snd x : query rmOYear (map snd xs))
                     else addDelim ysd  $ snd x : processYS (query rmOYear (map snd xs))
      rmOYearSuf o
          | OYearSuf {} <- o = ONull
          | otherwise        = o
      rmOYear o
          | OYearSuf {} <- o = [o]
          | otherwise        = []

collapseYearSufRanged :: [Output] -> [Output]
collapseYearSufRanged = process
    where
      getOYS o
          | OYearSuf s _ _ f <- o = [(if s /= "" then ord (T.head s) else 0, f)]
          | otherwise             = []
      sufOf   = foldr const (0,emptyFormatting) . query getOYS
      newSuf  = map sufOf >>> (map fst >>> groupConsec) &&& map snd >>> uncurry zip
      process xs = flip concatMap (newSuf xs) $
                   \(x,f) -> if length x > 2
                             then return $ Output [ OStr (T.singleton . chr $ head x) f
                                                  , OPan [Str "\x2013"]
                                                  , OStr (T.singleton . chr $ last x) f
                                                  ] emptyFormatting
                             else map (\y -> if y == 0 then ONull else flip OStr f . T.singleton . chr $ y) x

addCiteAffixes :: Cite -> Output -> Output
addCiteAffixes c x =
  if isNumStyle [x]
      then x
      else Output ( addCiteAff True (citePrefix c) ++ [x] ++
                    addCiteAff False (citeSuffix c)) emptyFormatting
  where
      addCiteAff isprefix y =
          case y of
            Formatted  []    -> []
            Formatted ils
              | isprefix  -> case reverse ils of
                                  (Str zs@(T.uncons -> Just (_,_)):_) |
                                    T.last zs == '\160' -> [OPan ils]
                                  _ -> [OPan ils, OSpace]
              | otherwise -> case ils of
                                  (Str (T.uncons -> Just (z,_)):_)
                                    | isAlphaNum z ||
                                      z == '(' -> [OSpace, OPan ils]
                                  _            -> [OPan ils]


isNumStyle :: [Output] -> Bool
isNumStyle = getAny . query ocitnum
    where
      ocitnum OCitNum {} = Any True
      ocitnum _            = Any False

-- | Group consecutive integers:
--
-- > groupConsec [1,2,3,5,6,8,9] == [[1,2,3],[5,6],[8,9]]
groupConsec :: [Int] -> [[Int]]
groupConsec = groupConsecWith id

groupConsecWith ::  (a -> Int) -> [a] -> [[a]]
groupConsecWith f = foldr go [] . sortBy (comparing f)
  where go x []     = [[x]]
        go x ((y:ys):gs) = if (f x + 1) == f y
                              then (x:y:ys):gs
                              else [x]:(y:ys):gs
        go _ ([]:_) = error "groupConsec: head of list is empty"
