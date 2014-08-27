{-# LANGUAGE PatternGuards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.CSL.Proc.Disamb
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Andrea Rossato <andrea.rossato@unitn.it>
-- Stability   :  unstable
-- Portability :  unportable
--
-- This module provides functions for processing the evaluated
-- 'Output' for citation disambiguation.
--
-- Describe the disambiguation process.
--
-----------------------------------------------------------------------------

module Text.CSL.Proc.Disamb where

import Control.Arrow ( (&&&), (>>>), second )
import Data.Char ( chr )
import Data.List ( elemIndex, find, findIndex, sortBy, mapAccumL
                 , nub, nubBy, groupBy, isPrefixOf )
import Data.Maybe
import Data.Ord ( comparing )

import Text.CSL.Eval
import Text.CSL.Reference
import Text.CSL.Style
import Text.CSL.Util (query, proc)

-- | Given the 'Style', the list of references and the citation
-- groups, disambiguate citations according to the style options.
disambCitations :: Style -> [Reference] -> Citations -> [CitationGroup]
                -> ([(String, String)], [CitationGroup])
disambCitations s bibs cs groups
   = (,) yearSuffs citOutput
    where
      -- utils
      when_ b f = if b then f else []
      filter_ f = concatMap (map fst) . map (filter f) . map (uncurry zip)

      -- the list of the position and the reference of each citation
      -- for each citation group.
      refs   = processCites bibs cs
      -- name data of name duplicates
      nameDupls = getDuplNameData groups
      -- citation data of ambiguous cites
      duplics   = getDuplCiteData hasNamesOpt hasYSuffOpt groups

      -- check the options set in the style
      isByCite = let gno = getOptionVal "givenname-disambiguation-rule" (citOptions $ citation s)
                 in  gno == "by-cite" || gno == []
      disOpts     = getCitDisambOptions s
      hasNamesOpt = "disambiguate-add-names"       `elem` disOpts
      hasGNameOpt = "disambiguate-add-givenname"   `elem` disOpts
      hasYSuffOpt = "disambiguate-add-year-suffix" `elem` disOpts
      givenNames = if hasGNameOpt
                   then if isByCite then ByCite else AllNames
                   else NoGiven

      clean     = if hasGNameOpt then id else proc rmHashAndGivenNames
      withNames = flip map duplics $ same . clean .
                  map (if hasNamesOpt then disambData else return . disambYS)

      needNames = filter_ (not . snd) $ zip duplics withNames
      needYSuff = filter_        snd  $ zip duplics withNames

      newNames :: [CiteData]
      newNames  = when_ (hasNamesOpt || hasGNameOpt) $ disambAddNames givenNames $ needNames ++
                  if hasYSuffOpt && givenNames == NoGiven then [] else needYSuff

      newGName :: [NameData]
      newGName  = when_ hasGNameOpt $ concatMap disambAddGivenNames nameDupls

      -- the list of citations that need re-evaluation with the
      -- \"disambiguate\" condition set to 'True'
      reEval      = let chk = if hasYSuffOpt then filter ((==) [] . citYear) else id
                    in  chk needYSuff
      reEvaluated = if or (query hasIfDis s) && not (null reEval)
                    then map (uncurry $ reEvaluate s reEval) $ zip refs groups
                    else groups

      withYearS = addNames $
                  if hasYSuffOpt
                  then map (mapCitationGroup $ setYearSuffCollision hasNamesOpt needYSuff) $ reEvaluated
                  else rmYearSuff $ reEvaluated

      yearSuffs = when_ hasYSuffOpt . generateYearSuffix bibs . concatMap getYearSuffixes $ withYearS

      addNames  = proc (updateContrib givenNames newNames newGName)
      processed = if hasYSuffOpt
                  then proc (updateYearSuffixes yearSuffs) $ withYearS
                  else withYearS

      citOutput = if disOpts /= [] then processed else reEvaluated

mapDisambData :: (Output -> Output) -> CiteData -> CiteData
mapDisambData f (CD k c ys d r s y) = CD k c ys (proc f d) r s y

mapCitationGroup :: ([Output] -> [Output]) -> CitationGroup ->  CitationGroup
mapCitationGroup f (CG cs fm d os) = CG cs fm d (zip (map fst os) . f $ map snd os)

data GiveNameDisambiguation
    = NoGiven
    | ByCite
    | AllNames
    deriving (Show, Eq)

disambAddNames :: GiveNameDisambiguation -> [CiteData] -> [CiteData]
disambAddNames b needName = addLNames
    where
      clean     = if b == NoGiven then proc rmHashAndGivenNames else id
      disSolved = zip needName' . disambiguate . map disambData $ needName'
      needName' = nub' needName []
      addLNames = map (\(c,n) -> c { disambed = if null n then collision c else head n }) disSolved
      nub' []     r = r
      nub' (x:xs) r = case elemIndex (disambData $ clean x) (map (disambData . clean) r) of
                         Nothing -> nub' xs (x:r)
                         Just i  -> let y = r !! i
                                    in nub' xs (y {sameAs = key x : sameAs y} : filter (/= y) r)

disambAddGivenNames :: [NameData] -> [NameData]
disambAddGivenNames needName = addGName
    where
      disSolved = zip needName (disambiguate $ map nameDisambData needName)
      addGName = map (\(c,n) -> c { nameDataSolved = if null n then nameCollision c else head n }) disSolved

updateContrib :: GiveNameDisambiguation -> [CiteData] -> [NameData] -> Output -> Output
updateContrib g c n o
    | OContrib k r s d dd <- o = case filter (key &&& sameAs >>> uncurry (:) >>> elem k) c of
                                  x:_ | clean (disambData x) == clean (d:dd) ->
                                          OContrib k r (map processGNames $ disambed x) [] dd
                                  _ | null c, AllNames <- g -> OContrib k r (map processGNames s) d dd
                                    | otherwise             -> o
    | otherwise = o
    where
      clean         = if g == NoGiven then proc rmHashAndGivenNames else id
      processGNames = if g /= NoGiven then updateOName n else id

updateOName :: [NameData] -> Output -> Output
updateOName n o
    | OName _ _ [] _ <- o = o
    | OName k x _  f <- o = case elemIndex (ND k (clean x) [] []) n of
                              Just i -> OName emptyAgent (nameDataSolved $ n !! i) [] f
                              _      -> o
    | otherwise           = o
    where
      clean = proc rmGivenNames

-- | Evaluate again a citation group with the 'EvalState' 'disamb'
-- field set to 'True' (for matching the @\"disambiguate\"@
-- condition).
reEvaluate :: Style -> [CiteData] -> [(Cite, Reference)] -> CitationGroup -> CitationGroup
reEvaluate (Style {citation = ct, csMacros = ms , styleLocale = lo,
                   styleAbbrevs = as}) l cr (CG a f d os)
    = CG a f d . flip concatMap (zip cr os) $
      \((c,r),out) -> if unLiteral (refId r) `elem` map key l
                      then return . second (flip Output emptyFormatting) $
                           (,) c $ evalLayout (citLayout ct) (EvalCite c) True lo ms (citOptions ct) as r
                      else [out]

-- | Check if the 'Style' has any conditional for disambiguation. In
-- this case the conditional will be try after all other
-- disambiguation strategies have failed. To be used with the generic
-- 'query' function.
hasIfDis :: IfThen -> [Bool]
hasIfDis (IfThen (Condition {disambiguation = (_:_)}) _ _) = [True]
hasIfDis _                                                 = [False]

-- | Get the list of disambiguation options set in the 'Style' for
-- citations.
getCitDisambOptions :: Style -> [String]
getCitDisambOptions
    = map fst . filter ((==) "true" . snd) .
      filter (isPrefixOf "disambiguate" . fst) . citOptions . citation

-- | Group citation data (with possible alternative names) of
-- citations which have a duplicate (same 'collision', and same
-- 'citYear' if year suffix disambiiguation is used). If the first
-- 'Bool' is 'False', then we need to retrieve data for year suffix
-- disambiguation. The second 'Bool' is 'True' when comparing both
-- year and contributors' names for finding duplicates (when the
-- year-suffix option is set).
getDuplCiteData :: Bool -> Bool -> [CitationGroup] -> [[CiteData]]
getDuplCiteData b1 b2 g
    = groupBy (\x y -> collide x == collide y) . sortBy (comparing collide)
      $ duplicates
    where
      whatToGet  = if b1 then collision else disambYS
      collide    = proc rmExtras . proc rmHashAndGivenNames . whatToGet
      citeData   = nubBy (\a b -> collide a == collide b && key a == key b) $
                   concatMap (mapGroupOutput $ getCiteData) g
      duplicates = [c | c <- citeData , d <- citeData , collides c d]
      collides x y = x /= y && (collide x == collide y)
                            && (not b2 || citYear x == citYear y)

rmExtras :: [Output] -> [Output]
rmExtras os
    | Output         x _ : xs <- os = case rmExtras x of
                                           [] -> rmExtras xs
                                           ys -> ys ++ rmExtras xs
    | OContrib _ _ x _ _ : xs <- os = OContrib [] [] x [] [] : rmExtras xs
    | OYear        y _ f : xs <- os = OYear y [] f : rmExtras xs
    | ODel             _ : xs <- os = rmExtras xs
    | OLoc           _ _ : xs <- os = rmExtras xs
    | x                  : xs <- os = x : rmExtras xs
    | otherwise                     = []

-- | For an evaluated citation get its 'CiteData'. The disambiguated
-- citation and the year fields are empty. Only the first list of
-- contributors' disambiguation data are collected for disambiguation
-- purposes.
getCiteData :: Output -> [CiteData]
getCiteData out
    = (contribs &&& years >>> zipData) out
    where
      contribs x = case query contribsQ x of
                        [] -> [CD [] [out] [] [] [] [] []]
                              -- allow title to disambiguate
                        xs -> xs
      years o = case query getYears o of
                     []    -> [([],[])]
                     r     -> r
      zipData = uncurry . zipWith $ \c y -> if key c /= []
                                            then c {citYear = snd y}
                                            else c {key     = fst y
                                                   ,citYear = snd y}
      contribsQ o
          | OContrib k _ _ d dd <- o = [CD k [out] d (d:dd) [] [] []]
          | otherwise                = []

getYears :: Output -> [(String,String)]
getYears o
    | OYear x k _ <- o = [(k,x)]
    | otherwise        = []

getDuplNameData :: [CitationGroup] -> [[NameData]]
getDuplNameData g
    = groupBy (\a b -> collide a == collide b) . sortBy (comparing collide) $ duplicates
    where
      collide    = nameCollision
      nameData   = nub $ concatMap (mapGroupOutput getName) g
      duplicates = filter (flip elem (getDuplNames g) . collide) nameData

getDuplNames :: [CitationGroup] -> [[Output]]
getDuplNames xs
    = nub . catMaybes . snd . mapAccumL dupl [] . getData $ xs
    where
      getData = concatMap (mapGroupOutput getName)
      dupl a c = if nameCollision c `elem` map nameCollision a
                 then (a,Just $ nameCollision c)
                 else (c:a,Nothing)

getName :: Output -> [NameData]
getName = query getName'
    where
      getName' o
          | OName i n ns _ <- o = [ND i n (n:ns) []]
          | otherwise           = []

generateYearSuffix :: [Reference] -> [(String, [Output])] -> [(String,String)]
generateYearSuffix refs
    = concatMap (flip zip suffs) .
      -- sort clashing cites using their position in the sorted bibliography
      getFst . map sort' . map (filter ((/=) 0 . snd)) . map (map getP) .
      -- group clashing cites
      getFst . filter (\grp -> length grp >= 2) . map nub . groupBy (\a b -> snd a == snd b) . sort' . filter ((/=) [] . snd)
    where
      sort'  :: (Ord a, Ord b) => [(a,b)] -> [(a,b)]
      sort'  = sortBy (comparing snd)
      getFst = map $ map fst
      getP k = case findIndex ((==) k . unLiteral . refId) refs of
                   Just x -> (k, x + 1)
                   _      -> (k,     0)
      suffs = l ++ [x ++ y | x <- l, y <- l ]
      l = map (return . chr) [97..122]

setYearSuffCollision :: Bool -> [CiteData] -> [Output] -> [Output]
setYearSuffCollision b cs = proc (setYS cs) . (map $ \x -> if hasYearSuf x then x else addYearSuffix x)
    where
      setYS c o
          | OYearSuf _ k _ f <- o = OYearSuf [] k (getCollision k c) f
          | otherwise             = o
      collide = if b then disambed else disambYS
      getCollision k c = case find ((==) k . key) c of
                           Just x -> if collide x == []
                                     then [OStr (citYear x) emptyFormatting]
                                     else collide x
                           _      -> []

updateYearSuffixes :: [(String, String)] -> Output -> Output
updateYearSuffixes yss o
 | OYearSuf _ k c f <- o = case lookup k yss of
                             Just x -> OYearSuf x k c f
                             _      -> ONull
 | otherwise             = o

getYearSuffixes :: CitationGroup -> [(String,[Output])]
getYearSuffixes (CG _ _ _ d) = map go d
  where go (c,x) = (citeId c, relevant False [x])
        relevant :: Bool -> [Output] -> [Output] -- bool is true if has contrib
        -- we're only interested in OContrib and OYear, unless there is no OContrib
        relevant c (Output xs _ : rest) = relevant c xs ++ relevant c rest
        relevant c (OYear n _ _ : rest) = OStr n emptyFormatting : relevant c rest
        relevant False (OStr s _    : rest) = OStr s emptyFormatting : relevant False rest
        relevant False (OSpace      : rest) = OSpace : relevant False rest
        relevant False (OPan ils    : rest) = OPan ils : relevant False rest
        relevant _ (OContrib _ _ v _ _ : rest ) = relevant False v ++ relevant True rest
        relevant c (OName _ v _ _ : rest ) = relevant c v ++ relevant c rest
        relevant c (_           : rest) = relevant c rest
        relevant _ []                   = []

rmYearSuff :: [CitationGroup] -> [CitationGroup]
rmYearSuff = proc rmYS
    where
      rmYS o
          | OYearSuf _ _ _ _  <- o = ONull
          | otherwise              = o

-- List Utilities

-- | Try to disambiguate a list of lists by returning the first non
-- colliding element, if any, of each list:
--
-- > disambiguate [[1,2],[1,3],[2]] = [[2],[3],[2]]
disambiguate :: (Eq a) => [[a]] -> [[a]]
disambiguate [] = []
disambiguate l
    = if  hasMult l && not (allTheSame l) && hasDuplicates heads
      then disambiguate (rest l)
      else heads
    where
      heads = map (take 1) l
      rest  = map (\(b,x) -> if b then tail_ x else take 1 x) . zip (same heads)

      hasMult []     = False
      hasMult (x:xs) = length x > 1 || hasMult xs

      tail_ [x] = [x]
      tail_  x  = if null x then x else tail x

-- | For each element a list of 'Bool': 'True' if the element has a
-- duplicate in the list:
--
-- > same [1,2,1] = [True,False,True]
same :: Eq a => [a] -> [Bool]
same l = map (`elem` dupl) l
    where
      dupl = catMaybes . snd . macc [] $ l
      macc = mapAccumL $ \a x -> if x `elem` a
                                 then (a,Just x)
                                 else (x:a,Nothing)

hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates = or . same

allTheSame :: Eq a => [a] -> Bool
allTheSame []     = True
allTheSame (x:xs) = all (== x) xs

-- | Add the year suffix to the year. Needed for disambiguation.
addYearSuffix :: Output -> Output
addYearSuffix o
    | OYear y k     f <- o = Output [OYear y k emptyFormatting,OYearSuf [] k [] emptyFormatting] f
    | ODate  (x:xs)   <- o = if or $ map hasYear xs
                             then Output (x : [addYearSuffix $ ODate xs]) emptyFormatting
                             else addYearSuffix (Output (x:xs) emptyFormatting)
    | Output (x:xs) f <- o = if or $ map hasYearSuf (x : xs)
                             then Output (x : xs) f
                             else if hasYear x
                                  then Output (addYearSuffix x : xs) f
                                  else Output (x : [addYearSuffix $ Output xs emptyFormatting]) f
    | otherwise            = o

hasYear :: Output -> Bool
hasYear = not . null . query getYear
    where getYear o
              | OYear _ _ _ <- o = [o]
              | otherwise        = []


hasYearSuf :: Output -> Bool
hasYearSuf = not . null . query getYearSuf
    where getYearSuf :: Output -> [String]
          getYearSuf o
              | OYearSuf _ _ _ _ <- o = ["a"]
              | otherwise             = []

-- | Removes all given names and name hashes from OName elements.
rmHashAndGivenNames :: Output -> Output
rmHashAndGivenNames (OName _ s _ f) = OName emptyAgent s [] f
rmHashAndGivenNames o = o

rmGivenNames :: Output -> Output
rmGivenNames (OName a s _ f) = OName a s [] f
rmGivenNames o = o

-- | Add, with 'proc', a give name to the family name. Needed for
-- disambiguation.
addGivenNames :: [Output] -> [Output]
addGivenNames
    = addGN True
    where
      addGN _ [] = []
      addGN b (o:os)
          | OName i _ xs f <- o
          , xs /= []  = if b then OName i (head xs) (tail xs) f : addGN False os else o:os
          | otherwise = o : addGN b os

-- | Map the evaluated output of a citation group.
mapGroupOutput :: (Output -> [a]) -> CitationGroup -> [a]
mapGroupOutput f (CG _ _ _ os) = concatMap f $ map snd os
