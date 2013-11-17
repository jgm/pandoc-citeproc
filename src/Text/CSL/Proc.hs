{-# LANGUAGE PatternGuards, DeriveDataTypeable, OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.CSL.Proc
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Andrea Rossato <andrea.rossato@unitn.it>
-- Stability   :  unstable
-- Portability :  unportable
--
-- This module provides functions for processing the evaluated
-- 'Output' for disambiguation and citation collapsing.
--
-----------------------------------------------------------------------------

module Text.CSL.Proc where

import Control.Arrow ( (&&&), (>>>), second )
import Data.Char ( toLower )
import Data.List
import Data.Ord  ( comparing )
import Data.Maybe ( mapMaybe )
import Text.CSL.Eval
import Text.CSL.Util ( capitalize, proc, proc', query, toShow, last' )
import Text.CSL.Output.Plain
import Text.CSL.Proc.Collapse
import Text.CSL.Proc.Disamb
import Text.CSL.Reference
import Text.CSL.Style
import Data.Aeson
import Control.Applicative ((<|>))
import Text.Pandoc.Definition (Inline(Space, Str, Note), Block(Para))

data ProcOpts
    = ProcOpts
      { bibOpts :: BibOpts
      }
    deriving ( Show, Read, Eq )

data BibOpts
    = Select  [(String, String)] [(String, String)]
    | Include [(String, String)] [(String, String)]
    | Exclude [(String, String)] [(String, String)]
    deriving ( Show, Read, Eq )

newtype FieldVal = FieldVal{
                      unFieldVal :: (String, String)
                    } deriving Show

instance FromJSON FieldVal where
  parseJSON (Object v) = do
    x <- v .: "field"
    y <- v .: "value"
    return $ FieldVal (x,y)
  parseJSON _ = fail "Could not parse FieldVal"

instance FromJSON BibOpts where
  parseJSON (Object v) = do
    quash <- v .:? "quash".!= []
    let quash' = map unFieldVal quash
    (v .: "select" >>= \x -> return $ Select (map unFieldVal x) quash')
     <|>
     (v .: "include" >>= \x -> return $ Include (map unFieldVal x) quash')
     <|>
     (v .: "exclude" >>= \x -> return $ Exclude (map unFieldVal x) quash')
     <|>
     return (Select [] quash')
  parseJSON _ = return $ Select [] []

procOpts :: ProcOpts
procOpts = ProcOpts (Select [] [])

-- | With a 'Style', a list of 'Reference's and the list of citation
-- groups (the list of citations with their locator), produce the
-- 'FormattedOutput' for each citation group.
processCitations :: ProcOpts -> Style -> [Reference] -> Citations -> [FormattedOutput]
processCitations ops s rs
    = citations . citeproc ops s rs

-- | With a 'Style' and the list of 'Reference's produce the
-- 'FormattedOutput' for the bibliography.
processBibliography :: ProcOpts -> Style -> [Reference] -> [FormattedOutput]
processBibliography ops s rs
    = bibliography $ citeproc ops s rs [map (\r -> emptyCite { citeId = unLiteral $ refId r}) rs]

-- | With a 'Style', a list of 'Reference's and the list of
-- 'Citations', produce the 'FormattedOutput' for each citation group
-- and the bibliography.
citeproc :: ProcOpts -> Style -> [Reference] -> Citations -> BiblioData
citeproc ops s rs cs
    = BD citsOutput biblioOutput
    where
      -- the list of bib entries, as a list of Reference, with
      -- position, locator and year suffix set.
      biblioRefs   = procRefs s . mapMaybe (getReference rs) .
                     nubBy (\a b -> citeId a == citeId b) . concat $ cs
      biblioOutput = if "disambiguate-add-year-suffix" `elem` getCitDisambOptions s
                     then map formatOutputList $
                          map (proc (updateYearSuffixes yearS) . map addYearSuffix) $
                          procBiblio (bibOpts ops) s biblioRefs
                     else map formatOutputList $
                          procBiblio (bibOpts ops) s biblioRefs
      citsAndRefs  = processCites biblioRefs cs
      (yearS,citG) = disambCitations s biblioRefs cs $ map (procGroup s) citsAndRefs
      citsOutput   = map (formatCitLayout s) . collapseCitGroups s $ citG

-- | Given the CSL 'Style' and the list of 'Reference's sort the list
-- according to the 'Style' and assign the citation number to each
-- 'Reference'.
procRefs :: Style -> [Reference] -> [Reference]
procRefs (Style {biblio = mb, csMacros = ms , styleLocale = l, styleAbbrevs = as, csOptions = opts}) rs
    = maybe (setCNum rs) process mb
    where
      opts'   b = mergeOptions (bibOptions b) opts
      setCNum   = map (\(x,y) -> x { citationNumber = fromIntegral y }) . flip zip ([1..] :: [Int])
      sort_   b = evalSorting (EvalSorting emptyCite {citePosition = "first"}) l ms (opts' b) (bibSort b) as
      process b = setCNum . sortItems . map (id &&& sort_ b) $ rs

sortItems :: Show a => [(a,[Sorting])] -> [a]
sortItems [] = []
sortItems l
    = case head . concatMap (map snd) $ result of
        [] -> concatMap (map fst) result
        _  -> if or $ map ((<) 1 . length) result
              then concatMap sortItems result
              else concatMap (map fst) result
    where
      result = process l
      process = sortBy (comparing $ take 1 . snd)                 >>>
                groupBy (\a b -> take 1 (snd a) == take 1 (snd b)) >>>
                map (map $ second (drop 1))

-- | With a 'Style' and a sorted list of 'Reference's produce the
-- evaluated output for the bibliography.
procBiblio :: BibOpts -> Style -> [Reference] -> [[Output]]
procBiblio bos (Style {biblio = mb, csMacros = ms , styleLocale = l,
                       styleAbbrevs = as, csOptions = opts}) rs
    = maybe [] process mb
    where
      process b   = map (formatBiblioLayout (layFormat $ bibLayout b) (layDelim $ bibLayout b)) $ render b
      render  b   = subsequentAuthorSubstitute b . map (evalBib b) . filterRefs bos $ rs
      evalBib b r = evalLayout (bibLayout b) (EvalBiblio emptyCite {citePosition = "first"}) False l ms
                               (mergeOptions (bibOptions b) opts) as r

subsequentAuthorSubstitute :: Bibliography -> [[Output]] -> [[Output]]
subsequentAuthorSubstitute b = if null subAuthStr then id else chkCreator
    where
      subAuthStr  = getOptionVal "subsequent-author-substitute"      (bibOptions b)
      subAuthRule = getOptionVal "subsequent-author-substitute-rule" (bibOptions b)

      queryContrib = proc' rmLabel . query contribsQ
      getContrib = if null subAuthStr
                   then const []
                   else case subAuthRule of
                          "partial-first" -> take 1  . query namesQ  . queryContrib
                          "partial-each"  ->          query namesQ  . queryContrib
                          _               ->                          queryContrib

      getPartialEach x xs = concat . take 1 . map fst . reverse .
                            sortBy (comparing $ length . snd) . filter ((<) 0 . length . snd) .
                            zip xs . map (takeWhile id . map (uncurry (==)) . zip x) $ xs

      chkCreator = if subAuthRule == "partial-each" then chPartialEach [] else chkCr []

      chkCr _ []     = []
      chkCr a (x:xs) = let contribs = getContrib x in
                       if  contribs `elem` a
                       then substituteAuth []
                            x : chkCr             a  xs
                       else x : chkCr (contribs : a) xs

      chPartialEach _ [] = []
      chPartialEach a (x:xs) = let contribs = getContrib x
                                   partial  = getPartialEach contribs a in
                               if not $ null partial
                               then substituteAuth partial x :
                                    if length partial < length contribs
                                    then chPartialEach (contribs : a) xs
                                    else chPartialEach             a  xs
                               else x  : chPartialEach (contribs : a) xs

      substituteAuth a = if subAuthRule == "complete-each"
                         then proc chNamas else proc (updateContribs a)

      updateContribs a o@(OContrib i r y ds os)
          = if r == "author" || r == "authorsub" then OContrib i r upCont ds os else o
          where
            upCont = case subAuthRule of
                       "partial-first" -> rmFirstName      y
                       "partial-each"  -> rmSelectedName a y
                       _               -> OStr subAuthStr emptyFormatting : proc rmNames y
      updateContribs _ o = o

      contribsQ o
          | OContrib _ r c _ _ <- o = if r == "author" || r == "authorsub" then c else []
          | otherwise               = []
      namesQ o
          | OName {} <- o = [o]
          | otherwise     = []
      rmSelectedName _ [] = []
      rmSelectedName a (o:os)
          | OName {} <- o = (if o `elem` a then OStr subAuthStr emptyFormatting else o) : rmSelectedName a os
          | otherwise     = o : rmSelectedName a os
      rmFirstName [] = []
      rmFirstName (o:os)
          | OName {} <- o = OStr subAuthStr emptyFormatting : os
          | otherwise     = o : rmFirstName os
      chNamas o
          | OName s _ os f <- o = OName s [OStr subAuthStr emptyFormatting] os f
          | otherwise           = o
      rmNames o
          | OName {} <- o = ONull
          | OStr  {} <- o = ONull
          | ODel  {} <- o = ONull
          | otherwise     = o
      rmLabel [] = []
      rmLabel (o:os)
          | OLabel {} <- o =     rmLabel os
          | otherwise      = o : rmLabel os

filterRefs :: BibOpts -> [Reference] -> [Reference]
filterRefs bos refs
    | Select  s q <- bos = filter (select  s) . filter (quash q) $ refs
    | Include i q <- bos = filter (include i) . filter (quash q) $ refs
    | Exclude e q <- bos = filter (exclude e) . filter (quash q) $ refs
    | otherwise          = refs
    where
      quash  [] _ = True
      quash   q r = not . and . flip map q $ \(f,v) ->       lookup_ r f v
      select  s r =       and . flip map s $ \(f,v) ->       lookup_ r f v
      include i r =       or  . flip map i $ \(f,v) ->       lookup_ r f v
      exclude e r =       and . flip map e $ \(f,v) -> not $ lookup_ r f v
      lookup_ r f v = case f of
                        "type"         -> look "ref-type"
                        "id"           -> look "ref-id"
                        "categories"   -> look "categories"
                        x              -> look x
          where
            look s = case lookup s (mkRefMap r) of
                       Just x | Just v' <- (fromValue x :: Maybe RefType  ) -> v == toShow (show v')
                              | Just v' <- (fromValue x :: Maybe String   ) -> v  == v'
                              | Just v' <- (fromValue x :: Maybe [String] ) -> v `elem` v'
                              | Just v' <- (fromValue x :: Maybe [Agent]  ) -> v == [] && v' == [] || v == show v'
                              | Just v' <- (fromValue x :: Maybe [RefDate]) -> v == [] && v' == [] || v == show v'
                       _                                                    -> False

-- | Given the CSL 'Style' and the list of 'Cite's coupled with their
-- 'Reference's, generate a 'CitationGroup'. The citations are sorted
-- according to the 'Style'.
procGroup :: Style -> [(Cite, Reference)] -> CitationGroup
procGroup (Style {citation = ct, csMacros = ms , styleLocale = l,
                  styleAbbrevs = as, csOptions = opts}) cr
    = CG authIn (layFormat $ citLayout ct) (layDelim $ citLayout ct) (authIn ++ co)
    where
      (co, authIn) = case cr of
                       (c:_) -> if authorInText (fst c)
                                then (filter (eqCites (/=) c) $ result,
                                      take 1 .  filter (eqCites (==) c) $ result)
                                else (result, [])
                       _     -> (result, [])
      eqCites eq c = fst >>> citeId &&& citeHash >>> eq (citeId &&& citeHash $ fst c)
      opts'        = mergeOptions (citOptions ct) opts
      format (c,r) = (c,  evalLayout (citLayout ct) (EvalCite c) False l ms opts' as r)
      sort_  (c,r) = evalSorting (EvalSorting c) l ms opts' (citSort ct) as r
      process      = map (second (flip Output emptyFormatting) . format &&& sort_)
      result       = sortItems $ process cr

formatBiblioLayout :: Formatting -> Delimiter -> [Output] -> [Output]
formatBiblioLayout  f d = appendOutput f . addDelim d

formatCitLayout :: Style -> CitationGroup -> FormattedOutput
formatCitLayout s (CG co f d cs)
    | [a] <- co = combine (formatAuth a)
                  (formatCits $
                   (fst >>> citeId &&& citeHash >>> setAsSupAu $ a) $ cs)
    | otherwise = formatCits cs
    where
      isNote    = styleClass s == "note"
      toNote xs = [Note [Para xs]]
      combine xs [] = xs
      combine [] ys = ys
      combine xs ys@(Note _ : _) = xs ++ ys
      combine xs ys@(Str [c]:_) | c `elem` ", ;:" = xs ++ ys
      combine xs ys = xs ++ (Space : ys)
      formatAuth   = formatOutput . localMod
      formatCits   = (if isNote then toNote else id) .
                     formatOutputList . appendOutput formatting . addAffixes f .
                     addDelim d .
                     map (fst &&& localMod >>> uncurry addCiteAffixes)
      formatting   = unsetAffixes f
      localMod     = uncurry $ localModifiers s (not $ null co)
      setAsSupAu h = map $ \(c,o) -> if (citeId c, citeHash c) == h
                                     then flip (,) o c { authorInText   = False
                                                       , suppressAuthor = True }
                                     else flip (,) o c

addAffixes :: Formatting -> [Output] -> [Output]
addAffixes f os
    | []      <- os = []
    | [ONull] <- os = []
    | otherwise     = pref ++ suff
    where
      pref = if not (null (prefix f))
             then [OStr (prefix f) emptyFormatting] ++ os
             else os
      suff = case suffix f of
                  (c:cs)
                    | c `elem` ",.:?!"
                    , [c] == lastOutput -> [OStr cs emptyFormatting]
                  [] -> []
                  cs  -> [OStr cs emptyFormatting]
      lastOutput = last' $ renderPlain (formatOutputList os)

-- | The 'Bool' is 'True' if we are formatting a textual citation (in
-- pandoc terminology).
localModifiers :: Style -> Bool -> Cite -> Output -> Output
localModifiers s b c
    | authorInText   c = check . return . proc rmFormatting . contribOnly s
    | suppressAuthor c = check . rmContrib . return
    | otherwise        = id
    where
      isPunct' [] = False
      isPunct' xs = all (`elem` ".,;:!? ") xs
      check o = case cleanOutput o of
                  [] -> ONull
                  x  -> case trim' x of
                          [] -> ONull
                          x' -> Output x' emptyFormatting
      hasOutput o
          | Output [] _ <- o = [False]
          | ODel      _ <- o = [False]
          | OSpace      <- o = [False]
          | ONull       <- o = [False]
          | otherwise        = [True]
      trim' [] = []
      trim' (o:os)
          | Output ot f <- o, p <- prefix f,  p /= []
          , isPunct' p        = trim' $ Output ot f { prefix = []} : os
          | Output ot f <- o  = if or (query hasOutput ot)
                                then Output (trim' ot) f : os
                                else Output       ot  f : trim' os
          | ODel _      <- o  = trim' os
          | OSpace      <- o  = trim' os
          | OStr    x f <- o  = OStr x (if isPunct' (prefix f)
                                        then f { prefix = []} else f) : os
          | otherwise         = o:os
      rmFormatting f
          | Formatting {} <- f = emptyFormatting { prefix = prefix f
                                                 , suffix = suffix f}
          | otherwise          = f
      rmCitNum o
          | OCitNum {} <- o = ONull
          | otherwise       = o
      rmContrib [] = []
      rmContrib o
          | b, isNumStyle o = proc rmCitNum o
          | otherwise       = rmContrib' o
      rmContrib' [] = []
      rmContrib' (o:os)
          | Output ot f <- o = Output (rmContrib' ot) f : rmContrib' os
          | ODel _ <- o
          , OContrib _ "author"
                     _ _ _ : xs <- os = rmContrib' xs
          | ODel _ <- o
          , OContrib _ "authorsub"
                     _ _ _ : xs <- os = rmContrib' xs
          | OContrib _ "author" _ _ _ <- o
          , ODel _ : xs <- os =     rmContrib' xs
          | OContrib _ "authorsub" _ _ _ <- o
          , ODel _ : xs <- os =     rmContrib' xs
          | OContrib _ "author"
                  _ _ _ <- o =     rmContrib' os
          | OContrib _ "authorsub"
                  _ _ _ <- o =     rmContrib' os
          | OStr x _ <- o
          , "ibid" <- filter (/= '.') (map toLower x) = rmContrib' os

          | otherwise        = o : rmContrib' os

contribOnly :: Style -> Output -> Output
contribOnly s o
    | isNumStyle [o]
    , OCitNum  {} <- o = Output [ OStr (query getRefTerm s) emptyFormatting
                                , OSpace, o] emptyFormatting
    | OContrib _ "author"
            _ _ _ <- o = o
    | OContrib _ "authorsub"
            _ _ _ <- o = o
    | Output ot f <- o = Output (cleanOutput $ map (contribOnly s) ot) f
    | OStr    x _ <- o
    , "ibid" <- filter (/= '.')
       (map toLower x) = o
    | otherwise        = ONull
    where
      getRefTerm :: CslTerm -> String
      getRefTerm t
          | CT "reference" Long _ _ x _ _ <- t = capitalize x
          | otherwise                          = []
