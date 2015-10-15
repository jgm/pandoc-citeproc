{-# LANGUAGE PatternGuards, OverloadedStrings #-}
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
import Data.Char ( toLower, isLetter, isDigit )
import Data.List
import Data.Ord  ( comparing )
import Data.Maybe ( mapMaybe )
import Text.CSL.Eval
import Text.CSL.Util ( proc, proc', query, uncamelize, tr' )
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
      , linkCitations :: Bool
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
procOpts = ProcOpts
      { bibOpts = Select [] []
      , linkCitations = False
      }

-- | With a 'Style', a list of 'Reference's and the list of citation
-- groups (the list of citations with their locator), produce the
-- 'Formatted' for each citation group.
processCitations :: ProcOpts -> Style -> [Reference] -> Citations -> [Formatted]
processCitations ops s rs
    = citations . citeproc ops s rs

-- | With a 'Style' and the list of 'Reference's produce the
-- 'Formatted' for the bibliography.
processBibliography :: ProcOpts -> Style -> [Reference] -> [Formatted]
processBibliography ops s rs
    = bibliography $ citeproc ops s rs [map (\r -> emptyCite { citeId = unLiteral $ refId r}) rs]

-- | With a 'Style', a list of 'Reference's and the list of
-- 'Citations', produce the 'Formatted' for each citation group
-- and the bibliography.
citeproc :: ProcOpts -> Style -> [Reference] -> Citations -> BiblioData
citeproc ops s rs cs
    = BD citsOutput biblioOutput $ map (unLiteral . refId) biblioRefs
    where
      -- the list of bib entries, as a list of Reference, with
      -- position, locator and year suffix set.
      biblioRefs   = procRefs s . mapMaybe (getReference rs) .
                     nubBy (\a b -> citeId a == citeId b) . concat $ cs
      biblioOutput = if "disambiguate-add-year-suffix" `elem` getCitDisambOptions s
                     then map (formatOutputList .
                               proc (updateYearSuffixes yearS) . map addYearSuffix) $
                          procBiblio (bibOpts ops) s biblioRefs
                     else map formatOutputList $
                          tr' "citeproc:after procBiblio" $
                          procBiblio (bibOpts ops) s biblioRefs
      citsAndRefs  = processCites biblioRefs cs
      (yearS,citG) = disambCitations s biblioRefs cs $ map (procGroup s) citsAndRefs
      citsOutput   = map (formatCitLayout s) .
                     tr' "citeproc:collapsed" .
                     collapseCitGroups s .
                     (if linkCitations ops && styleClass s == "in-text"
                         then proc addLink
                         else id) .
                     tr' "citeproc:citG" $
                     citG
      addLink :: (Cite, Output) -> (Cite, Output)
      addLink (cit, outp) = (cit, proc (addLink' (citeId cit)) outp)
      addLink' citeid (OYear y _ f) =
         OYear y citeid f{hyperlink = "#ref-" ++ citeid}
      addLink' citeid (OYearSuf y _ d f) =
         OYearSuf y citeid d f{hyperlink = "#ref-" ++ citeid}
      addLink' citeid (OCitNum n f) =
         OCitNum n f{hyperlink = "#ref-" ++ citeid}
      addLink' _ x = x

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
        _  -> if any ((<) 1 . length) result
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
    = map addSpaceAfterCitNum $ maybe [] process mb
    where
      -- handle second-field-align (sort of)
      addSpaceAfterCitNum [Output (OCitNum n f : xs) f']
        | secondFieldAlign == Just "flush"  =
            [Output (OCitNum n f : OSpace : xs) f']
        | secondFieldAlign == Just "margin" =
            [Output (OCitNum n f : OSpace : xs) f']
        | otherwise = [Output (OCitNum n f : xs) f']
      addSpaceAfterCitNum xs = xs

      secondFieldAlign = lookup "second-field-align" $ maybe [] bibOptions mb

      process :: Bibliography -> [[Output]]
      process b   = map (formatBiblioLayout (layFormat $ bibLayout b) (layDelim $ bibLayout b)) $ render b

      render :: Bibliography -> [[Output]]
      render  b   = subsequentAuthorSubstitute b . map (evalBib b) . filterRefs bos $ rs

      evalBib :: Bibliography -> Reference -> [Output]
      evalBib b = evalLayout (bibLayout b) (EvalBiblio emptyCite {citePosition = "first"}) False l ms
                             (mergeOptions (bibOptions b) opts) as

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

      getPartialEach x xs = concat . take 1 . map fst .
                            sortBy (flip (comparing $ length . snd)) . filter ((<) 0 . length . snd) .
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
                       Just x | Just v' <- (fromValue x :: Maybe RefType  ) -> v == uncamelize (show v')
                              | Just v' <- (fromValue x :: Maybe String   ) -> v  == v'
                              | Just v' <- (fromValue x :: Maybe [String] ) -> v `elem` v'
                              | Just v' <- (fromValue x :: Maybe [Agent]  ) -> null v && null v' || v == show v'
                              | Just v' <- (fromValue x :: Maybe [RefDate]) -> null v && null v' || v == show v'
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
                                then (filter (eqCites (/=) c) result,
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

formatCitLayout :: Style -> CitationGroup -> Formatted
formatCitLayout s (CG co f d cs)
    | [a] <- co = combine (formatAuth a)
                  (formatCits $
                   (fst >>> citeId &&& citeHash >>> setAsSupAu $ a) cs)
    | otherwise = formatCits cs
    where
      isNote    = styleClass s == "note"
      toNote (Formatted xs) = Formatted [Note [Para xs]]
      combine (Formatted []) ys = ys
      combine xs ys =
        case ys of
             Formatted [] -> xs
             Formatted (Note _ : _) -> xs <> ys
             Formatted (Str [c]:_) | c `elem` (", ;:" :: String) -> xs <> ys
             _ -> xs <> Formatted [Space] <> ys
      formatAuth   = formatOutput . localMod
      formatCits   = (if isNote then toNote else id) .
                     formatOutputList . appendOutput formatting . addAffixes f .
                     addDelim d .
                     map (fst &&& localMod >>> uncurry addCiteAffixes)
      formatting   = f{ prefix = [], suffix = [],
                        verticalAlign = if isAuthorInText cs
                                           then ""
                                           else verticalAlign f }
      isAuthorInText [] = False
      isAuthorInText ((c,_):_) = authorInText c
      localMod     = uncurry $ localModifiers s (not $ null co)
      setAsSupAu h = map $ \(c,o) -> if (citeId c, citeHash c) == h
                                     then (c { authorInText   = False
                                             , suppressAuthor = True }, o)
                                     else (c, o)

addAffixes :: Formatting -> [Output] -> [Output]
addAffixes f os
    | []      <- os            = []
    | [ONull] <- os            = []
    | [Output [ONull] _] <- os = []
    | otherwise                = pref ++ suff
    where
      pref = if not (null (prefix f))
             then OStr (prefix f) emptyFormatting : os
             else os
      suff = case suffix f of
                  []     -> []
                  (c:cs)
                    | isLetter c || isDigit c || c == '(' || c == '[' ->
                         [OSpace, OStr (c:cs) emptyFormatting]
                    | otherwise -> [OStr (c:cs) emptyFormatting]

-- | The 'Bool' is 'True' if we are formatting a textual citation (in
-- pandoc terminology).
localModifiers :: Style -> Bool -> Cite -> Output -> Output
localModifiers s b c
    | authorInText   c = check . return . contribOnly s
    | suppressAuthor c = check . rmContrib . return
    | otherwise        = id
    where
      isPunct' [] = False
      isPunct' xs = all (`elem` (".,;:!? " :: String)) xs
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
    , OCitNum n f <- o = Output [ OCitNum n f{
                                       verticalAlign = "",
                                       prefix = "",
                                       suffix = "" } ] emptyFormatting
    | OContrib _ "author"
            _ _ _ <- o = o
    | OContrib _ "authorsub"
            _ _ _ <- o = o
    | Output ot f <- o = Output (cleanOutput $ map (contribOnly s) ot)
                         f{ verticalAlign = "",
                            prefix = "",
                            suffix = "" }
    | OStr    x _ <- o
    , "ibid" <- filter (/= '.')
       (map toLower x) = o
    | otherwise        = ONull
