{-# LANGUAGE PatternGuards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.CSL.Eval.Names
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

module Text.CSL.Eval.Names where

import Control.Applicative ( (<$>) )
import Control.Monad.State
import Data.Char  ( isLower, isUpper, isSpace )
import Data.List  ( nub )
import Data.Maybe ( isJust )
import Data.Monoid

import Text.CSL.Eval.Common
import Text.CSL.Eval.Output
import Text.CSL.Util ( readNum, (<^>), (<+>), query, toRead, capitalize, trim )
import Text.CSL.Reference
import Text.CSL.Style
import Text.Pandoc.Definition

evalNames :: Bool -> [String] -> [Name] -> String -> State EvalState [Output]
evalNames skipEdTrans ns nl d
    | [sa,sb] <- ns, not skipEdTrans
    , sa == "editor" && sb == "translator" ||
      sb == "editor" && sa == "translator" = do
        aa <- getAgents' sa
        ab <- getAgents' sb
        if aa /= [] && aa == ab
           then modify (\s -> s { edtrans = True }) >>
                evalNames True [sa] nl d
           else evalNames True  ns  nl d
    | (s:xs) <- ns = do
        resetEtal
        ags <- getAgents s
        k   <- getStringVar "ref-id"
        p   <- gets (citePosition . cite . env)
        ops <- gets (options . env)
        aus <- gets authSub
        r   <- do res <- agents p            s ags
                  st  <- get
                  fb  <- agents "subsequent" s ags
                  put st
                  if res /= []
                    then let role = if aus == ["author"] then concat aus ++ "sub" else s
                         in  return . return . OContrib k role res fb =<< gets etal
                    else     return []
        r'  <- evalNames skipEdTrans xs nl d
        num <- gets contNum
        return $ if r /= [] && r' /= []
                 then count num (r ++ [ODel $ delim ops] ++ r')
                 else count num $ cleanOutput (r ++ r')
    | otherwise = return []
    where
      agents p s a = concatMapM (formatNames (hasEtAl nl) d p s a) nl
      delim    ops = if d == [] then getOptionVal "names-delimiter" ops else d
      resetEtal    = modify (\s -> s { etal = [] })
      count  num x = if hasCount nl && num /= [] -- FIXME!! le zero!!
                     then [OContrib [] [] [ONum (length num) emptyFormatting] [] []]
                     else x
      hasCount     = or . query hasCount'
      hasCount' n
          | Name Count _ _ _ _  <- n = [True]
          | otherwise                = [False]

-- | The 'Bool' is 'True' when formatting a name with a final "et-al".
-- The first 'String' represents the position and the second the role
-- (e.i. editor, translator, etc.).
formatNames :: Bool -> Delimiter -> String -> String -> [Agent] -> Name -> State EvalState [Output]
formatNames ea del p s as n
    | Name f _ ns _ _ <- n, Count <- f = do
        b <- isBib <$> gets mode
        o <- gets (options . env) >>= return . mergeOptions ns
        modify $ \st -> st { contNum = nub $ (++) (take (snd $ isEtAl b o p as) as) $ contNum st }
        return []

    | Name f fm ns d np <- n = do
        b <- isBib <$> gets mode
        o <- gets (options . env) >>= return . mergeOptions ns
        m <- gets mode
        let odel  = if del /= [] then del else getOptionVal "name-delimiter" o
            del'  = if d   /= [] then d   else if odel == [] then ", " else odel
            (_,i) = isEtAl b o p as
            form  = case f of
                      NotSet -> case getOptionVal "name-form" o of
                                  [] -> Long
                                  x  -> read $ toRead x
                      _      -> f
            genName x = do etal' <- formatEtAl o ea "et-al" fm del' x
                           if etal' == []
                              then do t <- getTerm False Long "and"
                                      return $ delim t o del' $ format m o form fm np x
                              else do return $ (addDelim del' $ format m o form fm np x) ++ etal'
        setLastName o $ formatName m False f fm o np (last as)
        updateEtal =<< mapM genName [1 + i .. length as]
        genName i

    | NameLabel f fm pl <- n = when' (isVarSet s) $ do
        b <- gets edtrans
        res <- formatLabel f fm (isPlural pl $ length as) $ if b then "editortranslator" else s
        modify $ \st -> st { edtrans = False }
        updateEtal [res]
        return res

    | EtAl fm t <- n = do
        o <- gets (options . env)
        if (getOptionVal "et-al-min" o == [])
           then return []
           else do
             et <- gets etal
             let i = length as - length et
                 t' = if null t then "et-al" else t
             r <- mapM (et_al o False t' fm del) [i .. length as]
             let (r',r'') = case r of
                              (x:xs) -> ( x,xs ++ [])
                              _      -> ([],      [])
             updateEtal r''
             return r'

    | otherwise = return []
    where
      isBib (EvalBiblio _) = True
      isBib  _             = False
      updateEtal x = modify $ \st ->
                     let x' = if length x == 1 then repeat $ head x else x
                     in st { etal = if etal st /= []
                                    then map (uncurry (++)) . zip (etal st) $ x'
                                    else x
                           }
      isWithLastName os
          | "true" <-       getOptionVal "et-al-use-last"  os
          , em <- readNum $ getOptionVal "et-al-min"       os
          , uf <- readNum $ getOptionVal "et-al-use-first" os
          , em - uf > 1 = True
          | otherwise   = False
      setLastName os x
          | as /= []
          , isWithLastName os = modify $ \st -> st { lastName = x}
          | otherwise         = return ()

      format m os f fm np i
          | (a:xs) <- take i as  = formatName m True  f fm os np  a ++
                        concatMap (formatName m False f fm os np) xs
          | otherwise = concatMap (formatName m True  f fm os np) . take i $ as
      delim t os d x
          | "always" <- getOptionVal "delimiter-precedes-last" os
          , length x == 2 = addDelim d (init x) ++ ODel (d <^> andStr t os) : [last x]
          | length x == 2 = addDelim d (init x) ++ ODel (andStr'   t d os) : [last x]
          | "never" <- getOptionVal "delimiter-precedes-last" os
          , length x >  2 = addDelim d (init x) ++ ODel (andStr'   t d os) : [last x]
          | length x >  2 = addDelim d (init x) ++ ODel (d <^> andStr t os) : [last x]
          | otherwise     = addDelim d x
      andStr t os
          | "text"   <- getOptionVal "and" os = " " ++ t ++ " "
          | "symbol" <- getOptionVal "and" os = " & "
          | otherwise                          = []
      andStr' t d os = if andStr t os == [] then d else andStr t os

      formatEtAl o b t fm d i = do
        ln <- gets lastName
        if isWithLastName o
           then case () of
                  _ | (length as - i) == 1 -> et_al o b t fm d i -- is that correct? FIXME later
                    | (length as - i) >  1 -> return $ [ODel d, OPan [Str "\x2026"], OSpace] ++ ln
                    | otherwise            -> return []
           else et_al o b t fm d i
      et_al o b t fm d i
          = when' (gets mode >>= return . not . isSorting) $
            if b || length as <= i
            then return []
            else do x <- getTerm False Long t
                    when' (return $ x /= []) $
                          case getOptionVal "delimiter-precedes-et-al" o of
                            "never"  -> return . (++) [OSpace] $ output fm x
                            "always" -> return . (++) [ODel d] $ output fm x
                            _        -> if i > 1
                                        then return . (++) [ODel d] $ output fm x
                                        else return . (++) [OSpace] $ output fm x

-- | The first 'Bool' is 'True' if we are evaluating the bibliography.
-- The 'String' is the cite position. The function also returns the
-- number of contributors to be displayed.
isEtAl :: Bool -> [Option] -> String -> [Agent] -> (Bool, Int)
isEtAl b os p as
    | p /= "first"
    , isOptionSet    "et-al-subsequent-min"       os
    , isOptionSet    "et-al-subsequent-use-first" os
    , le  <- etAlMin "et-al-subsequent-min"
    , le' <- etAlMin "et-al-subsequent-use-first"
    , length as >= le
    , length as >  le' = (,) True le'
    | isOptionSet'    "et-al-min"       "et-al-subsequent-min"
    , isOptionSet'    "et-al-use-first" "et-al-subsequent-use-first"
    , le  <- etAlMin' "et-al-min"       "et-al-subsequent-min"
    , le' <- etAlMin' "et-al-use-first" "et-al-subsequent-use-first"
    , length as >= le
    , length as >  le' = (,) True le'
    | isOptionSet'    "et-al-min"       "et-al-subsequent-min"
    , le  <- etAlMin' "et-al-min"       "et-al-subsequent-min"
    , length as >= le
    , length as >    1 = (,) True getUseFirst
    | otherwise        = (,) False $ length as
    where
      etAlMin  x   = read $ getOptionVal x os
      etAlMin' x y = if b then etAlMin x else read $ getOptionVal' x y
      isOptionSet'  s1 s2 = if b
                            then isOptionSet s1 os
                            else or $ (isOptionSet s1 os) : [(isOptionSet s2 os)]
      getOptionVal' s1 s2 = if null (getOptionVal s1 os)
                            then getOptionVal s2 os
                            else getOptionVal s1 os
      getUseFirst = let u = if b
                            then getOptionVal  "et-al-use-first" os
                            else getOptionVal' "et-al-use-first" "et-al-subsequent-min"
                    in if null u then 1 else read u

-- | Generate the 'Agent's names applying et-al options, with all
-- possible permutations to disambiguate colliding citations. The
-- 'Bool' indicate whether we are formatting the first name or not.
formatName :: EvalMode -> Bool -> Form -> Formatting -> [Option] -> [NamePart] -> Agent -> [Output]
formatName m b f fm ops np n
    -- TODO awkward to use serialized string version of an Agent here;
    -- why not make OName take an Agent instead of a String??
    | literal n /= mempty = return $ OName (show n)  institution []         fm
    | Short      <- f = return $ OName (show n)  shortName       disambdata fm
    | otherwise       = return $ OName (show n) (longName given) disambdata fm
    where
      institution = [OStr (unLiteral $ literal n) $ form "family"]
      when_ c o = if c /= [] then o else []
      addAffixes [] _ [] = []
      addAffixes s sf ns  = [Output ((oStr' s (form sf) { prefix = [], suffix = [] }) ++ ns) $
                                   emptyFormatting { prefix = prefix (form sf)
                                                   , suffix = suffix (form sf)}]

      form    s = case filter (\(NamePart n' _) -> n' == s) np of
                    NamePart _ fm':_ -> fm'
                    _                -> emptyFormatting

      hasHyphen = elem '-'
      hyphen    = if getOptionVal "initialize-with-hyphen" ops == "false"
                  then getOptionVal "initialize-with" ops
                  else filter (not . isSpace) $ getOptionVal "initialize-with" ops  ++ "-"
      isInit [c] = isUpper c
      isInit _   = False
      initial (Literal x) =
                  case lookup "initialize-with" ops of
                       Just iw
                         | getOptionVal "initialize" ops == "false"
                         , isInit x  -> addIn x iw
                         | getOptionVal "initialize" ops /= "false"
                         , not (all isLower x) -> addIn x iw
                       Nothing
                         | isInit x  -> addIn x " " -- default
                       _ -> " " ++ x ++ " "
      addIn x i = if hasHyphen x
                  then head (       takeWhile (/= '-') x) : hyphen ++
                       head (tail $ dropWhile (/= '-') x) : i
                  else head x : i

      sortSep g s = when_ g $ separator ++ addAffixes (g <+> s) "given" []
      separator   = if getOptionVal "sort-separator" ops == []
                    then oStr "," ++ [OSpace]
                    else oStr (getOptionVal "sort-separator" ops)

      suff      = if commaSuffix n && nameSuffix n /= mempty
                  then suffCom
                  else suffNoCom
      suffCom   = when_ (unLiteral $ nameSuffix n) $ separator ++ [        OStr (unLiteral $ nameSuffix n) fm]
      suffNoCom = when_ (unLiteral $ nameSuffix n) $              [OSpace, OStr (unLiteral $ nameSuffix n) fm]

      onlyGiven = not (null $ givenName n) && null family
      given     = if onlyGiven
                     then givenLong
                     else when_ (givenName  n) . trim . fixsp . concatMap initial $ givenName n
      fixsp     (' ':' ':xs) = fixsp (' ':xs)
      fixsp     (x:xs)       = x : fixsp xs
      fixsp     []           = []
      givenLong = when_ (givenName  n) . unwords $ map unLiteral $ givenName n
      family    = unLiteral $ familyName n
      dropping  = unLiteral $ droppingPart n
      nondropping  = unLiteral $ nonDroppingPart n
      isByzantine c = c <= '\x17f' || (c >= '\x590' && c <= '\x05ff') ||
                      (c >= '\x400' && c <= '\x52f') || ( c >= '\x370' && c <= '\x3ff') ||
                      (c >= '\x1f00' && c <= '\x1fff') || (c >= '\x0600' && c <= '\x200c') ||
                      (c >= '\x200d' && c <= '\x200e') || (c >= '\x0218' && c <= '\x0219') ||
                      (c >= '\x21a' && c <= '\x21b') || (c >= '\x202a' && c <= '\x202e')
      shortName = oStr' (nondropping <+> family) (form "family")
      longName g = if isSorting m
                   then let firstPart = case getOptionVal "demote-non-dropping-particle" ops of
                                           "never" -> nondropping <+> family  <+> dropping
                                           _       -> family  <+> dropping <+> nondropping
                        in [OStr firstPart (form "family")] <++> oStr' g (form "given") ++ suffCom
                   else if (b && getOptionVal "name-as-sort-order" ops == "first") ||
                           getOptionVal "name-as-sort-order" ops == "all"
                        then let (fam,par) = case getOptionVal "demote-non-dropping-particle" ops of
                                               "never"     -> (nondropping <+> family, dropping)
                                               "sort-only" -> (nondropping <+> family, dropping)
                                               _           -> (family, dropping <+> nondropping)
                             in oStr' fam (form "family") ++ sortSep g par ++ suffCom
                          else let fam = addAffixes (dropping <+> nondropping <+> family) "family" suff
                                   gvn = oStr' g (form "given")
                               in  if all isByzantine family
                                   then gvn <++> fam
                                   else fam ++ gvn

      disWithGiven = getOptionVal "disambiguate-add-givenname" ops == "true"
      initialize   = isJust (lookup "initialize-with" ops) && not onlyGiven
      isLong       = f /= Short && initialize
      givenRule    = let gr = getOptionVal "givenname-disambiguation-rule" ops
                     in if null gr then "by-cite" else gr
      disambdata   = case () of
                       _ | "all-names-with-initials"    <- givenRule
                         , disWithGiven, Short <- f, initialize    -> [longName given]
                         | "primary-name-with-initials" <- givenRule
                         , disWithGiven, Short <- f, initialize, b -> [longName given]
                         | disWithGiven, Short <- f, b
                         , "primary-name" <- givenRule -> [longName given, longName givenLong]
                         | disWithGiven, Short <- f
                         , "all-names"    <- givenRule -> [longName given, longName givenLong]
                         | disWithGiven, Short <- f
                         , "by-cite"      <- givenRule -> [longName given, longName givenLong]
                         | disWithGiven, isLong        -> [longName givenLong]
                         | otherwise                   -> []

formatTerm :: Form -> Formatting -> Bool -> String -> State EvalState [Output]
formatTerm f fm p s = do
  t <- getTerm p f s
  pos <- gets (citePosition . cite . env)
  let t' = if pos == "ibid-with-locator-c" || pos == "ibid-c"
              then capitalize t
              else t
  return $ oStr' t' fm

formatLabel :: Form -> Formatting -> Bool -> String -> State EvalState [Output]
formatLabel f fm p s
    | "locator" <- s = when' (gets (citeLocator . cite . env) >>= return . (/=) []) $ do
                       (l,v) <- getLocVar
                       form (\fm' -> return . flip OLoc emptyFormatting . output fm') id l ('-' `elem` v)
    | "page"    <- s = checkPlural
    | "volume"  <- s = checkPlural
    | "ibid"    <- s = format' s p
    | isRole       s = do a <- getAgents' s
                          if a /= [] then form (\fm' x -> [OLabel x fm']) id s p else return []
    | otherwise      = format s p
    where
      isRole = flip elem ["author", "collection-editor", "composer", "container-author"
                         ,"director", "editor", "editorial-director", "editortranslator"
                         ,"illustrator", "interviewer", "original-author", "recipient"
                         ,"reviewed-author", "translator"]
      checkPlural = when' (isVarSet s) $ do
                      v <- getStringVar s
                      format  s ('-' `elem` v)
      format      = form output id
      format' t b = gets (citePosition . cite . env) >>= \po ->
                    if po == "ibid-with-locator-c" || po == "ibid-c"
                    then form output capitalize t b
                    else format t b
      form o g t b = return . o fm =<< g . period <$> getTerm (b && p) f t
      period      = if stripPeriods fm then filter (/= '.') else id

