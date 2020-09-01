{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternGuards    #-}
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

import Prelude
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import           Data.Char              (isLower, isUpper)
import           Data.List              (intersperse, nub)
import           Data.List.Split        (wordsBy)
import           Data.Maybe             (isJust, fromMaybe)
import           Data.Text              (Text)
import qualified Data.Text              as T

import           Text.CSL.Eval.Common
import           Text.CSL.Eval.Output
import           Text.CSL.Style
import           Text.CSL.Util          (headInline, isRange, lastInline, query,
                                         readNum, splitStrWhen, toRead, (<^>))
import qualified Text.Pandoc.Builder    as B
import           Text.Pandoc.Definition
import           Text.Pandoc.Shared     (stringify)

evalNames :: Bool -> [Text] -> [Name] -> Text -> State EvalState [Output]
evalNames skipEdTrans ns nl d
    | [sa,sb] <- ns, not skipEdTrans
    , (sa == "editor" && sb == "translator") ||
      (sb == "editor" && sa == "translator") = do
        aa <- getAgents' sa
        ab <- getAgents' sb
        if not (null aa) && aa == ab
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
                  if null res
                    then     return []
                    else let role = if aus == ["author"] then "authorsub" else s
                         in return . OContrib k role res fb <$> gets etal
        r'  <- evalNames skipEdTrans xs nl d
        num <- gets contNum
        return $ if r /= [] && r' /= []
                 then count num (r ++ [ODel $ delim ops] ++ r')
                 else count num $ cleanOutput (r ++ r')
    | otherwise = return []
    where
      agents :: Text -> Text -> [Agent] -> State EvalState [Output]
      agents p s a = do
        fromMaybe [] <$> runMaybeT (concatMapM (formatNames (hasEtAl nl) d p s a) nl)
      delim    ops = if T.null d then getOptionVal "names-delimiter" ops else d
      resetEtal    = modify (\s -> s { etal = [] })
      count  num x = if hasCount nl && num /= [] -- FIXME!! le zero!!
                     then [OContrib "" "" [ONum (length num) emptyFormatting] [] []]
                     else x
      hasCount     = or . query hasCount'
      hasCount' n
          | Name Count _ _ _ _  <- n = [True]
          | otherwise                = [False]

-- | The 'Bool' is 'True' when formatting a name with a final "et-al".
-- The first 'Text' represents the position and the second the role
-- (e.i. editor, translator, etc.).
formatNames :: Bool -> Delimiter -> Text -> Text -> [Agent] -> Name -> MaybeT (State EvalState) [Output]
formatNames ea del p s as n
    | Name _ _ attrs _ _  <- n
    , isOptionSet "suppress-min" attrs
    , length as >= read (T.unpack $ getOptionVal "suppress-min" attrs)
    = mzero
    | Name _ _ attrs _ _  <- n
    , isOptionSet "suppress-max" attrs
    , length as <= read (T.unpack $ getOptionVal "suppress-max" attrs)
    = mzero
    | Name f _ ns _ _ <- n, Count <- f = do
        b <- isBib <$> gets mode
        o <- mergeOptions ns <$> gets (options . env)
        modify $ \st -> st { contNum = nub $ (++) (take (snd $ isEtAl b o p as) as) $ contNum st }
        return []

    | Name f fm ns d np <- n = do
        b <- isBib <$> gets mode
        o <- mergeOptions ns <$> gets (options . env)
        m <- gets mode
        let odel  = if del /= "" then del else getOptionVal "name-delimiter" o
            del'
              | d   /= "" = d
              | T.null odel = ", "
              | otherwise = odel
            (_,i) = isEtAl b o p as
            form  = case f of
                      NotSet -> case getOptionVal "name-form" o of
                                  "" -> Long
                                  x  -> read . T.unpack $ toRead x
                      _      -> f
            genName x = do etal' <- formatEtAl o ea "et-al" fm del' x
                           if null etal'
                              then do t <- getTerm False Long "and"
                                      return $ delim t o del'
                                             $ format m o form fm np x
                              else return $
                                    addDelim del' (format m o form fm np x)
                                    ++ etal'
        setLastName o $ formatName m False f fm o np (last as)
        lift $ updateEtal =<< mapM genName [1 + i .. length as]
        lift $ genName i

    | NameLabel f fm pl <- n = lift . when' (isVarSet s) $ do
        b <- gets edtrans
        res <- formatLabel f fm (isPlural pl $ length as) $
               if b then "editortranslator" else s
        modify $ \st -> st { edtrans = False }
        -- Note: the following line was here previously.
        -- It produces spurious 'et al's and seems to have no function,
        -- so I have commented it out:
        -- updateEtal [tr' "res" res]
        return res

    | EtAl fm t <- n = do
        o <- gets (options . env)
        et <- gets etal
        let i = length as - length et
            t' = if T.null t then "et-al" else t
        r <- lift $ mapM (et_al o False t' fm del) [i .. length as]
        let (r',r'') = case r of
                         (x:xs) -> (x, xs)
                         []     -> ([],[])
        updateEtal r''
        return r'

    | otherwise = return []
    where
      isBib (EvalBiblio _) = True
      isBib  _             = False
      updateEtal x = modify $ \st ->
                     let x' = if length x == 1 then repeat $ head x else x
                     in st { etal = case etal st of
                                         [] -> x
                                         ys -> zipWith (++) ys x'
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
          | "text"   <- getOptionVal "and" os = " " <> t <> " "
          | "symbol" <- getOptionVal "and" os = " & "
          | otherwise                         = ""
      andStr' t d os = if T.null (andStr t os) then d else andStr t os

      formatEtAl o b t fm d i = do
        ln <- gets lastName
        if isWithLastName o
           then case () of
                  _ | (length as - i) == 1 -> et_al o b t fm d i -- is that correct? FIXME later
                    | (length as - i) >  1 -> return $ [ODel d, OPan [Str "\x2026"], OSpace] ++ ln
                    | otherwise            -> return []
           else et_al o b t fm d i
      et_al o b t fm d i
          = when' ( not . isSorting <$> gets mode) $
            if b || length as <= i
            then return []
            else do x <- getTerm False Long t
                    when' (return $ x /= "") $
                          case getOptionVal "delimiter-precedes-et-al" o of
                            "never"  -> return . (++) [OSpace] $ output fm x
                            "always" -> return . (++) [ODel d] $ output fm x
                            _        -> if i > 1 && not (T.null d)
                                        then return . (++) [ODel d] $ output fm x
                                        else return . (++) [OSpace] $ output fm x

-- | The first 'Bool' is 'True' if we are evaluating the bibliography.
-- The 'Text' is the cite position. The function also returns the
-- number of contributors to be displayed.
isEtAl :: Bool -> [Option] -> Text -> [Agent] -> (Bool, Int)
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
      etAlMin  x   = read . T.unpack $ getOptionVal x os
      etAlMin' x y = if b then etAlMin x else read . T.unpack $ getOptionVal' x y
      isOptionSet'  s1 s2 = if b
                            then isOptionSet s1 os
                            else or $ isOptionSet s1 os : [isOptionSet s2 os]
      getOptionVal' s1 s2 = if T.null (getOptionVal s1 os)
                            then getOptionVal s2 os
                            else getOptionVal s1 os
      getUseFirst = let u = if b
                            then getOptionVal  "et-al-use-first" os
                            else getOptionVal' "et-al-use-first" "et-al-subsequent-min"
                    in if T.null u then 1 else read (T.unpack u)

-- | Generate the 'Agent's names applying et-al options, with all
-- possible permutations to disambiguate colliding citations. The
-- 'Bool' indicate whether we are formatting the first name or not.
formatName :: EvalMode -> Bool -> Form -> Formatting -> [Option] -> [NamePart] -> Agent -> [Output]
formatName m b f fm ops np n
    | literal n /= mempty = return $ OName n  institution []         fm
    | Short      <- f = return $ OName n  shortName       disambdata fm
    | otherwise       = return $ OName n (longName given) disambdata fm
    where
      institution = oPan' (unFormatted $ literal n) (form "family")
      when_ c o = if c /= mempty then o else mempty
      addAffixes (Formatted []) _ [] = []
      addAffixes s sf ns  = [Output (Output [OPan (unFormatted s)]
                            (form sf){ prefix = mempty, suffix = mempty} : ns)
                                   emptyFormatting { prefix = prefix (form sf)
                                                   , suffix = suffix (form sf)}]

      form    s = case filter (\(NamePart n' _) -> n' == s) np of
                    NamePart _ fm':_ -> fm'
                    _                -> emptyFormatting

      hyphenate new []    = new
      hyphenate new accum =
                  if getOptionVal "initialize-with-hyphen" ops == "false"
                  then new ++ accum
                  else trimsp new ++ [Str "-"] ++ accum
      isInit [Str (T.unpack -> [c])] = isUpper c
      isInit _         = False
      initial (Formatted x) =
                  case lookup "initialize-with" ops of
                       Just iw
                         | getOptionVal "initialize" ops == "false"
                         , isInit x  -> addIn x $ B.toList $ B.text iw
                         | getOptionVal "initialize" ops /= "false"
                         , not (all isLower $ query (:[]) x) -> addIn x $ B.toList $ B.text iw
                       Nothing
                         | isInit x  -> addIn x [Space] -- default
                       _ -> Space : x ++ [Space]
      addIn x i = foldr (hyphenate . (\z -> Str (maybe "" T.singleton $ headInline z) : i)) []
                     $ wordsBy (== Str "-")
                     $ splitStrWhen (=='-') x

      sortSep g s = when_ g $ separator ++ addAffixes (g <+> s) "given" mempty
      separator   = if isByzantineFamily
                       then [OPan (B.toList (B.text
                              (getOptionValWithDefault "sort-separator" ", " ops)))]
                       else []
      suff      = if commaSuffix n && nameSuffix n /= mempty
                  then suffCom
                  else suffNoCom
      suffCom   = when_ (nameSuffix n) $ separator ++
                        oPan' (unFormatted $ nameSuffix n) fm
      suffNoCom = when_ (nameSuffix n) $ OSpace : oPan' (unFormatted $ nameSuffix n) fm

      onlyGiven = givenName n /= mempty && family == mempty
      given     = if onlyGiven
                     then givenLong
                     else when_ (givenName  n) . Formatted . trimsp . fixsp . concatMap initial $ givenName n
      fixsp     (Space:Space:xs) = fixsp (Space:xs)
      fixsp     (x:xs)           = x : fixsp xs
      fixsp     []               = []
      trimsp = reverse . dropWhile (==Space) . reverse . dropWhile (==Space)
      givenLong = when_ (givenName  n) . mconcat . intersperse (Formatted [Space]) $ givenName n
      family    = familyName n
      dropping  = droppingPart n
      nondropping  = nonDroppingPart n
      -- see src/load.js ROMANESQUE_REGEX in citeproc-js:
      -- /[-0-9a-zA-Z\u0e01-\u0e5b\u00c0-\u017f\u0370-\u03ff\u0400-\u052f\u0590-\u05d4\u05d6-\u05ff\u1f00-\u1fff\u0600-\u06ff\u200c\u200d\u200e\u0218\u0219\u021a\u021b\u202a-\u202e]/
      isByzantine c = c == '-' ||
                      (c >= '0' && c <= '9') ||
                      (c >= 'a' && c <= 'z') ||
                      (c >= 'A' && c <= 'Z') ||
                      (c >= '\x0e01' && c <= '\x0e5b') ||
                      (c >= '\x00c0' && c <= '\x017f') ||
                      (c >= '\x0370' && c <= '\x03ff') ||
                      (c >= '\x0400' && c <= '\x052f') ||
                      (c >= '\x0590' && c <= '\x05d4') ||
                      (c >= '\x05d6' && c <= '\x05ff') ||
                      (c >= '\x1f00' && c <= '\x1fff') ||
                      (c >= '\x0600' && c <= '\x06ff') ||
                      (c >= '\x200c' && c <= '\x200e') ||
                      (c >= '\x2018' && c <= '\x2019') ||
                      (c >= '\x021a' && c <= '\x021b') ||
                      (c >= '\x202a' && c <= '\x202e')

      isByzantineFamily = T.any isByzantine (stringify family)
      shortName = oPan' (unFormatted $ nondropping <+> family) (form "family")

      longName g
        | isSorting m = let firstPart = case getOptionVal "demote-non-dropping-particle" ops of
                                           "never" -> nondropping <+> family  <+> dropping
                                           _       -> family  <+> dropping <+> nondropping
                        in oPan' (unFormatted firstPart) (form "family") <++> oPan' (unFormatted g) (form "given") <> suffCom
        | (b && getOptionVal "name-as-sort-order" ops == "first") ||
         getOptionVal "name-as-sort-order" ops == "all" = let (fam,par) = case getOptionVal "demote-non-dropping-particle" ops of
                                                                            "never"     -> (nondropping <+> family, dropping)
                                                                            "sort-only" -> (nondropping <+> family, dropping)
                                                                            _           -> (family, dropping <+> nondropping)
                                                          in oPan' (unFormatted fam) (form "family") <> sortSep g par <> suffCom
        | otherwise = let fam = addAffixes (dropping <+> nondropping <+> family) "family" suff
                          gvn = oPan' (unFormatted g) (form "given")
                      in  if isByzantineFamily
                          then gvn <++> fam
                          else fam <> gvn

      disWithGiven = getOptionVal "disambiguate-add-givenname" ops == "true"
      initialize   = isJust (lookup "initialize-with" ops) && not onlyGiven
      isLong       = f /= Short && initialize
      givenRule    = let gr = getOptionVal "givenname-disambiguation-rule" ops
                     in if T.null gr then "by-cite" else gr
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

formatTerm :: Form -> Formatting -> Bool -> Text -> Text
           -> State EvalState [Output]
formatTerm f fm p refid s = do
  plural <- if s `elem` ["page", "volume", "issue"]
               then do
                 varset <- isVarSet s
                 if varset
                    then isRange <$> getStringVar s
                    else return p
               else return p
  t <- getTerm plural f s
  return $
     if s == "no date"
        then [OYear t refid fm]
        else oStr' t fm

formatLabel :: Form -> Formatting -> Bool -> Text -> State EvalState [Output]
formatLabel f fm p s = when' (isVarSet s) go
    where
      go
        | "locator" <- s = when' ( (/=) "" <$> gets (citeLocator . cite . env)) $ do
                           (l,v) <- getLocVar
                           form (\fm' -> return . flip OLoc emptyFormatting . output fm') id l (isRange v)
        | "page"    <- s = checkPlural
        | "volume"  <- s = checkPlural
        | "issue"   <- s = checkPlural
        | "ibid"    <- s = format s p
        | isRole       s = do a <- getAgents' (if s == "editortranslator"
                                                  then "editor"
                                                  else s)
                              if null a
                                 then return []
                                 else form (\fm' x -> [OLabel x fm']) id s p
        | otherwise      = format s p
      isRole = flip elem ["author", "collection-editor", "composer", "container-author"
                         ,"director", "editor", "editorial-director", "editortranslator"
                         ,"illustrator", "interviewer", "original-author", "recipient"
                         ,"reviewed-author", "translator"]
      checkPlural = when' (isVarSet s) $ do
                      v <- getStringVar s
                      format  s (isRange v)
      format      = form output id
      form o g t b = o fm . g . period <$> getTerm (b && p) f t
      period      = if stripPeriods fm then T.filter (/= '.') else id

(<+>) :: Formatted -> Formatted -> Formatted
Formatted [] <+> ss = ss
s  <+> Formatted [] = s
Formatted xs <+> Formatted ys =
  case lastInline xs of
       Just '’' -> Formatted (xs ++ ys)
       Just '-' -> Formatted (xs ++ ys)
       _        -> Formatted (xs ++ [Space] ++ ys)

(<++>) :: [Output] -> [Output] -> [Output]
[] <++> o  = o
o  <++> [] = o
o1 <++> o2 = o1 ++ [OSpace] ++ o2
