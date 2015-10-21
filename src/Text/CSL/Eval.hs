{-# LANGUAGE PatternGuards, FlexibleContexts, ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.CSL.Eval
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

module Text.CSL.Eval
    ( evalLayout
    , evalSorting
    , module Text.CSL.Eval.Common
    , module Text.CSL.Eval.Output
    ) where

import Control.Arrow
import Control.Monad.State
import Data.Monoid (Any(..))
import Data.Char ( toLower, isDigit, isLetter )
import Data.Maybe
import Data.String ( fromString )
import Text.Pandoc.Definition (Inline(Str, Space, Link))
import Text.Pandoc.Walk (walk)
import Text.Pandoc.Shared (stringify)
import qualified Data.Text as T

import Text.CSL.Eval.Common
import Text.CSL.Eval.Output
import Text.CSL.Eval.Date
import Text.CSL.Eval.Names
import Text.CSL.Output.Plain
import Text.CSL.Reference
import Text.CSL.Style hiding (Any)
import Text.CSL.Util ( readNum, last', proc, proc', query, betterThan,
                       safeRead )

-- | Produce the output with a 'Layout', the 'EvalMode', a 'Bool'
-- 'True' if the evaluation happens for disambiguation purposes, the
-- 'Locale', the 'MacroMap', the position of the cite and the
-- 'Reference'.
evalLayout :: Layout   -> EvalMode -> Bool -> [Locale] -> [MacroMap]
           -> [Option] -> Abbreviations -> Reference -> [Output]
evalLayout (Layout _ _ es) em b l m o a r
    = cleanOutput evalOut
    where
      evalOut = case evalState job initSt of
                  [] -> if isSorting em
                        then []
                        else [noOutputError]
                  x | title r == Formatted [Str (citeId cit), Space, Str "not", Space,
                                   Str "found!"]             -> [noBibDataError $ cit]
                    | otherwise                              -> suppTC x
      locale = case l of
                 [x] -> x
                 _   -> Locale [] [] [] [] []
      job    = evalElements es
      cit    = case em of
                 EvalCite    c -> c
                 EvalSorting c -> c
                 EvalBiblio  c -> c
      initSt = EvalState (mkRefMap r) (Env cit (localeTerms locale) m
                         (localeDate locale) o [] a) [] em b False [] [] False [] [] []
      suppTC = let getLang = take 2 . map toLower in
               case (getLang $ localeLang locale, getLang $ unLiteral $ language r) of
                 (_,  "en") -> id
                 ("en", []) -> id
                 _          -> proc' rmTitleCase

evalSorting :: EvalMode -> [Locale] -> [MacroMap] -> [Option] ->
               [Sort] -> Abbreviations -> Reference -> [Sorting]
evalSorting m l ms opts ss as r
    = map (format . sorting) ss
    where
      render       = renderPlain . formatOutputList
      format (s,e) = applaySort s . render $ uncurry eval e
      eval     o e = evalLayout (Layout emptyFormatting [] [e]) m False l ms o as r
      applaySort c s
          | Ascending {} <- c = Ascending  s
          | otherwise         = Descending s

      unsetOpts ("et-al-min"                 ,_) = ("et-al-min"           ,"")
      unsetOpts ("et-al-use-first"           ,_) = ("et-al-use-first"     ,"")
      unsetOpts ("et-al-subsequent-min"      ,_) = ("et-al-subsequent-min","")
      unsetOpts ("et-al-subsequent-use-first",_) = ("et-al-subsequent-use-first","")
      unsetOpts  x                               = x
      setOpts s i = if i /= 0 then (s, show i) else ([],[])
      sorting s
          = case s of
              SortVariable str s'     -> (s', ( ("name-as-sort-order","all") : opts
                                              , Variable [str] Long emptyFormatting []))
              SortMacro  str s' a b c -> (s', ( setOpts "et-al-min"       a : ("et-al-use-last",c) :
                                                setOpts "et-al-use-first" b : proc unsetOpts opts
                                              , Macro str emptyFormatting))

evalElements :: [Element] -> State EvalState [Output]
evalElements = concatMapM evalElement

evalElement :: Element -> State EvalState [Output]
evalElement el
    | Const    s   fm       <- el = return $ addSpaces s
                                           $ if fm == emptyFormatting
                                                then [OPan (readCSLString s)]
                                                else [Output [OPan (readCSLString s)] fm]
                                    -- NOTE: this conditional seems needed for
                                    -- locator_SimpleLocators.json:
    | Number   s f fm       <- el = if s == "locator"
                                       then getLocVar >>= formatRange fm . snd
                                       else formatNumber f fm s =<<
                                            getStringVar s
    | Variable s f fm d     <- el = return . addDelim d =<< concatMapM (getVariable f fm) s
    | Group        fm d l   <- el = outputList fm d <$> tryGroup l
    | Date     _ _ _  _ _ _ <- el = evalDate el
    | Label    s f fm _     <- el = formatLabel f fm True s -- FIXME !!
    | Term     s f fm p     <- el = formatTerm  f fm p    s
    | Names    s n fm d sub <- el = modify (\st -> st { contNum = [] }) >>
                                    ifEmpty (evalNames False s n d)
                                            (withNames s el $ evalElements sub)
                                            (appendOutput fm)
    | Substitute (e:els)    <- el = do
                        res <- consuming $ substituteWith e
                        if null res
                           then if null els
                                   then return [ONull]
                                   else evalElement (Substitute els)
                           else return res
    -- All macros and conditionals should have been expanded
    | Choose i ei xs        <- el = do
                        res <- evalIfThen i ei xs
                        evalElements res
    | Macro    s   fm       <- el = do
                        ms <- gets (macros . env)
                        case lookup s ms of
                             Nothing  -> error $ "Macro " ++ show s ++ " not found!"
                             Just els -> do
                               res <- concat <$> mapM evalElement els
                               if null res
                                  then return []
                                  else return [Output res fm]
    | otherwise                   = return []
    where
      addSpaces strng = (if take 1 strng == " " then (OSpace:) else id) .
                        (if last' strng == " " then (++[OSpace]) else id)
      substituteWith e = head <$> gets (names . env) >>= \(Names _ ns fm d _) -> do
                           case e of
                             Names rs [Name NotSet fm'' [] [] []] fm' d' []
                                 -> let nfm = mergeFM fm'' $ mergeFM fm' fm in
                                    evalElement $ Names rs ns nfm (d' `betterThan` d) []
                             _   -> evalElement e

      -- from citeproc documentation: "cs:group implicitly acts as a
      -- conditional: cs:group and its child elements are suppressed if
      -- a) at least one rendering element in cs:group calls a variable
      -- (either directly or via a macro), and b) all variables that are
      -- called are empty. This accommodates descriptive cs:text elements."

      -- TODO:  problem, this approach gives wrong results when the variable
      -- is in a conditional and the other branch is followed.  the term
      -- provided by the other branch (e.g. 'n.d.') is not printed.  we
      -- should ideally expand conditionals when we expand macros.
      tryGroup l = if getAny $ query hasVar l
                   then do
                     oldState <- get
                     res <- evalElements (rmTermConst l)
                     put oldState
                     let numVars = [s | Number s _ _ <- l]
                     nums <- mapM getStringVar numVars
                     let pluralizeTerm (Term s f fm _) = Term s f fm $
                            case numVars of
                              ["number-of-volumes"] -> not $ any (== "1") nums
                              ["number-of-pages"]   -> not $ any (== "1") nums
                              _ -> any
                                   (\x -> '-' `elem` x || '\x2013' `elem` x)
                                   nums
                         pluralizeTerm x = x
                     if null res
                        then return []
                        else evalElements $ map pluralizeTerm l
                   else evalElements l
      hasVar e
          | Variable {} <- e = Any True
          | Date     {} <- e = Any True
          | Names    {} <- e = Any True
          | Number   {} <- e = Any True
          | otherwise        = Any False
      rmTermConst = proc $ filter (not . isTermConst)
      isTermConst e
          | Term  {} <- e = True
          | Const {} <- e = True
          | otherwise     = False

      ifEmpty p t e = p >>= \r -> if r == [] then t else return (e r)

      withNames e n f = modify (\s -> s { authSub = e ++ authSub s
                                        , env = (env s)
                                          {names = n : names (env s)}}) >> f >>= \r ->
                         modify (\s -> s { authSub = filter (not . flip elem e) (authSub s)
                                        , env = (env s)
                                          {names = tail $ names (env s)}}) >> return r

      getVariable f fm s = if isTitleVar s || isTitleShortVar s
                           then consumeVariable s >> formatTitle s f fm else
                           case map toLower s of
                             "year-suffix" -> getStringVar "ref-id" >>= \k  ->
                                              return . return $ OYearSuf [] k [] fm
                             "page"        -> getStringVar "page" >>= formatRange fm
                             "locator"     -> getLocVar >>= formatRange fm . snd
                             "url"         -> getStringVar "url" >>= \k ->
                                              if null k then return [] else return [Output [OPan [Link [Str k] (k,"")]] fm]
                             "doi"         -> do d <- getStringVar "doi"
                                                 let (prefixPart, linkPart) = T.breakOn (T.pack "http") (T.pack (prefix fm))
                                                 let u = if T.null linkPart
                                                            then "https://doi.org/" ++ d
                                                            else T.unpack linkPart ++ d
                                                 if null d
                                                    then return []
                                                    else return [Output [OPan [Link [Str (T.unpack linkPart ++ d)] (u, "")]]
                                                          fm{ prefix = T.unpack prefixPart, suffix = suffix fm }]
                             "pmid"        -> getStringVar "pmid" >>= \d ->
                                              if null d
                                                 then return []
                                                 else return [Output [OPan [Link [Str d] ("http://www.ncbi.nlm.nih.gov/pubmed/" ++ d, "")]] fm]
                             "pmcid"       -> getStringVar "pmcid" >>= \d ->
                                              if null d
                                                 then return []
                                                 else return [Output [OPan [Link [Str d] ("http://www.ncbi.nlm.nih.gov/pmc/articles/" ++ d, "")]] fm]
                             _             -> gets (env >>> options &&& abbrevs) >>= \(opts,as) ->
                                              getVar [] (getFormattedValue opts as f fm s) s >>= \r ->
                                              consumeVariable s >> return r

evalIfThen :: IfThen -> [IfThen] -> [Element] -> State EvalState [Element]
evalIfThen (IfThen c' m' el') ei e = whenElse (evalCond m' c') (return el') rest
  where
      rest = case ei of
                  []     -> return e
                  (x:xs) -> evalIfThen x xs e
      evalCond m c = do t <- checkCond chkType         isType          c m
                        v <- checkCond isVarSet        isSet           c m
                        n <- checkCond chkNumeric      isNumeric       c m
                        d <- checkCond chkDate         isUncertainDate c m
                        p <- checkCond chkPosition     isPosition      c m
                        a <- checkCond chkDisambiguate disambiguation  c m
                        l <- checkCond chkLocator      isLocator       c m
                        return $ match m $ concat [t,v,n,d,p,a,l]

      checkCond a f c m = case f c of
                               []  -> case m of
                                           All -> return [True]
                                           _   -> return [False]
                               xs  -> mapM a xs

      chkType         t = let chk = (==) (formatVariable t) . show . fromMaybe NoType . fromValue
                          in  getVar False chk "ref-type"
      chkNumeric      v = do val <- getStringVar v
                             as  <- gets (abbrevs . env)
                             let val' = if getAbbreviation as v val == [] then val else getAbbreviation as v val
                             return (isNumericString val')
      chkDate         v = getDateVar v >>= return . not . null . filter circa
      chkPosition     s = if s == "near-note"
                          then gets (nearNote . cite . env)
                          else gets (citePosition . cite . env) >>= return . compPosition s
      chkDisambiguate s = gets disamb  >>= return . (==) (formatVariable s) . map toLower . show
      chkLocator      v = getLocVar    >>= return . (==) v . fst
      isIbid          s = not (s == "first" || s == "subsequent")
      compPosition a b
          | "first"             <- a = b == "first"
          | "subsequent"        <- a = b /= "first"
          | "ibid-with-locator" <- a = b == "ibid-with-locator" ||
                                       b == "ibid-with-locator-c"
          | otherwise                = isIbid b

getFormattedValue :: [Option] -> Abbreviations -> Form -> Formatting -> String -> Value -> [Output]
getFormattedValue o as f fm s val
    | Just v <- fromValue val :: Maybe Formatted =
       if v == mempty
          then []
          else let ys = maybe (unFormatted v) (unFormatted . fromString)
                        $ getAbbr (stringify $ unFormatted v)
               in  if null ys
                      then []
                      else [Output [OPan $ walk value' ys] fm]
    | Just v <- fromValue val :: Maybe String    = (:[]) . flip OStr fm . maybe v id . getAbbr $ value v
    | Just v <- fromValue val :: Maybe Int       = output  fm (if v == 0 then [] else show v)
    | Just v <- fromValue val :: Maybe CNum      = if v == 0 then [] else [OCitNum (unCNum v) fm]
    | Just v <- fromValue val :: Maybe [RefDate] = formatDate (EvalSorting emptyCite) [] [] sortDate v
    | Just v <- fromValue val :: Maybe [Agent]   = concatMap (formatName (EvalSorting emptyCite) True f
                                                              fm nameOpts []) v
    | otherwise                                  = []
    where
      value     = if stripPeriods fm then filter (/= '.') else id
      value' (Str x) = Str (value x)
      value' x       = x
      getAbbr v = if f == Short
                  then case getAbbreviation as s v of
                             []   -> Nothing
                             y    -> Just y
                  else Nothing
      nameOpts = ("name-as-sort-order","all") : o
      sortDate = [ DatePart "year"  "numeric-leading-zeros" "" emptyFormatting
                 , DatePart "month" "numeric-leading-zeros" "" emptyFormatting
                 , DatePart "day"   "numeric-leading-zeros" "" emptyFormatting]

formatTitle :: String -> Form -> Formatting -> State EvalState [Output]
formatTitle s f fm
    | Short <- f
    , isTitleVar      s = try (getIt $ s ++ "-short") $ getIt s
    | isTitleShortVar s = try (getIt s) $ return . (:[]) . flip OStr fm =<< getTitleShort s
    | otherwise         = getIt s
    where
      try g h = g >>= \r -> if r == [] then h else return r
      getIt x = do
        o <- gets (options . env)
        a <- gets (abbrevs . env)
        getVar [] (getFormattedValue o a f fm x) x

formatNumber :: NumericForm -> Formatting -> String -> String -> State EvalState [Output]
formatNumber f fm v n
    = gets (abbrevs . env) >>= \as ->
      if isNumericString (getAbbr as n)
      then gets (terms . env) >>=
           return . output fm . flip process (getAbbr as n)
      else return . output fm . getAbbr as $ n
    where
      getAbbr       as   = if getAbbreviation as v n == [] then id else getAbbreviation as v
      checkRange'   ts   = if v == "page" then checkRange ts else id
      process       ts   = checkRange' ts . printNumStr . map (renderNumber ts) .
                           breakNumericString . words
      renderNumber  ts x = if isTransNumber x then format ts x else x

      format tm = case f of
                    Ordinal     -> ordinal     tm v
                    LongOrdinal -> longOrdinal tm v
                    Roman       -> if readNum n < 6000 then roman else id
                    _           -> id

      roman     = foldr (++) [] . reverse . map (uncurry (!!)) . zip romanList .
                  map (readNum . return) . take 4 . reverse
      romanList = [[ "", "i", "ii", "iii", "iv", "v", "vi", "vii", "viii", "ix" ]
                  ,[ "", "x", "xx", "xxx", "xl", "l", "lx", "lxx", "lxxx", "xc" ]
                  ,[ "", "c", "cc", "ccc", "cd", "d", "dc", "dcc", "dccc", "cm" ]
                  ,[ "", "m", "mm", "mmm", "mmmm", "mmmmm"]
                  ]


checkRange :: [CslTerm] -> String -> String
checkRange _ [] = []
checkRange ts (x:xs) = if x == '-' || x == '\x2013'
                       then pageRange ts ++ checkRange ts xs
                       else x             : checkRange ts xs

printNumStr :: [String] -> String
printNumStr []     = []
printNumStr (x:[]) = x
printNumStr (x:"-":y:xs) = x ++ "-"  ++ y ++ printNumStr xs
printNumStr (x:",":y:xs) = x ++ ", " ++ y ++ printNumStr xs
printNumStr (x:xs)
    | x == "-"  = x ++        printNumStr xs
    | otherwise = x ++ " " ++ printNumStr xs

pageRange :: [CslTerm] -> String
pageRange = maybe "\x2013" termPlural . findTerm "page-range-delimiter" Long

isNumericString :: String -> Bool
isNumericString [] = False
isNumericString s  = all (\c -> isNumber c || isSpecialChar c) $ words s

isTransNumber, isSpecialChar,isNumber :: String -> Bool
isTransNumber = all isDigit
isSpecialChar = all (`elem` "&-,\x2013")
isNumber   cs = case [c | c <- cs
                        , not (isLetter c)
                        , not (c `elem` "&-,\x2013")] of
                     []  -> False
                     xs  -> all isDigit xs

breakNumericString :: [String] -> [String]
breakNumericString [] = []
breakNumericString (x:xs)
    | isTransNumber x = x : breakNumericString xs
    | otherwise       = let (a,b) = break (`elem` "&-\x2013,") x
                            (c,d) = if null b
                                       then ("","")
                                       else span (`elem` "&-\x2013,") b
                        in filter (/= []) $  a : c : breakNumericString (d : xs)

formatRange :: Formatting -> String -> State EvalState [Output]
formatRange _ [] = return []
formatRange fm p = do
  ops <- gets (options . env)
  ts  <- gets (terms . env)
  let opt = getOptionVal "page-range-format" ops
      pages = tupleRange . breakNumericString . words $ p

      tupleRange [] = []
      tupleRange (x:cs:[]  )
        | cs `elem` ["-", "--", "\x2013"] = return (x,[])
      tupleRange (x:cs:y:xs)
        | cs `elem` ["-", "--", "\x2013"] = (x, y) : tupleRange xs
      tupleRange (x:      xs) = (x,[]) : tupleRange xs

      joinRange (a, []) = a
      joinRange (a,  b) = a ++ "-" ++ b

      process = checkRange ts . printNumStr . case opt of
                 "expanded" -> map (joinRange . expandedRange)
                 "chicago"  -> map (joinRange . chicagoRange )
                 "minimal"  -> map (joinRange . minimalRange 1)
                 "minimal-two"  -> map (joinRange . minimalRange 2)
                 _          -> map joinRange
  return [flip OLoc fm $ [OStr (process pages) emptyFormatting]]

-- Abbreviated page ranges are expanded to their non-abbreviated form:
-- 42–45, 321–328, 2787–2816
expandedRange :: (String, String) -> (String, String)
expandedRange (sa, []) = (sa,[])
expandedRange (sa, sb)
  | length sb < length sa =
      case (safeRead sa, safeRead sb) of
           -- check to make sure we have regular numbers
           (Just (_ :: Int), Just (_ :: Int)) ->
             (sa, take (length sa - length sb) sa ++ sb)
           _ -> (sa, sb)
  | otherwise = (sa, sb)

-- All digits repeated in the second number are left out:
-- 42–5, 321–8, 2787–816.  The minDigits parameter indicates
-- a minimum number of digits for the second number; thus, with
-- minDigits = 2, we have 328-28.
minimalRange :: Int -> (String, String) -> (String, String)
minimalRange minDigits ((a:as), (b:bs))
  | a == b
  , length as == length bs
  , length bs >= minDigits =
                let (_, bs') = minimalRange minDigits (as, bs)
                in  (a:as, bs')
minimalRange _ (as, bs) = (as, bs)

-- Page ranges are abbreviated according to the Chicago Manual of Style-rules:
-- First number             Second number    Examples
-- Less than 100            Use all digits   3–10; 71–72
-- 100 or multiple of 100   Use all digits   100–104; 600–613; 1100–1123
-- 101 through 109 (in multiples of 100) Use changed part only  10002-6, 505-17
-- 110 through 199          Use 2 digits or more  321-25, 415-532
-- if numbers are 4 digits long or more and 3 digits change, use all digits
--         1496-1504
chicagoRange :: (String, String) -> (String, String)
chicagoRange (sa, sb)
    = case (safeRead sa :: Maybe Int) of
          Just n | n < 100 -> expandedRange (sa, sb)
                 | n `mod` 100 == 0 -> expandedRange (sa, sb)
                 | n >= 1000 -> let (sa', sb') = minimalRange 1 (sa, sb)
                                in  if length sb' >= 3
                                       then expandedRange (sa, sb)
                                       else (sa', sb')
                  | n > 100 -> if n `mod` 100 < 10
                                 then minimalRange 1 (sa, sb)
                                 else minimalRange 2 (sa, sb)
          _ -> expandedRange (sa, sb)
