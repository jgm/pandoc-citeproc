{-# LANGUAGE PatternGuards #-}
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
import Control.Applicative ( (<$>) )
import Control.Monad.State
import Data.Monoid (mempty)
import Data.Char ( toLower, isDigit, isLetter )
import Data.Maybe
import Data.String ( fromString )
import Text.Pandoc.Definition (Inline(Str, Space))
import Text.Pandoc.Walk (walk)
import Text.Pandoc.Shared (stringify)

import Text.CSL.Eval.Common
import Text.CSL.Eval.Output
import Text.CSL.Eval.Date
import Text.CSL.Eval.Names
import Text.CSL.Output.Plain
import Text.CSL.Reference
import Text.CSL.Style
import Text.CSL.Util ( readNum, last', proc, proc', query, betterThan )

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
                  [] -> if (isSorting $ em)
                        then []
                        else [noOutputError]
                  x | title r == Formatted [Str (citeId cit), Space, Str "not", Space,
                                   Str "found!"]             -> [noBibDataError $ cit]
                    | otherwise                              -> suppTC x
      locale = case l of
                 [x] -> x
                 _   -> Locale [] [] [] [] []
      job    = concatMapM evalElement es
      cit    = case em of
                 EvalCite    c -> c
                 EvalSorting c -> c
                 EvalBiblio  c -> c
      initSt = EvalState (mkRefMap r) (Env cit (localeTerms locale) m
                         (localeDate locale) o [] a) [] em b False [] [] False [] [] []
      -- TODO: is this needed? shouldn't the style specify titlecase or not
      -- depending on the locale?
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
    | Choose i ei e         <- el = evalIfThen i ei e
    | Macro    s   fm       <- el = return . appendOutput fm =<< evalElements =<< getMacro s
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
    | Substitute (e:els)    <- el = ifEmpty (consuming $ substituteWith e)
                                            (getFirst els) id
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

      tryGroup l = if hasVar l
                   then do
                     oldState <- get
                     res <- evalElements (rmTermConst l)
                     put oldState
                     nums <- mapM getStringVar [s | Number s _ _ <- l]
                     let plur = any ('-' `elem`) nums
                     let pluralizeTerm (Term s f fm _) = Term s f fm plur
                         pluralizeTerm x = x
                     if null res
                        then return []
                        else evalElements $ map pluralizeTerm l
                   else evalElements l
      hasVar  = not . null . query hasVarQ
      hasVarQ e
          | Variable {} <- e = [e]
          | Date     {} <- e = [e]
          | Names    {} <- e = [e]
          | Number   {} <- e = [e]
          | otherwise        = []
      rmTermConst [] = []
      rmTermConst (e:es)
          | Term  {} <- e = rmTermConst es
          | Const {} <- e = rmTermConst es
          | otherwise = e : rmTermConst es

      ifEmpty p t e = p >>= \r -> if r == [] then t else return (e r)

      withNames e n f = modify (\s -> s { authSub = e ++ authSub s
                                        , env = (env s)
                                          {names = n : names (env s)}}) >> f >>= \r ->
                         modify (\s -> s { authSub = filter (not . flip elem e) (authSub s)
                                        , env = (env s)
                                          {names = tail $ names (env s)}}) >> return r

      getFirst        [] = return []
      getFirst    (x:xs) = whenElse ((/=) []  <$> substituteWith x)
                                    (consuming $  substituteWith x)
                                    (getFirst xs)
      getMacro         s = maybe [] id . lookup s <$> gets (macros . env)
      getVariable f fm s = if isTitleVar s || isTitleShortVar s
                           then consumeVariable s >> formatTitle s f fm else
                           case map toLower s of
                             "year-suffix" -> getStringVar "ref-id" >>= \k  ->
                                              return . return $ OYearSuf [] k [] fm
                             "page"        -> getStringVar "page" >>= formatRange fm
                             "locator"     -> getLocVar >>= formatRange fm . snd
                             "url"         -> getStringVar "url" >>= \k ->
                                              if null k then return [] else return [OUrl (k,k) fm]
                             "doi"         -> getStringVar "doi" >>= \d ->
                                              if null d
                                                 then return []
                                                 else return [OUrl ("http://dx.doi.org/" ++ d, d) fm]
                             _             -> gets (env >>> options &&& abbrevs) >>= \(opts,as) ->
                                              getVar [] (getFormattedValue opts as f fm s) s >>= \r ->
                                              consumeVariable s >> return r

evalIfThen :: IfThen -> [IfThen] -> [Element] -> State EvalState [Output]
evalIfThen i ei e
    | IfThen c m el <- i = ifElse c m el
    | otherwise          = evalElements e
    where
      ifElse c m el = if ei == []
                      then whenElse (evalCond m c)
                                    (evalElements el)
                                    (evalElements e )
                      else whenElse (evalCond m c)
                                    (evalElements el)
                                    (evalIfThen (head ei) (tail ei) e)
      evalCond m c = do t <- checkCond chkType         isType          c m
                        v <- checkCond isVarSet        isSet           c m
                        n <- checkCond chkNumeric      isNumeric       c m
                        d <- checkCond chkDate         isUncertainDate c m
                        p <- checkCond chkPosition     isPosition      c m
                        a <- checkCond chkDisambiguate disambiguation  c m
                        l <- checkCond chkLocator      isLocator       c m
                        return $ match m $ concat [t,v,n,d,p,a,l]

      checkCond a f c m = if f c /= [] then mapM a (f c) else checkMatch m
      checkMatch m
          | All    <- m = return [True]
          | otherwise   = return [False]

      chkType         t = let chk = (==) (formatVariable t) . show . fromMaybe NoType . fromValue
                          in  getVar False chk "ref-type"
      chkNumeric      v = do val <- getStringVar v
                             as  <- gets (abbrevs . env)
                             let val' = if getAbbreviation as v val == [] then val else getAbbreviation as v val
                             return (isNumericString val')
      chkDate         v = getDateVar v >>= return . not . null . filter ((/=) [] . unLiteral . circa)
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
checkRange ts (x:xs) = if x == '-'
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
isNumericString s  = null . filter (not . isNumber &&& not . isSpecialChar >>> uncurry (&&)) $
                     words s

isTransNumber, isSpecialChar,isNumber :: String -> Bool
isTransNumber = and . map isDigit
isSpecialChar = and . map (flip elem "&-,")
isNumber      = filter (not . isLetter) >>> filter (not . flip elem "&-,") >>>
                map isDigit >>> and &&& not . null >>> uncurry (&&)

breakNumericString :: [String] -> [String]
breakNumericString [] = []
breakNumericString (x:xs)
    | isTransNumber x = x : breakNumericString xs
    | otherwise       = let (a,b) = break (flip elem "&-,") x
                            (c,d) = if null b then ("","") else (take 1 b, tail b)
                        in filter (/= []) $  a : c : breakNumericString (d : xs)

formatRange :: Formatting -> String -> State EvalState [Output]
formatRange _ [] = return []
formatRange fm p = do
  ops <- gets (options . env)
  ts  <- gets (terms . env)
  let opt = getOptionVal "page-range-format" ops
      pages = tupleRange . breakNumericString . words $ p

      tupleRange [] = []
      tupleRange (x:"-":[]  ) = return (x,[])
      tupleRange (x:"-":y:xs) = (x, y) : tupleRange xs
      tupleRange (x:      xs) = (x,[]) : tupleRange xs

      joinRange (a, []) = a
      joinRange (a,  b) = a ++ "-" ++ b

      process = case opt of
                 "expanded" -> checkRange ts . printNumStr . map (joinRange . uncurry expandedRange)
                 "chicago"  -> checkRange ts . printNumStr . map (joinRange . uncurry chicagoRange )
                 "minimal"  -> checkRange ts . printNumStr . map (joinRange . uncurry minimalRange )
                 _          -> checkRange ts . printNumStr . map (joinRange)
  return [flip OLoc fm $ [OStr (process pages) emptyFormatting]]

expandedRange :: String -> String -> (String, String)
expandedRange sa [] = (sa,[])
expandedRange sa sb = (p ++ reverse nA', reverse nB')
    where
      (nA,pA) = reverse >>> break isLetter >>> reverse *** reverse $ sa
      (nB,pB) = reverse >>> break isLetter >>> reverse *** reverse $ sb
      zipNum x y = zipWith (\a b -> if b == '+' then (a,a) else (a,b))
                           (reverse x ++ take 10 (repeat '*'))
                   >>> unzip >>> filter (/= '*') *** filter (/= '*') $
                   (reverse y ++ repeat '+')
      checkNum a b = let a' = take (length b) a
                     in  readNum a' > readNum b
      (p,(nA',nB'))
          = case () of
              _ | pA /= []
                , checkNum nA nB       -> (,) [] $ (reverse $ pA ++ nA, reverse $ pB ++ nB)
                | pA /= pB
                , last' pA == last' pB -> (,) pA $ second (flip (++) (last' pA)) $ zipNum nA nB
                | pA == pB             -> (,) pA $ second (flip (++) (last' pA)) $ zipNum nA nB
                | pB == []             -> (,) pA $ second (flip (++) (last' pA)) $ zipNum nA nB
                | otherwise            -> (,) [] $ (reverse $ pA ++ nA, reverse $ pB ++ nB)

minimalRange :: String -> String -> (String, String)
minimalRange sa sb
    = res
    where
      (a,b) = expandedRange sa sb
      res   = if length a == length b
              then second (filter (/= '+')) $ unzip $ doit a b
              else (a,b)
      doit (x:xs) (y:ys) = if x == y
                           then (x,'+') : doit xs ys
                           else zip (x:xs) (y:ys)
      doit _      _      = []

chicagoRange :: String -> String -> (String, String)
chicagoRange sa sb
    = case () of
        _ | length sa < 3    -> expandedRange sa sb
          | '0':'0':_ <- sa' -> expandedRange sa sb
          | _  :'0':_ <- sa' -> minimalRange  sa sb
          | _  :a2:as <- sa'
          , b1 :b2:bs <- sb'
          , comp as bs       -> if a2 == b2
                                then (sa, [b2,b1])
                                else minimalRange sa sb

          | _:a2:a3:_:[] <- sa'
          , _:b2:b3:_    <- sb' -> if a3 /= b3 && a2 /= b2
                                   then expandedRange sa sb
                                   else minimalRange  sa sb
          | otherwise           -> minimalRange sa sb
      where
        sa' = reverse sa
        sb' = reverse sb
        comp a b = let b' = takeWhile isDigit b
                   in take (length b') a == b'
