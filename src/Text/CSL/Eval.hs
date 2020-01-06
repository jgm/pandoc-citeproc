{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

import Prelude
import           Control.Arrow
import qualified Control.Exception      as E
import           Control.Monad.State
import           Data.Char              (isDigit, isLetter)
import           Data.Maybe
import           Data.Monoid            (Any (..))
import           Data.String            (fromString)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Text.Pandoc.Definition (Inline (Link, Span, Str), nullAttr)
import           Text.Pandoc.Shared     (stringify, escapeURI)
import           Text.Pandoc.Walk       (walk)

import           Text.CSL.Eval.Common
import           Text.CSL.Eval.Date
import           Text.CSL.Eval.Names
import           Text.CSL.Eval.Output
import           Text.CSL.Exception
import           Text.CSL.Output.Plain
import           Text.CSL.Reference
import           Text.CSL.Style         hiding (Any)
import           Text.CSL.Util          (isRange, proc,
                                         proc', query, readNum, safeRead)

-- | Produce the output with a 'Layout', the 'EvalMode', a 'Bool'
-- 'True' if the evaluation happens for disambiguation purposes, the
-- 'Locale', the 'MacroMap', the position of the cite and the
-- 'Reference'.
evalLayout :: Layout   -> EvalMode -> Bool -> [Locale] -> [MacroMap]
           -> [Option] -> Abbreviations -> Maybe Reference -> [Output]
evalLayout (Layout _ _ es) em b l m o a mbr
    = cleanOutput evalOut
    where
      evalOut = case evalState job initSt of
                  x | isNothing mbr -> [noBibDataError cit]
                    | null x        -> []
                    | otherwise     -> suppTC x
      locale = case l of
                 [x] -> x
                 _   -> Locale "" "" [] [] []
      job    = evalElements es
      cit    = case em of
                 EvalCite    c -> c
                 EvalSorting c -> c
                 EvalBiblio  c -> c
      initSt = EvalState (mkRefMap mbr) (Env cit (localeTerms locale) m
                         (localeDate locale) o [] a) [] em b False [] [] False [] [] []
      suppTC = let getLang = T.take 2 . T.toLower in
               case (getLang $ localeLang locale,
                     getLang . unLiteral . language <$> mbr) of
                 (_,  Just "en") -> id
                 (_,  Nothing)   -> id
                 ("en", Just "") -> id
                 _               -> proc' rmTitleCase'

evalSorting :: EvalMode -> [Locale] -> [MacroMap] -> [Option] ->
               [Sort] -> Abbreviations -> Maybe Reference -> [Sorting]
evalSorting m l ms opts ss as mbr
    = map (format . sorting) ss
    where
      render       = renderPlain . formatOutputList . proc removeDelimAndLabel
      removeDelimAndLabel OLabel{} = ONull
      removeDelimAndLabel ODel{}   = ONull
      -- for sorting purposes, we need to distinguish between the space
      -- inside a last name like ben Gurion, and the space between the
      -- last name and the first.  OSpace is used for the latter.
      removeDelimAndLabel OSpace{} = OStr "," emptyFormatting
      removeDelimAndLabel x          = x
      format (s,e) = applySort s . render $ uncurry eval e
      eval     o e = evalLayout (Layout emptyFormatting "" [e]) m False l ms o as mbr
      applySort c s
          | Ascending {} <- c = Ascending  s
          | otherwise         = Descending s

      unsetOpts :: (Text, Text) -> (Text, Text)
      unsetOpts ("et-al-min"                 ,_) = ("et-al-min"           ,"")
      unsetOpts ("et-al-use-first"           ,_) = ("et-al-use-first"     ,"")
      unsetOpts ("et-al-subsequent-min"      ,_) = ("et-al-subsequent-min","")
      unsetOpts ("et-al-subsequent-use-first",_) = ("et-al-subsequent-use-first","")
      unsetOpts  x                               = x
      setOpts s i = if i /= 0 then (s, T.pack $ show i) else ("","")
      sorting s
          = case s of
              SortVariable str s'     -> (s', ( ("name-as-sort-order","all") : opts
                                              , Variable [str] Long emptyFormatting ""))
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
    | Variable s f fm d     <- el = addDelim d <$> concatMapM (getVariable f fm) s
    | Group        fm d l   <- el = outputList fm d <$> tryGroup l
    | Date{} <- el = evalDate el
    | Label    s f fm _     <- el = formatLabel f fm True s -- FIXME !!
    | Term     s f fm p     <- el = getStringVar "ref-id" >>= \refid ->
                                      formatTerm  f fm p refid  s
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
                             Nothing  -> E.throw $ MacroNotFound (show s)
                             Just els -> do
                               res <- concat <$> mapM evalElement els
                               if null res
                                  then return []
                                  else return [Output res fm]
    | otherwise                   = return []
    where
      addSpaces strng = (if T.take 1 strng == " " then (OSpace:) else id) .
                        (if (== Just ' ') (snd <$> T.unsnoc strng)
                         then (++[OSpace])
                         else id)
      substituteWith e =
        gets (names . env) >>= \case
          (Names _ ns fm d _ : _) -> evalElement $ proc replaceNames e
             where
               replaceNames (Names rs [Name NotSet fm'' [] "" []] fm' d' []) =
                  let nfm = mergeFM fm'' $ mergeFM fm' fm in
                  Names rs ns nfm (if T.null d' then d else d') []
               replaceNames x = x
          _ -> return []

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
                              ["number-of-volumes"] -> "1" `notElem` nums
                              ["number-of-pages"]   -> "1" `notElem` nums
                              _ -> any isRange nums
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

      ifEmpty p t e = p >>= \r -> if null r then t else return (e r)

      withNames e n f = modify (\s -> s { authSub = e ++ authSub s
                                        , env = (env s)
                                          {names = n : names (env s)}}) >> f >>= \r ->
                         modify (\s -> s { authSub = filter (not . flip elem e) (authSub s)
                                        , env = (env s)
                                          {names = tail $ names (env s)}}) >> return r

      getVariable f fm s
        | isTitleVar s || isTitleShortVar s =
             consumeVariable s >> formatTitle s f fm
        | otherwise =
             case T.toLower s of
               "first-reference-note-number"
                             -> do refid <- getStringVar "ref-id"
                                   return [Output [OPan [Span ("",["first-reference-note-number"],[("refid",refid)]) [Str "0"]]] fm]

               "year-suffix" -> getStringVar "ref-id" >>= \k  ->
                                return . return $ OYearSuf "" k [] fm
               "status"      -> do
                  (opts, as) <- gets (env >>> options &&& abbrevs)
                  r <- getVar mempty (getFormattedValue opts as f fm s)
                        "status"
                  consumeVariable s
                  return r
               "page"        -> getStringVar "page" >>= formatRange fm
               "locator"     -> getLocVar >>= formatRange fm . snd
               "url"         -> getStringVar "url" >>= \k ->
                                if T.null k
                                then return []
                                else return [Output [OPan [Link nullAttr [Str k] (escapeURI k,"")]] fm]
               "doi"         -> do d <- getStringVar "doi"
                                   let (prefixPart, linkPart) = T.breakOn (T.pack "http") (prefix fm)
                                   let u = if T.null linkPart
                                              then "https://doi.org/" <> d
                                              else linkPart <> d
                                   if T.null d
                                      then return []
                                      else return [Output [OPan [Link nullAttr [Str (linkPart <> d)] (escapeURI u, "")]]
                                            fm{ prefix = prefixPart, suffix = suffix fm }]
               "isbn"        -> getStringVar "isbn" >>= \d ->
                                if T.null d
                                   then return []
                                   else return [Output [OPan [Link nullAttr [Str d] ("https://worldcat.org/isbn/" <> escapeURI d, "")]] fm]
               "pmid"        -> getStringVar "pmid" >>= \d ->
                                if T.null d
                                   then return []
                                   else return [Output [OPan [Link nullAttr [Str d] ("https://www.ncbi.nlm.nih.gov/pubmed/" <> escapeURI d, "")]] fm]
               "pmcid"       -> getStringVar "pmcid" >>= \d ->
                                if T.null d
                                   then return []
                                   else return [Output [OPan [Link nullAttr [Str d] ("https://www.ncbi.nlm.nih.gov/pmc/articles/" <> escapeURI d, "")]] fm]
               _ -> do (opts, as) <- gets (env >>> options &&& abbrevs)
                       r <- getVar []
                              (getFormattedValue opts as f fm s) s
                       consumeVariable s
                       return r

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

      chkType         t = let chk = (==) (formatVariable t) . T.pack . show
                                  . fromMaybe NoType . fromValue
                          in  getVar False chk "ref-type"
      chkNumeric      v = do val <- getStringVar v
                             as  <- gets (abbrevs . env)
                             let val' = if T.null (getAbbreviation as v val)
                                           then val
                                           else getAbbreviation as v val
                             return (isNumericString val')
      chkDate         v = any circa <$> getDateVar v
      chkPosition     s = if s == "near-note"
                          then gets (nearNote . cite . env)
                          else compPosition s <$> gets (citePosition . cite . env)
      chkDisambiguate s = (==) (formatVariable s) . T.toLower . T.pack . show
                          <$> gets disamb
      chkLocator      v = (==) v . fst <$> getLocVar
      isIbid          s = not (s == "first" || s == "subsequent")
      compPosition a b
          | "first"             <- a = b == "first"
          | "subsequent"        <- a = b /= "first"
          | "ibid-with-locator" <- a = b == "ibid-with-locator" ||
                                       b == "ibid-with-locator-c"
          | otherwise                = isIbid b

getFormattedValue :: [Option] -> Abbreviations -> Form -> Formatting -> Text -> Value -> [Output]
getFormattedValue o as f fm s val
    | Just (Formatted v) <- fromValue val :: Maybe Formatted =
       case v of
          [] -> []
          _  -> case maybe v (unFormatted . fromString . T.unpack) $
                           getAbbr (stringify v) of
                  [] -> []
                  ys -> [Output [(if s == "status"
                                     then OStatus
                                     else OPan) $ walk value' ys] fm]
    | Just v <- fromValue val :: Maybe Text =
         case value v of
            "" -> []
            xs -> case getAbbr xs of
                    Nothing -> [OStr xs fm]
                    Just ys -> [OStr ys fm]
    | Just (Literal v) <- fromValue val :: Maybe Literal =
         case value v of
            "" -> []
            xs -> case getAbbr xs of
                    Nothing -> [OStr xs fm]
                    Just ys -> [OStr ys fm]
    | Just v <- fromValue val :: Maybe Int       = output  fm (if v == 0 then "" else T.pack $ show v)
    | Just v <- fromValue val :: Maybe CNum      = if v == 0 then [] else [OCitNum (unCNum v) fm]
    | Just v <- fromValue val :: Maybe CLabel    = if v == mempty then [] else [OCitLabel (unCLabel v) fm]
    | Just v <- fromValue val :: Maybe [RefDate] = formatDate (EvalSorting emptyCite) "" [] sortDate v
    | Just v <- fromValue val :: Maybe [Agent]   = concatMap (formatName (EvalSorting emptyCite) True f
                                                              fm nameOpts []) v
    | otherwise                                  = []
    where
      value     = if stripPeriods fm then T.filter (/= '.') else id
      value' (Str x) = Str $ value x
      value' x       = x
      getAbbr v = if f == Short
                  then case getAbbreviation as s v of
                             "" -> Nothing
                             y  -> Just y
                  else Nothing
      nameOpts = ("name-as-sort-order","all") : o
      sortDate = [ DatePart "year"  "numeric-leading-zeros" "" emptyFormatting
                 , DatePart "month" "numeric-leading-zeros" "" emptyFormatting
                 , DatePart "day"   "numeric-leading-zeros" "" emptyFormatting]

formatTitle :: Text -> Form -> Formatting -> State EvalState [Output]
formatTitle s f fm
    | Short <- f
    , isTitleVar      s = try (getIt $ s <> "-short") $ getIt s
    | isTitleShortVar s = try (getIt s) $ (:[]) . flip OStr fm <$> getTitleShort s
    | otherwise         = getIt s
    where
      try g h = g >>= \r -> if null r then h else return r
      getIt x = do
        o <- gets (options . env)
        a <- gets (abbrevs . env)
        getVar [] (getFormattedValue o a f fm x) x

formatNumber :: NumericForm -> Formatting -> Text -> Text -> State EvalState [Output]
formatNumber f fm v n
    = gets (abbrevs . env) >>= \as ->
      if isNumericString (getAbbr as n)
      then output fm . flip process (getAbbr as n) <$> gets (terms . env)
      else return . output fm . getAbbr as $ n
    where
      getAbbr       as   = if T.null (getAbbreviation as v n)
                              then id
                              else getAbbreviation as v
      checkRange'   ts   = if v == "page" then checkRange ts else id
      process       ts   = checkRange' ts . printNumStr . map (renderNumber ts) .
                           breakNumericString . T.words
      renderNumber  ts x = if isTransNumber x then format ts x else x

      format tm = case f of
                    Ordinal     -> maybe "" (ordinal     tm v) . safeRead
                    LongOrdinal -> maybe "" (longOrdinal tm v) . safeRead
                    Roman       -> maybe ""
                                   (\x -> if x < 6000 then roman x else T.pack $ show x) .
                                   safeRead
                    _           -> maybe "" (T.pack . show) .
                                         (safeRead :: T.Text -> Maybe Int)

      roman :: Int -> Text
      roman     = T.concat . reverse . zipWith (!!) romanList .
                  map (readNum . T.singleton) . take 4 .
                  reverse . show
      romanList = [[ "", "i", "ii", "iii", "iv", "v", "vi", "vii", "viii", "ix" ]
                  ,[ "", "x", "xx", "xxx", "xl", "l", "lx", "lxx", "lxxx", "xc" ]
                  ,[ "", "c", "cc", "ccc", "cd", "d", "dc", "dcc", "dccc", "cm" ]
                  ,[ "", "m", "mm", "mmm", "mmmm", "mmmmm"]
                  ]


checkRange :: [CslTerm] -> Text -> Text
checkRange ts txt = case T.uncons txt of
  Just (x,xs) -> if x == '-' || x == '\x2013'
                 then pageRange ts <> checkRange ts xs
                 else T.cons x $ checkRange ts xs
  Nothing -> ""

printNumStr :: [Text] -> Text
printNumStr []  = ""
printNumStr [x] = x
printNumStr (x:"-":y:xs) = T.concat [x, "-" , y, printNumStr xs]
printNumStr (x:",":y:xs) = T.concat [x, ", ", y, printNumStr xs]
printNumStr (x:xs)
    | x == "-"  = x <>        printNumStr xs
    | otherwise = x <> " " <> printNumStr xs

pageRange :: [CslTerm] -> Text
pageRange = maybe "\x2013" termPlural . findTerm "page-range-delimiter" Long

isNumericString :: Text -> Bool
isNumericString "" = False
isNumericString s  = all (\c -> isNumber c || isSpecialChar c) $ T.words s

isTransNumber, isSpecialChar,isNumber :: Text -> Bool
isTransNumber = T.all isDigit
isSpecialChar = T.all (`elem` ("&-,.\x2013" :: String))
isNumber   cs = case [c | c <- T.unpack cs
                        , not (isLetter c)
                        , c `notElem` ("&-.,\x2013" :: String)] of
                     [] -> False
                     xs -> all isDigit xs

breakNumericString :: [Text] -> [Text]
breakNumericString [] = []
breakNumericString (x:xs)
    | isTransNumber x = x : breakNumericString xs
    | otherwise       = let (a,b) = T.break (`elem` ("&-\x2013," :: String)) x
                            (c,d) = if T.null b
                                       then ("","")
                                       else T.span (`elem` ("&-\x2013," :: String)) b
                        in filter (not . T.null) $
                           a : c : breakNumericString (d : xs)

formatRange :: Formatting -> Text -> State EvalState [Output]
formatRange _ "" = return []
formatRange fm p = do
  ops <- gets (options . env)
  ts  <- gets (terms . env)
  let opt = getOptionVal "page-range-format" ops
      pages = tupleRange . breakNumericString . T.words $ p

      tupleRange :: [Text] -> [(Text, Text)]
      tupleRange [] = []
      tupleRange [x, cs]
        | cs `elem` ["-", "--", "\x2013"] = return (x,"")
      tupleRange (x:cs:y:xs)
        | cs `elem` ["-", "--", "\x2013"] = (x, y) : tupleRange xs
      tupleRange (x:      xs) = (x,"") : tupleRange xs

      joinRange (a, "") = a
      joinRange (a,  b) = a <> "-" <> b

      process = checkRange ts . printNumStr . case opt of
                 "expanded"    -> map (joinRange . expandedRange)
                 "chicago"     -> map (joinRange . chicagoRange )
                 "minimal"     -> map (joinRange . minimalRange 1)
                 "minimal-two" -> map (joinRange . minimalRange 2)
                 _             -> map joinRange
  return [OLoc [OStr (process pages) emptyFormatting] fm]

-- Abbreviated page ranges are expanded to their non-abbreviated form:
-- 42–45, 321–328, 2787–2816
expandedRange :: (Text, Text) -> (Text, Text)
expandedRange (sa, "") = (sa,"")
expandedRange (sa, sb)
  | T.length sb < T.length sa =
      case (safeRead sa, safeRead sb) of
           -- check to make sure we have regular numbers
           (Just (_ :: Int), Just (_ :: Int)) ->
             (sa, T.take (T.length sa - T.length sb) sa <> sb)
           _ -> (sa, sb)
  | otherwise = (sa, sb)

-- All digits repeated in the second number are left out:
-- 42–5, 321–8, 2787–816.  The minDigits parameter indicates
-- a minimum number of digits for the second number; thus, with
-- minDigits = 2, we have 328-28.
minimalRange :: Int -> (Text, Text) -> (Text, Text)
minimalRange minDigits (a,b) =
  case T.commonPrefixes a b of
    Just (_, a', b') | T.length a' == T.length b' ->
                       (a, T.takeEnd (max minDigits (T.length b')) b)
    _ -> (a, b)

-- Page ranges are abbreviated according to the Chicago Manual of Style-rules:
-- First number             Second number    Examples
-- Less than 100            Use all digits   3–10; 71–72
-- 100 or multiple of 100   Use all digits   100–104; 600–613; 1100–1123
-- 101 through 109 (in multiples of 100) Use changed part only  10002-6, 505-17
-- 110 through 199          Use 2 digits or more  321-25, 415-532
-- if numbers are 4 digits long or more and 3 digits change, use all digits
--         1496-1504
chicagoRange :: (Text, Text) -> (Text, Text)
chicagoRange (sa, sb)
    = case (safeRead sa :: Maybe Int) of
          Just n | n < 100 -> expandedRange (sa, sb)
                 | n `mod` 100 == 0 -> expandedRange (sa, sb)
                 | n >= 1000 -> let (sa', sb') = minimalRange 1 (sa, sb)
                                in  if T.length sb' >= 3
                                       then expandedRange (sa, sb)
                                       else (sa', sb')
                  | n > 100 -> if n `mod` 100 < 10
                                 then minimalRange 1 (sa, sb)
                                 else minimalRange 2 (sa, sb)
          _ -> expandedRange (sa, sb)
