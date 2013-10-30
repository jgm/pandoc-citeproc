{-# LANGUAGE OverloadedStrings, PatternGuards, DeriveDataTypeable,
    ScopedTypeVariables, FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.CSL.Style
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Andrea Rossato <andrea.rossato@unitn.it>
-- Stability   :  unstable
-- Portability :  unportable
--
-- The Style types
--
-----------------------------------------------------------------------------

module Text.CSL.Style where

import Text.Pandoc.UTF8 (fromStringLazy)
import Data.Aeson hiding (Number)
import Control.Arrow
import Control.Applicative hiding (Const)
import Data.List ( nubBy, isPrefixOf, isInfixOf )
import Data.Generics ( Typeable, Data, everywhere
                     , everywhere', everything, mkT, mkQ )
import Data.Maybe ( listToMaybe )
import qualified Data.Map as M
import Text.Pandoc.Definition hiding (Citation, Cite)
import Data.Char (isPunctuation)
import Text.CSL.Util (mb, parseBool, (.#?), (.#:), readNum)
import qualified Data.Text as T

#ifdef UNICODE_COLLATION
import qualified Data.Text     as T
import qualified Data.Text.ICU as T
#else
import Data.RFC5051 (compareUnicode)
#endif
import Text.CSL.Pickle
import Text.CSL.Data    ( getLocale )
import Data.Char        ( isUpper, toUpper, toLower )
import qualified Data.Vector as V
import Data.Maybe       ( catMaybes                 )
import qualified Data.ByteString.Lazy as L
#ifdef USE_NETWORK
import Network.HTTP ( getResponseBody, mkRequest, RequestMethod(..) )
import Network.Browser ( browse, setAllowRedirects, setUserAgent, request )
import Network.URI ( parseURI, URI(..) )
#endif
import qualified Data.Text.Encoding as T

-- | The representation of a parsed CSL style.
data Style
    = Style
      { styleVersion       ::  String
      , styleClass         ::  String
      , styleInfo          ::  Maybe CSInfo
      , styleDefaultLocale ::  String
      , styleLocale        :: [Locale]
      , styleAbbrevs       :: Abbreviations
      , csOptions          :: [Option]
      , csMacros           :: [MacroMap]
      , citation           ::  Citation
      , biblio             ::  Maybe Bibliography
      } deriving ( Show, Read, Typeable, Data )

instance FromJSON Style where
  parseJSON (String s) = return $ parseCSL' $ L.fromChunks [T.encodeUtf8 s]
  parseJSON _ = fail "Could not parse Style"

data Locale
    = Locale
      { localeVersion :: String
      , localeLang    :: String
      , localeOptions :: [Option]
      , localeTerms   :: [CslTerm]
      , localeDate    :: [Element]
      } deriving ( Show, Read, Eq, Typeable, Data )

-- | With the 'defaultLocale', the locales-xx-XX.xml loaded file and
-- the parsed 'Style' cs:locale elements, produce the final 'Locale'
-- as the only element of a list, taking into account CSL locale
-- prioritization.
mergeLocales :: String -> Locale -> [Locale] -> [Locale]
mergeLocales s l ls = doMerge list
    where
      list = filter ((==) s . localeLang) ls ++
             filter ((\x -> x /= [] && x `isPrefixOf` s) . localeLang) ls ++
             filter ((==) [] . localeLang) ls
      doMerge x = return l { localeOptions = newOpt     x
                           , localeTerms   = newTerms   x
                           , localeDate    = newDate    x
                           }
      cht          = cslTerm &&& termForm &&& termGenderForm
      checkedLoc   = if hasOrdinals ls then rmOrdinals (localeTerms l) else localeTerms l
      newTerms   x = nubBy (\a b -> cht a == cht b) (concatMap localeTerms   x ++ checkedLoc)
      newOpt     x = nubBy (\a b -> fst a == fst b) (concatMap localeOptions x ++ localeOptions l)
      newDate    x = nubBy (\(Date _ a _ _ _ _)
                             (Date _ b _ _ _ _) -> a == b) (concatMap localeDate x ++ localeDate l)

data CslTerm
    = CT
      { cslTerm        :: String
      , termForm       :: Form
      , termGender     :: Gender
      , termGenderForm :: Gender
      , termSingular   :: String
      , termPlural     :: String
      , termMatch      :: String
      } deriving ( Show, Read, Eq, Typeable, Data )

newTerm :: CslTerm
newTerm = CT [] Long Neuter Neuter [] [] []

findTerm :: String -> Form -> [CslTerm] -> Maybe CslTerm
findTerm s f
    = listToMaybe . filter (cslTerm &&& termForm >>> (==) (s, f))

findTerm' :: String -> Form -> Gender -> [CslTerm] -> Maybe CslTerm
findTerm' s f g
    = listToMaybe . filter (cslTerm &&& termForm &&& termGenderForm >>> (==) (s,(f,g)))

hasOrdinals :: Data a => a -> Bool
hasOrdinals = or . query hasOrd
    where
      hasOrd o
          | CT {cslTerm = t} <- o
          , "ordinal" `isInfixOf` t = [True]
          | otherwise               = [False]

rmOrdinals :: Data a => a -> a
rmOrdinals = proc' doRemove
    where
      doRemove [] = []
      doRemove (o:os)
          | CT {cslTerm = t} <- o
          , "ordinal" `isInfixOf` t =   doRemove os
          | otherwise               = o:doRemove os

newtype Abbreviations = Abbreviations {
           unAbbreviations :: M.Map String (M.Map String (M.Map String String))
           } deriving ( Show, Read, Typeable, Data )

instance FromJSON Abbreviations where
  parseJSON (Object v)   = Abbreviations <$> parseJSON (Object v)
  parseJSON (Bool False) = return $ Abbreviations M.empty
  parseJSON _            = fail "Could not read Abbreviations"

type MacroMap
    = (String,[Element])

data Citation
    = Citation
      { citOptions :: [Option]
      , citSort    :: [Sort]
      , citLayout  ::  Layout
      } deriving ( Show, Read, Typeable, Data )

data Bibliography
    = Bibliography
      { bibOptions :: [Option]
      , bibSort    :: [Sort]
      , bibLayout  ::  Layout
      } deriving ( Show, Read, Typeable, Data )

type Option = (String,String)

mergeOptions :: [Option] -> [Option] -> [Option]
mergeOptions os = nubBy (\x y -> fst x == fst y) . (++) os

data Layout
    = Layout
      { layFormat ::  Formatting
      , layDelim  ::  Delimiter
      , elements  :: [Element]
      } deriving ( Show, Read, Typeable, Data )

data Element
    = Choose       IfThen    [IfThen]    [Element]
    | Macro        String                 Formatting
    | Const        String                 Formatting
    | Variable    [String]    Form        Formatting Delimiter
    | Term         String     Form        Formatting Bool
    | Label        String     Form        Formatting Plural
    | Number       String     NumericForm Formatting
    | Names       [String]   [Name]       Formatting Delimiter [Element]
    | Substitute  [Element]
    | Group        Formatting Delimiter  [Element]
    | Date        [String]    DateForm    Formatting Delimiter [DatePart] String
      deriving ( Show, Read, Eq, Typeable, Data )

data IfThen
    = IfThen Condition Match [Element]
      deriving ( Eq, Show, Read, Typeable, Data )

data Condition
    = Condition
      { isType          :: [String]
      , isSet           :: [String]
      , isNumeric       :: [String]
      , isUncertainDate :: [String]
      , isPosition      :: [String]
      , disambiguation  :: [String]
      , isLocator       :: [String]
      } deriving ( Eq, Show, Read, Typeable, Data )

type Delimiter = String

data Match
    = Any
    | All
    | None
      deriving ( Show, Read, Eq, Typeable, Data )

match :: Match -> [Bool] -> Bool
match All  = and
match Any  = or
match None = and . map not

data DatePart
    = DatePart
      { dpName       :: String
      , dpForm       :: String
      , dpRangeDelim :: String
      , dpFormatting :: Formatting
      } deriving ( Show, Read, Eq, Typeable, Data )

defaultDate :: [DatePart]
defaultDate
    = [ DatePart "year"  "" "-" emptyFormatting
      , DatePart "month" "" "-" emptyFormatting
      , DatePart "day"   "" "-" emptyFormatting]

data Sort
    = SortVariable String Sorting
    | SortMacro    String Sorting Int Int String
      deriving ( Eq, Show, Read, Typeable, Data )

data Sorting
    = Ascending  String
    | Descending String
      deriving ( Read, Show, Eq, Typeable, Data )

instance Ord Sorting where
    compare (Ascending  []) (Ascending  []) = EQ
    compare (Ascending  []) (Ascending   _) = GT
    compare (Ascending   _) (Ascending  []) = LT
    compare (Ascending   a) (Ascending   b) = compare' a b
    compare (Descending []) (Descending []) = EQ
    compare (Descending []) (Descending  _) = GT
    compare (Descending  _) (Descending []) = LT
    compare (Descending  a) (Descending  b) = compare' b a
    compare              _               _  = EQ

compare' :: String -> String -> Ordering
compare' x y
    = case (x, y) of
        ('-':_,'-':_) -> comp (dropPunct y) (dropPunct x)
        ('-':_, _ )   -> LT
        (_  ,'-':_)   -> GT
        _             -> comp (dropPunct x) (dropPunct y)
      where
        dropPunct = dropWhile isPunctuation
#ifdef UNICODE_COLLATION
        comp a b = T.collate (T.collator T.Current) (T.pack a) (T.pack b)
#else
        comp a b = compareUnicode a b
#endif

data Form
    = Long
    | Short
    | Count
    | Verb
    | VerbShort
    | Symbol
    | NotSet
      deriving ( Eq, Show, Read, Typeable, Data )

data Gender
    = Feminine
    | Masculine
    | Neuter
      deriving ( Eq, Show, Read, Typeable, Data )

data NumericForm
    = Numeric
    | Ordinal
    | Roman
    | LongOrdinal
      deriving ( Eq, Show, Read, Typeable, Data )

data DateForm
    = TextDate
    | NumericDate
    | NoFormDate
      deriving ( Eq, Show, Read, Typeable, Data )

data Plural
    = Contextual
    | Always
    | Never
      deriving ( Eq, Show, Read, Typeable, Data )

data Name
    = Name      Form Formatting NameAttrs Delimiter [NamePart]
    | NameLabel Form Formatting Plural
    | EtAl           Formatting String
      deriving ( Eq, Show, Read, Typeable, Data )

type NameAttrs = [(String, String)]

data NamePart
    = NamePart String Formatting
      deriving ( Show, Read, Eq, Typeable, Data )

isPlural :: Plural -> Int -> Bool
isPlural p l
    = case p of
        Always     -> True
        Never      -> False
        Contextual -> l > 1

isName :: Name -> Bool
isName x = case x of Name {} -> True; _ -> False

isNames :: Element -> Bool
isNames x = case x of Names {} -> True; _ -> False

hasEtAl :: [Name] -> Bool
hasEtAl = not . null . query getEtAl
    where getEtAl n
              | EtAl _ _ <- n = [n]
              | otherwise     = []

data Formatting
    = Formatting
      { prefix         :: String
      , suffix         :: String
      , fontFamily     :: String
      , fontStyle      :: String
      , fontVariant    :: String
      , fontWeight     :: String
      , textDecoration :: String
      , verticalAlign  :: String
      , textCase       :: String
      , display        :: String
      , quotes         :: Quote
      , stripPeriods   :: Bool
      , noCase         :: Bool
      , noDecor        :: Bool
      } deriving ( Read, Eq, Ord, Typeable, Data )

instance Show Formatting where show _ = "emptyFormatting"

rmTitleCase :: Formatting -> Formatting
rmTitleCase f
    | Formatting _ _ _ _ _ _ _ _ "title" _ _ _ _ _ <- f = f {textCase = []}
    | otherwise                                         = f

data Quote
    = NativeQuote
    | ParsedQuote
    | NoQuote
    deriving ( Read, Eq, Ord, Typeable, Data )

emptyFormatting :: Formatting
emptyFormatting
    = Formatting [] [] [] [] [] [] [] [] [] [] NoQuote False False False

unsetAffixes :: Formatting -> Formatting
unsetAffixes f = f {prefix = [], suffix = []}

mergeFM :: Formatting -> Formatting -> Formatting
mergeFM (Formatting aa ab ac ad ae af ag ah ai aj ak al am an)
        (Formatting ba bb bc bd be bf bg bh bi bj bk bl bm bn) =
                   Formatting (ba `betterThan` aa)
                              (bb `betterThan` ab)
                              (bc `betterThan` ac)
                              (bd `betterThan` ad)
                              (be `betterThan` ae)
                              (bf `betterThan` af)
                              (bg `betterThan` ag)
                              (bh `betterThan` ah)
                              (bi `betterThan` ai)
                              (bj `betterThan` aj)
                              (if bk == NoQuote then ak else bk)
                              (bl || al)
                              (bm || am)
                              (bn || an)

data CSInfo
    = CSInfo
      { csiTitle      :: String
      , csiAuthor     :: CSAuthor
      , csiCategories :: [CSCategory]
      , csiId         :: String
      , csiUpdated    :: String
      } deriving ( Show, Read, Typeable, Data )

data CSAuthor   = CSAuthor   String String String deriving ( Show, Read, Eq, Typeable, Data )
data CSCategory = CSCategory String String String deriving ( Show, Read, Eq, Typeable, Data )

-- | The formatted output, produced after post-processing the
-- evaluated citations.
type FormattedOutput = [Inline]

formattingToAttr :: Formatting -> Attr
formattingToAttr f = ("", [], kvs)
  where kvs = filter (\(_, v) -> not (null v))
         [ ("csl-prefix", prefix f)
         , ("csl-suffix", suffix f)
         , ("csl-font-family", fontFamily f)
         , ("csl-font-style", fontStyle f)
         , ("csl-font-variant", fontVariant f)
         , ("csl-font-weight", fontWeight f)
         , ("csl-text-decoration", textDecoration f)
         , ("csl-vertical-align", verticalAlign f)
         , ("csl-text-case", textCase f)
         , ("csl-display", display f)
         , ("csl-quotes", case quotes f of{ NativeQuote -> "native-quote";
                                        ParsedQuote -> "parsed-quote";
                                        NoQuote     -> ""})
         , ("csl-strip-periods", if stripPeriods f then "true" else "")
         , ("csl-no-case", if noCase f then "true" else "")
         , ("csl-no-decor", if noDecor f then "true" else "")
         ]

data CiteprocError
   = NoOutput
   | ReferenceNotFound String
   deriving ( Eq, Ord, Show, Typeable, Data )

-- | The 'Output' generated by the evaluation of a style. Must be
-- further processed for disambiguation and collapsing.
data Output
    = ONull
    | OSpace
    | OPan    [Inline]
    | ODel     String                                   -- ^ A delimiter string.
    | OStr     String             Formatting            -- ^ A simple 'String'
    | OErr     CiteprocError                            -- ^ Warning message
    | OLabel   String             Formatting            -- ^ A label used for roles
    | ONum     Int                Formatting            -- ^ A number (used to count contributors)
    | OCitNum  Int                Formatting            -- ^ The citation number
    | ODate   [Output]                                  -- ^ A (possibly) ranged date
    | OYear    String    String   Formatting            -- ^ The year and the citeId
    | OYearSuf String    String   [Output]   Formatting -- ^ The year suffix, the citeId and a holder for collision data
    | OName    String   [Output] [[Output]]  Formatting -- ^ A (family) name with the list of given names.
    | OContrib String    String   [Output] [Output] [[Output]] -- ^ The citation key, the role (author, editor, etc.), the contributor(s),
                                                        -- the output needed for year suf. disambiguation, and everything used for
                                                        -- name disambiguation.
    | OUrl    Target              Formatting            -- ^ An URL
    | OLoc    [Output]            Formatting            -- ^ The citation's locator
    | Output  [Output]            Formatting            -- ^ Some nested 'Output'
      deriving ( Eq, Ord, Show, Typeable, Data )

data Affix
    = PlainText String
    | PandocText [Inline]
      deriving ( Show, Read, Eq, Ord, Typeable, Data )

instance FromJSON Affix where
  parseJSON (String s) = pure $ PlainText (T.unpack s)
  parseJSON _          = fail "Could not parse affix"

type Citations = [[Cite]]
data Cite
    = Cite
      { citeId         :: String
      , citePrefix     :: Affix
      , citeSuffix     :: Affix
      , citeLabel      :: String
      , citeLocator    :: String
      , citeNoteNumber :: String
      , citePosition   :: String
      , nearNote       :: Bool
      , authorInText   :: Bool
      , suppressAuthor :: Bool
      , citeHash       :: Int
      } deriving ( Show, Eq, Typeable, Data )

instance FromJSON Cite where
  parseJSON (Object v) = Cite <$>
              v .#: "id" <*>
              v .:? "prefix" .!= PlainText "" <*>
              v .:? "suffix" .!= PlainText "" <*>
              v .#? "label" .!= "" <*>
              v .#? "locator"  .!= "" <*>
              v .#? "note-number" .!= "" <*>
              v .#? "position" .!= "" <*>
              (v .:? "near-note" >>= mb parseBool) .!= False <*>
              (v .:? "author-in-text" >>= mb parseBool) .!= False <*>
              (v .:? "suppress-author" >>= mb parseBool) .!= False <*>
              v .:? "cite-hash" .!= 0
  parseJSON _ = fail "Could not parse Cite"

instance FromJSON [[Cite]] where
  parseJSON (Array v) = mapM parseJSON $ V.toList v
  parseJSON _ = return []

emptyAffix :: Affix
emptyAffix = PlainText []

emptyCite :: Cite
emptyCite  = Cite [] emptyAffix emptyAffix [] [] [] [] False False False 0

-- | A citation group: the first list has a single member when the
-- citation group starts with an "author-in-text" cite, the
-- 'Formatting' to be applied, the 'Delimiter' between individual
-- citations and the list of evaluated citations.
data CitationGroup = CG [(Cite, Output)] Formatting Delimiter [(Cite, Output)] deriving ( Show, Eq, Typeable, Data )

data BiblioData
    = BD
      { citations    :: [[FormattedOutput]]
      , bibliography :: [[FormattedOutput]]
      } deriving ( Show )

-- | A record with all the data to produce the 'FormattedOutput' of a
-- citation: the citation key, the part of the formatted citation that
-- may be colliding with other citations, the form of the citation
-- when a year suffix is used for disambiguation , the data to
-- disambiguate it (all possible contributors and all possible given
-- names), and, after processing, the disambiguated citation and its
-- year, initially empty.
data CiteData
    = CD
      { key        ::   String
      , collision  ::  [Output]
      , disambYS   ::  [Output]
      , disambData :: [[Output]]
      , disambed   ::  [Output]
      , sameAs     ::  [String]
      , citYear    ::   String
      } deriving ( Show, Typeable, Data )

instance Eq CiteData where
    (==) (CD ka ca _ _ _ _ _)
         (CD kb cb _ _ _ _ _) = ka == kb && ca == cb

data NameData
    = ND
      { nameKey        ::   String
      , nameCollision  ::  [Output]
      , nameDisambData :: [[Output]]
      , nameDataSolved ::  [Output]
      } deriving ( Show, Typeable, Data )

instance Eq NameData where
    (==) (ND ka ca _ _)
         (ND kb cb _ _) = ka == kb && ca == cb

formatOutputList :: [Output] -> FormattedOutput
formatOutputList = concatMap formatOutput

-- | Convert evaluated 'Output' into 'FormattedOutput', ready for the
-- output filters.
formatOutput :: Output -> FormattedOutput
formatOutput o =
  case o of
      OSpace              -> [Space]
      OPan     i          -> i
      ODel     []         -> []
      ODel     " "        -> [Space]
      ODel     s          -> [Str s]
      OStr     []      _  -> []
      OStr     s       f  -> case formattingToAttr f of
                                     ("",[],[]) -> [Str s]
                                     attr       -> [Span attr [Str s]]
      OErr NoOutput       -> [Span ("",["citeproc-no-output"],[])
                                     [Strong [Str "???"]]]
      OErr (ReferenceNotFound r)
                          -> [Span ("",["citeproc-not-found"],
                                            [("data-reference-id",r)])
                                     [Strong [Str "???"]]]
      OLabel   []      _  -> []
      OLabel   s       f  -> formatOutput (OStr s f)
      ODate    os         -> format os
      OYear    s _     f  -> formatOutput (OStr s f)
      OYearSuf s _ _   f  -> formatOutput (OStr s f)
      ONum     i       f  -> formatOutput (OStr (show i) f)
      OCitNum  i       f  -> formatOutput (OStr (add00 i) f)
      OUrl     s       f  -> [Link (formatOutput (OStr (fst s) f)) s]
      OName  _ os _    f  -> formatOutput (Output os f)
      OContrib _ _ os _ _ -> format os
      OLoc     os      f  -> formatOutput (Output os f)
      Output   os      f  -> case formattingToAttr f of
                                     ("",[],[]) -> format os
                                     attr       -> [Span attr $ format os]
      _                   -> []
    where
      format = concatMap formatOutput
      add00  = reverse . take 5 . flip (++) (repeat '0') . reverse . show

-- | Map the evaluated output of a citation group.
mapGroupOutput :: (Output -> [a]) -> CitationGroup -> [a]
mapGroupOutput f (CG _ _ _ os) = concatMap f $ map snd os

-- | A generic processing function.
proc :: (Typeable a, Data b) => (a -> a) -> b -> b
proc f = everywhere (mkT f)

-- | A generic processing function: process a data structure in
-- top-down manner.
proc' :: (Typeable a, Data b) => (a -> a) -> b -> b
proc' f = everywhere' (mkT f)

-- | A generic query function.
query :: (Typeable a, Data b) => (a -> [c]) -> b -> [c]
query f = everything (++) ([] `mkQ` f)

-- | Removes all given names form a 'OName' element with 'proc'.
rmGivenNames :: Output -> Output
rmGivenNames o
    | OName i s _ f <- o = OName i s [] f
    | otherwise          = o

rmNameHash :: Output -> Output
rmNameHash o
    | OName _ s ss f <- o = OName [] s ss f
    | otherwise           = o

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

betterThan :: [a] -> [a] -> [a]
betterThan [] b = b
betterThan a  _ = a

-- following was Text.CSL.Parser:

-- | Read and parse a CSL style file into a localized sytle.
readCSLFile :: FilePath -> IO Style
readCSLFile src = do
#ifdef USE_NETWORK
  let readURI u = do rsp <- browse $ do
                              setAllowRedirects True
                              setUserAgent "citeproc-hs"
                              request $ mkRequest GET u
                     getResponseBody (Right $ snd rsp)
  f <- case parseURI src of
         Just u | uriScheme u `elem` ["http:","https:"] -> readURI u
         _                                              -> readFile' src
#else
  f <- readFile' src
#endif
  localizeCSL $ parseCSL' f

-- | Parse a 'String' into a 'Style' (with default locale).
parseCSL :: String -> Style
parseCSL = parseCSL' . fromStringLazy

parseCSL' :: L.ByteString -> Style
parseCSL' f = readXmlString xpStyle f

-- | Merge locale into a CSL style.
localizeCSL :: Style -> IO Style
localizeCSL s = do
  l <- readXmlString xpLocale `fmap` getLocale (styleDefaultLocale s)
  return s { styleLocale = mergeLocales (styleDefaultLocale s) l (styleLocale s)}

instance XmlPickler Layout where
    xpickle = xpWrap (uncurry3 Layout, \(Layout f d e) -> (f,d,e)) $
              xpIElem "layout" $
              xpTriple xpickle xpDelimiter xpickle

instance XmlPickler Element where
    xpickle = xpAlt tag ps
        where
          tag (Choose       {}) =  0
          tag (Macro        {}) =  1
          tag (Const        {}) =  2
          tag (Variable     {}) =  4
          tag (Term         {}) =  5
          tag (Label        {}) =  6
          tag (Names        {}) =  7
          tag (Substitute   {}) =  9
          tag (Group        {}) = 10
          tag (Number       {}) = 11
          tag (Date         {}) = 12
          ps = [ xpChoose
               , xpMacro
               , xpConst
               , xpVariable
               , xpTerm
               , xpLabel
               , xpNames
               , xpSubStitute
               , xpGroup
               , xpNumber
               , xpDate
               ]

instance XmlPickler IfThen where
    xpickle = xpWrap (uncurry3 IfThen, \(IfThen c m e) -> (c,m,e)) $
              xpTriple xpickle xpickle xpickle

instance XmlPickler Condition where
    xpickle = xpWrap ( \ ((t,v,n),(d,p,a,l)) ->
                           Condition (words t) (words v) (words n)
                                     (words d) (words p) (words a) (words l),
                       \ (Condition t v n d p a l) ->
                           ((unwords t,unwords v,unwords n)
                           ,(unwords d,unwords p,unwords a,unwords l))) $
              xpPair (xpTriple (xpAttrText' "type"             )
                               (xpAttrText' "variable"         )
                               (xpAttrText' "is-numeric"       ))
                     (xp4Tuple (xpAttrText' "is-uncertain-date")
                               (xpAttrText' "position"         )
                               (xpAttrText' "disambiguate"     )
                               (xpAttrText' "locator"          ))

instance XmlPickler Formatting where
    xpickle = xpWrap ( \(((p,s,ff),(fs,fv,fw)),(td,va,tc,d),(q,sp))
                         -> Formatting p s ff fs fv fw td va tc d
                            (if q then NativeQuote else NoQuote) sp False False
                     , \(Formatting p s ff fs fv fw td va tc d _ sp _ _)
                         -> (((p,s,ff),(fs,fv,fw)),(td,va,tc,d),(False,sp))) $
              xpTriple (xpPair (xpTriple (xpAttrText' "prefix"      )
                                         (xpAttrText' "suffix"      )
                                         (xpAttrText' "font-family" ))
                               (xpTriple (xpAttrText' "font-style"  )
                                         (xpAttrText' "font-variant")
                                         (xpAttrText' "font-weight" )))
                       (xp4Tuple (xpAttrText' "text-decoration")
                                 (xpAttrText' "vertical-align" )
                                 (xpAttrText' "text-case"      )
                                 (xpAttrText' "display"        ))
                       (xpPair   (xpAttrWithDefault False "quotes"        xpickle)
                                 (xpAttrWithDefault False "strip-periods" xpickle))

instance XmlPickler Sort where
    xpickle = xpAlt tag ps
        where
          readSort = read . flip (++) " \"\"" . toRead
          tag (SortVariable {}) = 0
          tag (SortMacro    {}) = 1
          ps = [ xpWrap ( \(v,s) -> SortVariable v (readSort s)
                        , \(SortVariable v s) -> (v,toShow $ show s)) $
                 xpElem "key" $
                 xpPair (xpAttrText "variable")
                        (xpAttrWithDefault "ascending" "sort" xpText)

               , xpWrap ( \(v,s,a,b,c) -> SortMacro v (readSort s) (readNum a) (readNum b) c
                        , \(SortMacro v s a b c) -> (v,toShow $ show s,show a,show b, c)) $
                 xpElem "key" $
                 xp5Tuple (xpAttrText "macro")
                          (xpAttrWithDefault "ascending" "sort"            xpText)
                          (xpAttrWithDefault ""          "names-min"       xpText)
                          (xpAttrWithDefault ""          "names-use-first" xpText)
                          (xpAttrWithDefault ""          "names-use-last"  xpText)
               ]

instance XmlPickler Bool where
    xpickle = xpWrap readable xpText

instance XmlPickler Gender where
    xpickle = xpWrap readable xpText

instance XmlPickler Form where
    xpickle = xpWrap readable
                     (xpAttrWithDefault "long" "form" xpText)

instance XmlPickler NumericForm where
    xpickle = xpWrap readable
                     (xpAttrWithDefault "numeric" "form" xpText)

instance XmlPickler DateForm where
    xpickle = xpWrap (read . toRead . flip (++) "-date", const [])
                     (xpAttrWithDefault "no-form" "form" xpText)

instance XmlPickler Match where
    xpickle = xpWrap readable
                     (xpAttrWithDefault "all" "match" xpText)

instance XmlPickler DatePart where
    xpickle = xpWrap (uncurry4 DatePart, \(DatePart s f d fm) -> (s,f,d,fm)) $
              xpElem "date-part" $
              xp4Tuple (xpAttrText "name")
                       (xpAttrWithDefault "long" "form"            xpText)
                       (xpAttrWithDefault "-"    "range-delimiter" xpText)
                        xpickle

instance XmlPickler Name where
    xpickle = xpAlt tag ps
        where
          tag (Name      {}) = 0
          tag (NameLabel {}) = 1
          tag (EtAl      {}) = 2
          ps = [ xpWrap (uncurry5 Name, \(Name f fm nas d nps) -> (f,fm,nas,d,nps)) $
                 xpElem "name"  $ xp5Tuple xpNameForm xpickle xpNameAttrs xpDelimiter xpickle
               , xpWrap (uncurry3 NameLabel, \(NameLabel f fm p) -> (f, fm,p)) $
                 xpElem "label" $ xpTriple xpickle xpickle xpPlural
               , xpWrap (uncurry EtAl, \(EtAl fm t) -> (fm,t)) $
                 xpElem "et-al" $ xpPair xpickle $ xpAttrText' "term"
               ]
          xpNameForm = xpWrap readable $ xpAttrWithDefault "not-set" "form" xpText

instance XmlPickler NamePart where
    xpickle = xpWrap (uncurry NamePart, \(NamePart s fm) -> (s,fm)) $
              xpElem "name-part" $
              xpPair (xpAttrText "name")
                      xpickle

instance XmlPickler CSInfo where
    xpickle = xpWrap ( \ ((t,i,u),(a,c)) -> CSInfo t a c i u
                     , \ s -> ((csiTitle s,  csiId s, csiUpdated s)
                              ,(csiAuthor s, csiCategories s))) $
              xpPair (xpTriple (get "title"  )
                               (get "id"     )
                               (get "updated"))
                     (xpPair   (xpIElemWithDefault (CSAuthor   "" "" "") "author" xpickle)
                               (xpDefault [] $ xpList $ xpIElem "category" xpickle))
                  where
                    get = flip xpIElem xpText

instance XmlPickler CSAuthor where
    xpickle = xpWrap   (uncurry3 CSAuthor, \(CSAuthor a b c) -> (a, b, c)) $
              xpTriple (xpIElemWithDefault [] "name"  xpText)
                       (xpIElemWithDefault [] "email" xpText)
                       (xpIElemWithDefault [] "uri"   xpText)

instance XmlPickler CSCategory where
    xpickle = xpWrap   (uncurry3 CSCategory, \(CSCategory a b c) -> (a, b, c)) $
              xpTriple (xpAttrText  "term"  )
                       (xpAttrText' "schema")
                       (xpAttrText' "label" )

xpStyle :: PU Style
xpStyle
    = xpWrap ( \ ((v,sc,si,sl,l),(o,m,c,b))   -> Style v sc si sl l (Abbreviations M.empty) o m c b
             , \ (Style v sc si sl l _ o m c b) -> ((v,sc,si,sl,l),(o,m,c,b))) $
      xpIElem "style" $
      xpPair (xp5Tuple (xpAttrText "version")
                       (xpAttrText "class")
                        xpInfo
                       (xpAttrWithDefault "en-US" "default-locale" xpText)
                       (xpList xpLocale))
             (xp4Tuple  xpStyleOpts
                        xpMacros
                        xpCitation
                       (xpOption xpBibliography))

xpInfo :: PU (Maybe CSInfo)
xpInfo  = xpOption . xpIElem "info" $ xpickle

xpLocale :: PU Locale
xpLocale
    = xpWrap ( \ ((v,l),(o,t,d))   -> Locale v l o t d
             , \ (Locale v l o t d) -> ((v,l),(o,t,d))) $
      xpIElem "locale" $
      xpPair (xpPair   (xpAttrText' "version" )
                       (xpAttrText' "lang"))
             (xpTriple (xpIElemWithDefault [] "style-options" $  xpOpt "punctuation-in-quote")
                        xpTerms
                        (xpList xpLocaleDate))

xpTerms :: PU [CslTerm]
xpTerms
    = xpWrap (concat,return) $ xpList $
      xpIElem "terms" $ xpList $ xpElem "term" $
      xpWrap (\((n,f,g,gf,m),(s,p)) -> CT n f g gf s p m,
             undefined) $
             xpPair (xp5Tuple (xpAttrText "name")
                              xpickle
                              (xpAttrWithDefault Neuter "gender" xpickle)
                              (xpAttrWithDefault Neuter "gender-form" xpickle)
                              (xpAttrText' "match"))
                    (xpChoice (xpWrap (\s -> (s,s), fst)  xpText0)
                              (xpPair (xpIElem "single"   xpText0)
                              (xpIElem "multiple" xpText0))
                              xpLift)

xpMacros :: PU [MacroMap]
xpMacros
    = xpList $ xpIElem "macro" $
      xpPair (xpAttrText "name") xpickle

xpCitation :: PU Citation
xpCitation
    = xpWrap (uncurry3 Citation, \(Citation o s l) -> (o,s,l)) $
      xpIElem "citation" $
      xpTriple xpCitOpts xpSort xpickle

xpBibliography :: PU Bibliography
xpBibliography
    = xpWrap (uncurry3 Bibliography, \(Bibliography o s l) -> (o,s,l)) $
      xpIElem "bibliography" $
      xpTriple xpBibOpts xpSort xpickle

xpOpt :: String -> PU [Option]
xpOpt n
    = xpWrap (\a -> filter ((/=) [] . snd) $ [(n,a)], const []) $
      xpAttrText' n

xpNamesOpt :: PU [Option]
xpNamesOpt = xpOpt "names-delimiter"

xpNameFormat :: PU [Option]
xpNameFormat
    = xpWrap (\(a,b,c,d,e,f) ->
                  catMaybes [ checkOpt "and"                     a
                            , checkOpt "delimiter-precedes-last" b
                            , checkOpt "sort-separator"          c
                            , checkOpt "initialize"              d
                            , checkOpt "initialize-with"         e
                            , checkOpt "name-as-sort-order"      f
                            ] , const (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)) $
      xp6Tuple (getOpt "and")
               (getOpt "delimiter-precedes-last")
               (getOpt "sort-separator")
               (getOpt "initialize")
               (getOpt "initialize-with")
               (getOpt "name-as-sort-order")

    where
      getOpt n = xpOption $ xpAttr n xpText
      checkOpt _ Nothing  = Nothing
      checkOpt n (Just s) = Just (n,s)

xpNameAttrs :: PU NameAttrs
xpNameAttrs
    = xpWrap (\((a,b,c,d,e),(f,g)) ->
                  filter ((/=) [] . snd) [("et-al-min",a)
                                         ,("et-al-use-first",b)
                                         ,("et-al-subsequent-min",c)
                                         ,("et-al-subsequent-use-first",d)
                                         ,("et-al-use-last",e)
                                         ,("delimiter-precedes-et-al",f)] ++ g
             , const (([],[],[],[],[]),([],[]))) $
      xpPair (xp5Tuple (xpAttrText' "et-al-min")
                       (xpAttrText' "et-al-use-first")
                       (xpAttrText' "et-al-subsequent-min")
                       (xpAttrText' "et-al-subsequent-use-first")
                       (xpAttrText' "et-al-use-last")) $
              xpPair   (xpAttrText' "delimiter-precedes-et-al")
                       xpNameFormat

xpNameOpt :: PU [Option]
xpNameOpt
    = xpWrap (\(a,b,c) ->
                  filter ((/=) [] . snd) $ a ++ [("name-delimiter",b)
                                                ,("name-form",c)], const ([],[],[])) $
      xpTriple  xpNameAttrs
               (xpAttrText' "name-delimiter")
               (xpAttrText' "name-form")

xpBibOpts :: PU [Option]
xpBibOpts
    = xpWrap ( \((a,b,c,d,e,f),(g,h)) ->
                 filter ((/=) [] . snd) $ [("hanging-indent",a)
                                          ,("second-field-align",b)
                                          ,("subsequent-author-substitute",c)
                                          ,("subsequent-author-substitute-rule",d)
                                          ,("line-spacing",e)
                                          ,("entry-spacing",f)] ++ g ++ h
                , const (([],[],[],[],[],[]),([],[]))) $
      xpPair (xp6Tuple (xpAttrText' "hanging-indent")
                       (xpAttrText' "second-field-align")
                       (xpAttrText' "subsequent-author-substitute")
                       (xpAttrText' "subsequent-author-substitute-rule")
                       (xpAttrText' "line-spacing")
                       (xpAttrText' "entry-spacing")) $
             xpPair xpNameOpt xpNamesOpt

xpCitOpts :: PU [Option]
xpCitOpts
    = xpWrap ( \((a,b,c),(d,e,f,g,h,i),(j,k)) ->
                 filter ((/=) [] . snd) $ [("disambiguate-add-names",a)
                                          ,("disambiguate-add-givenname",b)
                                          ,("disambiguate-add-year-suffix",c)
                                          ,("givenname-disambiguation-rule",d)
                                          ,("collapse",e)
                                          ,("cite-group-delimiter",f)
                                          ,("year-suffix-delimiter",g)
                                          ,("after-collapse-delimiter",h)
                                          ,("near-note-distance",i)] ++ j ++ k
                , const (([],[],[]),([],[],[],[],[],[]),([],[]))) $
      xpTriple (xpTriple (xpAttrText' "disambiguate-add-names")
                         (xpAttrText' "disambiguate-add-givenname")
                         (xpAttrText' "disambiguate-add-year-suffix"))
               (xp6Tuple (xpAttrText' "givenname-disambiguation-rule")
                         (xpAttrText' "collapse")
                         (xpAttrText' "cite-group-delimiter")
                         (xpAttrText' "year-suffix-delimiter")
                         (xpAttrText' "after-collapse-delimiter")
                         (xpAttrText' "near-note-distance"))
               (xpPair xpNameOpt xpNamesOpt)

xpStyleOpts :: PU [Option]
xpStyleOpts
    = xpWrap ( \((a,b,c),(d,e)) ->
                 filter ((/=) [] . snd) $ [("page-range-format",a)
                                          ,("demote-non-dropping-particle",b)
                                          ,("initialize-with-hyphen",c)] ++ d ++ e
                , const (([],[],[]),([],[]))) $
      xpPair (xpTriple (xpAttrText' "page-range-format")
                       (xpAttrText' "demote-non-dropping-particle")
                       (xpAttrText' "initialize-with-hyphen")) $
               (xpPair xpNameOpt xpNamesOpt)

xpSort :: PU [Sort]
xpSort
    = xpDefault [] $ xpElem "sort" $ xpList xpickle

xpChoose :: PU Element
xpChoose
    = xpWrap (uncurry3 Choose, \(Choose b t e) -> (b,t,e)) $
      xpElem "choose" $
      xpTriple (                        xpElem "if"      xpickle)
               (xpDefault [] $ xpList $ xpElem "else-if" xpickle)
               (xpDefault []          $ xpElem "else"    xpickle)

xpMacro :: PU Element
xpMacro
    = xpWrap (uncurry Macro, \(Macro s fm) -> (s,fm)) $
      xpTextElem $ xpPair (xpAttrText "macro") xpickle

xpConst :: PU Element
xpConst
    = xpWrap (uncurry Const, \(Const s fm) -> (s,fm)) $
      xpTextElem $ xpPair (xpAttrText "value") xpickle

xpVariable :: PU Element
xpVariable
    = xpWrap ( \((v,f,fm),d)        -> Variable (words v) f fm d
             , \(Variable v f fm d) -> ((unwords v,f,fm),d)) $
      xpTextElem $ xpPair (xpCommon "variable") xpDelimiter

xpTerm :: PU Element
xpTerm
    = xpWrap ( \((t,f,fm),p)    -> Term t f fm p
             , \(Term t f fm p) -> ((t,f,fm),p)) $
      xpTextElem $ xpPair (xpCommon "term") $
                   xpAttrWithDefault True "plural" xpickle

xpNames :: PU Element
xpNames
    = xpWrap ( \((a,n,fm),d,sb)     -> Names (words a) n fm d sb
             , \(Names a n fm d sb) -> ((unwords a,n,fm),d,sb)) $
      xpElem "names" $ xpTriple names xpDelimiter xpickle
    where names    = xpTriple (xpAttrText "variable") xpName xpickle
          xpName   = xpChoice xpZero xpickle check
          check [] = xpLift [Name NotSet emptyFormatting [] [] []]
          check  l = if any isName l then xpLift l else xpZero

xpLabel :: PU Element
xpLabel
    = xpWrap ( uncurry4 Label
             , \(Label s f fm p) -> (s,f,fm,p)) $
      xpElem "label" $
      xp4Tuple (xpAttrText' "variable")
                xpickle xpickle xpPlural

xpSubStitute :: PU Element
xpSubStitute
    = xpWrap (Substitute, \(Substitute es) -> es) $
      xpElem "substitute" xpickle

xpGroup :: PU Element
xpGroup
    = xpWrap (uncurry3 Group, \(Group fm d e) -> (fm,d,e)) $
      xpElem "group" $
      xpTriple xpickle xpDelimiter xpickle

xpNumber :: PU Element
xpNumber
    = xpWrap (uncurry3 Number, \(Number s f fm) -> (s,f,fm)) $
      xpElem "number" $ xpCommon "variable"

xpDate :: PU Element
xpDate
    = xpWrap ( \((s,f,fm),(d,dp,dp'))    -> Date (words s) f fm d dp dp'
             , \(Date s f fm d dp dp') -> ((unwords s,f,fm),(d,dp,dp'))) $
      xpElem  "date" $
      xpPair (xpCommon "variable")
             (xpTriple xpDelimiter xpickle (xpAttrText' "date-parts"))

xpLocaleDate :: PU Element
xpLocaleDate
    = xpWrap ( \((s,f,fm),(d,dp,dp'))    -> Date (words s) f fm d dp dp'
             , \(Date s f fm d dp dp') -> ((unwords s,f,fm),(d,dp,dp'))) $
      xpIElem  "date" $
      xpPair  (xpTriple (xpLift []) xpickle xpickle)
              (xpTriple xpDelimiter xpickle (xpLift []))

xpTextElem :: PU a -> PU a
xpTextElem = xpElem "text"

xpDelimiter :: PU String
xpDelimiter = xpAttrText' "delimiter"

xpPlural :: PU Plural
xpPlural = xpWrap readable $ xpAttrWithDefault "contextual" "plural" xpText

xpCommon :: (XmlPickler b, XmlPickler c) => String -> PU (String,b,c)
xpCommon s = xpTriple (xpAttrText s) xpickle xpickle

-- | For mandatory attributes.
xpAttrText :: String -> PU String
xpAttrText n = xpAttr n xpText

-- | For optional attributes.
xpAttrText' ::  String -> PU String
xpAttrText' n = xpAttrWithDefault [] n xpText

xpAttrWithDefault :: Eq a => a -> String -> PU a -> PU a
xpAttrWithDefault d n = xpDefault d . xpAttr n

xpIElemWithDefault :: Eq a => a -> String -> PU a -> PU a
xpIElemWithDefault d n = xpDefault d . xpIElem n

readable :: (Read a, Show b) => (String -> a, b -> String)
readable =  (read . toRead, toShow . show)

toShow :: String -> String
toShow = foldr g [] . f
    where g    x xs  = if isUpper x then '-' : toLower x : xs else x : xs
          f (  x:xs) = toLower x : xs
          f       [] = []

toRead :: String -> String
toRead    []  = []
toRead (s:ss) = toUpper s : camel ss
    where
      camel x
          | '-':y:ys <- x = toUpper y : camel ys
          | '_':y:ys <- x = toUpper y : camel ys
          |     y:ys <- x =         y : camel ys
          | otherwise     = []
