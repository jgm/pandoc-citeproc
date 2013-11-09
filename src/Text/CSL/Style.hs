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

import Data.Aeson hiding (Number)
import Control.Arrow
import Control.Applicative hiding (Const)
import Data.List ( nubBy, isPrefixOf, isInfixOf, intercalate )
import Data.Generics ( Data, Typeable )
import Data.Maybe ( listToMaybe )
import qualified Data.Map as M
import Text.Pandoc.Definition hiding (Citation, Cite)
import Data.Char (isPunctuation)
import Text.CSL.Util (mb, parseBool, (.#?), (.#:), proc', query,
                      betterThan)
import qualified Data.Text as T

#ifdef UNICODE_COLLATION
import qualified Data.Text     as T
import qualified Data.Text.ICU as T
#else
import Data.RFC5051 (compareUnicode)
#endif
import qualified Data.Vector as V
#ifdef USE_NETWORK
#endif

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

-- custom instance to make debugging output less busy
instance Show Formatting where
  show x
    | x == emptyFormatting = "emptyFormatting"
    | otherwise            = "emptyFormatting{"
        ++ intercalate ", "
           [ k ++ " = " ++ f x |
             (k, f) <- [("prefix", show . prefix)
                       ,("suffix", show . suffix)
                       ,("fontFamily", show . fontFamily)
                       ,("fontStyle", show . fontStyle)
                       ,("fontVariant", show . fontVariant)
                       ,("fontWeight", show . fontWeight)
                       ,("textDecoration", show . textDecoration)
                       ,("verticalAlign", show . verticalAlign)
                       ,("textCase", show . textCase)
                       ,("display", show . display)
                       ,("quotes", show . quotes)
                       ,("stripPeriods", show . stripPeriods)
                       ,("noCase", show . noCase)
                       ,("noDecor", show . noDecor)],
             f x /= f emptyFormatting ]
        ++ "}"

rmTitleCase :: Formatting -> Formatting
rmTitleCase f = f{ textCase = if textCase f == "title" then "" else textCase f  }

data Quote
    = NativeQuote
    | ParsedQuote
    | NoQuote
    deriving ( Show, Read, Eq, Ord, Typeable, Data )

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
      { citations    :: [FormattedOutput]
      , bibliography :: [FormattedOutput]
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
