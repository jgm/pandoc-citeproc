{-# LANGUAGE OverloadedStrings, PatternGuards, DeriveDataTypeable,
    ScopedTypeVariables, FlexibleInstances, DeriveGeneric,
    GeneralizedNewtypeDeriving #-}
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
import GHC.Generics (Generic)
import Data.String
import Data.Monoid (mempty, Monoid, mappend, mconcat)
import Control.Arrow hiding (left, right)
import Control.Applicative hiding (Const)
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Pair)
import Data.List ( nubBy, isPrefixOf, isInfixOf, intercalate )
import Data.List.Split ( splitWhen, wordsBy )
import Data.Generics ( Data, Typeable )
import Data.Maybe ( listToMaybe )
import qualified Data.Map as M
import Data.Char (isPunctuation)
import Text.CSL.Util (mb, parseBool, parseString, (.#?), (.#:), proc', query,
                      betterThan, trimr, tailInline, headInline, initInline,
                      lastInline)
import Text.Pandoc.Definition hiding (Citation, Cite)
import Text.Pandoc (readHtml, writeMarkdown, WriterOptions(..),
                    ReaderOptions(..), bottomUp, def)
import qualified Text.Pandoc.Walk as Walk
import qualified Text.Pandoc.Builder as B
import qualified Data.Text as T
import Text.Pandoc.XML (fromEntities)

#ifdef UNICODE_COLLATION
import qualified Data.Text     as T
import qualified Data.Text.ICU as T
#else
import Data.RFC5051 (compareUnicode)
#endif
import qualified Data.Vector as V

-- import Debug.Trace
--
-- tr' :: Show a => [Char] -> a -> a
-- tr' note' x = Debug.Trace.trace (note' ++ ": " ++ show x) x

-- Note:  FromJSON reads HTML, ToJSON writes Markdown.
-- This means that they aren't proper inverses of each other, which
-- is odd, but it makes sense given the uses here.  FromJSON is used
-- for reading JSON citeproc bibliographies.  ToJSON is used to create
-- pandoc metadata bibliographies.

readCSLString :: String -> [Inline]
readCSLString s = case readHtml def{ readerSmart = True
                                   , readerParseRaw = True }
                                (adjustScTags s) of
                        Pandoc _ [Plain ils]   -> ils
                        Pandoc _ [Para  ils]   -> ils
                        Pandoc _ x             -> Walk.query (:[]) x

adjustScTags :: String -> String
adjustScTags zs =
  case zs of
       ('<':'s':'c':'>':xs)     -> "<span style=\"font-variant:small-caps;\">" ++
                                    adjustScTags xs
       ('<':'/':'s':'c':'>':xs) -> "</span>" ++ adjustScTags xs
       (x:xs)                   -> x : adjustScTags xs
       []                       -> []


writeCSLString :: [Inline] -> String
writeCSLString ils =
  trimr $ writeMarkdown def{writerWrapText = False}
        $ Pandoc nullMeta [Plain $ bottomUp (concatMap adjustCSL) ils]

adjustCSL :: Inline -> [Inline]
adjustCSL (Span ("",[],[]) xs) = xs
adjustCSL (Span ("",["citeproc-no-output"],[]) _) =
  [Str "[CSL STYLE ERROR: reference with no printed form.]"]
adjustCSL (SmallCaps xs) =
  RawInline (Format "html") "<span style=\"font-variant:small-caps;\">" : xs
    ++ [RawInline (Format "html") "</span>"]
adjustCSL (Subscript xs) =
  RawInline (Format "html") "<span style=\"vertical-align:sub;\">" : xs
    ++ [RawInline (Format "html") "</span>"]
adjustCSL (Superscript xs) =
  RawInline (Format "html") "<span style=\"vertical-align:sup;\">" : xs
    ++ [RawInline (Format "html") "</span>"]
adjustCSL x = [x]

-- We use a newtype wrapper so we can have custom ToJSON, FromJSON
-- instances.
newtype Formatted = Formatted { unFormatted :: [Inline] }
  deriving ( Show, Read, Eq, Ord, Data, Typeable, Generic )

instance FromJSON Formatted where
  parseJSON v@(Array _) =
   Formatted <$> (parseJSON v
             <|> ((query (:[]) :: [Block] -> [Inline]) <$> parseJSON v))
  parseJSON v           = fmap (Formatted . readCSLString) $ parseString v

instance ToJSON Formatted where
  toJSON = toJSON . writeCSLString . unFormatted

instance IsString Formatted where
  fromString = Formatted . toStr

instance Monoid Formatted where
  mempty = Formatted []
  mappend = appendWithPunct
  mconcat = foldr mappend mempty

toStr :: String -> [Inline]
toStr = intercalate [Str "\n"] .
        map (B.toList . B.text . tweak . fromEntities) .
        splitWhen (=='\n')
    where
      tweak ('«':' ':xs) = "«\8239" ++ tweak xs
      tweak (' ':'»':xs) = "\8239»" ++ tweak xs
      tweak (' ':';':xs) = "\8239;" ++ tweak xs
      tweak (' ':':':xs) = "\8239:" ++ tweak xs
      tweak (' ':'!':xs) = "\8239!" ++ tweak xs
      tweak (' ':'?':xs) = "\8239?" ++ tweak xs
      tweak ( x :xs    ) = x : tweak xs
      tweak []           = []

appendWithPunct :: Formatted -> Formatted -> Formatted
appendWithPunct (Formatted left) (Formatted right) =
  Formatted $
  case concat [lastleft, firstright] of
       [' ',d] | d `elem` ",.:;" -> initInline left ++ right
       [c,d] | c `elem` " ,.:;", d == c -> left ++ tailInline right
       [c,'.'] | c `elem` ",.!:;?" -> left ++ tailInline right
       [c,':'] | c `elem` ",!:;?" -> left ++ tailInline right  -- Mich.: 2005
       [c,'!'] | c `elem` ",.!:;?" -> left ++ tailInline right
       [c,'?'] | c `elem` ",.!:;?" -> left ++ tailInline right
       [c,';'] | c `elem` ",:;" -> left ++ tailInline right -- et al.;
       [':',c] | c `elem` ",.!:;?" -> left ++ tailInline right
       [';',c] | c `elem` ",.!:;?" -> left ++ tailInline right
       -- ".;" -> right  -- e.g. et al.;
       _    -> left ++ right
  where lastleft     = lastInline left
        firstright   = headInline right

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
      } deriving ( Show, Read, Typeable, Data, Generic )

data Locale
    = Locale
      { localeVersion :: String
      , localeLang    :: String
      , localeOptions :: [Option]
      , localeTerms   :: [CslTerm]
      , localeDate    :: [Element]
      } deriving ( Show, Read, Eq, Typeable, Data, Generic )

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
      } deriving ( Show, Read, Eq, Typeable, Data, Generic )

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
           } deriving ( Show, Read, Typeable, Data, Generic )

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
      } deriving ( Show, Read, Typeable, Data, Generic )

data Bibliography
    = Bibliography
      { bibOptions :: [Option]
      , bibSort    :: [Sort]
      , bibLayout  ::  Layout
      } deriving ( Show, Read, Typeable, Data, Generic )

type Option = (String,String)

mergeOptions :: [Option] -> [Option] -> [Option]
mergeOptions os = nubBy (\x y -> fst x == fst y) . (++) os

data Layout
    = Layout
      { layFormat ::  Formatting
      , layDelim  ::  Delimiter
      , elements  :: [Element]
      } deriving ( Show, Read, Typeable, Data, Generic )

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
    | Elements     Formatting            [Element]
    | Date        [String]    DateForm    Formatting Delimiter [DatePart] String
      deriving ( Show, Read, Eq, Typeable, Data, Generic )

data IfThen
    = IfThen Condition Match [Element]
      deriving ( Eq, Show, Read, Typeable, Data, Generic )

data Condition
    = Condition
      { isType          :: [String]
      , isSet           :: [String]
      , isNumeric       :: [String]
      , isUncertainDate :: [String]
      , isPosition      :: [String]
      , disambiguation  :: [String]
      , isLocator       :: [String]
      } deriving ( Eq, Show, Read, Typeable, Data, Generic )

type Delimiter = String

data Match
    = Any
    | All
    | None
      deriving ( Show, Read, Eq, Typeable, Data, Generic )

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
      } deriving ( Show, Read, Eq, Typeable, Data, Generic )

defaultDate :: [DatePart]
defaultDate
    = [ DatePart "year"  "" "-" emptyFormatting
      , DatePart "month" "" "-" emptyFormatting
      , DatePart "day"   "" "-" emptyFormatting]

data Sort
    = SortVariable String Sorting
    | SortMacro    String Sorting Int Int String
      deriving ( Eq, Show, Read, Typeable, Data, Generic )

data Sorting
    = Ascending  String
    | Descending String
      deriving ( Read, Show, Eq, Typeable, Data, Generic )

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
      deriving ( Eq, Show, Read, Typeable, Data, Generic )

data Gender
    = Feminine
    | Masculine
    | Neuter
      deriving ( Eq, Show, Read, Typeable, Data, Generic )

data NumericForm
    = Numeric
    | Ordinal
    | Roman
    | LongOrdinal
      deriving ( Eq, Show, Read, Typeable, Data, Generic )

data DateForm
    = TextDate
    | NumericDate
    | NoFormDate
      deriving ( Eq, Show, Read, Typeable, Data, Generic )

data Plural
    = Contextual
    | Always
    | Never
      deriving ( Eq, Show, Read, Typeable, Data, Generic )

data Name
    = Name      Form Formatting NameAttrs Delimiter [NamePart]
    | NameLabel Form Formatting Plural
    | EtAl           Formatting String
      deriving ( Eq, Show, Read, Typeable, Data, Generic )

type NameAttrs = [(String, String)]

data NamePart
    = NamePart String Formatting
      deriving ( Show, Read, Eq, Typeable, Data, Generic )

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
      } deriving ( Read, Eq, Ord, Typeable, Data, Generic )

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
    deriving ( Show, Read, Eq, Ord, Typeable, Data, Generic )

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
      } deriving ( Show, Read, Typeable, Data, Generic )

data CSAuthor   = CSAuthor   String String String
                deriving ( Show, Read, Eq, Typeable, Data, Generic )
data CSCategory = CSCategory String String String
                deriving ( Show, Read, Eq, Typeable, Data, Generic )

data CiteprocError
   = NoOutput
   | ReferenceNotFound String
   deriving ( Eq, Ord, Show, Typeable, Data, Generic )

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
    | OName    Agent    [Output] [[Output]]  Formatting -- ^ A (family) name with the list of given names.
    | OContrib String    String   [Output] [Output] [[Output]] -- ^ The citation key, the role (author, editor, etc.), the contributor(s),
                                                        -- the output needed for year suf. disambiguation, and everything used for
                                                        -- name disambiguation.
    | OLoc    [Output]            Formatting            -- ^ The citation's locator
    | Output  [Output]            Formatting            -- ^ Some nested 'Output'
      deriving ( Eq, Ord, Show, Typeable, Data, Generic )

type Citations = [[Cite]]
data Cite
    = Cite
      { citeId         :: String
      , citePrefix     :: Formatted
      , citeSuffix     :: Formatted
      , citeLabel      :: String
      , citeLocator    :: String
      , citeNoteNumber :: String
      , citePosition   :: String
      , nearNote       :: Bool
      , authorInText   :: Bool
      , suppressAuthor :: Bool
      , citeHash       :: Int
      } deriving ( Show, Eq, Typeable, Data, Generic )

instance FromJSON Cite where
  parseJSON (Object v) = Cite <$>
              v .#: "id" <*>
              v .:? "prefix" .!= mempty <*>
              v .:? "suffix" .!= mempty <*>
              v .#? "label" .!= "page" <*>
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

emptyCite :: Cite
emptyCite  = Cite [] mempty mempty [] [] [] [] False False False 0

-- | A citation group: the first list has a single member when the
-- citation group starts with an "author-in-text" cite, the
-- 'Formatting' to be applied, the 'Delimiter' between individual
-- citations and the list of evaluated citations.
data CitationGroup = CG [(Cite, Output)] Formatting Delimiter [(Cite, Output)] deriving ( Show, Eq, Typeable, Data, Generic )

data BiblioData
    = BD
      { citations    :: [Formatted]
      , bibliography :: [Formatted]
      } deriving ( Show, Typeable, Data, Generic )

-- | A record with all the data to produce the 'Formatted' of a
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
      } deriving ( Show, Typeable, Data, Generic )

instance Eq CiteData where
    (==) (CD ka ca _ _ _ _ _)
         (CD kb cb _ _ _ _ _) = ka == kb && ca == cb

data NameData
    = ND
      { nameKey        ::  Agent
      , nameCollision  ::  [Output]
      , nameDisambData :: [[Output]]
      , nameDataSolved ::  [Output]
      } deriving ( Show, Typeable, Data, Generic )

instance Eq NameData where
    (==) (ND ka ca _ _)
         (ND kb cb _ _) = ka == kb && ca == cb

isPunctuationInQuote :: Style -> Bool
isPunctuationInQuote sty =
  case styleLocale sty of
       (l:_) -> ("punctuation-in-quote","true") `elem` localeOptions l
       _     -> False

object' :: [Pair] -> Aeson.Value
object' = object . filter (not . isempty)
  where isempty (_, Array v)  = V.null v
        isempty (_, String t) = T.null t
        isempty ("first-reference-note-number", Aeson.Number n) = n == 0
        isempty ("citation-number", Aeson.Number n) = n == 0
        isempty (_, _)        = False

data Agent
    = Agent { givenName       :: [Formatted]
            , droppingPart    ::  Formatted
            , nonDroppingPart ::  Formatted
            , familyName      ::  Formatted
            , nameSuffix      ::  Formatted
            , literal         ::  Formatted
            , commaSuffix     ::  Bool
            }
      deriving ( Show, Read, Eq, Ord, Typeable, Data, Generic )

emptyAgent :: Agent
emptyAgent = Agent [] mempty mempty mempty mempty mempty False

instance FromJSON Agent where
  parseJSON (Object v) = Agent <$>
              (v .: "given" <|> ((map Formatted . wordsBy (== Space) . unFormatted) <$> v .: "given") <|> pure []) <*>
              v .:?  "dropping-particle" .!= mempty <*>
              v .:? "non-dropping-particle" .!= mempty <*>
              v .:? "family" .!= mempty <*>
              v .:? "suffix" .!= mempty <*>
              v .:? "literal" .!= mempty <*>
              v .:? "comma-suffix" .!= False
  parseJSON _ = fail "Could not parse Agent"

instance ToJSON Agent where
  toJSON agent = object' $ [
      "given" .= Formatted (intercalate [Space] $ map unFormatted
                                                $ givenName agent)
    , "dropping-particle" .= droppingPart agent
    , "non-dropping-particle" .= nonDroppingPart agent
    , "family" .= familyName agent
    , "suffix" .= nameSuffix agent
    , "literal" .= literal agent
    ] ++ ["comma-suffix" .= commaSuffix agent | nameSuffix agent /= mempty]

instance FromJSON [Agent] where
  parseJSON (Array xs) = mapM parseJSON $ V.toList xs
  parseJSON (Object v) = (:[]) `fmap` parseJSON (Object v)
  parseJSON _ = fail "Could not parse [Agent]"

-- instance ToJSON [Agent] where
-- toJSON xs  = Array (V.fromList $ map toJSON xs)
