{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ViewPatterns               #-}

{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternGuards              #-}
{-# LANGUAGE ScopedTypeVariables        #-}
#if MIN_VERSION_base(4,8,0)
#define OVERLAPS {-# OVERLAPPING #-}
#else
{-# LANGUAGE OverlappingInstances       #-}
#define OVERLAPS
#endif
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

module Text.CSL.Style ( readCSLString
                      , writeCSLString
                      , Formatted(..)
                      , Style(..)
                      , Locale(..)
                      , mergeLocales
                      , CslTerm(..)
                      , newTerm
                      , findTerm
                      , findTerm'
                      , Abbreviations(..)
                      , MacroMap
                      , Citation(..)
                      , Bibliography(..)
                      , Option
                      , mergeOptions
                      , Layout(..)
                      , Element(..)
                      , IfThen(..)
                      , Condition(..)
                      , Delimiter
                      , Match(..)
                      , match
                      , DatePart(..)
                      , defaultDate
                      , Sort(..)
                      , Sorting(..)
                      , compare'
                      , Form(..)
                      , Gender(..)
                      , NumericForm(..)
                      , DateForm(..)
                      , Plural(..)
                      , Name(..)
                      , NameAttrs
                      , NamePart(..)
                      , isPlural
                      , isName
                      , isNames
                      , hasEtAl
                      , Formatting(..)
                      , emptyFormatting
                      , rmTitleCase
                      , rmTitleCase'
                      , Quote(..)
                      , mergeFM
                      , CSInfo(..)
                      , CSAuthor(..)
                      , CSCategory(..)
                      , CiteprocError(..)
                      , Output(..)
                      , Citations
                      , Cite(..)
                      , emptyCite
                      , CitationGroup(..)
                      , BiblioData(..)
                      , CiteData(..)
                      , NameData(..)
                      , isPunctuationInQuote
                      , object'
                      , Agent(..)
                      , emptyAgent
                      )
where

import Prelude
import           Control.Applicative    ((<|>), (<$>))
import           Control.Arrow          hiding (left, right)
import           Control.Monad          (mplus)
import           Data.Aeson             hiding (Number)
import qualified Data.Aeson             as Aeson
import           Data.Aeson.Types       (Pair)
import           Data.Char              (isLetter, isPunctuation, isUpper, isDigit)
import qualified Data.Char              as Char
import           Data.Generics          (Data, Typeable)
import           Data.List              (intercalate, intersperse, nubBy)
import           Data.List.Split        (wordsBy)
import qualified Data.Map               as M
import           Data.Maybe             (listToMaybe, isNothing)
import           Data.String
import           Data.Text              (Text)
import           Data.Yaml.Builder      (ToYaml (..))
import qualified Data.Yaml.Builder      as Y
import           GHC.Generics           (Generic)
import           Text.CSL.Compat.Pandoc (readHtml, writeMarkdown)
import           Text.CSL.Util          (headInline, initInline,
                                         lastInline, mapping', mb, parseBool,
                                         parseString, query, splitWhen,
                                         splitStrWhen, tailInline, trimr,
                                         (.#:), (.#?),
                                         AddYaml(..), addSpaceAfterPeriod)
import qualified Text.Pandoc.Builder    as B
import           Text.Pandoc.Definition hiding (Citation, Cite)
import qualified Text.Pandoc.Walk       as Walk
import           Text.Pandoc.XML        (fromEntities)

import qualified Data.Text              as T
#ifdef UNICODE_COLLATION
import qualified Data.Text.ICU          as T
#else
import           Data.RFC5051           (compareUnicode)
#endif
import qualified Data.Vector            as V

-- Note:  FromJSON reads HTML, ToJSON writes Markdown.
-- This means that they aren't proper inverses of each other, which
-- is odd, but it makes sense given the uses here.  FromJSON is used
-- for reading JSON citeproc bibliographies.  ToJSON is used to create
-- pandoc metadata bibliographies.

readCSLString :: Text -> [Inline]
readCSLString s = Walk.walk handleSmallCapsSpans
                $ case readHtml (adjustScTags s) of
                        Pandoc _ [Plain ils] -> ils
                        Pandoc _ [Para  ils] -> ils
                        Pandoc _ x           -> Walk.query (:[]) x
  -- this is needed for versions of pandoc that don't turn
  -- a span with font-variant:small-caps into a SmallCaps element:
  where handleSmallCapsSpans (Span ("",[],[("style",sty)]) ils)
            | T.filter (`notElem` (" \t;" :: String)) sty == "font-variant:small-caps" =
              SmallCaps ils
        handleSmallCapsSpans x = x

-- <sc> is not a real HTML tag, but a CSL convention.  So we
-- replace it with a real tag that the HTML reader will understand.
adjustScTags :: Text -> Text
adjustScTags zs
  | Just xs <- T.stripPrefix "<sc>" zs =
      "<span style=\"font-variant:small-caps;\">" <> adjustScTags xs
  | Just xs <- T.stripPrefix "</sc>" zs =
      "</span>" <> adjustScTags xs
  | Just (x, xs) <- T.uncons zs =
    T.cons x (adjustScTags xs)
  | otherwise = ""

writeYAMLString :: [Inline] -> Text
writeYAMLString ils =
  trimr $ writeMarkdown
        $ Pandoc nullMeta
          [Plain $ Walk.walk (concatMap (adjustCSL False)) ils]

writeCSLString :: [Inline] -> Text
writeCSLString ils =
  trimr $ writeMarkdown
        $ Pandoc nullMeta
          [Plain $ Walk.walk (concatMap (adjustCSL True)) ils]

-- If the first param is True, we use special rich text conventions
-- for CSL JSON, described here:
-- http://docs.citationstyles.org/en/1.0/release-notes.html#rich-text-markup-within-fields
adjustCSL :: Bool -> Inline -> [Inline]
adjustCSL _ (Span ("",[],[]) xs) = xs
adjustCSL _ (Span ("",["citeproc-no-output"],[]) _) =
  [Str "[CSL STYLE ERROR: reference with no printed form.]"]
adjustCSL True (SmallCaps xs) =
  RawInline (Format "html") "<sc>" : xs
    ++ [RawInline (Format "html") "</sc>"]
adjustCSL True (Subscript xs) =
  RawInline (Format "html") "<sub>" : xs
    ++ [RawInline (Format "html") "</sub>"]
adjustCSL True (Superscript xs) =
  RawInline (Format "html") "<sup>" : xs
    ++ [RawInline (Format "html") "</sup>"]
adjustCSL True (Emph xs) =
  RawInline (Format "html") "<i>" : xs
    ++ [RawInline (Format "html") "</i>"]
adjustCSL True (Strong xs) =
  RawInline (Format "html") "<b>" : xs
    ++ [RawInline (Format "html") "</b>"]
adjustCSL _ x = [x]

-- We use a newtype wrapper so we can have custom ToJSON, FromJSON
-- instances.
newtype Formatted = Formatted { unFormatted :: [Inline] }
  deriving ( Show, Read, Eq, Ord, Data, Typeable, Generic )

instance FromJSON Formatted where
  parseJSON v@(Array _) =
   Formatted <$> (parseJSON v
             <|> ((query (:[]) :: [Block] -> [Inline]) <$> parseJSON v))
  parseJSON v           = (Formatted . readCSLString) Control.Applicative.<$> parseString v

instance ToJSON Formatted where
  toJSON = toJSON . writeCSLString . unFormatted

instance ToYaml Formatted where
  toYaml = Y.string . writeYAMLString . unFormatted

instance IsString Formatted where
  fromString = Formatted . toStr

instance AddYaml Formatted where
  x &= (Formatted y) =
           \acc -> if null y
                      then acc
                      else (x Y..= Formatted y) : acc

instance Semigroup Formatted where
  (<>) = appendWithPunct

instance Monoid Formatted where
  mempty = Formatted []
  mappend = (<>)
  mconcat = foldr mappend mempty

instance Walk.Walkable Inline Formatted where
  walk f  = Formatted . Walk.walk f . unFormatted
  walkM f = fmap Formatted . Walk.walkM f . unFormatted
  query f = Walk.query f . unFormatted

instance Walk.Walkable Formatted Formatted where
  walk f  = f
  walkM f = f
  query f = f

toStr :: String -> [Inline]
toStr = intercalate [Str "\n"] .
        map (B.toList . B.text . T.pack . tweak . T.unpack . fromEntities) .
        splitWhen (=='\n') . T.pack
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
  case (,) <$> lastleft <*> firstright of
    Just (' ', d) | d `elem` (",.:;" :: String) -> initInline left ++ right
    Just (c,d) | c `elem` (" ,.:;" :: String), d == c -> left ++ tailInline right
    Just (c,'.') | c `elem` (",.!:;?" :: String) -> left ++ tailInline right
    Just (c,':') | c `elem` (",!:;?" :: String) -> left ++ tailInline right  -- Mich.: 2005
    Just (c,'!') | c `elem` (",.!:;?" :: String) -> left ++ tailInline right
    Just (c,'?') | c `elem` (",.!:;?" :: String) -> left ++ tailInline right
    Just (c,';') | c `elem` (",:;" :: String) -> left ++ tailInline right -- et al.;
    Just (':',c) | c `elem` (",.!:;?" :: String) -> left ++ tailInline right
    Just (';',c) | c `elem` (",.!:;?" :: String) -> left ++ tailInline right
    -- ".;" -> right  -- e.g. et al.;
    _    -> left ++ right
  where lastleft     = lastInline left
        firstright   = headInline right

-- | The representation of a parsed CSL style.
data Style
    = Style
      { styleVersion       ::  Text
      , styleClass         ::  Text
      , styleInfo          ::  Maybe CSInfo
      , styleDefaultLocale ::  Text
      , styleLocale        :: [Locale]
      , styleAbbrevs       :: Abbreviations
      , csOptions          :: [Option]
      , csMacros           :: [MacroMap]
      , citation           ::  Citation
      , biblio             ::  Maybe Bibliography
      } deriving ( Show, Read, Typeable, Data, Generic )

data Locale
    = Locale
      { localeVersion :: Text
      , localeLang    :: Text
      , localeOptions :: [Option]
      , localeTerms   :: [CslTerm]
      , localeDate    :: [Element]
      } deriving ( Show, Read, Eq, Typeable, Data, Generic )

-- | With the 'defaultLocale', the locales-xx-XX.xml loaded file and
-- the parsed 'Style' cs:locale elements, produce the final 'Locale'
-- as the only element of a list, taking into account CSL locale
-- prioritization.
mergeLocales :: Text -> Locale -> [Locale] -> [Locale]
mergeLocales s l ls = doMerge list
    where
      list = filter ((==) s . localeLang) ls ++
             filter ((\x -> x /= "" && x `T.isPrefixOf` s) . localeLang) ls ++
             filter ((==) "" . localeLang) ls
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
      { cslTerm        :: Text
      , termForm       :: Form
      , termGender     :: Gender
      , termGenderForm :: Gender
      , termSingular   :: Text
      , termPlural     :: Text
      , termMatch      :: Text
      } deriving ( Show, Read, Eq, Typeable, Data, Generic )

newTerm :: CslTerm
newTerm = CT "" Long Neuter Neuter "" "" ""

findTerm :: Text -> Form -> [CslTerm] -> Maybe CslTerm
findTerm s f = findTerm'' s f Nothing

findTerm' :: Text -> Form -> Gender -> [CslTerm] -> Maybe CslTerm
findTerm' s f g = findTerm'' s f (Just g)

findTerm'' :: Text -> Form -> Maybe Gender -> [CslTerm] -> Maybe CslTerm
findTerm'' s f mbg ts
  = listToMaybe [ t | t <- ts, cslTerm t == s, termForm t == f,
                         isNothing mbg || mbg == Just (termGenderForm t) ]
  `mplus`
  -- fallback: http://citationstyles.org/downloads/specification.html#terms
  case f of
       VerbShort -> findTerm'' s Verb Nothing ts
       Symbol    -> findTerm'' s Short Nothing ts
       Verb      -> findTerm'' s Long Nothing ts
       Short     -> findTerm'' s Long Nothing ts
       _         -> Nothing

hasOrdinals :: [Locale] -> Bool
hasOrdinals = any (any hasOrd . localeTerms)
    where
      hasOrd o
          | CT {cslTerm = t} <- o
          , "ordinal" `T.isInfixOf` t = True
          | otherwise                 = False

rmOrdinals :: [CslTerm] -> [CslTerm]
rmOrdinals [] = []
rmOrdinals (o:os)
  | CT {cslTerm = t} <- o
  , "ordinal" `T.isInfixOf` t =   rmOrdinals os
  | otherwise                 = o:rmOrdinals os

newtype Abbreviations = Abbreviations {
           unAbbreviations :: M.Map Text (M.Map Text (M.Map Text Text))
           } deriving ( Show, Read, Typeable, Data, Generic )

instance FromJSON Abbreviations where
  parseJSON (Object v)   = Abbreviations <$> parseJSON (Object v)
  parseJSON (Bool False) = return $ Abbreviations M.empty
  parseJSON _            = fail "Could not read Abbreviations"

type MacroMap
    = (Text,[Element])

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

type Option = (Text,Text)

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
    | Macro        Text                   Formatting
    | Const        Text                   Formatting
    | Variable    [Text]      Form        Formatting Delimiter
    | Term         Text       Form        Formatting Bool
    | Label        Text       Form        Formatting Plural
    | Number       Text       NumericForm Formatting
    | Names       [Text  ]   [Name]       Formatting Delimiter [Element]
    | Substitute  [Element]
    | Group        Formatting Delimiter  [Element]
    | Date        [Text  ]    DateForm    Formatting Delimiter [DatePart] Text
      deriving ( Show, Read, Eq, Typeable, Data, Generic )

data IfThen
    = IfThen Condition Match [Element]
      deriving ( Eq, Show, Read, Typeable, Data, Generic )

data Condition
    = Condition
      { isType          :: [Text]
      , isSet           :: [Text]
      , isNumeric       :: [Text]
      , isUncertainDate :: [Text]
      , isPosition      :: [Text]
      , disambiguation  :: [Text]
      , isLocator       :: [Text]
      } deriving ( Eq, Show, Read, Typeable, Data, Generic )

type Delimiter = Text

data Match
    = Any
    | All
    | None
      deriving ( Show, Read, Eq, Typeable, Data, Generic )

match :: Match -> [Bool] -> Bool
match All  = and
match Any  = or
match None = all not

data DatePart
    = DatePart
      { dpName       :: Text
      , dpForm       :: Text
      , dpRangeDelim :: Text
      , dpFormatting :: Formatting
      } deriving ( Show, Read, Eq, Typeable, Data, Generic )

defaultDate :: [DatePart]
defaultDate
    = [ DatePart "year"  "" "-" emptyFormatting
      , DatePart "month" "" "-" emptyFormatting
      , DatePart "day"   "" "-" emptyFormatting]

data Sort
    = SortVariable Text Sorting
    | SortMacro    Text Sorting Int Int Text
      deriving ( Eq, Show, Read, Typeable, Data, Generic )

data Sorting
    = Ascending  Text
    | Descending Text
      deriving ( Read, Show, Eq, Typeable, Data, Generic )

instance Ord Sorting where
    compare (Ascending  "") (Ascending  "") = EQ
    compare (Ascending  "") (Ascending   _) = GT
    compare (Ascending   _) (Ascending  "") = LT
    compare (Ascending   a) (Ascending   b) = compare' a b
    compare (Descending "") (Descending "") = EQ
    compare (Descending "") (Descending  _) = GT
    compare (Descending  _) (Descending "") = LT
    compare (Descending  a) (Descending  b) = compare' b a
    compare              _               _  = EQ

compare' :: Text -> Text -> Ordering
compare' x y
    = case (T.uncons x, T.uncons y) of
        (Just ('-',_), Just ('-',_)) -> comp (normalize y) (normalize x)
        (Just ('-',_), _)            -> LT
        (_  ,Just ('-',_))           -> GT
        _                            -> comp (normalize x) (normalize y)
      where
        -- we zero pad numbers so they're sorted numerically, see #399
        zeropad t = if T.all isDigit t
                       then T.replicate (10 - T.length t) "0" <> t
                       else t
        normalize = zeropad .
                    T.map (\c -> if c == ',' || c == '.' then ' ' else c) .
                    T.filter (\c -> c == ',' ||
                                  not (isPunctuation c || Char.isSpace c
                                      -- ayn/hamza in transliterated arabic:
                                       || c == 'ʾ' || c == 'ʿ'
                                       ))
#ifdef UNICODE_COLLATION
        comp a b = T.collate (T.collator T.Current) a b
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
    | EtAl           Formatting Text
      deriving ( Eq, Show, Read, Typeable, Data, Generic )

type NameAttrs = [(Text, Text)]

data NamePart
    = NamePart Text Formatting
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
hasEtAl = any isEtAl
    where isEtAl (EtAl _ _) = True
          isEtAl _          = False

data Formatting
    = Formatting
      { prefix         :: Text
      , suffix         :: Text
      , fontFamily     :: Text
      , fontStyle      :: Text
      , fontVariant    :: Text
      , fontWeight     :: Text
      , textDecoration :: Text
      , verticalAlign  :: Text
      , textCase       :: Text
      , display        :: Text
      , quotes         :: Quote
      , stripPeriods   :: Bool
      , noCase         :: Bool
      , noDecor        :: Bool
      , hyperlink      :: Text  -- null for no link
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
                       ,("noDecor", show . noDecor)
                       ,("hyperlink", show . hyperlink)],
             f x /= f emptyFormatting ]
        ++ "}"

rmTitleCase :: Formatting -> Formatting
rmTitleCase f = f{ textCase = if textCase f == "title" then "" else textCase f  }

rmTitleCase' :: Output -> Output
rmTitleCase' o =
  case o of
       OStr s f    -> OStr s (rmTitleCase f)
       Output os f -> Output os (rmTitleCase f)
       _           -> o

data Quote
    = NativeQuote
    | ParsedQuote
    | NoQuote
    deriving ( Show, Read, Eq, Ord, Typeable, Data, Generic )

emptyFormatting :: Formatting
emptyFormatting
    = Formatting "" "" "" "" "" "" "" "" "" "" NoQuote False False False ""

mergeFM :: Formatting -> Formatting -> Formatting
mergeFM (Formatting aa ab ac ad ae af ag ah ai aj ak al am an ahl)
        (Formatting ba bb bc bd be bf bg bh bi bj bk bl bm bn bhl) =
                   Formatting (ba `orIfEmpty` aa)
                              (bb `orIfEmpty` ab)
                              (bc `orIfEmpty` ac)
                              (bd `orIfEmpty` ad)
                              (be `orIfEmpty` ae)
                              (bf `orIfEmpty` af)
                              (bg `orIfEmpty` ag)
                              (bh `orIfEmpty` ah)
                              (bi `orIfEmpty` ai)
                              (bj `orIfEmpty` aj)
                              (if bk == NoQuote then ak else bk)
                              (bl || al)
                              (bm || am)
                              (bn || an)
                              (bhl <> ahl)
 where orIfEmpty :: Text -> Text -> Text
       orIfEmpty "" fallback = fallback
       orIfEmpty t  _ = t

data CSInfo
    = CSInfo
      { csiTitle      :: Text
      , csiAuthor     :: CSAuthor
      , csiCategories :: [CSCategory]
      , csiId         :: Text
      , csiUpdated    :: Text
      } deriving ( Show, Read, Typeable, Data, Generic )

data CSAuthor   = CSAuthor   Text Text Text
                deriving ( Show, Read, Eq, Typeable, Data, Generic )
data CSCategory = CSCategory Text Text Text
                deriving ( Show, Read, Eq, Typeable, Data, Generic )

data CiteprocError
   = NoOutput
   | ReferenceNotFound Text
   deriving ( Eq, Ord, Show, Typeable, Data, Generic )

-- | The 'Output' generated by the evaluation of a style. Must be
-- further processed for disambiguation and collapsing.
data Output
    = ONull
    | OSpace
    | OPan    [Inline]
    | OStatus [Inline]
    | ODel     Text                                   -- ^ A delimiter string.
    | OStr     Text           Formatting            -- ^ A simple 'String'
    | OErr     CiteprocError                            -- ^ Warning message
    | OLabel   Text           Formatting            -- ^ A label used for roles
    | ONum     Int            Formatting            -- ^ A number (used to count contributors)
    | OCitNum  Int            Formatting            -- ^ The citation number
    | OCitLabel Text          Formatting            -- ^ The citation label
    | ODate   [Output]                                  -- ^ A (possibly) ranged date
    | OYear    Text    Text   Formatting            -- ^ The year and the citeId
    | OYearSuf Text    Text   [Output]   Formatting -- ^ The year suffix, the citeId and a holder for collision data
    | OName    Agent    [Output] [[Output]]  Formatting -- ^ A (family) name with the list of given names.
    | OContrib Text    Text   [Output] [Output] [[Output]] -- ^ The citation key, the role (author, editor, etc.), the contributor(s),
                                                        -- the output needed for year suf. disambiguation, and everything used for
                                                        -- name disambiguation.
    | OLoc    [Output]        Formatting            -- ^ The citation's locator
    | Output  [Output]        Formatting            -- ^ Some nested 'Output'
      deriving ( Eq, Ord, Show, Typeable, Data, Generic )

type Citations = [[Cite]]
data Cite
    = Cite
      { citeId         :: Text
      , citePrefix     :: Formatted
      , citeSuffix     :: Formatted
      , citeLabel      :: Text
      , citeLocator    :: Text
      , citeNoteNumber :: Text
      , citePosition   :: Text
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

instance OVERLAPS
         FromJSON [[Cite]] where
  parseJSON (Array v) = mapM parseJSON $ V.toList v
  parseJSON _         = return []

emptyCite :: Cite
emptyCite  = Cite "" mempty mempty "" "" "" "" False False False 0

-- | A citation group: the first list has a single member when the
-- citation group starts with an "author-in-text" cite, the
-- 'Formatting' to be applied, the 'Delimiter' between individual
-- citations and the list of evaluated citations.
data CitationGroup = CG [(Cite, Output)] Formatting Delimiter [(Cite, Output)] deriving ( Show, Eq, Typeable, Data, Generic )

data BiblioData
    = BD
      { citations    :: [Formatted]
      , bibliography :: [Formatted]
      , citationIds  :: [Text]
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
      { key        ::   Text
      , collision  ::  [Output]
      , disambYS   ::  [Output]
      , disambData :: [[Output]]
      , disambed   ::  [Output]
      , sameAs     ::  [Text]
      , citYear    ::   Text
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
  where isempty (_, Array v)                                    = V.null v
        isempty (_, String t)                                   = T.null t
        isempty ("first-reference-note-number", Aeson.Number n) = n == 0
        isempty ("citation-number", Aeson.Number n)             = n == 0
        isempty (_, _)                                          = False

data Agent
    = Agent { givenName       :: [Formatted]
            , droppingPart    :: Formatted
            , nonDroppingPart :: Formatted
            , familyName      :: Formatted
            , nameSuffix      :: Formatted
            , literal         :: Formatted
            , commaSuffix     :: Bool
            , parseNames      :: Bool
            }
      deriving ( Show, Read, Eq, Ord, Typeable, Data, Generic )

emptyAgent :: Agent
emptyAgent = Agent [] mempty mempty mempty mempty mempty False False

-- CSL JSON uses quotes to protect capitalization
removeQuoted :: Formatted -> Formatted
removeQuoted (Formatted ils) = Formatted (go ils)
  where go []                             = []
        go (Quoted DoubleQuote ils' : xs) = ils' ++ go xs
        go (x:xs)                         = x : go xs

instance FromJSON Agent where
  parseJSON (Object v) = nameTransform <$> (Agent <$>
              (v .: "given" <|> ((map Formatted . wordsBy isSpace .
                                  addSpaceAfterPeriod . unFormatted) <$>
                                  v .: "given") <|> pure []) <*>
              v .:?  "dropping-particle" .!= mempty <*>
              v .:? "non-dropping-particle" .!= mempty <*>
              (removeQuoted <$> (v .:? "family" .!= mempty)) <*>
              v .:? "suffix" .!= mempty <*>
              v .:? "literal" .!= mempty <*>
              (v .:? "comma-suffix" >>= mb parseBool) .!= False <*>
              (v .:? "parse-names" >>= mb parseBool) .!= False)
  parseJSON _ = fail "Could not parse Agent"

instance ToYaml Agent where
  toYaml ag = mapping' [ "family" &= familyName ag
                       , case givenName ag of
                              []    -> id
                              xs    -> "given" &= Formatted
                                                  (intercalate [Space]
                                                    (map unFormatted xs))
                       , "non-dropping-particle" &= nonDroppingPart ag
                       , "dropping-particle" &= droppingPart ag
                       , "suffix" &= nameSuffix ag
                       , "literal" &= literal ag
                       , "comma-suffix" &= T.pack (if commaSuffix ag
                                                      then "true"
                                                      else "")
                       , "parse-names" &= T.pack (if parseNames ag
                                                     then "true"
                                                     else "")
                       ]

-- See http://gsl-nagoya-u.net/http/pub/citeproc-doc.html#id28
nameTransform :: Agent -> Agent
nameTransform ag
  | parseNames ag = nonDroppingPartTransform .
                    droppingPartTransform .
                    suffixTransform $ ag{ parseNames = False }
  | otherwise = ag

nonDroppingPartTransform :: Agent -> Agent
nonDroppingPartTransform ag
  | nonDroppingPart ag == mempty =
    case break startWithCapital'
         (splitStrWhen (\c -> isPunctuation c || isUpper c) $
          unFormatted $ familyName ag) of
         ([], _)  -> ag
         (xs, ys)
           | lastInline xs `elem` map Just (" -'’" :: String) -> ag {
                          nonDroppingPart = Formatted $ trimSpace xs,
                          familyName = Formatted ys }
           | otherwise -> ag
  | otherwise = ag

trimSpace :: [Inline] -> [Inline]
trimSpace = reverse . dropWhile isSpace . reverse . dropWhile isSpace

isSpace :: Inline -> Bool
isSpace Space     = True
isSpace SoftBreak = True
isSpace _         = False

droppingPartTransform :: Agent -> Agent
droppingPartTransform ag
  | droppingPart ag == mempty =
    case break startWithCapital $ reverse $ givenName ag of
          ([],_)  -> ag
          (ys,zs) -> ag{ droppingPart = mconcat $
                                         intersperse (Formatted [Space]) $
                                         reverse ys
                       , givenName = reverse zs }
  | otherwise = ag

startWithCapital' :: Inline -> Bool
startWithCapital' (Str (T.uncons -> Just (c,_))) = isUpper c && isLetter c
startWithCapital' _           = False

startWithCapital :: Formatted -> Bool
startWithCapital (Formatted (x:_)) = startWithCapital' x
startWithCapital _                 = False

stripFinalComma :: Formatted -> (Text, Formatted)
stripFinalComma (Formatted ils) =
  case reverse $ splitStrWhen isPunctuation ils of
       Str ",":xs         -> (",", Formatted $ reverse xs)
       Str "!":Str ",":xs -> (",!", Formatted $ reverse xs)
       _                  -> ("", Formatted ils)

suffixTransform :: Agent -> Agent
suffixTransform ag
  | nameSuffix ag == mempty = fst $ foldl go
                              (ag{ givenName   = mempty
                                 , nameSuffix  = mempty
                                 , commaSuffix = False }, False)
                              (givenName ag)
  | otherwise = ag
  where go (ag', False) n =
               case stripFinalComma n of
                    ("", _)   -> (ag'{ givenName = givenName ag' ++ [n] }, False)
                    (",",n')  -> (ag'{ givenName = givenName ag' ++ [n'] }, True)
                    (",!",n') -> (ag'{ givenName = givenName ag' ++ [n']
                                     , commaSuffix = True }, True)
                    _         -> error "stripFinalComma returned unexpected value"
        go (ag', True) n = (ag'{ nameSuffix = if nameSuffix ag' == mempty
                                                 then n
                                                 else nameSuffix ag' <>
                                                      Formatted [Space] <> n }, True)

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
      ++ ["parse-names" .= True | parseNames agent ]

instance OVERLAPS
         FromJSON [Agent] where
  parseJSON (Array xs) = mapM parseJSON $ V.toList xs
  parseJSON (Object v) = (:[]) `fmap` parseJSON (Object v)
  parseJSON _          = fail "Could not parse [Agent]"

-- instance ToJSON [Agent] where
-- toJSON xs  = Array (V.fromList $ map toJSON xs)

