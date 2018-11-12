{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE IncoherentInstances        #-}
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
-- Module      :  Text.CSL.Reference
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Andrea Rossato <andrea.rossato@unitn.it>
-- Stability   :  unstable
-- Portability :  unportable
--
-- The Reference type
--
-----------------------------------------------------------------------------

module Text.CSL.Reference ( Literal(..)
                          , Value(..)
                          , ReferenceMap
                          , mkRefMap
                          , fromValue
                          , isValueSet
                          , Empty(..)
                          , Season(..)
                          , seasonToInt
                          , RefDate(..)
                          , handleLiteral
                          , toDatePart
                          , setCirca
                          , RefType(..)
                          , CNum(..)
                          , CLabel(..)
                          , Reference(..)
                          , emptyReference
                          , numericVars
                          , getReference
                          , processCites
                          , setPageFirst
                          , setNearNote
                          , parseEDTFDate
                          )
where

import Prelude
import           Control.Applicative ((<|>))
import           Control.Monad       (guard, mplus, msum)
import           Data.Aeson          hiding (Value)
import qualified Data.Aeson          as Aeson
import           Data.Aeson.Types    (Parser)
import           Data.Char           (isDigit, toLower)
import           Data.Either         (lefts, rights)
import           Data.Generics       hiding (Generic)
import qualified Data.HashMap.Strict as H
import           Data.List           (elemIndex)
import           Data.List.Split     (splitWhen)
import           Data.Maybe          (fromMaybe, isNothing)
import           Data.String
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Vector         as V
import           Data.Yaml.Builder   (ToYaml (..))
import qualified Data.Yaml.Builder   as Y
import           GHC.Generics        (Generic)
import           Text.CSL.Style      hiding (Number)
import           Text.CSL.Util       (camelize, capitalize, inlinesToString,
                                      mapping', parseBool, parseInt, parseMaybeInt,
                                      parseString, readNum, safeRead, trim,
                                      uncamelize, AddYaml(..))
import           Text.Pandoc         (Inline (Str))
import qualified Text.Parsec         as P
import qualified Text.Parsec.String  as P

newtype Literal = Literal { unLiteral :: String }
  deriving ( Show, Read, Eq, Data, Typeable, Semigroup, Monoid, Generic )

instance AddYaml Literal
  where x &= (Literal y) = x &= (T.pack y)

instance FromJSON Literal where
  parseJSON v             = Literal `fmap` parseString v

instance ToJSON Literal where
  toJSON = toJSON . unLiteral

instance ToYaml Literal where
  toYaml = Y.string . T.pack . unLiteral

instance IsString Literal where
  fromString = Literal

-- | An existential type to wrap the different types a 'Reference' is
-- made of. This way we can create a map to make queries easier.
data Value = forall a . Data a => Value a

-- for debuging
instance Show Value where
    show (Value a) = gshow a

type ReferenceMap = [(String, Value)]

mkRefMap :: Maybe Reference -> ReferenceMap
mkRefMap Nothing  = []
mkRefMap (Just r) = zip fields (gmapQ Value r)
      where fields = map uncamelize . constrFields . toConstr $ r

fromValue :: Data a => Value -> Maybe a
fromValue (Value a) = cast a

isValueSet :: Value -> Bool
isValueSet val
    | Just v <- fromValue val :: Maybe Literal   = v /= mempty
    | Just v <- fromValue val :: Maybe String    = v /= mempty
    | Just v <- fromValue val :: Maybe Formatted = v /= mempty
    | Just v <- fromValue val :: Maybe [Agent]   = v /= []
    | Just v <- fromValue val :: Maybe [RefDate] = v /= []
    | Just v <- fromValue val :: Maybe Int       = v /= 0
    | Just v <- fromValue val :: Maybe CNum      = v /= 0
    | Just v <- fromValue val :: Maybe CLabel    = v /= mempty
    | Just _ <- fromValue val :: Maybe Empty     = True
    | otherwise = False

data Empty = Empty deriving ( Typeable, Data, Generic )

data Season = Spring | Summer | Autumn | Winter | RawSeason String
     deriving (Show, Read, Eq, Typeable, Data, Generic)

instance ToYaml Season where
  toYaml Spring = toYaml (1 :: Int)
  toYaml Summer = toYaml (2 :: Int)
  toYaml Autumn = toYaml (3 :: Int)
  toYaml Winter = toYaml (4 :: Int)
  toYaml (RawSeason s) = toYaml (T.pack s)

seasonToInt :: Season -> Maybe Int
seasonToInt Spring = Just 1
seasonToInt Summer = Just 2
seasonToInt Autumn = Just 3
seasonToInt Winter = Just 4
seasonToInt _      = Nothing

intToSeason :: Int -> Maybe Season
intToSeason 1 = Just Spring
intToSeason 2 = Just Summer
intToSeason 3 = Just Autumn
intToSeason 4 = Just Winter
intToSeason _  = Nothing

pseudoMonthToSeason :: Int -> Maybe Season
pseudoMonthToSeason n
  | n >= 13 && n <= 16 = intToSeason (n - 12)
  | n >= 21 && n <= 24 = intToSeason (n - 20)
  | otherwise          = Nothing

-- | Parse JSON value as Maybe Season.
parseMaybeSeason :: Maybe Aeson.Value -> Parser (Maybe Season)
parseMaybeSeason Nothing = return Nothing
parseMaybeSeason (Just x) = do
  mbn <- parseMaybeInt (Just x) <|> return Nothing
  case mbn of
       Just n -> case intToSeason n of
                      Just s  -> return $ Just s
                      Nothing -> fail $ "Could not read season: " ++ show n
       Nothing -> do
         s <- parseString x
         if null s
            then return Nothing
            else return $ Just $ RawSeason s

data RefDate =
    RefDate { year   :: Maybe Int
            , month  :: Maybe Int
            , season :: Maybe Season
            , day    :: Maybe Int
            , other  :: Literal
            , circa  :: Bool
            } deriving ( Show, Read, Eq, Typeable, Data, Generic )

instance AddYaml RefDate where
  _ &= (RefDate Nothing Nothing Nothing Nothing o _) | o == mempty = id
  x &= y = x &= y

instance FromJSON RefDate where
  parseJSON (Array v) = handlePseudoMonths <$>
     case fromJSON (Array v) of
          Success [y]     -> RefDate <$> parseMaybeInt y <*>
                    pure Nothing <*> pure Nothing <*> pure Nothing <*>
                    pure "" <*> pure False
          Success [y,m]   -> RefDate <$> parseMaybeInt y <*> parseMaybeInt m <*>
                    pure Nothing <*> pure Nothing <*> pure "" <*> pure False
          Success [y,m,d] -> RefDate <$> parseMaybeInt y <*> parseMaybeInt m <*>
                    pure Nothing <*> parseMaybeInt d <*> pure "" <*> pure False
          Error e         -> fail $ "Could not parse RefDate: " ++ e
          _               -> fail "Could not parse RefDate"
     where handlePseudoMonths r =
              case month r >>= pseudoMonthToSeason of
                   Just s  -> r{ month = Nothing, season = Just s }
                   Nothing -> r
  parseJSON (Object v) = RefDate <$>
              (v .:? "year" >>= parseMaybeInt) <*>
              (v .:? "month" >>= parseMaybeInt) <*>
              (v .:? "season" >>= parseMaybeSeason) <*>
              (v .:? "day" >>= parseMaybeInt) <*>
              v .:? "literal" .!= "" <*>
              ((v .: "circa" >>= parseBool) <|> pure False)
  parseJSON _ = fail "Could not parse RefDate"

{-
instance ToJSON RefDate where
  toJSON refdate = object' $ [
      "year" .= year refdate
    , "month" .= month refdate
    , "season" .= season refdate
    , "day" .= day refdate
    , "literal" .= other refdate ] ++
    [ "circa" .= circa refdate | circa refdate ]
-}

instance ToYaml RefDate where
  toYaml r = mapping'
               [ "year" &= year r
               , "month" &= month r
               , "season" &= season r
               , "day" &= day r
               , "literal" &= other r
               , "circa" &= circa r
               ]

instance OVERLAPS
         FromJSON [RefDate] where
  parseJSON (Array xs) = mapM parseJSON $ V.toList xs
  parseJSON (Object v) = do
    raw' <- v .:? "raw"
    dateParts <- v .:? "date-parts"
    circa' <- (v .: "circa" >>= parseBool) <|> pure False
    season' <- v .:? "season" >>= parseMaybeSeason
    case dateParts of
         Just (Array y) | isNothing raw' ->
           case V.toList y of
              []           -> return []
              [Null]       -> return []
              [Array x]
                | V.null x -> return []
                   -- [ null ] and [ [] ] are sometimes seen. See
                   -- https://github.com/greenelab/manubot/issues/66
              ys           -> mapM (fmap (setCirca circa' .
                                maybe id setSeason season') . parseJSON) ys
         _ -> case raw' of
                  Nothing -> handleLiteral <$> parseJSON (Object v)
                  Just r  -> return $ parseRawDate r
  parseJSON x  = parseRawDate <$> parseJSON x

instance OVERLAPS
         ToJSON [RefDate] where
  toJSON = toJSONDate

toJSONDate :: [RefDate] -> Aeson.Value
toJSONDate [] = Array V.empty
toJSONDate ds = object' $
  [ "date-parts" .= dateparts | not (null dateparts) ] ++
  ["circa" .= (1 :: Int) | any circa ds] ++
  (case msum (map season ds) of
        Just (RawSeason s) -> ["season" .= s]
        _                  -> []) ++
  (case mconcat (map other ds) of
        Literal l | not (null l) -> ["literal" .= l]
        _                        -> [])
  where dateparts = filter (not . emptyDatePart) $ map toDatePart ds
        emptyDatePart [] = True
        emptyDatePart xs = all (== 0) xs

toDatePart :: RefDate -> [Int]
toDatePart refdate =
    case (year refdate, month refdate
           `mplus`
          ((12+) <$> (season refdate >>= seasonToInt)),
          day refdate) of
         (Just (y :: Int), Just (m :: Int), Just (d :: Int))
                                     -> [y, m, d]
         (Just y, Just m, Nothing)   -> [y, m]
         (Just y, Nothing, Nothing)  -> [y]
         _                           -> []


-- Zotero doesn't properly support date ranges, so a common
-- workaround is 2005_2007 or 2005_; support this as date range:
handleLiteral :: RefDate -> [RefDate]
handleLiteral d@(RefDate Nothing Nothing Nothing Nothing (Literal xs) b)
  = case splitWhen (=='_') xs of
         [x,y] | all isDigit x && all isDigit y &&
                 not (null x) ->
                 [RefDate (safeRead x) Nothing Nothing Nothing mempty b,
                  RefDate (safeRead y) Nothing Nothing Nothing mempty b]
         _ -> [d]
handleLiteral d = [d]

setCirca :: Bool -> RefDate -> RefDate
setCirca circa' rd = rd{ circa = circa' }

setSeason :: Season -> RefDate -> RefDate
setSeason season' rd = rd{ season = Just season' }

data RefType
    = NoType
    | Article
    | ArticleMagazine
    | ArticleNewspaper
    | ArticleJournal
    | Bill
    | Book
    | Broadcast
    | Chapter
    | Dataset
    | Entry
    | EntryDictionary
    | EntryEncyclopedia
    | Figure
    | Graphic
    | Interview
    | Legislation
    | LegalCase
    | Manuscript
    | Map
    | MotionPicture
    | MusicalScore
    | Pamphlet
    | PaperConference
    | Patent
    | Post
    | PostWeblog
    | PersonalCommunication
    | Report
    | Review
    | ReviewBook
    | Song
    | Speech
    | Thesis
    | Treaty
    | Webpage
      deriving ( Read, Eq, Typeable, Data, Generic )

instance Show RefType where
    show x = map toLower . uncamelize . showConstr . toConstr $ x

instance FromJSON RefType where
  -- found in one of the test cases:
  parseJSON (String "film") = return MotionPicture
  parseJSON (String t) =
    safeRead (capitalize . camelize . T.unpack $ t) <|>
    fail ("'" ++ T.unpack t ++ "' is not a valid reference type")
  parseJSON v@(Array _) =
    fmap (capitalize . camelize . inlinesToString) (parseJSON v) >>= \t ->
      safeRead t <|>
       fail ("'" ++ t ++ "' is not a valid reference type")
  parseJSON _ = fail "Could not parse RefType"

instance ToJSON RefType where
  toJSON reftype = toJSON (handleSpecialCases $ show reftype)

instance ToYaml RefType where
  toYaml r = Y.string (T.pack $ handleSpecialCases $ show r)

-- For some reason, CSL is inconsistent about hyphens and underscores:
handleSpecialCases :: String -> String
handleSpecialCases "motion-picture"         = "motion_picture"
handleSpecialCases "musical-score"          = "musical_score"
handleSpecialCases "personal-communication" = "personal_communication"
handleSpecialCases "legal-case"             = "legal_case"
handleSpecialCases x                        = x

newtype CNum = CNum { unCNum :: Int } deriving ( Show, Read, Eq, Num, Typeable, Data, Generic )

instance FromJSON CNum where
  parseJSON x = CNum `fmap` parseInt x

instance ToJSON CNum where
  toJSON (CNum n) = toJSON n

instance ToYaml CNum where
  toYaml r = Y.string (T.pack $ show $ unCNum r)

newtype CLabel = CLabel { unCLabel :: String } deriving ( Show, Read, Eq, Typeable, Data, Generic, Semigroup, Monoid )

instance FromJSON CLabel where
  parseJSON x = CLabel `fmap` parseString x

instance ToJSON CLabel where
  toJSON (CLabel s) = toJSON s

instance ToYaml CLabel where
  toYaml (CLabel s) = toYaml $ T.pack s

-- | The 'Reference' record.
data Reference =
    Reference
    { refId                    :: Literal
    , refType                  :: RefType

    , author                   :: [Agent]
    , editor                   :: [Agent]
    , translator               :: [Agent]
    , recipient                :: [Agent]
    , interviewer              :: [Agent]
    , composer                 :: [Agent]
    , director                 :: [Agent]
    , illustrator              :: [Agent]
    , originalAuthor           :: [Agent]
    , containerAuthor          :: [Agent]
    , collectionEditor         :: [Agent]
    , editorialDirector        :: [Agent]
    , reviewedAuthor           :: [Agent]

    , issued                   :: [RefDate]
    , eventDate                :: [RefDate]
    , accessed                 :: [RefDate]
    , container                :: [RefDate]
    , originalDate             :: [RefDate]
    , submitted                :: [RefDate]

    , title                    :: Formatted
    , titleShort               :: Formatted
    , reviewedTitle            :: Formatted
    , containerTitle           :: Formatted
    , volumeTitle              :: Formatted
    , collectionTitle          :: Formatted
    , containerTitleShort      :: Formatted
    , collectionNumber         :: Formatted --Int
    , originalTitle            :: Formatted
    , publisher                :: Formatted
    , originalPublisher        :: Formatted
    , publisherPlace           :: Formatted
    , originalPublisherPlace   :: Formatted
    , authority                :: Formatted
    , jurisdiction             :: Formatted
    , archive                  :: Formatted
    , archivePlace             :: Formatted
    , archiveLocation          :: Formatted
    , event                    :: Formatted
    , eventPlace               :: Formatted
    , page                     :: Formatted
    , pageFirst                :: Formatted
    , numberOfPages            :: Formatted
    , version                  :: Formatted
    , volume                   :: Formatted
    , numberOfVolumes          :: Formatted --Int
    , issue                    :: Formatted
    , chapterNumber            :: Formatted
    , medium                   :: Formatted
    , status                   :: Formatted
    , edition                  :: Formatted
    , section                  :: Formatted
    , source                   :: Formatted
    , genre                    :: Formatted
    , note                     :: Formatted
    , annote                   :: Formatted
    , abstract                 :: Formatted
    , keyword                  :: Formatted
    , number                   :: Formatted
    , references               :: Formatted
    , url                      :: Literal
    , doi                      :: Literal
    , isbn                     :: Literal
    , issn                     :: Literal
    , pmcid                    :: Literal
    , pmid                     :: Literal
    , callNumber               :: Literal
    , dimensions               :: Literal
    , scale                    :: Literal
    , categories               :: [Literal]
    , language                 :: Literal

    , citationNumber           :: CNum
    , firstReferenceNoteNumber :: Int
    , citationLabel            :: CLabel
    } deriving ( Eq, Show, Read, Typeable, Data, Generic )

instance FromJSON Reference where
  parseJSON (Object v') = do
     v <- parseSuppFields v' <|> return v'
     addPageFirst <$> (Reference <$>
       v .:? "id" .!= "" <*>
       v .:? "type" .!= NoType <*>
       v .:? "author" .!= [] <*>
       v .:? "editor" .!= [] <*>
       v .:? "translator" .!= [] <*>
       v .:? "recipient" .!= [] <*>
       v .:? "interviewer" .!= [] <*>
       v .:? "composer" .!= [] <*>
       v .:? "director" .!= [] <*>
       v .:? "illustrator" .!= [] <*>
       v .:? "original-author" .!= [] <*>
       v .:? "container-author" .!= [] <*>
       v .:? "collection-editor" .!= [] <*>
       v .:? "editorial-director" .!= [] <*>
       v .:? "reviewed-author" .!= [] <*>
       v .:? "issued" .!= [] <*>
       v .:? "event-date" .!= [] <*>
       v .:? "accessed" .!= [] <*>
       v .:? "container" .!= [] <*>
       v .:? "original-date" .!= [] <*>
       v .:? "submitted" .!= [] <*>
       v .:? "title" .!= mempty <*>
       (v .: "shortTitle" <|> (v .:? "title-short" .!= mempty)) <*>
       v .:? "reviewed-title" .!= mempty <*>
       v .:? "container-title" .!= mempty <*>
       v .:? "volume-title" .!= mempty <*>
       v .:? "collection-title" .!= mempty <*>
       (v .: "journalAbbreviation" <|> v .:? "container-title-short" .!= mempty) <*>
       v .:? "collection-number" .!= mempty <*>
       v .:? "original-title" .!= mempty <*>
       v .:? "publisher" .!= mempty <*>
       v .:? "original-publisher" .!= mempty <*>
       v .:? "publisher-place" .!= mempty <*>
       v .:? "original-publisher-place" .!= mempty <*>
       v .:? "authority" .!= mempty <*>
       v .:? "jurisdiction" .!= mempty <*>
       v .:? "archive" .!= mempty <*>
       v .:? "archive-place" .!= mempty <*>
       v .:? "archive_location" .!= mempty <*>
       v .:? "event" .!= mempty <*>
       v .:? "event-place" .!= mempty <*>
       v .:? "page" .!= mempty <*>
       v .:? "page-first" .!= mempty <*>
       v .:? "number-of-pages" .!= mempty <*>
       v .:? "version" .!= mempty <*>
       v .:? "volume" .!= mempty <*>
       v .:? "number-of-volumes" .!= mempty <*>
       v .:? "issue" .!= mempty <*>
       v .:? "chapter-number" .!= mempty <*>
       v .:? "medium" .!= mempty <*>
       v .:? "status" .!= mempty <*>
       v .:? "edition" .!= mempty <*>
       v .:? "section" .!= mempty <*>
       v .:? "source" .!= mempty <*>
       v .:? "genre" .!= mempty <*>
       v .:? "note" .!= mempty <*>
       v .:? "annote" .!= mempty <*>
       v .:? "abstract" .!= mempty <*>
       v .:? "keyword" .!= mempty <*>
       v .:? "number" .!= mempty <*>
       v .:? "references" .!= mempty <*>
       v .:? "URL" .!= "" <*>
       v .:? "DOI" .!= "" <*>
       v .:? "ISBN" .!= "" <*>
       v .:? "ISSN" .!= "" <*>
       v .:? "PMCID" .!= "" <*>
       v .:? "PMID" .!= "" <*>
       v .:? "call-number" .!= "" <*>
       v .:? "dimensions" .!= "" <*>
       v .:? "scale" .!= "" <*>
       v .:? "categories" .!= [] <*>
       v .:? "language" .!= "" <*>
       v .:? "citation-number" .!= CNum 0 <*>
       ((v .: "first-reference-note-number" >>= parseInt) <|> return 0) <*>
       v .:? "citation-label" .!= mempty)
    where takeFirstNum (Formatted (Str xs : _)) =
            case takeWhile isDigit xs of
                   [] -> mempty
                   ds -> Formatted [Str ds]
          takeFirstNum x = x
          addPageFirst ref = if pageFirst ref == mempty && page ref /= mempty
                                then ref{ pageFirst =
                                            takeFirstNum (page ref) }
                                else ref
  parseJSON _ = fail "Could not parse Reference"

-- Syntax for adding supplementary fields in note variable
-- {:authority:Superior Court of California}{:section:A}{:original-date:1777}
-- or
-- Foo\nissued: 2016-03-20/2016-07-31\nbar
-- see http://gsl-nagoya-u.net/http/pub/citeproc-doc.html#supplementary-fields
parseSuppFields :: Aeson.Object -> Parser Aeson.Object
parseSuppFields o = do
  nt <- o .: "note"
  case P.parse noteFields "note" nt of
       Left err -> fail (show err)
       Right fs -> return $ foldr (\(k,v) x -> H.insert k v x) o fs

noteFields :: P.Parser [(Text, Aeson.Value)]
noteFields = do
  fs <- P.many (Right <$> (noteField <|> lineNoteField) <|> Left <$> regText)
  P.spaces
  let rest = T.unwords (lefts fs)
  return (("note", Aeson.String rest) : rights fs)

noteField :: P.Parser (Text, Aeson.Value)
noteField = P.try $ do
  _ <- P.char '{'
  _ <- P.char ':'
  k <- P.manyTill (P.letter <|> P.char '-') (P.char ':')
  _ <- P.skipMany (P.char ' ')
  v <- P.manyTill P.anyChar (P.char '}')
  return (T.pack k, Aeson.String (T.pack v))

lineNoteField :: P.Parser (Text, Aeson.Value)
lineNoteField = P.try $ do
  _ <- P.char '\n'
  k <- P.manyTill (P.letter <|> P.char '-') (P.char ':')
  _ <- P.skipMany (P.char ' ')
  v <- P.manyTill P.anyChar (P.char '\n' <|> '\n' <$ P.eof)
  return (T.pack k, Aeson.String (T.pack v))

regText :: P.Parser Text
regText = (T.pack <$> P.many1 (P.noneOf "\n{")) <|> (T.singleton <$> P.anyChar)

instance ToJSON Reference where
  toJSON ref = object' [
      "id" .= refId ref
    , "type" .= refType ref
    , "author" .= author ref
    , "editor" .= editor ref
    , "translator" .= translator ref
    , "recipient" .= recipient ref
    , "interviewer" .= interviewer ref
    , "composer" .= composer ref
    , "director" .= director ref
    , "illustrator" .= illustrator ref
    , "original-author" .= originalAuthor ref
    , "container-author" .= containerAuthor ref
    , "collection-editor" .= collectionEditor ref
    , "editorial-director" .= editorialDirector ref
    , "reviewed-author" .= reviewedAuthor ref
    , "issued" .= issued ref
    , "event-date" .= eventDate ref
    , "accessed" .= accessed ref
    , "container" .= container ref
    , "original-date" .= originalDate ref
    , "submitted" .= submitted ref
    , "title" .= title ref
    , "title-short" .= titleShort ref
    , "reviewed-title" .= reviewedTitle ref
    , "container-title" .= containerTitle ref
    , "volume-title" .= volumeTitle ref
    , "collection-title" .= collectionTitle ref
    , "container-title-short" .= containerTitleShort ref
    , "collection-number" .= collectionNumber ref
    , "original-title" .= originalTitle ref
    , "publisher" .= publisher ref
    , "original-publisher" .= originalPublisher ref
    , "publisher-place" .= publisherPlace ref
    , "original-publisher-place" .= originalPublisherPlace ref
    , "authority" .= authority ref
    , "jurisdiction" .= jurisdiction ref
    , "archive" .= archive ref
    , "archive-place" .= archivePlace ref
    , "archive_location" .= archiveLocation ref
    , "event" .= event ref
    , "event-place" .= eventPlace ref
    , "page" .= page ref
    , "page-first" .= (if page ref == mempty then pageFirst ref else mempty)
    , "number-of-pages" .= numberOfPages ref
    , "version" .= version ref
    , "volume" .= volume ref
    , "number-of-volumes" .= numberOfVolumes ref
    , "issue" .= issue ref
    , "chapter-number" .= chapterNumber ref
    , "medium" .= medium ref
    , "status" .= status ref
    , "edition" .= edition ref
    , "section" .= section ref
    , "source" .= source ref
    , "genre" .= genre ref
    , "note" .= note ref
    , "annote" .= annote ref
    , "abstract" .= abstract ref
    , "keyword" .= keyword ref
    , "number" .= number ref
    , "references" .= references ref
    , "URL" .= url ref
    , "DOI" .= doi ref
    , "ISBN" .= isbn ref
    , "ISSN" .= issn ref
    , "PMCID" .= pmcid ref
    , "PMID" .= pmid ref
    , "call-number" .= callNumber ref
    , "dimensions" .= dimensions ref
    , "scale" .= scale ref
    , "categories" .= categories ref
    , "language" .= language ref
    , "citation-number" .= citationNumber ref
    , "first-reference-note-number" .= firstReferenceNoteNumber ref
    , "citation-label" .= citationLabel ref
    ]

instance ToYaml Reference where
  toYaml ref = mapping' [
      "id" &= refId ref
    , (("type" Y..= refType ref) :)
    , "author" &= author ref
    , "editor" &= editor ref
    , "translator" &= translator ref
    , "recipient" &= recipient ref
    , "interviewer" &= interviewer ref
    , "composer" &= composer ref
    , "director" &= director ref
    , "illustrator" &= illustrator ref
    , "original-author" &= originalAuthor ref
    , "container-author" &= containerAuthor ref
    , "collection-editor" &= collectionEditor ref
    , "editorial-director" &= editorialDirector ref
    , "reviewed-author" &= reviewedAuthor ref
    , "issued" &= issued ref
    , "event-date" &= eventDate ref
    , "accessed" &= accessed ref
    , "container" &= container ref
    , "original-date" &= originalDate ref
    , "submitted" &= submitted ref
    , "title" &= title ref
    , "title-short" &= titleShort ref
    , "reviewed-title" &= reviewedTitle ref
    , "container-title" &= containerTitle ref
    , "volume-title" &= volumeTitle ref
    , "collection-title" &= collectionTitle ref
    , "container-title-short" &= containerTitleShort ref
    , "collection-number" &= collectionNumber ref
    , "original-title" &= originalTitle ref
    , "publisher" &= publisher ref
    , "original-publisher" &= originalPublisher ref
    , "publisher-place" &= publisherPlace ref
    , "original-publisher-place" &= originalPublisherPlace ref
    , "authority" &= authority ref
    , "jurisdiction" &= jurisdiction ref
    , "archive" &= archive ref
    , "archive-place" &= archivePlace ref
    , "archive_location" &= archiveLocation ref
    , "event" &= event ref
    , "event-place" &= eventPlace ref
    , "page" &= page ref
    , "page-first" &= (if page ref == mempty then pageFirst ref else mempty)
    , "number-of-pages" &= numberOfPages ref
    , "version" &= version ref
    , "volume" &= volume ref
    , "number-of-volumes" &= numberOfVolumes ref
    , "issue" &= issue ref
    , "chapter-number" &= chapterNumber ref
    , "medium" &= medium ref
    , "status" &= status ref
    , "edition" &= edition ref
    , "section" &= section ref
    , "source" &= source ref
    , "genre" &= genre ref
    , "note" &= note ref
    , "annote" &= annote ref
    , "abstract" &= abstract ref
    , "keyword" &= keyword ref
    , "number" &= number ref
    , "references" &= references ref
    , "URL" &= url ref
    , "DOI" &= doi ref
    , "ISBN" &= isbn ref
    , "ISSN" &= issn ref
    , "PMCID" &= pmcid ref
    , "PMID" &= pmid ref
    , "call-number" &= callNumber ref
    , "dimensions" &= dimensions ref
    , "scale" &= scale ref
    , "categories" &= categories ref
    , "language" &= language ref
    , if citationNumber ref == CNum 0
         then id
         else (("citation-number" Y..= citationNumber ref) :)
    , if firstReferenceNoteNumber ref == 0
         then id
         else (("first-reference-note-number" Y..=
                firstReferenceNoteNumber ref) :)
    , if citationLabel ref == mempty
         then id
         else (("citation-label" Y..= citationLabel ref) :)
    ]

emptyReference :: Reference
emptyReference =
    Reference
    { refId               = mempty
    , refType             = NoType

    , author              = []
    , editor              = []
    , translator          = []
    , recipient           = []
    , interviewer         = []
    , composer            = []
    , director            = []
    , illustrator         = []
    , originalAuthor      = []
    , containerAuthor     = []
    , collectionEditor    = []
    , editorialDirector   = []
    , reviewedAuthor      = []

    , issued              = []
    , eventDate           = []
    , accessed            = []
    , container           = []
    , originalDate        = []
    , submitted           = []

    , title               = mempty
    , titleShort          = mempty
    , reviewedTitle       = mempty
    , containerTitle      = mempty
    , volumeTitle         = mempty
    , collectionTitle     = mempty
    , containerTitleShort = mempty
    , collectionNumber    = mempty
    , originalTitle       = mempty
    , publisher           = mempty
    , originalPublisher   = mempty
    , publisherPlace      = mempty
    , originalPublisherPlace = mempty
    , authority           = mempty
    , jurisdiction        = mempty
    , archive             = mempty
    , archivePlace        = mempty
    , archiveLocation     = mempty
    , event               = mempty
    , eventPlace          = mempty
    , page                = mempty
    , pageFirst           = mempty
    , numberOfPages       = mempty
    , version             = mempty
    , volume              = mempty
    , numberOfVolumes     = mempty
    , issue               = mempty
    , chapterNumber       = mempty
    , medium              = mempty
    , status              = mempty
    , edition             = mempty
    , section             = mempty
    , source              = mempty
    , genre               = mempty
    , note                = mempty
    , annote              = mempty
    , abstract            = mempty
    , keyword             = mempty
    , number              = mempty
    , references          = mempty
    , url                 = mempty
    , doi                 = mempty
    , isbn                = mempty
    , issn                = mempty
    , pmcid               = mempty
    , pmid                = mempty
    , callNumber          = mempty
    , dimensions          = mempty
    , scale               = mempty
    , categories          = mempty
    , language            = mempty

    , citationNumber           = CNum 0
    , firstReferenceNoteNumber = 0
    , citationLabel            = mempty
    }

numericVars :: [String]
numericVars = [ "edition", "volume", "number-of-volumes", "number", "issue", "citation-number"
              , "chapter-number", "collection-number", "number-of-pages"]

getReference :: [Reference] -> Cite -> Maybe Reference
getReference  r c
    = case citeId c `elemIndex` map (unLiteral . refId) r of
        Just i  -> Just $ setPageFirst $ r !! i
        Nothing -> Nothing

processCites :: [Reference] -> [[Cite]] -> [[(Cite, Maybe Reference)]]
processCites rs cs
    = procGr [] cs
    where
      procRef r = case filter ((==) (unLiteral $ refId r) . citeId) $ concat cs of
                    x:_ -> r { firstReferenceNoteNumber = readNum $ citeNoteNumber x}
                    []  -> r

      procGr _ [] = []
      procGr acc (x:xs) = let (a',res) = procCs acc x
                          in res : procGr ([] : a') xs

      -- process, given the accumulated history, the current group's cites
      procCs acc [] = (acc,[])
      procCs acc (c:xs) = let (a, rest) = procCs addCite xs
                              ref       = procRef <$> getReference rs c
                              c'        = c { citePosition = getCitePosition }
                          in  (a, (c', ref) : rest)
          where
            addCite = case acc of
                        []     -> [[c]]
                        (a:as) -> (c : a) : as

            -- http://docs.citationstyles.org/en/stable/specification.html#locators
            getCitePosition = fromMaybe notIbid (ibidPosition <$> prevSameCite)
                where
                    notIbid = if citeId c `elem` map citeId (concat acc)
                                 then "subsequent"
                                 else "first"

            ibidPosition x = let hasL k   = citeLocator k /= ""
                                 withIf b = if b then "ibid-with-locator" else "ibid"
                                 diffLoc  = citeLocator x /= citeLocator c
                                           || citeLabel x /= citeLabel c
                             in  case (hasL x, hasL c) of
                                   (False, cur)  -> withIf cur
                                   (True, True)  -> withIf diffLoc
                                   (True, False) -> "subsequent"

            -- x is previous cite in current group
            -- zs is the previous group
            prevSameCite = case acc of
                             []     -> Nothing
                             (a:as) -> psc a as
              where
                -- you can't have an ibid at the start of your document
                psc [] []     = Nothing

                -- a. the current cite immediately follows on another cite,
                --    within the same citation, that references the same item
                psc (x:_) _   = if citeId c == citeId x
                                   then Just x
                                   else Nothing

                -- b.  [] => the current cite is the first cite in the citation
                --     zs => and the previous citation consists of a single cite
                --           that refs the same item
                -- The spec appears to be concerned that you cannot know the
                -- correct one to match the locator against.
                -- It is a super clunky if you have [@a, 1; @a, 2] then [@a, 3],
                -- where the second citation gets "subsequent" even though there were
                -- only @a keys.
                -- This is silly. We will use the last one to match against.
                psc [] (zs:_) = case zs of
                                  [] -> Nothing
                                  (z:_) -> if all (== citeId c) (map citeId zs)
                                              then Just z
                                              else Nothing

setPageFirst :: Reference -> Reference
setPageFirst ref =
  let Formatted ils = page ref
      ils' = takeWhile (\i -> i /= Str "–" && i /= Str "-") ils
  in  if ils == ils'
         then ref
         else ref{ pageFirst = Formatted ils' }

setNearNote :: Style -> [[Cite]] -> [[Cite]]
setNearNote s cs
    = procGr [] cs
    where
      near_note   = let nn = fromMaybe [] . lookup "near-note-distance" . citOptions . citation $ s
                    in  if null nn then 5 else readNum nn
      procGr _ [] = []
      procGr a (x:xs) = let (a',res) = procCs a x
                        in res : procGr a' xs

      procCs a []     = (a,[])
      procCs a (c:xs) = (a', c { nearNote = isNear} : rest)
          where
            (a', rest) = procCs (c:a) xs
            isNear     = case filter ((==) (citeId c) . citeId) a of
                           x:_ -> citeNoteNumber c /= "0" &&
                                  citeNoteNumber x /= "0" &&
                                  readNum (citeNoteNumber c) - readNum (citeNoteNumber x) <= near_note
                           _   -> False

parseRawDate :: String -> [RefDate]
parseRawDate o =
  case P.parse rawDate "raw date" o of
       Left _   -> [RefDate Nothing Nothing Nothing Nothing (Literal o) False]
       Right ds -> ds

rawDate :: P.Parser [RefDate]
rawDate = rawDateISO <|> rawDateOld

parseEDTFDate :: String -> [RefDate]
parseEDTFDate o =
  case handleRanges (trim o) of
       [] -> []
       o' -> case P.parse rawDateISO "date" o' of
                Left _   -> []
                Right ds -> ds
    where handleRanges s =
            case splitWhen (=='/') s of
                 -- 199u EDTF format for a range
                 [x] | 'u' `elem` x ->
                      map (\c -> if c == 'u' then '0' else c) x ++ "/" ++
                      map (\c -> if c == 'u' then '9' else c) x
                 [x, "open"] -> x ++ "/"    -- EDTF
                 [x, "unknown"] -> x ++ "/" -- EDTF
                 _  -> s

rawDateISO :: P.Parser [RefDate]
rawDateISO = do
  d1 <- isoDate
  P.option [d1] (P.char '/' >>
                  (\x -> [d1, x]) <$>
                   (  isoDate <|> return emptydate )) <* P.eof
   where emptydate = RefDate Nothing Nothing Nothing Nothing mempty False

isoDate :: P.Parser RefDate
isoDate = P.try $ do
  extyear <- P.option False (True <$ P.char 'y')  -- EDTF year > 4 digits
  -- needed for bibtex
  y <- do
    sign <- P.option "" (P.string "-")
    rest <- P.count 4 P.digit
    extended <- if extyear
                   then P.many P.digit
                   else return []
    return $ case safeRead (sign ++ rest ++ extended) of
                    Just x | x <= 0 -> Just (x - 1)  -- 0 = -1 AD
                    x               -> x
  m' <- P.option Nothing $ Just <$> P.try (P.char '-' >> P.many1 P.digit)
  (m,s) <- case m' >>= safeRead of
                   Just (n::Int)
                          | n >= 1 && n <= 12  -> return (Just n, Nothing)
                          | n >= 13 && n <= 16 -> return (Nothing, pseudoMonthToSeason n)
                          | n >= 21 && n <= 24 -> return (Nothing, pseudoMonthToSeason n)
                   Nothing | isNothing m' -> return (Nothing, Nothing)
                   _ -> fail "Improper month"
  d <- P.option Nothing $ safeRead <$> P.try (P.char '-' >> P.many1 P.digit)
  guard $ case d of
           Nothing -> True
           Just (n::Int) | n >= 1 && n <= 31 -> True
           _ -> False
  P.optional $ do
    _ <- P.char 'T'
    _ <- P.many (P.digit <|> P.char ':')
    P.optional $ (P.oneOf "+-" >> P.many1 (P.digit <|> P.char ':'))
              <|> P.string "Z"
  _ <- P.optional (P.char '?')
  c <- P.option False (True <$ P.char '~')
  return RefDate{ year = y, month = m,
                  season = s, day = d,
                  other = mempty, circa = c }

rawDateOld :: P.Parser [RefDate]
rawDateOld = do
  let months   = ["jan","feb","mar","apr","may","jun","jul","aug",
                  "sep","oct","nov","dec"]
  let seasons  = ["spr","sum","fal","win"]
  let pmonth = P.try $ do
        xs <- P.many1 P.letter <|> P.many1 P.digit
        if all isDigit xs
           then case safeRead xs of
                      Just (n::Int) | n >= 1 && n <= 12 -> return (Just n)
                      _ -> fail "Improper month"
           else case elemIndex (map toLower $ take 3 xs) months of
                     Nothing -> fail "Improper month"
                     Just n  -> return (Just (n+1))
  let pseason = P.try $ do
        xs <- P.many1 P.letter
        case elemIndex (map toLower $ take 3 xs) seasons of
             Just 0  -> return (Just Spring)
             Just 1  -> return (Just Summer)
             Just 2  -> return (Just Autumn)
             Just 3  -> return (Just Winter)
             _       -> fail "Improper season"
  let pday = P.try $ do
        xs <- P.many1 P.digit
        case safeRead xs of
             Just (n::Int) | n >= 1 && n <= 31 -> return (Just n)
             _ -> fail "Improper day"
  let pyear = safeRead <$> P.many1 P.digit
  let sep = P.oneOf [' ','/',','] >> P.spaces
  let rangesep = P.try $ P.spaces >> P.char '-' >> P.spaces
  let refDate = RefDate Nothing Nothing Nothing Nothing mempty False
  let date = P.choice $ map P.try [
                 (do s <- pseason
                     sep
                     y <- pyear
                     return refDate{ year = y, season = s })
               , (do m <- pmonth
                     sep
                     d <- pday
                     sep
                     y <- pyear
                     return refDate{ year = y, month = m, day = d })
               , (do m <- pmonth
                     sep
                     y <- pyear
                     return refDate{ year = y, month = m })
               , (do y <- pyear
                     return refDate{ year = y })
               ]
  d1 <- date
  P.option [d1] ((\x -> [d1,x]) <$> (rangesep >> date))

