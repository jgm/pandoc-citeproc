{-# LANGUAGE GeneralizedNewtypeDeriving, PatternGuards, OverloadedStrings,
  DeriveDataTypeable, ExistentialQuantification, FlexibleInstances,
  ScopedTypeVariables, GeneralizedNewtypeDeriving, IncoherentInstances,
  DeriveGeneric #-}
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
                          , RefDate(..)
                          , handleLiteral
                          , toDatePart
                          , setCirca
                          , mkRefDate
                          , RefType(..)
                          , CNum(..)
                          , Reference(..)
                          , emptyReference
                          , numericVars
                          , getReference
                          , processCites
                          , setPageFirst
                          , setNearNote
                          )
where

import Data.List  ( elemIndex, intercalate )
import Data.List.Split ( splitWhen )
import Data.Maybe ( fromMaybe             )
import Data.Generics hiding (Generic)
import GHC.Generics (Generic)
import Data.Aeson hiding (Value)
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser)
import qualified Data.Yaml.Builder as Y
import Data.Yaml.Builder (ToYaml(..))
import Control.Applicative ((<|>))
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Vector as V
import Data.Char (toLower, isDigit)
import Text.CSL.Style hiding (Number)
import Text.CSL.Util (parseString, parseInt, parseBool, safeRead, readNum,
                      inlinesToString, capitalize, camelize, uncamelize,
                      (&=), mapping')
import Text.Pandoc (Inline(Str))
import Data.String
import qualified Text.Parsec as P
import qualified Text.Parsec.String as P
import qualified Data.HashMap.Strict as H

newtype Literal = Literal { unLiteral :: String }
  deriving ( Show, Read, Eq, Data, Typeable, Monoid, Generic )

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

mkRefMap :: Data a => a -> ReferenceMap
mkRefMap a = zip fields (gmapQ Value a)
    where fields = map uncamelize . constrFields . toConstr $ a

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
    | Just _ <- fromValue val :: Maybe Empty     = True
    | otherwise = False

data Empty = Empty deriving ( Typeable, Data, Generic )

data RefDate =
    RefDate { year   :: Literal
            , month  :: Literal
            , season :: Literal
            , day    :: Literal
            , other  :: Literal
            , circa  :: Bool
            } deriving ( Show, Read, Eq, Typeable, Data, Generic )

instance FromJSON RefDate where
  parseJSON (Array v) =
     case fromJSON (Array v) of
          Success [y]     -> RefDate <$> parseJSON y <*>
                    pure "" <*> pure "" <*> pure "" <*> pure "" <*> pure False
          Success [y,m]   -> RefDate <$> parseJSON y <*> parseJSON m <*>
                    pure "" <*> pure "" <*> pure "" <*> pure False
          Success [y,m,d] -> RefDate <$> parseJSON y <*> parseJSON m <*>
                    pure "" <*> parseJSON d <*> pure "" <*> pure False
          Error e         -> fail $ "Could not parse RefDate: " ++ e
          _               -> fail "Could not parse RefDate"
  parseJSON (Object v) = RefDate <$>
              v .:? "year" .!= "" <*>
              v .:? "month" .!= "" <*>
              v .:? "season" .!= "" <*>
              v .:? "day" .!= "" <*>
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
  toYaml r = mapping' [ "year" &= year r
                      , "month" &= month r
                      , "season" &= season r
                      , "day" &= day r
                      , "literal" &= other r
                      , "circa" &= T.pack (if circa r then "1" else "")
                      ]

instance FromJSON [RefDate] where
  parseJSON (Array xs) = mapM parseJSON $ V.toList xs
  parseJSON (Object v) = do
    dateParts <- v .:? "date-parts"
    circa' <- (v .: "circa" >>= parseBool) <|> pure False
    case dateParts of
         Just (Array xs) -> mapM (fmap (setCirca circa') . parseJSON)
                            $ V.toList xs
         _               -> handleLiteral <$> parseJSON (Object v)
  parseJSON x          = parseJSON x >>= mkRefDate

-- Zotero doesn't properly support date ranges, so a common
-- workaround is 2005_2007 or 2005_; support this as date range:
handleLiteral :: RefDate -> [RefDate]
handleLiteral d@(RefDate (Literal "") (Literal "") (Literal "")
                         (Literal "") (Literal xs) b)
  = case splitWhen (=='_') xs of
         [x,y] | all isDigit x && all isDigit y &&
                 not (null x) ->
                 [RefDate (Literal x) mempty mempty mempty mempty b,
                  RefDate (Literal y) mempty mempty mempty mempty b]
         _ -> [d]
handleLiteral d = [d]

toDatePart :: RefDate -> [Int]
toDatePart refdate =
    case (safeRead (unLiteral $ year refdate),
          safeRead (unLiteral $ month refdate),
          safeRead (unLiteral $ day refdate)) of
         (Just (y :: Int), Just (m :: Int), Just (d :: Int))
                                     -> [y, m, d]
         (Just y, Just m, Nothing)   -> [y, m]
         (Just y, Nothing, Nothing)  -> [y]
         _                           -> []

instance ToJSON [RefDate] where
  toJSON [] = Array V.empty
  toJSON xs = object' $
    case filter (not . null) (map toDatePart xs) of
         []  -> ["literal" .= intercalate "; " (map (unLiteral . other) xs)]
         dps -> (["date-parts" .= dps ] ++
                 ["circa" .= (1 :: Int) | or (map circa xs)] ++
                 ["season" .= s | s <- map season xs, s /= mempty])

-- instance ToJSON [RefDate]
-- toJSON xs  = Array (V.fromList $ map toJSON xs)

setCirca :: Bool -> RefDate -> RefDate
setCirca circa' rd = rd{ circa = circa' }

mkRefDate :: Literal -> Parser [RefDate]
mkRefDate z@(Literal xs)
  | all isDigit xs = return [RefDate z mempty mempty mempty mempty False]
  | otherwise      = return [RefDate mempty mempty mempty mempty z False]

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
    -- TODO: clean up this mess. For now, commenting these out makes
    -- motion pictures work properly again.
    -- show MotionPicture = "motion_picture"
    -- show MusicalScore = "musical_score"
    -- show PersonalCommunication = "personal_communication"
    -- show LegalCase = "legal_case"
    show x = map toLower . uncamelize . showConstr . toConstr $ x

instance FromJSON RefType where
  parseJSON (String t) = safeRead (capitalize . camelize . T.unpack $ t)
  parseJSON v@(Array _) = fmap (capitalize . camelize . inlinesToString)
    (parseJSON v) >>= safeRead
  parseJSON _ = fail "Could not parse RefType"

instance ToJSON RefType where
  toJSON reftype = toJSON (uncamelize $ show reftype)

instance ToYaml RefType where
  toYaml r = Y.string (T.pack $ uncamelize $ show r)

newtype CNum = CNum { unCNum :: Int } deriving ( Show, Read, Eq, Num, Typeable, Data, Generic )

instance FromJSON CNum where
  parseJSON x = CNum `fmap` parseInt x

instance ToJSON CNum where
  toJSON (CNum n) = toJSON n

instance ToYaml CNum where
  toYaml r = Y.string (T.pack $ show $ unCNum r)

-- | The 'Reference' record.
data Reference =
    Reference
    { refId               :: Literal
    , refType             :: RefType

    , author              :: [Agent]
    , editor              :: [Agent]
    , translator          :: [Agent]
    , recipient           :: [Agent]
    , interviewer         :: [Agent]
    , composer            :: [Agent]
    , director            :: [Agent]
    , illustrator         :: [Agent]
    , originalAuthor      :: [Agent]
    , containerAuthor     :: [Agent]
    , collectionEditor    :: [Agent]
    , editorialDirector   :: [Agent]
    , reviewedAuthor      :: [Agent]

    , issued              :: [RefDate]
    , eventDate           :: [RefDate]
    , accessed            :: [RefDate]
    , container           :: [RefDate]
    , originalDate        :: [RefDate]
    , submitted           :: [RefDate]

    , title               :: Formatted
    , titleShort          :: Formatted
    , reviewedTitle       :: Formatted
    , containerTitle      :: Formatted
    , volumeTitle         :: Formatted
    , collectionTitle     :: Formatted
    , containerTitleShort :: Formatted
    , collectionNumber    :: Formatted --Int
    , originalTitle       :: Formatted
    , publisher           :: Formatted
    , originalPublisher   :: Formatted
    , publisherPlace      :: Formatted
    , originalPublisherPlace :: Formatted
    , authority           :: Formatted
    , jurisdiction        :: Formatted
    , archive             :: Formatted
    , archivePlace        :: Formatted
    , archiveLocation     :: Formatted
    , event               :: Formatted
    , eventPlace          :: Formatted
    , page                :: Formatted
    , pageFirst           :: Formatted
    , numberOfPages       :: Formatted
    , version             :: Formatted
    , volume              :: Formatted
    , numberOfVolumes     :: Formatted --Int
    , issue               :: Formatted
    , chapterNumber       :: Formatted
    , medium              :: Formatted
    , status              :: Formatted
    , edition             :: Formatted
    , section             :: Formatted
    , source              :: Formatted
    , genre               :: Formatted
    , note                :: Formatted
    , annote              :: Formatted
    , abstract            :: Formatted
    , keyword             :: Formatted
    , number              :: Formatted
    , references          :: Formatted
    , url                 :: Literal
    , doi                 :: Literal
    , isbn                :: Literal
    , issn                :: Literal
    , pmcid               :: Literal
    , pmid                :: Literal
    , callNumber          :: Literal
    , dimensions          :: Literal
    , scale               :: Literal
    , categories          :: [Literal]
    , language            :: Literal

    , citationNumber           :: CNum
    , firstReferenceNoteNumber :: Int
    , citationLabel            :: Literal
    } deriving ( Eq, Show, Read, Typeable, Data, Generic )

instance FromJSON Reference where
  parseJSON (Object v') = do
     v <- parseSuppFields v'
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
       v .:? "citation-label" .!= "")
    where takeFirstNum (Formatted (Str xs : _)) =
            case takeWhile isDigit xs of
                   []   -> mempty
                   ds   -> Formatted [Str ds]
          takeFirstNum x = x
          addPageFirst ref = if pageFirst ref == mempty && page ref /= mempty
                                then ref{ pageFirst =
                                            takeFirstNum (page ref) }
                                else ref
  parseJSON _ = fail "Could not parse Reference"

-- Syntax for adding supplementary fields in note variable
-- {:authority:Superior Court of California}{:section:A}{:original-date:1777}
-- see http://gsl-nagoya-u.net/http/pub/citeproc-doc.html#supplementary-fields
parseSuppFields :: Aeson.Object -> Parser Aeson.Object
parseSuppFields o = do
  nt <- o .: "note" <|> return ("" :: String)
  case P.parse noteFields "note" nt of
       Left err -> fail (show err)
       Right fs -> return $ foldr (\(k,v) x -> H.insert k v x) o fs

noteFields :: P.Parser [(Text, Aeson.Value)]
noteFields = do
  -- TODO - actually parse the fields
  fs <- P.many noteField
  rest <- P.getInput
  return (("note", Aeson.String (T.pack rest)) : fs)

noteField :: P.Parser (Text, Aeson.Value)
noteField = P.try $ do
  P.char '{'
  P.char ':'
  k <- P.manyTill (P.letter <|> P.char '-') (P.char ':')
  v <- P.manyTill P.anyChar (P.char '}')
  return (T.pack k, Aeson.String (T.pack v))

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
    , "citation-label" &= citationLabel ref
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

processCites :: [Reference] -> [[Cite]] -> [[(Cite, Reference)]]
processCites rs cs
    = procGr [[]] cs
    where
      procRef r = case filter ((==) (unLiteral $ refId r) . citeId) $ concat cs of
                    x:_ -> r { firstReferenceNoteNumber = readNum $ citeNoteNumber x}
                    []  -> r
      getRef  c = case filter ((==) (citeId c) . unLiteral . refId) rs of
                    x:_ -> procRef $ setPageFirst x
                    []  -> emptyReference { title =
                            fromString $ citeId c ++ " not found!" }

      procGr _ [] = []
      procGr a (x:xs) = let (a',res) = procCs a x
                        in res : procGr (a' ++ [[]]) xs

      procCs a [] = (a,[])
      procCs a (c:xs)
          | isIbid,  isLocSet = go "ibid-with-locator"
          | isIbid            = go "ibid"
          | isElem            = go "subsequent"
          | otherwise         = go "first"
          where
            go s = let addCite    = init a ++ [last a ++ [c]]
                       (a', rest) = procCs addCite xs
                   in  (a', (c { citePosition = s}, getRef c) : rest)
            isElem   = citeId c `elem` map citeId (concat a)
            isIbid   = case reverse (last a) of
                            []    -> case reverse (init a) of
                                          []     -> False
                                          (zs:_) -> not (null zs) &&
                                                    all (== citeId c)
                                                        (map citeId zs)
                            (x:_) -> citeId c == citeId x
            isLocSet = citeLocator c /= ""

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
                    in  if nn == [] then 5 else readNum nn
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
