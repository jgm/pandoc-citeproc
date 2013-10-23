{-# LANGUAGE GeneralizedNewtypeDeriving, PatternGuards, OverloadedStrings,
  DeriveDataTypeable, ExistentialQuantification, FlexibleInstances #-}
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

module Text.CSL.Reference where

import Data.List  ( elemIndex, isPrefixOf )
import Data.Maybe ( fromMaybe             )
import Data.Generics
import Data.Aeson hiding (Value)
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser, Pair)
import Control.Applicative ((<$>), (<*>), (<|>), pure)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Char (toUpper, isSpace, toLower, isUpper, isLower, isDigit)

import Text.CSL.Style
import Text.CSL.Output.Plain ((<+>))

-- | An existential type to wrap the different types a 'Reference' is
-- made of. This way we can create a map to make queries easier.
data Value = forall a . Data a => Value a

-- for debuging
instance Show Value where
    show (Value a) = gshow a

type ReferenceMap = [(String, Value)]

mkRefMap :: Data a => a -> ReferenceMap
mkRefMap a = zip fields (gmapQ Value a)
    where fields = map formatField . constrFields . toConstr $ a

formatField :: String -> String
formatField = foldr f [] . g
    where f  x xs  = if isUpper x then '-' : toLower x : xs else x : xs
          g (x:xs) = toLower x : xs
          g     [] = []

fromValue :: Data a => Value -> Maybe a
fromValue (Value a) = cast a

isValueSet :: Value -> Bool
isValueSet val
    | Just v <- fromValue val :: Maybe String    = v /= []
    | Just v <- fromValue val :: Maybe [Agent]   = v /= []
    | Just v <- fromValue val :: Maybe [RefDate] = v /= []
    | Just v <- fromValue val :: Maybe Int       = v /= 0
    | Just v <- fromValue val :: Maybe CNum      = v /= 0
    | Just _ <- fromValue val :: Maybe Empty     = True
    | otherwise = False

data Empty = Empty deriving ( Typeable, Data )

data Agent
    = Agent { givenName       :: [String]
            , droppingPart    ::  String
            , nonDroppingPart ::  String
            , familyName      ::  String
            , nameSuffix      ::  String
            , literal         ::  String
            , commaSuffix     ::  Bool
            }
      deriving ( Read, Eq, Typeable, Data )

instance Show Agent where
    show (Agent g d n f s [] _) = (foldr (<+>) [] g) <+> d <+> n <+> f <+> s
    show (Agent _ _ _ _ _ l  _) = l

instance FromJSON Agent where
  parseJSON (Object v) = Agent <$>
              (v .: "given" <|> ((:[]) <$> v .: "given") <|> pure []) <*>
              v .:?  "dropping-particle" .!= "" <*>
              v .:? "non-dropping-particle" .!= "" <*>
              v .:? "family" .!= "" <*>
              v .:? "suffix" .!= "" <*>
              v .:? "literal" .!= "" <*>
              v .:? "comma-suffix" .!= False
  parseJSON _ = fail "Could not parse Agent"

instance ToJSON Agent where
  toJSON agent = object' $ [
      "given" .= givenName agent
    , "dropping-particle" .= droppingPart agent
    , "non-dropping-particle" .= nonDroppingPart agent
    , "family" .= familyName agent
    , "suffix" .= nameSuffix agent
    , "literal" .= literal agent
    ] ++ ["comma-suffix" .= commaSuffix agent | not (null (nameSuffix agent))]

instance FromJSON [Agent] where
  parseJSON (Array xs) = mapM parseJSON $ V.toList xs
  parseJSON (Object v) = (:[]) `fmap` parseJSON (Object v)
  parseJSON (String t) = parseJSON (String t) >>= mkAgent
  parseJSON _ = fail "Could not parse [Agent]"

instance ToJSON [Agent] where
  toJSON [x] = toJSON x
  toJSON xs  = Array (V.fromList $ map toJSON xs)

mkAgent :: Text -> Parser [Agent]
mkAgent t =
  case reverse (words $ T.unpack t) of
       (x:ys) -> return [Agent (reverse ys) [] [] x [] [] False]
       []     -> fail "Empty text cannot produce [Agent]"


data RefDate =
    RefDate { year   :: String
            , month  :: String
            , season :: String
            , day    :: String
            , other  :: String
            , circa  :: String
            } deriving ( Show, Read, Eq, Typeable, Data )

instance FromJSON RefDate where
  parseJSON (Array v) =
     case fromJSON (Array v) of
          Success [y]     -> return $ RefDate y "" "" "" "" ""
          Success [y,m]   -> return $ RefDate y m "" "" "" ""
          Success [y,m,d] -> return $ RefDate y m "" d "" ""
          Error e         -> fail $ "Could not parse RefDate: " ++ e
          _               -> fail "Could not parse RefDate"
  parseJSON (Object v) = RefDate <$>
              v .:? "year" .!= "" <*>
              v .:? "month" .!= "" <*>
              v .:? "season" .!= "" <*>
              v .:? "day" .!= "" <*>
              v .:? "other" .!= "" <*>
              v .:? "circa" .!= ""
  parseJSON _ = fail "Could not parse RefDate"

instance ToJSON RefDate where
  toJSON refdate = object' [
      "year" .= year refdate
    , "month" .= month refdate
    , "season" .= season refdate
    , "day" .= day refdate
    , "other" .= other refdate
    , "circa" .= circa refdate
    ]

instance FromJSON [RefDate] where
  parseJSON (Array xs) = mapM parseJSON $ V.toList xs
  parseJSON (Object v) = do
    dateParts <- v .:? "date-parts"
    case dateParts of
         Just (Array xs) -> mapM parseJSON $ V.toList xs
         _               -> (:[]) `fmap` parseJSON (Object v)
  parseJSON x          = parseJSON x >>= mkRefDate

instance ToJSON [RefDate] where
  toJSON [x] = toJSON x
  toJSON xs  = Array (V.fromList $ map toJSON xs)

mkRefDate :: String -> Parser [RefDate]
mkRefDate xs
  | all isDigit xs = return [RefDate xs "" "" "" "" ""]
  | otherwise      = return [RefDate "" "" "" "" xs ""]

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
      deriving ( Read, Eq, Typeable, Data )

instance Show RefType where
    show = map toLower . formatField . showConstr . toConstr

instance FromJSON RefType where
  parseJSON (String t) = safeRead (capitalize . camelize . T.unpack $ t)
    where camelize x
            | '-':y:ys <- x = toUpper y : camelize ys
            | '_':y:ys <- x = toUpper y : camelize ys
            |     y:ys <- x =        y : camelize ys
            | otherwise     = []
          capitalize (x:xs) = toUpper x : xs
          capitalize     [] = []
  parseJSON _ = fail "Could not parse RefType"

instance ToJSON RefType where
  toJSON reftype = toJSON (uncamelize $ uncapitalize $ show reftype)
   where uncamelize [] = []
         uncamelize (x:y:zs)
          | isLower x && isUpper y = x:'-':toLower y:uncamelize zs
         uncamelize (x:xs) = x : uncamelize xs
         uncapitalize (x:xs) = toLower x : xs
         uncapitalize []     = []

newtype CNum = CNum { unCNum :: Int } deriving ( Show, Read, Eq, Num, Typeable, Data )

instance FromJSON CNum where
  parseJSON x = case fromJSON x of
                     Success n -> return $ CNum n
                     _         -> fail "Could not parse CNum"

instance ToJSON CNum where
  toJSON (CNum n) = toJSON n

-- | The 'Reference' record.
data Reference =
    Reference
    { refId               :: String
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

    , title               :: String
    , titleShort          :: String
    , reviewedTitle       :: String
    , containerTitle      :: String
    , volumeTitle         :: String
    , collectionTitle     :: String
    , containerTitleShort :: String
    , collectionNumber    :: String --Int
    , originalTitle       :: String
    , publisher           :: String
    , originalPublisher   :: String
    , publisherPlace      :: String
    , originalPublisherPlace :: String
    , authority           :: String
    , jurisdiction        :: String
    , archive             :: String
    , archivePlace        :: String
    , archiveLocation     :: String
    , event               :: String
    , eventPlace          :: String
    , page                :: String
    , pageFirst           :: String
    , numberOfPages       :: String
    , version             :: String
    , volume              :: String
    , numberOfVolumes     :: String --Int
    , issue               :: String
    , chapterNumber       :: String
    , medium              :: String
    , status              :: String
    , edition             :: String
    , section             :: String
    , source              :: String
    , genre               :: String
    , note                :: String
    , annote              :: String
    , abstract            :: String
    , keyword             :: String
    , number              :: String
    , references          :: String
    , url                 :: String
    , doi                 :: String
    , isbn                :: String
    , issn                :: String
    , pmcid               :: String
    , pmid                :: String
    , callNumber          :: String
    , dimensions          :: String
    , scale               :: String
    , categories          :: [String]
    , language            :: String

    , citationNumber           :: CNum
    , firstReferenceNoteNumber :: Int
    , citationLabel            :: String
    } deriving ( Eq, Show, Read, Typeable, Data )

instance FromJSON Reference where
  parseJSON (Object v) = Reference <$>
       v .: "id" <*>
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
       v .:? "title" .!= "" <*>
       v .:? "title-short" .!= "" <*>
       v .:? "reviewed-title" .!= "" <*>
       v .:? "container-title" .!= "" <*>
       v .:? "volume-title" .!= "" <*>
       v .:? "collection-title" .!= "" <*>
       v .:? "container-title-short" .!= "" <*>
       v .:? "collection-number" .!= "" <*>
       v .:? "original-title" .!= "" <*>
       v .:? "publisher" .!= "" <*>
       v .:? "original-publisher" .!= "" <*>
       v .:? "publisher-place" .!= "" <*>
       v .:? "original-publisher-place" .!= "" <*>
       v .:? "authority" .!= "" <*>
       v .:? "jurisdiction" .!= "" <*>
       v .:? "archive" .!= "" <*>
       v .:? "archive-place" .!= "" <*>
       v .:? "archive-location" .!= "" <*>
       v .:? "event" .!= "" <*>
       v .:? "event-place" .!= "" <*>
       v .:? "page" .!= "" <*>
       v .:? "page-first" .!= "" <*>
       v .:? "number-of-pages" .!= "" <*>
       v .:? "version" .!= "" <*>
       v .:? "volume" .!= "" <*>
       v .:? "number-of-volumes" .!= "" <*>
       v .:? "issue" .!= "" <*>
       v .:? "chapter-number" .!= "" <*>
       v .:? "medium" .!= "" <*>
       v .:? "status" .!= "" <*>
       v .:? "edition" .!= "" <*>
       v .:? "section" .!= "" <*>
       v .:? "source" .!= "" <*>
       v .:? "genre" .!= "" <*>
       v .:? "note" .!= "" <*>
       v .:? "annote" .!= "" <*>
       v .:? "abstract" .!= "" <*>
       v .:? "keyword" .!= "" <*>
       v .:? "number" .!= "" <*>
       v .:? "references" .!= "" <*>
       v .:? "url" .!= "" <*>
       v .:? "doi" .!= "" <*>
       v .:? "isbn" .!= "" <*>
       v .:? "issn" .!= "" <*>
       v .:? "pmcid" .!= "" <*>
       v .:? "pmid" .!= "" <*>
       v .:? "call-number" .!= "" <*>
       v .:? "dimensions" .!= "" <*>
       v .:? "scale" .!= "" <*>
       v .:? "categories" .!= [] <*>
       v .:? "language" .!= "" <*>
       v .:? "citation-number" .!= CNum 0 <*>
       v .:? "first-reference-note-number" .!= 1 <*>
       v .:? "citation-label" .!= ""
  parseJSON _ = fail "Could not parse Reference"

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
    , "archive-location" .= archiveLocation ref
    , "event" .= event ref
    , "event-place" .= eventPlace ref
    , "page" .= page ref
    , "page-first" .= pageFirst ref
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
    , "url" .= url ref
    , "doi" .= doi ref
    , "isbn" .= isbn ref
    , "issn" .= issn ref
    , "pmcid" .= pmcid ref
    , "pmid" .= pmid ref
    , "call-number" .= callNumber ref
    , "dimensions" .= dimensions ref
    , "scale" .= scale ref
    , "categories" .= categories ref
    , "language" .= language ref
    , "citation-number" .= citationNumber ref
    , "first-reference-note-number" .= firstReferenceNoteNumber ref
    , "citation-label" .= citationLabel ref
    ]

emptyReference :: Reference
emptyReference =
    Reference
    { refId               = []
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

    , title               = []
    , titleShort          = []
    , reviewedTitle       = []
    , containerTitle      = []
    , volumeTitle         = []
    , collectionTitle     = []
    , containerTitleShort = []
    , collectionNumber    = []
    , originalTitle       = []
    , publisher           = []
    , originalPublisher   = []
    , publisherPlace      = []
    , originalPublisherPlace = []
    , authority           = []
    , jurisdiction        = []
    , archive             = []
    , archivePlace        = []
    , archiveLocation     = []
    , event               = []
    , eventPlace          = []
    , page                = []
    , pageFirst           = []
    , numberOfPages       = []
    , version             = []
    , volume              = []
    , numberOfVolumes     = []
    , issue               = []
    , chapterNumber       = []
    , medium              = []
    , status              = []
    , edition             = []
    , section             = []
    , source              = []
    , genre               = []
    , note                = []
    , annote              = []
    , abstract            = []
    , keyword             = []
    , number              = []
    , references          = []
    , url                 = []
    , doi                 = []
    , isbn                = []
    , issn                = []
    , pmcid               = []
    , pmid                = []
    , callNumber          = []
    , dimensions          = []
    , scale               = []
    , categories          = []
    , language            = []

    , citationNumber           = CNum 0
    , firstReferenceNoteNumber = 0
    , citationLabel            = []
    }

numericVars :: [String]
numericVars = [ "edition", "volume", "number-of-volumes", "number", "issue", "citation-number"
              , "chapter-number", "collection-number", "number-of-pages"]

parseLocator :: String -> (String, String)
parseLocator s
    | "b"    `isPrefixOf` formatField s = mk "book"
    | "ch"   `isPrefixOf` formatField s = mk "chapter"
    | "co"   `isPrefixOf` formatField s = mk "column"
    | "fi"   `isPrefixOf` formatField s = mk "figure"
    | "fo"   `isPrefixOf` formatField s = mk "folio"
    | "i"    `isPrefixOf` formatField s = mk "issue"
    | "l"    `isPrefixOf` formatField s = mk "line"
    | "n"    `isPrefixOf` formatField s = mk "note"
    | "o"    `isPrefixOf` formatField s = mk "opus"
    | "para" `isPrefixOf` formatField s = mk "paragraph"
    | "part" `isPrefixOf` formatField s = mk "part"
    | "p"    `isPrefixOf` formatField s = mk "page"
    | "sec"  `isPrefixOf` formatField s = mk "section"
    | "sub"  `isPrefixOf` formatField s = mk "sub verbo"
    | "ve"   `isPrefixOf` formatField s = mk "verse"
    | "v"    `isPrefixOf` formatField s = mk "volume"
    | otherwise                         =    ([], [])
    where
      mk c = if null s then ([], []) else (,) c . unwords . tail . words $ s

getReference :: [Reference] -> Cite -> Maybe Reference
getReference  r c
    = case citeId c `elemIndex` map refId r of
        Just i  -> Just $ setPageFirst $ r !! i
        Nothing -> Nothing

processCites :: [Reference] -> [[Cite]] -> [[(Cite, Reference)]]
processCites rs cs
    = procGr [[]] cs
    where
      procRef r = case filter ((==) (refId r) . citeId) $ concat cs of
                    x:_ -> r { firstReferenceNoteNumber = readNum $ citeNoteNumber x}
                    []  -> r
      getRef  c = case filter ((==) (citeId c) . refId) rs of
                    x:_ -> procRef $ setPageFirst x
                    []  -> emptyReference { title = citeId c ++ " not found!" }

      procGr _ [] = []
      procGr a (x:xs) = let (a',res) = procCs a x
                        in res : procGr (a' ++ [[]]) xs

      procCs a [] = (a,[])
      procCs a (c:xs)
          | isIbidC, isLocSet = go "ibid-with-locator-c"
          | isIbid,  isLocSet = go "ibid-with-locator"
          | isIbidC           = go "ibid-c"
          | isIbid            = go "ibid"
          | isElem            = go "subsequent"
          | otherwise         = go "first"
          where
            go s = let addCite    = if last a /= [] then init a ++ [last a ++ [c]] else init a ++ [[c]]
                       (a', rest) = procCs addCite xs
                   in  (a', (c { citePosition = s}, getRef c) : rest)
            isElem   = citeId c `elem` map citeId (concat a)
            -- Ibid in same citation
            isIbid   = last a /= [] && citeId c == citeId (last $ last a)
            -- Ibid in different citations (must be capitalized)
            isIbidC  = init a /= [] && length (last $ init a) == 1 &&
                       last a == [] && citeId c == citeId (head . last $ init a)
            isLocSet = citeLocator c /= ""

setPageFirst :: Reference -> Reference
setPageFirst r = if ('–' `elem` page r || '-' `elem` page r)
                 then r { pageFirst = takeWhile (not . flip elem "–-") $ page r}
                 else r

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

object' :: [Pair] -> Aeson.Value
object' = object . filter (not . isempty)
  where isempty (_, Array v)  = V.null v
        isempty (_, String t) = T.null t
        isempty ("first-reference-note-number", Aeson.Number n) = n == 0
        isempty ("citation-number", Aeson.Number n) = n == 0
        isempty (_, _)        = False

safeRead :: (Monad m, Read a) => String -> m a
safeRead s = case reads s of
                  (d,x):_
                    | all isSpace x -> return d
                  _                 -> fail $ "Could not read `" ++ s ++ "'"


readNum :: String -> Int
readNum s = case reads s of
              [(x,"")] -> x
              _        -> 0
