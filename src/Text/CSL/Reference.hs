{-# LANGUAGE GeneralizedNewtypeDeriving, PatternGuards,
  DeriveDataTypeable, ExistentialQuantification #-}
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

import Data.Char  ( isUpper, toLower      )
import Data.List  ( elemIndex, isPrefixOf )
import Data.Maybe ( fromMaybe             )
import Data.Generics

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

data RefDate =
    RefDate { year   :: String
            , month  :: String
            , season :: String
            , day    :: String
            , other  :: String
            , circa  :: String
            } deriving ( Show, Read, Eq, Typeable, Data )

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

newtype CNum = CNum { unCNum :: Int } deriving ( Show, Read, Eq, Num, Typeable, Data )

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

getReference :: [Reference] -> Cite -> Reference
getReference  r c
    = case citeId c `elemIndex` map refId r of
        Just i  -> setPageFirst $ r !! i
        Nothing -> emptyReference { title = citeId c ++ " not found!" }

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

readNum :: String -> Int
readNum s = case reads s of
              [(x,"")] -> x
              _        -> 0
