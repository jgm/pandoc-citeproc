{-# LANGUAGE PatternGuards, OverloadedStrings, FlexibleInstances,
    ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Text.CSL.Pandoc where -- (processCites, processCites') where

import Text.CSL.Reference (RefType(..), Agent(..), CNum(..), RefDate(..))
import Text.TeXMath (texMathToPandoc, DisplayType(..))
import Data.Aeson.Types (Parser, Pair)
import Text.Pandoc.Definition
import Text.Pandoc.Walk
import Text.HTML.TagSoup.Entity (lookupEntity)
import Paths_pandoc_citeproc (getDataFileName)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Applicative ((<$>),(<*>))
import Data.Char (toUpper, isSpace, toLower, isUpper, isLower)
import qualified Data.Vector as V
import qualified Data.Traversable as Traversable
import Data.Monoid
import Data.Aeson
import Data.List
import Data.Char ( isDigit, isPunctuation )
import qualified Data.Map as M
import Text.CSL hiding ( Cite(..), Citation(..), endWithPunct )
import qualified Text.CSL as CSL ( Cite(..) )
import Text.Pandoc.Generic
import Text.Parsec hiding (State)
import Control.Monad
import Control.Monad.State

-- | Process a 'Pandoc' document by adding citations formatted
-- according to a CSL style.  Add a bibliography (if one is called
-- for) at the end of the document.
processCites :: Style -> [Reference] -> Pandoc -> Pandoc
processCites style refs doc =
  let doc'       = evalState (walkM setHashes doc) 1
      grps       = query getCitation doc'
      result     = citeproc procOpts style refs (setNearNote style $
                      map (map toCslCite) grps)
      cits_map   = M.fromList $ zip grps (citations result)
      biblioList = map (renderPandoc' style) (bibliography result)
      Pandoc m b = bottomUp mvPunct . deNote .
                       topDown (processCite style cits_map) $ doc'
      (bs, lastb) = case reverse b of
                         x@(Header _ _ _) : xs -> (reverse xs, [x])
                         _                     -> (b,  [])
  in  Pandoc m $ bs ++ [Div ("",["references"],[]) (lastb ++ biblioList)]

-- | Process a 'Pandoc' document by adding citations formatted
-- according to a CSL style.  The style filename is derived from
-- the `csl` field of the metadata, and the references are taken
-- from the `references` field or read from a file in the `bibliography`
-- field.
processCites' :: Pandoc -> IO Pandoc
processCites' (Pandoc meta blocks) = do
  let inlineRefError s = error $ "Error parsing references: " ++ s
  let inlineRefs = either inlineRefError id
                   $ convertRefs $ lookupMeta "references" meta
  bibRefs <- getBibRefs $ maybe (MetaList []) id
                        $ lookupMeta "bibliography" meta
  let refs = inlineRefs ++ bibRefs
  let cslfile = lookupMeta "csl" meta >>= toPath
  let cslAbbrevFile = lookupMeta "csl-abbrevs" meta >>= toPath
  csl <- maybe (getDataFileName "chicago-author-date.csl") return cslfile
             >>= readFile >>= parseCSL
  abbrevs <- maybe (return []) readJsonAbbrevFile cslAbbrevFile
  let csl' = csl{ styleAbbrevs = abbrevs }
  return $ processCites csl' refs $ Pandoc meta blocks

toPath :: MetaValue -> Maybe String
toPath (MetaString s) = Just s
toPath (MetaInlines ils) = Just $ stringify ils
toPath _ = Nothing

stringify :: [Inline] -> String
stringify = query getStr
  where getStr (Str x) = x
        getStr _ = ""

getBibRefs :: MetaValue -> IO [Reference]
getBibRefs (MetaList xs) = concat `fmap` mapM getBibRefs xs
getBibRefs (MetaInlines xs) =
  map unescapeRefId `fmap` readBiblioFile (stringify xs)
getBibRefs (MetaString s) =
  map unescapeRefId `fmap` readBiblioFile s
getBibRefs _ = return []

-- unescape reference ids, which may contain XML entities, so
-- that we can do lookups with regular string equality
unescapeRefId :: Reference -> Reference
unescapeRefId ref = ref{ refId = decodeEntities (refId ref) }

decodeEntities :: String -> String
decodeEntities [] = []
decodeEntities ('&':xs) =
  let (ys,zs) = break (==';') xs
  in  case zs of
           ';':ws -> case lookupEntity ('&':ys ++ ";") of
                                       Just c  -> [c] ++ decodeEntities ws
                                       Nothing -> '&' : decodeEntities xs
           _      -> '&' : decodeEntities xs
decodeEntities (x:xs) = x : decodeEntities xs

convertRefs :: Maybe MetaValue -> Either String [Reference]
convertRefs Nothing = Right []
convertRefs (Just v) =
  case metaValueToJSON blocksToString inlinesToString v >>= fromJSON of
       Data.Aeson.Error s   -> Left s
       Success x            -> Right x

instance FromJSON RefType where
  parseJSON (String t) = safeRead (capitalize . camelize . T.unpack $ t)
    where camelize x
            | '-':y:ys <- x = toUpper y : camelize ys
            | '_':y:ys <- x = toUpper y : camelize ys
            |     y:ys <- x =        y : camelize ys
            | otherwise     = []
          capitalize (x:xs) = toUpper x : xs
          capitalize     [] = []
  parseJSON _ = mzero

instance ToJSON RefType where
  toJSON reftype = toJSON (uncamelize $ uncapitalize $ show reftype)
   where uncamelize [] = []
         uncamelize (x:y:zs)
          | isLower x && isUpper y = x:'-':toLower y:uncamelize zs
         uncamelize (x:xs) = x : uncamelize xs
         uncapitalize (x:xs) = toLower x : xs
         uncapitalize []     = []

instance FromJSON Agent where
  parseJSON (Object v) = Agent <$>
              v .:? "given" .!= [] <*>
              v .:?  "dropping-particle" .!= "" <*>
              v .:? "non-dropping-particle" .!= "" <*>
              v .:? "family" .!= "" <*>
              v .:? "suffix" .!= "" <*>
              v .:? "literal" .!= "" <*>
              v .:? "comma-suffix" .!= False
  parseJSON _ = mzero

instance ToJSON Agent where
  toJSON agent = object' [
      "given" .= givenName agent
    , "dropping-particle" .= droppingPart agent
    , "non-dropping-particle" .= nonDroppingPart agent
    , "family" .= familyName agent
    , "suffix" .= nameSuffix agent
    , "literal" .= literal agent
    , "comma-suffix" .= commaSuffix agent
    ]

instance FromJSON CNum where
  parseJSON x = case fromJSON x of
                     Success n -> return $ CNum n
                     _         -> mzero

instance ToJSON CNum where
  toJSON (CNum n) = toJSON n

instance FromJSON RefDate where
  parseJSON (Object v) = RefDate <$>
              v .:? "year" .!= "" <*>
              v .:? "month" .!= "" <*>
              v .:? "season" .!= "" <*>
              v .:? "day" .!= "" <*>
              v .:? "other" .!= "" <*>
              v .:? "circa" .!= ""
  parseJSON _ = mzero

instance ToJSON RefDate where
  toJSON refdate = object' [
      "year" .= year refdate
    , "month" .= month refdate
    , "season" .= season refdate
    , "day" .= day refdate
    , "other" .= other refdate
    , "circa" .= circa refdate
    ]

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
  parseJSON _ = mzero

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

instance FromJSON [Agent] where
  parseJSON (Array xs) = mapM parseJSON $ V.toList xs
  parseJSON (Object v) = (:[]) `fmap` parseJSON (Object v)
  parseJSON (String t) = parseJSON (String t) >>= mkAgent
  parseJSON _ = mzero

mkAgent :: Text -> Parser [Agent]
mkAgent t =
  case reverse (words $ T.unpack t) of
       (x:ys) -> return [Agent (reverse ys) [] [] x [] [] False]
       []     -> mzero

instance FromJSON [RefDate] where
  parseJSON (Array xs) = mapM parseJSON $ V.toList xs
  parseJSON (Object v) = (:[]) `fmap` parseJSON (Object v)
  parseJSON x          = parseJSON x >>= mkRefDate

mkRefDate :: String -> Parser [RefDate]
mkRefDate xs
  | all isDigit xs = return [RefDate xs "" "" "" "" ""]
  | otherwise      = return [RefDate "" "" "" "" xs ""]

instance FromJSON [String] where
  parseJSON (Array xs) = mapM parseJSON $ V.toList xs
  parseJSON (String t) = (:[]) `fmap` parseJSON (String t)
  parseJSON (Number n) = return [show n]
  parseJSON _ = mzero

metaValueToJSON :: Monad m
                => ([Block] -> m String)
                -> ([Inline] -> m String)
                -> MetaValue
                -> m Value
metaValueToJSON blockWriter inlineWriter (MetaMap metamap) = liftM toJSON $
  Traversable.mapM (metaValueToJSON blockWriter inlineWriter) metamap
metaValueToJSON blockWriter inlineWriter (MetaList xs) = liftM toJSON $
  Traversable.mapM (metaValueToJSON blockWriter inlineWriter) xs
metaValueToJSON _ _ (MetaBool b) = return $ toJSON b
metaValueToJSON _ _ (MetaString s) = return $ toJSON s
metaValueToJSON blockWriter _ (MetaBlocks bs) = liftM toJSON $ blockWriter bs
metaValueToJSON _ inlineWriter (MetaInlines bs) = liftM toJSON $ inlineWriter bs

blocksToString :: (Functor m, Monad m) => [Block] -> m String
blocksToString = fmap (unlines . intersperse "") . mapM go
  where go (Plain xs) = inlinesToString xs
        go (Para xs)  = inlinesToString xs
        go _          = return ""

inlinesToString :: (Functor m, Monad m) => [Inline] -> m String
inlinesToString = fmap mconcat . mapM go
  where go (Str xs)         = return xs
        go Space            = return " "
        go (Emph xs)        = inTag "i" <$> inlinesToString xs
        go (Strong xs)      = inTag "b" <$> inlinesToString xs
        go (Superscript xs) = inTag "sup" <$> inlinesToString xs
        go (Subscript xs)   = inTag "sub" <$> inlinesToString xs
        go (SmallCaps xs)   = inTag "sc" <$> inlinesToString xs
        go (Code _ xs)      = return xs
        go (Link xs _)      = inlinesToString xs
        go (Image xs _)     = inlinesToString xs
        go (RawInline f xs) | f == Format "citeproc"
                            = return xs
        go (Span _ xs)      = inlinesToString xs
        go (Note _)         = return ""
        go (LineBreak)      = return " "
        go (Math _ xs)      = either (\_ -> return $ surround '$' '$' xs)
                              inlinesToString
                              $ texMathToPandoc DisplayInline xs
        go (Cite _ ils)     = inlinesToString ils
        go (Quoted SingleQuote xs) = surround '‘' '’' <$> inlinesToString xs
        go (Quoted DoubleQuote xs) = surround '“' '”' <$> inlinesToString xs
        go _                = return ""

surround :: Char -> Char -> String -> String
surround beg end s = beg : s ++ [end]

inTag :: String -> String -> String
inTag t s = "<" ++ t ++ ">" ++ s ++ "</" ++ takeWhile (/=' ') t ++ ">"

-- | Substitute 'Cite' elements with formatted citations.
processCite :: Style -> M.Map [Citation] [FormattedOutput] -> Inline -> Inline
processCite s cs (Cite t _) =
   case M.lookup t cs of
        Just (x:xs)
          | isTextualCitation t && not (null xs) ->
             let xs' = renderPandoc s xs
             in  if styleClass s == "note"
                    then Cite t (renderPandoc s [x] ++ [Note [Para xs']])
                    else Cite t (renderPandoc s [x] ++ [Space | not (startWithPunct xs')] ++ xs')
          | otherwise -> if styleClass s == "note"
                            then Cite t [Note [Para $ renderPandoc s (x:xs)]]
                            else Cite t (renderPandoc s (x:xs))
        _             -> Strong [Str "???"]  -- TODO raise error instead?
processCite _ _ x = x

isNote :: Inline -> Bool
isNote (Note _) = True
isNote (Cite _ [Note _]) = True
isNote _ = False

mvPunct :: [Inline] -> [Inline]
mvPunct (Space : Space : xs) = Space : xs
mvPunct (Space : x : ys) | isNote x, startWithPunct ys =
   Str (headInline ys) : x : tailFirstInlineStr ys
mvPunct (Space : x : ys) | isNote x = x : ys
mvPunct xs = xs

-- A replacement for citeproc-hs's endWithPunct, which wrongly treats
-- a sentence ending in '.)' as not ending with punctuation, leading
-- to an extra period.
endWithPunct :: [Inline] -> Bool
endWithPunct [] = True
endWithPunct xs@(_:_) = case reverse (stringify [last xs]) of
                              []                       -> True
                              (')':c:_) | isEndPunct c -> True
                              (c:_) | isEndPunct c     -> True
                                    | otherwise        -> False
  where isEndPunct c = c `elem` ".,;:!?"

deNote :: Pandoc -> Pandoc
deNote = topDown go
  where go (Cite (c:cs) [Note xs]) =
            Cite (c:cs) [Note $ bottomUp go' $ sanitize c xs]
        go (Note xs) = Note $ bottomUp go' xs
        go x = x
        go' (Note [Para xs]:ys) =
             if startWithPunct ys && endWithPunct xs
                then initInline xs ++ ys
                else xs ++ ys
        go' xs = xs
        sanitize :: Citation -> [Block] -> [Block]
        sanitize Citation{citationPrefix = pref} [Para xs] =
           case (null pref, endWithPunct xs) of
                (True, False)  -> [Para $ xs ++ [Str "."]]
                (True, True)   -> [Para xs]
                (False, False) -> [Para $ toCapital $ xs ++ [Str "."]]
                (False, True)  -> [Para $ toCapital xs]
        sanitize _ bs = bs

isTextualCitation :: [Citation] -> Bool
isTextualCitation (c:_) = citationMode c == AuthorInText
isTextualCitation _     = False

-- | Retrieve all citations from a 'Pandoc' docuument. To be used with
-- 'query'.
getCitation :: Inline -> [[Citation]]
getCitation i | Cite t _ <- i = [t]
              | otherwise     = []

setHashes :: Inline -> State Int Inline
setHashes i | Cite t ils <- i = do t' <- mapM setHash t
                                   return $ Cite t' ils
            | otherwise       = return i

setHash :: Citation -> State Int Citation
setHash c = do
  ident <- get
  put $ ident + 1
  return c{ citationHash = ident }

toCslCite :: Citation -> CSL.Cite
toCslCite c
    = let (l, s)  = locatorWords $ citationSuffix c
          (la,lo) = parseLocator l
          s'      = case (l,s) of
                         -- treat a bare locator as if it begins with space
                         -- so @item1 [blah] is like [@item1, blah]
                         ("",(x:_))
                           | not (isPunct x) -> [Space] ++ s
                         _                   -> s
          isPunct (Str (x:_)) = isPunctuation x
          isPunct _           = False
          citMode = case citationMode c of
                      AuthorInText   -> (True, False)
                      SuppressAuthor -> (False,True )
                      NormalCitation -> (False,False)
      in   emptyCite { CSL.citeId         = citationId c
                     , CSL.citePrefix     = PandocText $ citationPrefix c
                     , CSL.citeSuffix     = PandocText s'
                     , CSL.citeLabel      = la
                     , CSL.citeLocator    = lo
                     , CSL.citeNoteNumber = show $ citationNoteNum c
                     , CSL.authorInText   = fst citMode
                     , CSL.suppressAuthor = snd citMode
                     , CSL.citeHash       = citationHash c
                     }

locatorWords :: [Inline] -> (String, [Inline])
locatorWords inp =
  case parse pLocatorWords "suffix" $ breakup inp of
       Right r   -> r
       Left _    -> ("",inp)
   where breakup [] = []
         breakup (Str x : xs) = map Str (splitup x) ++ breakup xs
         breakup (x : xs) = x : breakup xs
         splitup = groupBy (\x y -> x /= '\160' && y /= '\160')

pLocatorWords :: Parsec [Inline] st (String, [Inline])
pLocatorWords = do
  l <- pLocator
  s <- getInput -- rest is suffix
  if length l > 0 && last l == ','
     then return (init l, Str "," : s)
     else return (l, s)

pMatch :: (Inline -> Bool) -> Parsec [Inline] st Inline
pMatch condition = try $ do
  t <- anyToken
  guard $ condition t
  return t

pSpace :: Parsec [Inline] st Inline
pSpace = pMatch (\t -> t == Space || t == Str "\160")

pLocator :: Parsec [Inline] st String
pLocator = try $ do
  optional $ pMatch (== Str ",")
  optional pSpace
  f  <- (guardFollowingDigit >> return [Str "p"]) -- "page" the default
     <|> many1 (notFollowedBy pSpace >> anyToken)
  gs <- many1 pWordWithDigits
  return $ stringify f ++ (' ' : unwords gs)

guardFollowingDigit :: Parsec [Inline] st ()
guardFollowingDigit = do
  t <- lookAhead anyToken
  case t of
       Str (d:_) | isDigit d -> return ()
       _                     -> mzero

pWordWithDigits :: Parsec [Inline] st String
pWordWithDigits = try $ do
  optional pSpace
  r <- many1 (notFollowedBy pSpace >> anyToken)
  let s = stringify r
  guard $ any isDigit s
  return s

safeRead :: (Monad m, Read a) => String -> m a
safeRead s = case reads s of
                  (d,x):_
                    | all isSpace x -> return d
                  _                 -> fail $ "Could not read `" ++ s ++ "'"

object' :: [Pair] -> Value
object' = object . filter (not . isempty)
  where isempty (_, Array v)  = V.null v
        isempty (_, String t) = T.null t
        isempty ("first-reference-note-number", Number n) = n == 0
        isempty ("citation-number", Number n) = n == 0
        isempty ("comma-suffix", Bool b) = not b
        isempty (_, _)        = False
