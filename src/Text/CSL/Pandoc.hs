{-# LANGUAGE PatternGuards, OverloadedStrings #-}
module Text.CSL.Pandoc (processCites, processCites') where

import Text.CSL (readBiblioFile, Reference(..),
                 Style(..), parseCSL,
                 readJsonAbbrevFile)
import Text.CSL.Reference (RefType(..), Agent(..), CNum(..), RefDate(..))
import Text.Pandoc.Definition
import Text.Pandoc.JSON
import Text.Pandoc.Walk
import Text.HTML.TagSoup.Entity (lookupEntity)
import Paths_pandoc_citeproc (getDataFileName)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Applicative ((<$>),(<*>))
import Data.Char (toUpper, isSpace)

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
  let inlineRefs = maybe [] id $ lookupMeta "references" meta >>= convertRefs
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

handleCite :: Style -> [Reference] -> Inline -> Inline
handleCite sty refs (Cite cs ils) = Cite cs [Str "CITE"]
handleCite _ _ x = x

convertRefs :: MetaValue -> Maybe [Reference]
convertRefs v =
  case metaValueToJSON blockWriter inlineWriter v >>= fromJSON of
       Data.Aeson.Error s   -> Nothing
       Success x            -> Just x

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

instance FromJSON CNum where
  parseJSON x = case fromJSON x of
                     Success n -> return $ CNum n
                     _         -> mzero

instance FromJSON RefDate where
  parseJSON (Object v) = RefDate <$>
              v .:? "year" .!= "" <*>
              v .:? "month" .!= "" <*>
              v .:? "season" .!= "" <*>
              v .:? "day" .!= "" <*>
              v .:? "other" .!= "" <*>
              v .:? "circa" .!= ""
  parseJSON _ = mzero

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

blockWriter :: (Functor m, Monad m) => [Block] -> m String
blockWriter [Plain xs] = inlineWriter xs
blockWriter [Para xs] = inlineWriter xs
blockWriter _ = fail "unsupported"

inlineWriter :: (Functor m, Monad m) => [Inline] -> m String
inlineWriter = fmap mconcat . mapM go
  where go (Str xs) = return $ xs
        go Space    = return " "
        go _        = fail "unsupported"

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


