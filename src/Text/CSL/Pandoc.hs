{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Text.CSL.Pandoc (processCites, processCites')
where

import           Control.Applicative      ((<|>))
import qualified Control.Exception        as E
import           Control.Monad
import           Control.Monad.State
import           Data.Aeson
import qualified Data.ByteString.Lazy     as L
import           Data.Char                (isDigit, isPunctuation, isSpace,
                                           toLower)
import qualified Data.Map                 as M
import qualified Data.Set                 as Set
import           Data.Maybe               (fromMaybe)
import           System.Directory         (getAppUserDataDirectory)
import           System.Environment       (getEnv)
import           System.FilePath
import           System.IO.Error          (isDoesNotExistError)
import           System.SetEnv            (setEnv)
import           Text.CSL.Data            (getDefaultCSL)
import           Text.CSL.Exception
import           Text.CSL.Input.Bibutils  (convertRefs, readBiblioFile)
import           Text.CSL.Output.Pandoc   (renderPandoc, renderPandoc',
                      headInline, initInline, tailInline, toCapital)
import           Text.CSL.Parser
import           Text.CSL.Proc
import           Text.CSL.Reference       hiding (Value, processCites)
import           Text.CSL.Style           hiding (Citation (..), Cite (..))
import qualified Text.CSL.Style           as CSL
import           Text.CSL.Util            (findFile, lastInline,
                                           parseRomanNumeral, splitStrWhen, tr',
                                           trim)
import           Text.HTML.TagSoup.Entity (lookupEntity)
import           Text.Pandoc
import           Text.Pandoc.Builder      (deleteMeta, setMeta)
import           Text.Pandoc.Shared       (stringify)
import           Text.Pandoc.Walk
import           Text.Parsec              hiding (State, (<|>))

-- | Process a 'Pandoc' document by adding citations formatted
-- according to a CSL style.  Add a bibliography (if one is called
-- for) at the end of the document.
processCites :: Style -> [Reference] -> Pandoc -> Pandoc
processCites style refs (Pandoc m1 b1) =
  let metanocites   = lookupMeta "nocite" m1
      nocites       = mkNociteWildcards refs . query getCitation <$> metanocites
      Pandoc m2 b2  = evalState (walkM setHashes $ Pandoc (deleteMeta "nocite" m1) b1) 1
      grps          = query getCitation (Pandoc m2 b2) ++ fromMaybe [] nocites
      locMap        = locatorMap style
      result        = citeproc procOpts{ linkCitations = isLinkCitations m2}
                        style refs (setNearNote style $
                        map (map (toCslCite locMap)) grps)
      cits_map      = tr' "cits_map" $ M.fromList $ zip grps (citations result)
      biblioList    = map (renderPandoc' style) $ zip (bibliography result) (citationIds result)
      moveNotes     = case lookupMeta "notes-after-punctuation" m1 of
                           Just (MetaBool False) -> False
                           _                     -> True
      Pandoc m3 bs  = bottomUp (mvPunct moveNotes style) . deNote .
                        topDown (processCite style cits_map) $ Pandoc m2 b2
      m             = case metanocites of
                            Nothing -> m3
                            Just x  -> setMeta "nocite" x m3
  in  Pandoc m $ bottomUp (concatMap removeNocaseSpans)
               $ insertRefs m biblioList bs

-- if document contains a Div with id="refs", insert
-- references as its contents.  Otherwise, insert references
-- at the end of the document in a Div with id="refs"
insertRefs :: Meta -> [Block] -> [Block] -> [Block]
insertRefs _    []   bs = bs
insertRefs meta refs bs =
  if isRefRemove meta
     then bs
     else case runState (walkM go bs) False of
               (bs', True) -> bs'
               (_, False)  ->
                  case reverse bs of
                        Header lev (id',classes,kvs) ys : xs ->
                          reverse xs ++
                            [Header lev (id',addUnNumbered classes,kvs) ys,
                             Div ("refs",["references"],[]) refs]
                        _   -> bs ++ refHeader ++
                                [Div ("refs",["references"],[]) refs]
  where go :: Block -> State Bool Block
        go (Div attr@("refs",_,_) xs) = do
          put True
          -- refHeader isn't used if you have an explicit references div
          return $ Div attr (xs ++ refs)
        go x = return x
        addUnNumbered cs = "unnumbered" : [c | c <- cs, c /= "unnumbered"]
        refHeader = case refTitle meta of
                     Just ils ->
                       [Header 1 ("bibliography", ["unnumbered"], []) ils]
                     _        -> []

refTitle :: Meta -> Maybe [Inline]
refTitle meta =
  case lookupMeta "reference-section-title" meta of
    Just (MetaString s)           -> Just [Str s]
    Just (MetaInlines ils)        -> Just ils
    Just (MetaBlocks [Plain ils]) -> Just ils
    Just (MetaBlocks [Para ils])  -> Just ils
    _                             -> Nothing

isRefRemove :: Meta -> Bool
isRefRemove meta =
  case lookupMeta "suppress-bibliography" meta of
    Just (MetaBool True) -> True
    _                    -> False

isLinkCitations :: Meta -> Bool
isLinkCitations meta =
  case lookupMeta "link-citations" meta of
    Just (MetaBool True)   -> True
    Just (MetaString s)    -> map toLower s `elem` yesValues
    Just (MetaInlines ils) -> map toLower (stringify ils) `elem` yesValues
    _                      -> False
  where yesValues = ["true", "yes", "on"]

-- if the 'nocite' Meta field contains a citation with id = '*',
-- create a cite with to all the references.
mkNociteWildcards :: [Reference] -> [[Citation]] -> [[Citation]]
mkNociteWildcards refs nocites =
  map expandStar nocites
  where expandStar cs =
         case [c | c <- cs
                 , citationId c == "*"] of
              [] -> cs
              _  -> allcites
        allcites = map (\ref -> Citation{
                                  citationId = unLiteral (refId ref),
                                  citationPrefix = [],
                                  citationSuffix = [],
                                  citationMode = NormalCitation,
                                  citationNoteNum = 0,
                                  citationHash = 0 }) refs

removeNocaseSpans :: Inline -> [Inline]
removeNocaseSpans (Span ("",["nocase"],[]) xs) = xs
removeNocaseSpans x                            = [x]

-- | Process a 'Pandoc' document by adding citations formatted
-- according to a CSL style.  The style filename is derived from
-- the `csl` field of the metadata, and the references are taken
-- from the `references` field or read from a file in the `bibliography`
-- field.
processCites' :: Pandoc -> IO Pandoc
processCites' (Pandoc meta blocks) = do
  mbcsldir <- E.catch (Just <$> getAppUserDataDirectory "csl") $ \e ->
                 if isDoesNotExistError e
                    then return Nothing
                    else E.throwIO e
  mbpandocdir <- E.catch (Just <$> getAppUserDataDirectory "pandoc") $ \e ->
                 if isDoesNotExistError e
                    then return Nothing
                    else E.throwIO e
  let inlineRefError s = E.throw $ ErrorParsingReferences s
  let inlineRefs = either inlineRefError id
                   $ convertRefs $ lookupMeta "references" meta
  let cslfile = (lookupMeta "csl" meta <|> lookupMeta "citation-style" meta)
                >>= toPath
  let mbLocale = (lookupMeta "lang" meta `mplus` lookupMeta "locale" meta)
                   >>= toPath
  let tryReadCSLFile Nothing _  = mzero
      tryReadCSLFile (Just d) f = E.catch (readCSLFile mbLocale (d </> f))
                                     (\(_ :: E.SomeException) -> mzero)
  csl <- case cslfile of
               Just f | not (null f) -> readCSLFile mbLocale f
               _ ->  tryReadCSLFile mbpandocdir "default.csl"
                   `mplus` tryReadCSLFile mbcsldir "chicago-author-date.csl"
                   `mplus` (getDefaultCSL >>=
                             localizeCSL mbLocale . parseCSL')
  -- set LANG environment from locale; this affects unicode collation
  -- if pandoc-citeproc compiled with unicode_collation flag
  case styleLocale csl of
       (l:_) -> do
         setEnv "LC_ALL" (localeLang l)
         setEnv "LANG"   (localeLang l)
       []    -> do
         envlang <- getEnv "LANG"
         if null envlang
            then do
              -- Note that LANG needs to be set for bibtex conversion:
              setEnv "LANG" "en-US.UTF-8"
              setEnv "LC_ALL" "en-US.UTF-8"
            else
              setEnv "LC_ALL" envlang
  let citids = query getCitationIds (Pandoc meta blocks)
  let idpred = if "*" `Set.member` citids
                  then const True
                  else (`Set.member` citids)
  bibRefs <- getBibRefs idpred $ Data.Maybe.fromMaybe (MetaList [])
                               $ lookupMeta "bibliography" meta
  let refs = inlineRefs ++ bibRefs
  let cslAbbrevFile = lookupMeta "citation-abbreviations" meta >>= toPath
  let skipLeadingSpace = L.dropWhile (\s -> s == 32 || (s >= 9 && s <= 13))
  abbrevs <- maybe (return (Abbreviations M.empty))
             (\f -> findFile (maybe ["."] (\g -> [".", g]) mbcsldir) f >>=
                    maybe (E.throwIO $ CouldNotFindAbbrevFile f) return >>=
               L.readFile >>=
               either error return . eitherDecode . skipLeadingSpace)
             cslAbbrevFile
  let csl' = csl{ styleAbbrevs = abbrevs }
  return $ processCites csl' refs $ Pandoc meta blocks

toPath :: MetaValue -> Maybe String
toPath (MetaString s) = Just s
-- take last in a list
toPath (MetaList xs) = case reverse xs of
                             []    -> Nothing
                             (x:_) -> toPath x
toPath (MetaInlines ils) = Just $ stringify ils
toPath _ = Nothing

getBibRefs :: (String -> Bool) -> MetaValue -> IO [Reference]
getBibRefs idpred (MetaList xs) = concat `fmap` mapM (getBibRefs idpred) xs
getBibRefs idpred (MetaInlines xs) = getBibRefs idpred (MetaString $ stringify xs)
getBibRefs idpred (MetaString s) = do
  path <- findFile ["."] s >>= maybe (E.throwIO $ CouldNotFindBibFile s) return
  map unescapeRefId `fmap` readBiblioFile idpred path
getBibRefs _ _ = return []

-- unescape reference ids, which may contain XML entities, so
-- that we can do lookups with regular string equality
unescapeRefId :: Reference -> Reference
unescapeRefId ref = ref{ refId = Literal $ decodeEntities (unLiteral $ refId ref) }

decodeEntities :: String -> String
decodeEntities [] = []
decodeEntities ('&':xs) =
  let (ys,zs) = break (==';') xs
  in  case zs of
           ';':ws -> case lookupEntity ('&':ys ++ ";") of
#if MIN_VERSION_tagsoup(0,13,0)
                                       Just s  -> s ++ decodeEntities ws
#else
                                       Just c  -> [c] ++ decodeEntities ws
#endif
                                       Nothing -> '&' : decodeEntities xs
           _      -> '&' : decodeEntities xs
decodeEntities (x:xs) = x : decodeEntities xs

-- | Substitute 'Cite' elements with formatted citations.
processCite :: Style -> M.Map [Citation] Formatted -> Inline -> Inline
processCite s cs (Cite t _) =
   case M.lookup t cs of
        Just (Formatted xs)
          | not (null xs) || all isSuppressAuthor t
               -> Cite t (renderPandoc s (Formatted xs))
        _      -> Strong [Str "???"] -- TODO raise error instead?
    where isSuppressAuthor c = citationMode c == SuppressAuthor
processCite _ _ x = x

isNote :: Inline -> Bool
isNote (Note _)          = True
isNote (Cite _ [Note _]) = True
isNote _                 = False

mvPunctInsideQuote :: Inline -> Inline -> [Inline]
mvPunctInsideQuote (Quoted qt ils) (Str s) | s `elem` [".", ","] =
  [Quoted qt (init ils ++ mvPunctInsideQuote (last ils) (Str s))]
mvPunctInsideQuote il il' = [il, il']

isSpacy :: Inline -> Bool
isSpacy Space     = True
isSpacy SoftBreak = True
isSpacy _         = False

mvPunct :: Bool -> Style -> [Inline] -> [Inline]
mvPunct _ _ (x : Space : xs)
  | isSpacy x = x : xs
mvPunct moveNotes _ (s : x : ys)
  | isSpacy s
  , isNote x
  , startWithPunct ys
  = if moveNotes
       then Str (headInline ys) : x : tailInline ys
       else x : ys
mvPunct moveNotes _ (Cite cs ils : ys)
   | length ils > 1
   , isNote (last ils)
   , startWithPunct ys
   , moveNotes
   = Cite cs (init ils
     ++ [Str (headInline ys) | not (endWithPunct False (init ils))]
     ++ [last ils]) : tailInline ys
mvPunct moveNotes sty (q@(Quoted _ _) : w@(Str _) : x : ys)
  | isNote x
  , isPunctuationInQuote sty
  , moveNotes
  = mvPunctInsideQuote q w ++ (x : ys)
mvPunct _ _ (s : x : ys) | isSpacy s, isNote x = x : ys
mvPunct _ _ (s : x@(Cite _ (Superscript _ : _)) : ys) | isSpacy s = x : ys
mvPunct _ _ (Cite cs ils : Str "." : ys)
  | lastInline ils == "."
  = Cite cs ils : ys
mvPunct _ _ xs = xs

endWithPunct :: Bool -> [Inline] -> Bool
endWithPunct _ [] = True
endWithPunct onlyFinal xs@(_:_) =
  case reverse (stringify xs) of
       []                       -> True
       -- covers .), .", etc.:
       (d:c:_) | isPunctuation d
                 && not onlyFinal
                 && isEndPunct c -> True
       (c:_) | isEndPunct c      -> True
             | otherwise         -> False
  where isEndPunct c = c `elem` (".,;:!?" :: String)

startWithPunct :: [Inline] -> Bool
startWithPunct = all (`elem` (".,;:!?" :: String)) . headInline

deNote :: Pandoc -> Pandoc
deNote = topDown go
  where go (Cite (c:cs) [Note [Para xs]]) =
            Cite (c:cs) [Note [Para $ toCapital xs]]
        go (Note xs) = Note $ topDown go' xs
        go x = x
        go' (x : Cite cs [Note [Para xs]] : ys) | not (isSpacy x) =
             x : Str "," : Space : comb (\zs -> [Cite cs zs]) xs ys
        go' (x : Note [Para xs] : ys) | not (isSpacy x) =
             x : Str "," : Space : comb id xs ys
        go' (Cite cs [Note [Para xs]] : ys) = comb (\zs -> [Cite cs zs]) xs ys
        go' (Note [Para xs] : ys) = comb id xs ys
        go' xs = xs

comb :: ([Inline] -> [Inline]) -> [Inline] -> [Inline] -> [Inline]
comb f xs ys =
  let xs' = if startWithPunct ys && endWithPunct True xs
               then initInline $ removeLeadingPunct xs
               else removeLeadingPunct xs
      removeLeadingPunct (Str [c] : s : zs)
          | isSpacy s && (c == ',' || c == '.' || c == ':') = zs
      removeLeadingPunct zs = zs
  in  f xs' ++ ys

-- | Retrieve all citations from a 'Pandoc' docuument. To be used with
-- 'query'.
getCitation :: Inline -> [[Citation]]
getCitation i | Cite t _ <- i = [t]
              | otherwise     = []

getCitationIds :: Inline -> Set.Set String
getCitationIds (Cite cs _) = Set.fromList (map citationId cs)
getCitationIds _ = mempty

setHashes :: Inline -> State Int Inline
setHashes i | Cite t ils <- i = do t' <- mapM setHash t
                                   return $ Cite t' ils
            | otherwise       = return i

setHash :: Citation -> State Int Citation
setHash c = do
  ident <- get
  put $ ident + 1
  return c{ citationHash = ident }

toCslCite :: LocatorMap -> Citation -> CSL.Cite
toCslCite locMap c
    = let (la, lo, s)  = locatorWords locMap $ citationSuffix c
          s'      = case (la,lo,s) of
                         -- treat a bare locator as if it begins with space
                         -- so @item1 [blah] is like [@item1, blah]
                         ("","",x:_)
                           | not (isPunct x) -> Space : s
                         _                   -> s
          isPunct (Str (x:_)) = isPunctuation x
          isPunct _           = False
      in   emptyCite { CSL.citeId         = citationId c
                     , CSL.citePrefix     = Formatted $ citationPrefix c
                     , CSL.citeSuffix     = Formatted s'
                     , CSL.citeLabel      = la
                     , CSL.citeLocator    = lo
                     , CSL.citeNoteNumber = show $ citationNoteNum c
                     , CSL.authorInText   = citationMode c == AuthorInText
                     , CSL.suppressAuthor = citationMode c == SuppressAuthor
                     , CSL.citeHash       = citationHash c
                     }

locatorWords :: LocatorMap -> [Inline] -> (String, String, [Inline])
locatorWords locMap inp =
  case parse (pLocatorWords locMap) "suffix" $
         splitStrWhen (\c -> isLocatorPunct c || isSpace c) inp of
       Right r -> r
       Left _  -> ("","",inp)

pLocatorWords :: LocatorMap -> Parsec [Inline] st (String, String, [Inline])
pLocatorWords locMap = do
  (la,lo) <- pLocator locMap
  s <- getInput -- rest is suffix
  return (la, lo, s)

pMatch :: (Inline -> Bool) -> Parsec [Inline] st Inline
pMatch condition = try $ do
  t <- anyToken
  guard $ condition t
  return t

pSpace :: Parsec [Inline] st Inline
pSpace = pMatch (\t -> isSpacy t || t == Str "\160")

pLocator :: LocatorMap -> Parsec [Inline] st (String, String)
pLocator locMap = try $ do
  optional $ pMatch (== Str ",")
  optional pSpace
  la <- try (do ts <- many1 (notFollowedBy (pWordWithDigits True) >> anyToken)
                case M.lookup (trim (stringify ts)) locMap of
                       Just l  -> return l
                       Nothing -> mzero)
      <|> (lookAhead pDigit >> return "page")
  g <- pWordWithDigits True
  gs <- many (pWordWithDigits False)
  let lo = concat (g:gs)
  return (la, lo)

pRoman :: Parsec [Inline] st String
pRoman = try $ do
  t <- anyToken
  case t of
       Str xs -> case parseRomanNumeral xs of
                      Nothing -> mzero
                      Just _  -> return xs
       _      -> mzero

-- we want to capture:  123, 123A, C22, XVII, 33-44, 22-33; 22-11
pWordWithDigits :: Bool -> Parsec [Inline] st String
pWordWithDigits isfirst = try $ do
  punct <- if isfirst
              then return ""
              else stringify `fmap` pLocatorPunct
  sp <- option "" (pSpace >> return " ")
  s <-  pRoman <|>
        try (do ts <- many1 (notFollowedBy pSpace >>
                             notFollowedBy pLocatorPunct >>
                             anyToken)
                let ts' = stringify ts
                guard (any isDigit ts')
                return ts')
  return $ punct ++ sp ++ s

pDigit :: Parsec [Inline] st ()
pDigit = do
  t <- anyToken
  case t of
      Str (d:_) | isDigit d -> return ()
      _         -> mzero

pLocatorPunct :: Parsec [Inline] st Inline
pLocatorPunct = pMatch isLocatorPunct'
  where isLocatorPunct' (Str [c]) = isLocatorPunct c
        isLocatorPunct' _         = False

isLocatorPunct :: Char -> Bool
isLocatorPunct ':' = False
isLocatorPunct c   = isPunctuation c

type LocatorMap = M.Map String String

locatorMap :: Style -> LocatorMap
locatorMap sty =
  foldr (\term -> M.insert (termSingular term) (cslTerm term)
                . M.insert (termPlural term) (cslTerm term))
    M.empty
    (concatMap localeTerms $ styleLocale sty)
