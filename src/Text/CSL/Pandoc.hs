{-# LANGUAGE PatternGuards, OverloadedStrings, FlexibleInstances,
    ScopedTypeVariables, CPP #-}
module Text.CSL.Pandoc (processCites, processCites') where

import Text.Pandoc
import Text.Pandoc.Walk
import Text.Pandoc.Builder (setMeta, deleteMeta, Inlines, cite)
import Text.Pandoc.Shared (stringify)
import Text.HTML.TagSoup.Entity (lookupEntity)
import qualified Data.ByteString.Lazy as L
import System.SetEnv (setEnv)
import Control.Applicative ((<|>))
import Data.Aeson
import Data.Char ( isDigit, isPunctuation, toLower, isSpace )
import qualified Data.Map as M
import Text.CSL.Reference hiding (processCites, Value)
import Text.CSL.Input.Bibutils (readBiblioFile, convertRefs)
import Text.CSL.Style hiding (Cite(..), Citation(..))
import Text.CSL.Proc
import Text.CSL.Output.Pandoc (renderPandoc, renderPandoc')
import qualified Text.CSL.Style as CSL
import Text.CSL.Parser
import Text.CSL.Output.Pandoc ( headInline, tailFirstInlineStr, initInline,
                                toCapital )
import Text.CSL.Data (getDefaultCSL)
import Text.Parsec hiding (State, (<|>))
import Control.Monad
import qualified Control.Exception as E
import Control.Monad.State
import System.FilePath
import System.Directory (doesFileExist, getAppUserDataDirectory)
import Text.CSL.Util (findFile, splitStrWhen, tr', parseRomanNumeral, trim)
import System.IO.Error (isDoesNotExistError)

-- | Process a 'Pandoc' document by adding citations formatted
-- according to a CSL style.  Add a bibliography (if one is called
-- for) at the end of the document.
processCites :: Style -> [Reference] -> Pandoc -> Pandoc
processCites style refs (Pandoc m1 b1) =
  let m2            = setMeta "nocites-wildcards" (mkNociteWildcards refs m1) m1
      Pandoc m3 b2  = evalState (walkM setHashes $ Pandoc m2 b1) 1
      grps          = query getCitation $ Pandoc m3 b2
      m4            = deleteMeta "nocites-wildcards" m3
      locMap        = locatorMap style
      result        = citeproc procOpts{ linkCitations = isLinkCitations m4}
                        style refs (setNearNote style $
                        map (map (toCslCite locMap)) grps)
      cits_map      = tr' "cits_map" $ M.fromList $ zip grps (citations result)
      biblioList    = map (renderPandoc' style) $ zip (bibliography result) (citationIds result)
      Pandoc m bs   = bottomUp (mvPunct style) . deNote .
                        topDown (processCite style cits_map) $ Pandoc m4 b2
  in  Pandoc m $ bottomUp (concatMap removeNocaseSpans)
               $ insertRefs m biblioList bs

-- if document contains a Div with id="refs", insert
-- references as its contents.  Otherwise, insert references
-- at the end of the document in a Div with id="refs"
insertRefs :: Meta -> [Block] -> [Block] -> [Block]
insertRefs meta refs bs =
  if isRefRemove meta
     then bs
     else case runState (walkM go bs) False of
               (bs', True) -> bs'
               (_, False)  ->
                  case reverse bs of
                        (Header lev (id',classes,kvs) ys) : xs ->
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
mkNociteWildcards :: [Reference] -> Meta -> Inlines
mkNociteWildcards refs meta =
  case lookupMeta "nocite" meta of
       Nothing      -> mempty
       Just nocites ->
         case [c | c <- concat (query getCitation nocites)
                 , citationId c == "*"] of
              []    -> mempty
              (_:_) -> cite allcites mempty
         where allcites = map (\ref -> Citation{
                                  citationId = unLiteral (refId ref),
                                  citationPrefix = [],
                                  citationSuffix = [],
                                  citationMode = NormalCitation,
                                  citationNoteNum = 0,
                                  citationHash = 0 }) refs

removeNocaseSpans :: Inline -> [Inline]
removeNocaseSpans (Span ("",["nocase"],[]) xs) = xs
removeNocaseSpans x = [x]

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
  let inlineRefError s = error $ "Error parsing references: " ++ s
  let inlineRefs = either inlineRefError id
                   $ convertRefs $ lookupMeta "references" meta
  let cslfile = (lookupMeta "csl" meta <|> lookupMeta "citation-style" meta)
                >>= toPath
  let mbLocale = (lookupMeta "lang" meta `mplus` lookupMeta "locale" meta)
                   >>= toPath
  let getDefaultCSL' = case mbcsldir of
                            Just csldir -> do
                              let f = csldir </> "chicago-author-date.csl"
                              exists <- doesFileExist f
                              if exists
                                 then L.readFile f
                                 else getDefaultCSL
                            Nothing -> getDefaultCSL
  csl <- case cslfile of
               Just f | not (null f) -> readCSLFile mbLocale f
               _ -> do
                 -- get default CSL: look first in ~/.csl, and take
                 -- from distribution if not found
                 raw <- case mbpandocdir of
                          Just pandocdir -> do
                            let f = pandocdir </> "default.csl"
                            exists <- doesFileExist f
                            if exists
                               then L.readFile f
                               else getDefaultCSL'
                          Nothing -> getDefaultCSL'
                 localizeCSL mbLocale $ parseCSL' raw
  -- set LANG environment from locale; this affects unicode collation
  -- if pandoc-citeproc compiled with unicode_collation flag
  setEnv "LC_ALL" $ case styleLocale csl of
                        (l:_) -> localeLang l
                        _     -> "en-US"
  bibRefs <- getBibRefs $ maybe (MetaList []) id
                        $ lookupMeta "bibliography" meta
  let refs = inlineRefs ++ bibRefs
  let cslAbbrevFile = lookupMeta "citation-abbreviations" meta >>= toPath
  let skipLeadingSpace = L.dropWhile (\s -> s == 32 || (s >= 9 && s <= 13))
  abbrevs <- maybe (return (Abbreviations M.empty))
             (\f -> findFile (maybe ["."] (\g -> [".", g]) mbcsldir) f >>=
                    maybe (error $ "Could not find " ++ f) return >>=
               L.readFile >>=
               either error return . eitherDecode . skipLeadingSpace)
             cslAbbrevFile
  let csl' = csl{ styleAbbrevs = abbrevs }
  return $ processCites csl' refs $ Pandoc meta blocks

toPath :: MetaValue -> Maybe String
toPath (MetaString s) = Just s
toPath (MetaInlines ils) = Just $ stringify ils
toPath _ = Nothing

getBibRefs :: MetaValue -> IO [Reference]
getBibRefs (MetaList xs) = concat `fmap` mapM getBibRefs xs
getBibRefs (MetaInlines xs) = getBibRefs (MetaString $ stringify xs)
getBibRefs (MetaString s) = do
  path <- findFile ["."] s >>= maybe (error $ "Could not find " ++ s) return
  map unescapeRefId `fmap` readBiblioFile path
getBibRefs _ = return []

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
        Just (Formatted (x:xs)) -> Cite t (renderPandoc s (Formatted (x:xs)))
        _             -> Strong [Str "???"]  -- TODO raise error instead?
processCite _ _ x = x

isNote :: Inline -> Bool
isNote (Note _) = True
isNote (Cite _ [Note _]) = True
isNote _ = False

mvPunctInsideQuote :: Inline -> Inline -> [Inline]
mvPunctInsideQuote (Quoted qt ils) (Str s) | s `elem` [".", ","] =
  [Quoted qt (init ils ++ (mvPunctInsideQuote (last ils) (Str s)))]
mvPunctInsideQuote il il' = [il, il']

mvPunct :: Style -> [Inline] -> [Inline]
mvPunct _ (Space : Space : xs) = Space : xs
mvPunct _ (Space : x : ys) | isNote x, startWithPunct ys =
   Str (headInline ys) : x : tailFirstInlineStr ys
mvPunct _ (Cite cs ils : ys) |
     length ils > 1
   , isNote (last ils)
   , startWithPunct ys
   = Cite cs (init ils ++ [Str (headInline ys) | not (endWithPunct False (init ils))]
     ++ [last ils]) : tailFirstInlineStr ys
mvPunct sty (q@(Quoted _ _) : w@(Str _) : x : ys)
  | isNote x, isPunctuationInQuote sty  =
    mvPunctInsideQuote q w ++ (x : ys)
mvPunct _ (Space : x : ys) | isNote x = x : ys
mvPunct _ (Space : x@(Cite _ (Superscript _ : _)) : ys) = x : ys
mvPunct _ xs = xs

endWithPunct :: Bool -> [Inline] -> Bool
endWithPunct _ [] = True
endWithPunct onlyFinal xs@(_:_) =
  case reverse (stringify xs) of
       []                       -> True
       -- covers .), .", etc.:
       (d:c:_) | isPunctuation d && not onlyFinal
                && isEndPunct c -> True
       (c:_) | isEndPunct c     -> True
             | otherwise        -> False
  where isEndPunct c = c `elem` (".,;:!?" :: String)

startWithPunct :: [Inline] -> Bool
startWithPunct = and . map (`elem` (".,;:!?" :: String)) . headInline

deNote :: Pandoc -> Pandoc
deNote = topDown go
  where go (Cite (c:cs) [Note xs]) =
            Cite (c:cs) [Note $ sanitize xs]
        go (Note xs) = Note $ topDown go' xs
        go x = x
        go' (x : Cite cs [Note [Para xs]] : ys) | x /= Space =
             x : Str "," : Space : comb (\zs -> [Cite cs zs]) xs ys
        go' (x : Note [Para xs] : ys) | x /= Space =
             x : Str "," : Space : comb id xs ys
        go' (Cite cs [Note [Para xs]] : ys) = comb (\zs -> [Cite cs zs]) xs ys
        go' (Note [Para xs] : ys) = comb id xs ys
        go' xs = xs
        removeLeadingPunct (Str [c] : Space : xs)
          | c == ',' || c == '.' || c == ':' = xs
        removeLeadingPunct xs = xs
        comb f xs ys =
           let xs' = if startWithPunct ys && endWithPunct True xs
                        then initInline $ removeLeadingPunct xs
                        else removeLeadingPunct xs
           in f xs' ++ ys
        sanitize :: [Block] -> [Block]
        sanitize [Para xs] =
           [Para $ toCapital xs ++ if endWithPunct False xs then [Space] else []]
        sanitize bs = bs

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

toCslCite :: LocatorMap -> Citation -> CSL.Cite
toCslCite locMap c
    = let (la, lo, s)  = locatorWords locMap $ citationSuffix c
          s'      = case (la,lo,s) of
                         -- treat a bare locator as if it begins with space
                         -- so @item1 [blah] is like [@item1, blah]
                         ("","",(x:_))
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
         splitStrWhen (\c -> isPunctuation c || isSpace c) inp of
       Right r   -> r
       Left _    -> ("","",inp)

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
pSpace = pMatch (\t -> t == Space || t == Str "\160")

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
      _ -> mzero

pLocatorPunct :: Parsec [Inline] st Inline
pLocatorPunct = pMatch isLocatorPunct

isLocatorPunct :: Inline -> Bool
isLocatorPunct (Str [c]) = isPunctuation c
isLocatorPunct _         = False

type LocatorMap = M.Map String String

locatorMap :: Style -> LocatorMap
locatorMap sty =
  foldr (\term -> M.insert (termSingular term) (cslTerm term)
                . M.insert (termPlural term) (cslTerm term))
    M.empty
    (concatMap localeTerms $ styleLocale sty)
