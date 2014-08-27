{-# LANGUAGE PatternGuards, OverloadedStrings, FlexibleInstances,
    ScopedTypeVariables, CPP #-}
module Text.CSL.Pandoc (processCites, processCites') where

import Text.Pandoc
import Text.Pandoc.Walk
import Text.Pandoc.Builder (setMeta, deleteMeta, Inlines, cite)
import Text.Pandoc.Shared (stringify)
import Text.HTML.TagSoup.Entity (lookupEntity)
import qualified Data.ByteString.Lazy as L
import Control.Applicative ((<|>))
import Data.Aeson
import Data.Char ( isDigit, isPunctuation, isSpace )
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
import Data.Monoid (mempty)
import Control.Monad.State
import System.FilePath
import System.Directory (doesFileExist, getAppUserDataDirectory)
import Text.CSL.Util (findFile, splitStrWhen)

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
      result        = citeproc procOpts style refs (setNearNote style $
                        map (map (toCslCite locMap)) grps)
      cits_map      = M.fromList $ zip grps (citations result)
      biblioList    = map (renderPandoc' style) (bibliography result)
      Pandoc m b3   = bottomUp (mvPunct style) . deNote .
                        topDown (processCite style cits_map) $ Pandoc m4 b2
      (bs, lastb)    = case reverse b3 of
                          (Header lev (id',classes,kvs) ys) : xs ->
                           (reverse xs, [Header lev (id',classes',kvs) ys])
                            where classes' = "unnumbered" :
                                       [c | c <- classes, c /= "unnumbered"]
                          _                                      -> (b3,  [])
  in  Pandoc m $ bottomUp (concatMap removeNocaseSpans)
               $ bs ++
                 if lookupMeta "suppress-bibliography" m == Just (MetaBool True)
                    then []
                    else [Div ("",["references"],[]) (lastb ++ biblioList)]

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
  csldir <- getAppUserDataDirectory "csl"
  let inlineRefError s = error $ "Error parsing references: " ++ s
  let inlineRefs = either inlineRefError id
                   $ convertRefs $ lookupMeta "references" meta
  bibRefs <- getBibRefs $ maybe (MetaList []) id
                        $ lookupMeta "bibliography" meta
  let refs = inlineRefs ++ bibRefs
  let cslfile = (lookupMeta "csl" meta <|> lookupMeta "citation-style" meta)
                >>= toPath
  let mbLocale = lookupMeta "locale" meta >>= toPath
  csl <- case cslfile of
               Just f | not (null f) -> readCSLFile mbLocale f
               _ -> do
                 -- get default CSL: look first in ~/.csl, and take
                 -- from distribution if not found
                 let f = csldir </> "chicago-author-date.csl"
                 exists <- doesFileExist f
                 raw <- if exists
                           then L.readFile f
                           else getDefaultCSL
                 localizeCSL mbLocale $ parseCSL' raw
  let cslAbbrevFile = lookupMeta "citation-abbreviations" meta >>= toPath
  let skipLeadingSpace = L.dropWhile (\s -> s == 32 || (s >= 9 && s <= 13))
  abbrevs <- maybe (return (Abbreviations M.empty))
             (\f -> findFile [".", csldir] f >>=
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
        Just (Formatted (x:xs))
          | isTextualCitation t && not (null xs) ->
                         Cite t (renderPandoc s (Formatted [x]) ++
                                 renderPandoc s (Formatted xs))
          | otherwise -> Cite t (renderPandoc s (Formatted (x:xs)))
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
   = Cite cs (init ils ++ [Str (headInline ys) | not (endWithPunct (init ils))]
     ++ [last ils]) : tailFirstInlineStr ys
mvPunct sty (q@(Quoted _ _) : w@(Str _) : x : ys)
  | isNote x, isPunctuationInQuote sty  =
    mvPunctInsideQuote q w ++ (x : ys)
mvPunct _ (Space : x : ys) | isNote x = x : ys
mvPunct _ (Space : x@(Cite _ (Superscript _ : _)) : ys) = x : ys
mvPunct _ xs = xs

endWithPunct :: [Inline] -> Bool
endWithPunct [] = True
endWithPunct xs@(_:_) = case reverse (stringify xs) of
                              []                       -> True
                              -- covers .), .", etc.:
                              (d:c:_) | isPunctuation d
                                       && isEndPunct c -> True
                              (c:_) | isEndPunct c     -> True
                                    | otherwise        -> False
  where isEndPunct c = c `elem` ".,;:!?"

startWithPunct :: [Inline] -> Bool
startWithPunct = and . map (`elem` ".,;:!?") . headInline

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
           let xs' = if startWithPunct ys && endWithPunct xs
                        then initInline $ removeLeadingPunct xs
                        else removeLeadingPunct xs
           in f xs' ++ ys
        sanitize :: [Block] -> [Block]
        sanitize [Para xs] =
           [Para $ toCapital xs ++ if endWithPunct xs then [Space] else []]
        sanitize bs = bs

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
          citMode = case citationMode c of
                      AuthorInText   -> (True, False)
                      SuppressAuthor -> (False,True )
                      NormalCitation -> (False,False)
      in   emptyCite { CSL.citeId         = citationId c
                     , CSL.citePrefix     = Formatted $ citationPrefix c
                     , CSL.citeSuffix     = Formatted s'
                     , CSL.citeLabel      = la
                     , CSL.citeLocator    = lo
                     , CSL.citeNoteNumber = show $ citationNoteNum c
                     , CSL.authorInText   = fst citMode
                     , CSL.suppressAuthor = snd citMode
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
  rawLoc <- many
     (notFollowedBy pSpace >> notFollowedBy (pWordWithDigits True) >> anyToken)
  la <- case stringify rawLoc of
                 ""   -> lookAhead (pSpace >> pDigit) >> return "page"
                 s    -> maybe mzero return $ parseLocator locMap s
  g <- pWordWithDigits True
  gs <- many (pWordWithDigits False)
  let lo = concat (g:gs)
  return (la, lo)

-- we want to capture:  123, 123A, C22, XVII, 33-44, 22-33; 22-11
pWordWithDigits :: Bool -> Parsec [Inline] st String
pWordWithDigits isfirst = try $ do
  punct <- if isfirst
              then return ""
              else stringify `fmap` pLocatorPunct
  sp <- option "" (pSpace >> return " ")
  r <- many1 (notFollowedBy pSpace >> notFollowedBy pLocatorPunct >> anyToken)
  let s = stringify r
  guard $ any isDigit s || all (`elem` "IVXLCM") s
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

parseLocator :: LocatorMap -> String -> Maybe String
parseLocator locMap s = M.lookup s locMap

locatorMap :: Style -> LocatorMap
locatorMap sty =
  foldr (\term -> M.insert (termSingular term) (cslTerm term)
                . M.insert (termPlural term) (cslTerm term))
    M.empty
    (concatMap localeTerms $ styleLocale sty)
