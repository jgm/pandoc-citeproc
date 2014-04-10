{-# LANGUAGE PatternGuards, OverloadedStrings, FlexibleInstances,
    ScopedTypeVariables, CPP #-}
module Text.CSL.Pandoc (processCites, processCites') where

import Text.Pandoc
import Text.Pandoc.Walk
import Text.Pandoc.Shared (stringify)
import Text.HTML.TagSoup.Entity (lookupEntity)
import qualified Data.ByteString.Lazy as L
import Control.Applicative ((<|>))
import Data.Aeson
import Data.List
import Data.Char ( isDigit, isPunctuation )
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
import Control.Monad.State
import System.FilePath
import System.Directory (doesFileExist, getAppUserDataDirectory)

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
      Pandoc m b = bottomUp (mvPunct style) . deNote .
                     topDown (processCite style cits_map) $ doc'
      hdrInlines = refTitle m
      (bs, lastb) = case reverse b of
                         x@(Header _ _ _) : xs -> (reverse xs, [x])
                         _                     -> (b,  [])
      refHeader = case hdrInlines of
        Just ils -> lastb ++ [Header 1 ("bibliography", ["unnumbered"], []) ils]
        _        -> lastb
      refDiv    = case isRefRemove m of
        True  -> []
        False -> [Div ("",["references"],[]) (refHeader ++ biblioList)]
  in  Pandoc m $ bottomUp (concatMap removeNocaseSpans)
               $ bs ++ refDiv

refTitle :: Meta -> Maybe [Inline]
refTitle meta =
  case lookupMeta "ref-section-title" meta of
    Just (MetaString s)           -> Just [Str s]
    Just (MetaInlines ils)        -> Just ils
    Just (MetaBlocks [Plain ils]) -> Just ils
    Just (MetaBlocks [Para ils])  -> Just ils
    _                             -> Nothing

isRefRemove :: Meta -> Bool
isRefRemove meta =
  case lookupMeta "remove-ref-section" meta of
    Just (MetaBool True) -> True
    _                    -> False

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
  rawCSL <- maybe getDefaultCSL (\f -> findFile [".", csldir] f >>= L.readFile)
               cslfile
  let mbLocale = lookupMeta "locale" meta >>= toPath
  csl <- localizeCSL mbLocale $ parseCSL' rawCSL
  let cslAbbrevFile = lookupMeta "citation-abbreviations" meta >>= toPath
  let skipLeadingSpace = L.dropWhile (\s -> s == 32 || (s >= 9 && s <= 13))
  abbrevs <- maybe (return (Abbreviations M.empty))
             (\f -> findFile [".", csldir] f >>=
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
  path <- findFile ["."] s
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
mvPunct sty (q@(Quoted _ _) : w@(Str _) : x : ys)
  | isNote x, isPunctuationInQuote sty  =
    mvPunctInsideQuote q w ++ (x : ys)
mvPunct _ (Space : x : ys) | isNote x = x : ys
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
            Cite (c:cs) [Note $ dropInitialPunct $ bottomUp go' $ sanitize c xs]
        go (Note xs) = Note $ topDown go' xs
        go x = x
        go' (x : Note [Para xs] : ys) | x /= Space =
             x : Str "," : Space :
             if startWithPunct ys && endWithPunct xs
                then initInline xs ++ ys
                else xs ++ ys
        go' (Note [Para xs] : ys) =
             if startWithPunct ys && endWithPunct xs
                then initInline xs ++ ys
                else xs ++ ys
        go' xs = xs
        dropInitialPunct [Para (Str [c]:Space:xs)] | c `elem` ",;:" = [Para xs]
        dropInitialPunct bs                                         = bs
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
                     , CSL.citePrefix     = Formatted $ citationPrefix c
                     , CSL.citeSuffix     = Formatted s'
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

findFile :: [FilePath] -> FilePath -> IO FilePath
findFile [] f = fail $ "Not found: " ++ f
findFile (p:ps) f = do
  exists <- doesFileExist (p </> f)
  if exists
     then return (p </> f)
     else findFile ps f

