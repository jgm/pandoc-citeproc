{-# LANGUAGE PatternGuards, OverloadedStrings, FlexibleInstances,
    ScopedTypeVariables, CPP #-}
module Text.CSL.Pandoc (processCites, processCites') where

import Text.Pandoc
import Text.Pandoc.Walk
import Text.Pandoc.Shared (stringify)
import Text.HTML.TagSoup.Entity (lookupEntity)
import qualified Data.ByteString.Lazy as L
import Control.Applicative ((<|>))
import qualified Data.Traversable as Traversable
import Data.Aeson
import Data.List
import Data.Char ( isDigit, isPunctuation )
import qualified Data.Map as M
import Text.CSL hiding ( Cite(..), Citation(..))
import Text.CSL.Output.Pandoc ( headInline, tailFirstInlineStr, initInline,
                                toCapital )
import Text.CSL.Data (getDefaultCSL)
import qualified Text.CSL as CSL ( Cite(..) )
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
      Pandoc m b = bottomUp mvPunct . deNote .
                     topDown (processCite style cits_map) $ doc'
      (bs, lastb) = case reverse b of
                         x@(Header _ _ _) : xs -> (reverse xs, [x])
                         _                     -> (b,  [])
  in  Pandoc m $ bottomUp (concatMap removeNocaseSpans)
               $ bs ++ [Div ("",["references"],[]) (lastb ++ biblioList)]

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
  csl <- localizeCSL $ parseCSL' rawCSL
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
unescapeRefId ref = ref{ refId = decodeEntities (refId ref) }

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

convertRefs :: Maybe MetaValue -> Either String [Reference]
convertRefs Nothing = Right []
convertRefs (Just v) =
  case metaValueToJSON blocksToMarkdown inlinesToMarkdown v >>= fromJSON of
       Data.Aeson.Error s   -> Left s
       Success x            -> Right x

blocksToMarkdown :: (Functor m, Monad m) => [Block] -> m String
blocksToMarkdown bs = return $ writeMarkdown def $ Pandoc nullMeta bs

inlinesToMarkdown :: (Functor m, Monad m) => [Inline] -> m String
inlinesToMarkdown ils = blocksToMarkdown [Plain ils]

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

-- | Substitute 'Cite' elements with formatted citations.
processCite :: Style -> M.Map [Citation] FormattedOutput -> Inline -> Inline
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

endWithPunct :: [Inline] -> Bool
endWithPunct [] = True
endWithPunct xs@(_:_) = case reverse (stringify [last xs]) of
                              []                       -> True
                              (')':c:_) | isEndPunct c -> True
                              (c:_) | isEndPunct c     -> True
                                    | otherwise        -> False
  where isEndPunct c = c `elem` ".,;:!?"

startWithPunct :: [Inline] -> Bool
startWithPunct = and . map (`elem` ".,;:!?") . headInline

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

findFile :: [FilePath] -> FilePath -> IO FilePath
findFile [] f = fail $ "Not found: " ++ f
findFile (p:ps) f = do
  exists <- doesFileExist (p </> f)
  if exists
     then return (p </> f)
     else findFile ps f

