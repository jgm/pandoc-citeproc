{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Text.CSL.Pandoc (processCites, processCites')
where

import Prelude
import           Control.Applicative      ((<|>))
import qualified Control.Exception        as E
import           Control.Monad
import           Control.Monad.State
import           Data.Aeson
import qualified Data.ByteString.Lazy     as L
import           Data.Char                (isDigit, isPunctuation, isSpace)
import qualified Data.Map                 as M
import qualified Data.Set                 as Set
import           Data.Text                (Text)
import qualified Data.Text                as T
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
import           Text.Pandoc.Shared       (stringify, ordNub)
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
      moveNotes     = maybe True truish $
                        lookupMeta "notes-after-punctuation" m1
      Pandoc m3 bs  = walk (mvPunct moveNotes style) . deNote .
                        walk (processCite style cits_map) $ Pandoc m2 b2
      m             = case metanocites of
                            Nothing -> m3
                            Just x  -> setMeta "nocite" x m3
      notemap       = mkNoteMap (Pandoc m3 bs)
      hanging       = (== Just "true")
                       (biblio style >>=
                        lookup "hanging-indent" . bibOptions)
  in  Pandoc m $ walk (addFirstNoteNumber notemap)
               $ walk (concatMap removeNocaseSpans)
               $ insertRefs hanging m biblioList bs

addFirstNoteNumber :: M.Map Text Int -> Inline -> Inline
addFirstNoteNumber notemap
  s@(Span ("",["first-reference-note-number"],[("refid",refid)]) _)
  = case M.lookup refid notemap of
         Nothing -> s
         Just n  -> Str $ T.pack (show n)
addFirstNoteNumber _ -- see below, these spans added by deNote
  (Note [Para (Span ("",["reference-id-list"],_) [] : ils)])
  = Note [Para ils]
addFirstNoteNumber _ x = x

mkNoteMap :: Pandoc -> M.Map Text Int
mkNoteMap doc =
  foldr go mempty $ splitUp $ zip [1..] $ query getNoteCitationIds doc
  where
   splitUp :: [(Int, [Text])] -> [(Int, Text)]
   splitUp = concatMap (\(n,ss) -> map (n,) ss)
   go :: (Int, Text) -> M.Map Text Int -> M.Map Text Int
   go (notenumber, citeid) = M.insert citeid notenumber

-- if document contains a Div with id="refs", insert
-- references as its contents.  Otherwise, insert references
-- at the end of the document in a Div with id="refs"
insertRefs :: Bool -> Meta -> [Block] -> [Block] -> [Block]
insertRefs _ _  []   bs = bs
insertRefs hanging meta refs bs =
  if isRefRemove meta
     then bs
     else case runState (walkM go bs) False of
               (bs', True) -> bs'
               (_, False)
                 -> case refTitle meta of
                      Nothing ->
                        case reverse bs of
                          Header lev (id',classes,kvs) ys : xs ->
                            reverse xs ++
                            [Header lev (id',addUnNumbered classes,kvs) ys,
                             Div ("refs",refclasses,[]) refs]
                          _ -> bs ++ [refDiv]
                      Just ils -> bs ++
                        [Header 1 ("bibliography", ["unnumbered"], []) ils,
                         refDiv]
  where
   refclasses = "references" : if hanging then ["hanging-indent"] else []
   refDiv = Div ("refs", refclasses, []) refs
   addUnNumbered cs = "unnumbered" : [c | c <- cs, c /= "unnumbered"]
   go :: Block -> State Bool Block
   go (Div ("refs",cs,kvs) xs) = do
     put True
     -- refHeader isn't used if you have an explicit references div
     let cs' = ordNub $ cs ++ refclasses
     return $ Div ("refs",cs',kvs) (xs ++ refs)
   go x = return x

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
  maybe False truish $ lookupMeta "suppress-bibliography" meta

isLinkCitations :: Meta -> Bool
isLinkCitations meta =
  maybe False truish $ lookupMeta "link-citations" meta

truish :: MetaValue -> Bool
truish (MetaBool t) = t
truish (MetaString s) = isYesValue (T.toLower s)
truish (MetaInlines ils) = isYesValue (T.toLower (stringify ils))
truish (MetaBlocks [Plain ils]) = isYesValue (T.toLower (stringify ils))
truish _ = False

isYesValue :: Text -> Bool
isYesValue "t" = True
isYesValue "true" = True
isYesValue "yes" = True
isYesValue "on" = True
isYesValue _ = False

-- if the 'nocite' Meta field contains a citation with id = '*',
-- create a cite with to all the references.
mkNociteWildcards :: [Reference] -> [[Citation]] -> [[Citation]]
mkNociteWildcards refs = map expandStar
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
                   >>= toText
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
         setEnv "LC_ALL" (T.unpack $ localeLang l)
         setEnv "LANG"   (T.unpack $ localeLang l)
       []    -> do
         envlang <- getEnv "LANG"
         if null envlang
            then do
              -- Note that LANG needs to be set for bibtex conversion:
              setEnv "LANG" "en_US.UTF-8"
              setEnv "LC_ALL" "en_US.UTF-8"
            else
              setEnv "LC_ALL" envlang
  let citids = query getCitationIds (Pandoc meta blocks)
  let idpred = if "*" `Set.member` citids
                  then const True
                  else (`Set.member` citids)
  bibRefs <- getBibRefs idpred $ fromMaybe (MetaList [])
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
  return $ processCites (tr' "CSL" csl') refs $ Pandoc meta blocks

toText :: MetaValue -> Maybe Text
toText (MetaString s) = Just s
-- take last in a list
toText (MetaList xs) = case reverse xs of
                             []    -> Nothing
                             (x:_) -> toText x
toText (MetaInlines ils) = Just $ stringify ils
toText _ = Nothing

toPath :: MetaValue -> Maybe String
toPath (MetaString s) = Just $ T.unpack s
-- take last in a list
toPath (MetaList xs) = case reverse xs of
                             []    -> Nothing
                             (x:_) -> toPath x
toPath (MetaInlines ils) = Just $ T.unpack $ stringify ils
toPath _ = Nothing

getBibRefs :: (Text -> Bool) -> MetaValue -> IO [Reference]
getBibRefs idpred (MetaList xs) = concat `fmap` mapM (getBibRefs idpred) xs
getBibRefs idpred (MetaInlines xs) = getBibRefs idpred (MetaString $ stringify xs)
getBibRefs idpred (MetaString s) = do
  path <- findFile ["."] (T.unpack s) >>= maybe (E.throwIO $ CouldNotFindBibFile $ T.unpack s) return
  map unescapeRefId `fmap` readBiblioFile idpred path
getBibRefs _ _ = return []

-- unescape reference ids, which may contain XML entities, so
-- that we can do lookups with regular string equality
unescapeRefId :: Reference -> Reference
unescapeRefId ref = ref{ refId = Literal $ decodeEntities (unLiteral $ refId ref) }

decodeEntities :: Text -> Text
decodeEntities t = case T.uncons t of
  Nothing -> ""
  Just ('&',xs) ->
    let (ys,zs) = T.break (==';') xs
    in  case T.uncons zs of
           Just (';',ws) -> case lookupEntity  ('&': T.unpack ys ++ ";") of
#if MIN_VERSION_tagsoup(0,13,0)
                              Just s  -> T.pack s <> decodeEntities ws
#else
                              Just c  -> T.cons c (decodeEntities ws)
#endif
                              Nothing -> T.cons '&' (decodeEntities xs)
           _      -> T.cons '&' (decodeEntities xs)
  Just (x,xs) -> T.cons x (decodeEntities xs)

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

getNoteCitationIds :: Inline -> [[Text]]
getNoteCitationIds (Note [Para (Span ("",["reference-id-list"]
                                      ,[("refids",refids)]) [] : _)])
  -- see deNote below which inserts this special Span
  = [T.words refids]
getNoteCitationIds (Note _) = [[]]
getNoteCitationIds _        = []

isNote :: Inline -> Bool
isNote (Note _)          = True
isNote (Cite _ [Note _]) = True
 -- the following allows citation styles that are "in-text" but use superscript
 -- references to be treated as if they are "notes" for the purposes of moving
 -- the citations after trailing punctuation (see <https://github.com/jgm/pandoc-citeproc/issues/382>):
isNote (Cite _ [Superscript _]) = True
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
mvPunct moveNotes sty (x : Space : xs)
  | isSpacy x = x : mvPunct moveNotes sty xs
mvPunct moveNotes sty (q : s : x : ys)
  | isSpacy s
  , isNote x
  , startWithPunct ys
  = if moveNotes
       then mvPunct moveNotes sty $
             case headInline ys of
               Nothing -> q : x : tailInline ys
               Just w  -> q : Str (T.singleton w) : x : tailInline ys
       else q : x : mvPunct moveNotes sty ys
mvPunct moveNotes sty (Cite cs ils : ys)
   | length ils > 1
   , isNote (last ils)
   , startWithPunct ys
   , moveNotes
   = Cite cs
      (init ils ++
         (case headInline ys of
            Nothing -> []
            Just s' | not (endWithPunct False (init ils)) -> [Str $ T.singleton s']
                    | otherwise                           -> [])
       ++ [last ils]) : mvPunct moveNotes sty (tailInline ys)
mvPunct moveNotes sty (q@(Quoted _ _) : w@(Str _) : x : ys)
  | isNote x
  , isPunctuationInQuote sty
  , moveNotes
  = mvPunctInsideQuote q w ++ (x : mvPunct moveNotes sty ys)
mvPunct moveNotes sty (s : x : ys) | isSpacy s, isNote x =
  x : mvPunct moveNotes sty ys
mvPunct moveNotes sty (s : x@(Cite _ (Superscript _ : _)) : ys)
  | isSpacy s = x : mvPunct moveNotes sty ys
mvPunct moveNotes sty (Cite cs ils : Str "." : ys)
  | lastInline ils == Just '.'
  = Cite cs ils : mvPunct moveNotes sty ys
mvPunct moveNotes sty (x:xs) = x : mvPunct moveNotes sty xs
mvPunct _ _ [] = []

endWithPunct :: Bool -> [Inline] -> Bool
endWithPunct _ [] = True
endWithPunct onlyFinal xs@(_:_) =
  case reverse (T.unpack $ stringify xs) of
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
            Cite (c:cs) [Note [Para $ specialSpan (c:cs) : toCapital xs]]
        go (Note xs) = Note $ topDown go' xs
        go x = x
        -- we insert this to help getNoteCitationIds:
        specialSpan cs =
          Span ("",["reference-id-list"],
            [("refids", T.unwords (map citationId cs))]) []
        go' (Str "(" : Cite cs [Note [Para xs]] : Str ")" : ys) =
             Str "(" : Cite cs xs : Str ")" : ys
        go' (x : Cite cs [Note [Para xs]] : ys) | not (isSpacy x) =
             x : Str "," : Space : comb (\zs -> [Cite cs zs]) xs ys
        go' (Str "(" : Note [Para xs] : Str ")" : ys) =
             Str "(" : xs ++ (Str ")" : ys)
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
      removeLeadingPunct (Str (T.unpack -> [c]) : s : zs)
          | isSpacy s && (c == ',' || c == '.' || c == ':') = zs
      removeLeadingPunct zs = zs
  in  f xs' ++ ys

-- | Retrieve all citations from a 'Pandoc' document. To be used with
-- 'query'.
getCitation :: Inline -> [[Citation]]
getCitation i | Cite t _ <- i = [t]
              | otherwise     = []

getCitationIds :: Inline -> Set.Set Text
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
          isPunct (Str (T.uncons -> Just (x,_))) = isPunctuation x
          isPunct _           = False
      in   emptyCite { CSL.citeId         = citationId c
                     , CSL.citePrefix     = Formatted $ citationPrefix c
                     , CSL.citeSuffix     = Formatted s'
                     , CSL.citeLabel      = la
                     , CSL.citeLocator    = lo
                     , CSL.citeNoteNumber = T.pack $ show $ citationNoteNum c
                     , CSL.authorInText   = citationMode c == AuthorInText
                     , CSL.suppressAuthor = citationMode c == SuppressAuthor
                     , CSL.citeHash       = citationHash c
                     }

splitInp :: [Inline] -> [Inline]
splitInp = splitStrWhen (\c -> splitOn c || isSpace c)
  where
      splitOn ':' = False
      splitOn c   = isPunctuation c

locatorWords :: LocatorMap -> [Inline] -> (Text, Text, [Inline])
locatorWords locMap inp =
  case parse (pLocatorWords locMap) "suffix" $ splitInp inp of
       Right r -> r
       Left _  -> ("","",inp)

-- Some terminology
-- ----------------
-- Word       => 89
--               12-15
--               13(a)(i)-(iv)
--               [1.2.5]
--
-- Integrated => [@citekey, 89]
--               [@citekey, p. 40, 41, 89-199, suffix]
-- Delimited  => [@citekey{89}]
--               [@citekey, {p. literally anything except unbalanced curly quotes}, suffix]
--
-- When parsing integrated locators you have to be careful not to include
-- 'suffix' in the locator, so that means pretty strict control over when
-- you're allowed to use NO digits in a word. [@citekey, p. 40(a) (bcd)] will
-- stop parsing the locator after (a). You also have to be careful not to parse
-- random terms like 'and' in en-US as citeLabels, which means careful control
-- over what must come directly after any label string (via notFollowedBy).
--
-- With delimited locators, it's a different story. Parse as long a locator
-- label as you can find in the terms map, then include EVERYTHING in the outer
-- {} braces.
--
-- Ultimately the complexity is driven by wanting as many locator words as
-- possible being parsed in the integrated style, because it fits with the
-- aims of Markdown (being readable). Ideally, anything except a word with
-- neither roman numerals nor arabic digits can be integrated. Some
-- counter-examples:
-- a
-- (a)(b)(c)
-- (hello)

pLocatorWords :: LocatorMap -> Parsec [Inline] st (Text, Text, [Inline])
pLocatorWords locMap = do
  optional $ pMatchChar "," (== ',')
  optional pSpace
  (la, lo) <- pLocatorDelimited locMap <|> pLocatorIntegrated locMap
  s <- getInput -- rest is suffix
  -- need to trim, otherwise "p. 9" and "9" will have 'different' locators later on
  -- i.e. the first one will be " 9"
  return (la, trim lo, s)

pLocatorDelimited :: LocatorMap -> Parsec [Inline] st (Text, Text)
pLocatorDelimited locMap = try $ do
  _ <- pMatchChar "{" (== '{')
  skipMany pSpace -- gobble pre-spaces so label doesn't try to include them
  (la, _) <- pLocatorLabelDelimited locMap
  -- we only care about balancing {} and [] (because of the outer [] scope);
  -- the rest can be anything
  let inner = do { t <- anyToken; return (True, stringify t) }
  gs <- many (pBalancedBraces [('{','}'), ('[',']')] inner)
  _ <- pMatchChar "}" (== '}')
  let lo = T.concat $ map snd gs
  return (la, lo)

pLocatorLabelDelimited :: LocatorMap -> Parsec [Inline] st (Text, Bool)
pLocatorLabelDelimited locMap
  = pLocatorLabel' locMap lim <|> return ("page", True)
    where
        lim = stringify <$> anyToken

pLocatorIntegrated :: LocatorMap -> Parsec [Inline] st (Text, Text)
pLocatorIntegrated locMap = try $ do
  (la, wasImplicit) <- pLocatorLabelIntegrated locMap
  -- if we got the label implicitly, we have presupposed the first one is going
  -- to have a digit, so guarantee that. You _can_ have p. (a) because you
  -- specified it.
  let modifier = if wasImplicit
                    then requireDigits
                    else requireRomansOrDigits
  g <- try $ pLocatorWordIntegrated (not wasImplicit) >>= modifier
  gs <- many (try $ pLocatorWordIntegrated False >>= modifier)
  let lo = T.concat (g:gs)
  return (la, lo)

pLocatorLabelIntegrated :: LocatorMap -> Parsec [Inline] st (Text, Bool)
pLocatorLabelIntegrated locMap
  = pLocatorLabel' locMap lim <|> (lookAhead digital >> return ("page", True))
    where
      lim = try $ pLocatorWordIntegrated True >>= requireRomansOrDigits
      digital = try $ pLocatorWordIntegrated True >>= requireDigits

pLocatorLabel' :: LocatorMap -> Parsec [Inline] st Text -> Parsec [Inline] st (Text, Bool)
pLocatorLabel' locMap lim = go ""
    where
      -- grow the match string until we hit the end
      -- trying to find the largest match for a label
      go acc = try $ do
          -- advance at least one token each time
          -- the pathological case is "p.3"
          t <- anyToken
          ts <- manyTill anyToken (try $ lookAhead lim)
          let s = acc <> stringify (t:ts)
          case M.lookup (trim s) locMap of
            -- try to find a longer one, or return this one
            Just l -> go s <|> return (l, False)
            Nothing -> go s

-- hard requirement for a locator to have some real digits in it
requireDigits :: (Bool, Text) -> Parsec [Inline] st Text
requireDigits (_, s) = if not (T.any isDigit s)
                          then Prelude.fail "requireDigits"
                          else return s

-- soft requirement for a sequence with some roman or arabic parts
-- (a)(iv) -- because iv is roman
-- 1(a)  -- because 1 is an actual digit
-- NOT: a, (a)-(b), hello, (some text in brackets)
requireRomansOrDigits :: (Bool, Text) -> Parsec [Inline] st Text
requireRomansOrDigits (d, s) = if not d
                                  then Prelude.fail "requireRomansOrDigits"
                                  else return s

pLocatorWordIntegrated :: Bool -> Parsec [Inline] st (Bool, Text)
pLocatorWordIntegrated isFirst = try $ do
  punct <- if isFirst
              then return ""
              else (stringify <$> pLocatorSep) <|> return ""
  sp <- option "" (pSpace >> return " ")
  (dig, s) <- pBalancedBraces [('(',')'), ('[',']'), ('{','}')] pPageSeq
  return (dig, punct <> sp <> s)

-- we want to capture:  123, 123A, C22, XVII, 33-44, 22-33; 22-11
--                      34(1), 34A(A), 34(1)(i)(i), (1)(a)
--                      [17], [17]-[18], '591 [84]'
--                      (because CSL cannot pull out individual pages/sections
--                      to wrap in braces on a per-style basis)
pBalancedBraces :: [(Char, Char)]
                -> Parsec [Inline] st (Bool, Text)
                -> Parsec [Inline] st (Bool, Text)
pBalancedBraces braces p = try $ do
  ss <- many1 surround
  return $ anyWereDigitLike ss
  where
      except = notFollowedBy pBraces >> p
      -- outer and inner
      surround = foldl (\a (open, close) -> sur open close except <|> a)
                       except
                       braces

      isc c = stringify <$> pMatchChar [c] (== c)

      sur c c' m = try $ do
          (d, mid) <- between (isc c) (isc c') (option (False, "") m)
          return (d, T.cons c . flip T.snoc c' $  mid)

      flattened = concatMap (\(o, c) -> [o, c]) braces
      pBraces = pMatchChar "braces" (`elem` flattened)

-- YES 1, 1.2, 1.2.3
-- NO  1., 1.2. a.6
-- can't use sepBy because we want to leave trailing .s
pPageSeq :: Parsec [Inline] st (Bool, Text)
pPageSeq = oneDotTwo <|> withPeriod
  where
      oneDotTwo = do
          u <- pPageUnit
          us <- many withPeriod
          return $ anyWereDigitLike (u:us)
      withPeriod = try $ do
          -- .2
          p <- pMatchChar "." (== '.')
          u <- try pPageUnit
          return (fst u, stringify p <> snd u)

anyWereDigitLike :: [(Bool, Text)] -> (Bool, Text)
anyWereDigitLike as = (any fst as, T.concat $ map snd as)

pPageUnit :: Parsec [Inline] st (Bool, Text)
pPageUnit = roman <|> plainUnit
  where
      -- roman is a 'digit'
      roman = (True,) <$> pRoman
      plainUnit = do
          ts <- many1 (notFollowedBy pSpace >>
                       notFollowedBy pLocatorPunct >>
                       anyToken)
          let s = stringify ts
          -- otherwise look for actual digits or -s
          return (T.any isDigit s, s)

pRoman :: Parsec [Inline] st Text
pRoman = try $ do
  t <- anyToken
  case t of
       Str xs -> case parseRomanNumeral (T.unpack xs) of
                      Nothing -> mzero
                      Just _  -> return $ xs
       _      -> mzero

isLocatorPunct :: Char -> Bool
isLocatorPunct '-' = False -- page range
isLocatorPunct '–' = False -- page range, en dash
isLocatorPunct ':' = False -- vol:page-range hack
isLocatorPunct c   = isPunctuation c -- includes [{()}]

pLocatorPunct :: Parsec [Inline] st Inline
pLocatorPunct = pMatchChar "punctuation" isLocatorPunct

pLocatorSep :: Parsec [Inline] st Inline
pLocatorSep = pMatchChar "locator separator" isLocatorSep

isLocatorSep :: Char -> Bool
isLocatorSep ',' = True
isLocatorSep ';' = True
isLocatorSep _   = False

pMatchChar :: String -> (Char -> Bool) -> Parsec [Inline] st Inline
pMatchChar msg f = pMatch msg mc
    where
        mc (Str (T.unpack -> [c])) = f c
        mc _         = False

pSpace :: Parsec [Inline] st Inline
pSpace = pMatch "' '" (\t -> isSpacy t || t == Str "\160")

pMatch :: String -> (Inline -> Bool) -> Parsec [Inline] st Inline
pMatch msg condition = try $ do
  t <- anyToken
  if not (condition t)
     then Prelude.fail msg
     else return t

type LocatorMap = M.Map Text Text

locatorMap :: Style -> LocatorMap
locatorMap sty =
  foldr (\term -> M.insert (termSingular term) (cslTerm term)
                . M.insert (termPlural term) (cslTerm term))
    M.empty
    (concatMap localeTerms $ styleLocale sty)
