{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards         #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Text.CSL.Util
  ( safeRead
  , readNum
  , (<^>)
  , capitalize
  , camelize
  , uncamelize
  , isPunct
  , last'
  , init'
  , words'
  , trim
  , triml
  , trimr
  , parseBool
  , parseString
  , parseInt
  , parseMaybeInt
  , mb
  , (.#?)
  , (.#:)
  , onBlocks
  , titlecase
  , unTitlecase
  , protectCase
  , splitWhen
  , splitStrWhen
  , proc
  , proc'
  , procM
  , query
  , orIfNull
  , toRead
  , inlinesToString
  , headInline
  , lastInline
  , tailInline
  , initInline
  , tailFirstInlineStr
  , toCapital
  , mapHeadInline
  , tr'
  , findFile
  , AddYaml(..)
  , mapping'
  , parseRomanNumeral
  , isRange
  , addSpaceAfterPeriod
  ) where
import           Prelude
import           Control.Monad.State
import           Data.Aeson
import           Data.Aeson.Types    (Parser)
import           Data.Char           (isAscii, isLower, isPunctuation,
                                      isUpper, isLetter, toLower, toUpper)
import           Data.Generics       (Data, Typeable, everything, everywhere,
                                      everywhere', everywhereM, mkM, mkQ, mkT)
import           Data.List.Split     (wordsBy)
import qualified Data.Set            as Set
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Traversable
import           Data.Yaml.Builder   (ToYaml (..), YamlBuilder)
import qualified Data.Yaml.Builder   as Y
import           System.Directory    (doesFileExist)
import           System.FilePath
import           Text.Pandoc
import           Text.Pandoc.Shared  (safeRead, stringify)
import           Text.Pandoc.Walk    (walk)
import qualified Text.Parsec         as P

#ifdef TRACE
import qualified Debug.Trace
import           Text.Show.Pretty    (ppShow)
#endif

#ifdef TRACE
tr' :: Show a => String -> a -> a
tr' note' x = Debug.Trace.trace ("=== " ++ note' ++ "\n" ++ ppShow x ++ "\n") x
#else
tr' :: String -> a -> a
tr' _ x = x
#endif

readNum :: Text -> Int
readNum s = case reads (T.unpack s) of
              [(x,"")] -> x
              _        -> 0

-- | Conjoin strings, avoiding repeated punctuation.
(<^>) :: Text -> Text -> Text
"" <^> sb = sb
sa <^> "" = sa
sa <^> sb = case (,) <$> T.unsnoc sa <*> T.uncons sb of
  Just ((_,la), (c,xs)) | isPunct' la && isPunct' c -> sa <> xs
  _ -> sa <> sb
 where isPunct' = (`elem` (";:,. " :: String))

capitalize :: Text -> Text
capitalize t = case T.uncons t of
  Nothing      -> ""
  Just (c, cs) -> T.cons (toUpper c) cs

isPunct :: Char -> Bool
isPunct c = c `elem` (".;?!" :: String)

camelize :: Text -> String
camelize =
  let camelize' t = case t of
        ('-':y:ys) -> toUpper y : camelize' ys
        ('_':y:ys) -> toUpper y : camelize' ys
        (y:ys)     ->         y : camelize' ys
        _          -> []
  in camelize' . T.unpack

uncamelize :: String -> String
uncamelize = foldr g [] . f
    where g    x xs  = if isUpper x then '-' : toLower x : xs else x : xs
          f (  x:xs) = toLower x : xs
          f       [] = []

last' :: [a] -> [a]
last' [] = []
last' xs = [last xs]

init' :: [a] -> [a]
init' [] = []
init' xs = init xs

-- | Like words, but doesn't break on nonbreaking spaces etc.
words' :: String -> [String]
words' = wordsBy (\c -> c == ' ' || c == '\t' || c == '\r' || c == '\n')

-- | Remove leading and trailing space (including newlines) from string.
trim :: Text -> Text
trim = T.dropAround isSpaceOrNewline

triml :: Text -> Text
triml = T.dropWhile isSpaceOrNewline

trimr :: Text -> Text
trimr = T.dropWhileEnd isSpaceOrNewline

isSpaceOrNewline :: Char -> Bool
isSpaceOrNewline c = c `elem` (" \r\n\t" :: String)

-- | Parse JSON Boolean or Number as Bool.
parseBool :: Value -> Parser Bool
parseBool (Bool b)   = return b
parseBool (Number n) = case fromJSON (Number n) of
                            Success (0 :: Int) -> return False
                            Success _          -> return True
                            Error e            -> Prelude.fail $ "Could not read boolean: " ++ e
parseBool _          = Prelude.fail "Could not read boolean"

-- | Parse JSON value as String.
parseString :: Value -> Parser Text
parseString (String s) = return s
parseString (Number n) = case fromJSON (Number n) of
                            Success (x :: Int) -> return . T.pack $ show x
                            Error _ -> case fromJSON (Number n) of
                              Success (x :: Double) -> return . T.pack $ show x
                              Error e -> Prelude.fail $ "Could not read string: " ++ e
parseString (Bool b)   = return . T.toLower . T.pack $ show b
parseString v@(Array _)= inlinesToString `fmap` parseJSON v
parseString v          = Prelude.fail $ "Could not read as string: " ++ show v

-- | Parse JSON value as Int.
parseInt :: Value -> Parser Int
parseInt (Number n) = case fromJSON (Number n) of
                            Success (x :: Int) -> return x
                            Error e -> Prelude.fail $ "Could not read Int: " ++ e
parseInt x = parseString x >>= \s ->
              case safeRead s of
                   Just n  -> return n
                   Nothing -> Prelude.fail "Could not read Int"

-- | Parse JSON value as Maybe Int.
parseMaybeInt :: Maybe Value -> Parser (Maybe Int)
parseMaybeInt Nothing = return Nothing
parseMaybeInt (Just (Number n)) = case fromJSON (Number n) of
                                       Success (x :: Int) -> return (Just x)
                                       Error e -> Prelude.fail $ "Could not read Int: " ++ e
parseMaybeInt (Just x) =
  parseString x >>= \s ->
                   if T.null s
                      then return Nothing
                      else case safeRead s of
                                Just n  -> return (Just n)
                                Nothing -> Prelude.fail $ "Could not read as Int: " ++ show s

mb :: Monad m => (b -> m a) -> (Maybe b -> m (Maybe a))
mb  = Data.Traversable.mapM

-- | Parse as a string (even if the value is a number).
(.#?) :: Object -> Text -> Parser (Maybe Text)
x .#? y = (x .:? y) >>= mb parseString

(.#:) :: Object -> Text -> Parser Text
x .#: y = (x .: y) >>= parseString

onBlocks :: ([Inline] -> [Inline]) -> [Block] -> [Block]
onBlocks f = walk f'
  where f' (Para ils)  = Para (f ils)
        f' (Plain ils) = Plain (f ils)
        f' x           = x

hasLowercaseWord :: [Inline] -> Bool
hasLowercaseWord = any startsWithLowercase . splitStrWhen isPunctuation
  where startsWithLowercase (Str (T.uncons -> Just (x,_))) = isLower x
        startsWithLowercase _           = False

splitUpStr :: [Inline] -> [Inline]
splitUpStr ils =
  case reverse (combineInternalPeriods
         (splitStrWhen (\c -> isPunctuation c || c == '\160') ils)) of
         []     -> []
         (x:xs) -> reverse $ Span ("",["lastword"],[]) [x] : xs

-- We want to make sure that the periods in www.example.com, for
-- example, are not interpreted as sentence-ending punctuation.
combineInternalPeriods :: [Inline] -> [Inline]
combineInternalPeriods [] = []
combineInternalPeriods (Str xs:Str ".":Str ys:zs) =
  combineInternalPeriods $ Str (xs <> "." <> ys) : zs
combineInternalPeriods (x:xs) = x : combineInternalPeriods xs

unTitlecase :: [Inline] -> [Inline]
unTitlecase zs = evalState (caseTransform untc zs) SentenceBoundary
  where untc w = do
          st <- get
          case (w, st) of
               (y, NoBoundary) -> return y
               (Str (T.uncons -> Just (x,xs)), LastWordBoundary) | isUpper x ->
                 return $ Str (T.toLower (T.cons x xs))
               (Str (T.uncons -> Just (x,xs)), WordBoundary) | isUpper x ->
                 return $ Str (T.toLower (T.cons x xs))
               (Str (T.uncons -> Just (x,xs)), SentenceBoundary) | isLower x ->
                 return $ Str (T.cons (toUpper x) xs)
               (Span ("",[],[]) xs, _) | hasLowercaseWord xs ->
                 return $ Span ("",["nocase"],[]) xs
               _ -> return w

protectCase :: [Inline] -> [Inline]
protectCase zs = evalState (caseTransform protect zs) SentenceBoundary
  where protect (Span ("",[],[]) xs)
          | hasLowercaseWord xs = do
            st <- get
            case st of
                 NoBoundary -> return $ Span ("",[],[]) xs
                 _          -> return $ Span ("",["nocase"],[]) xs
        protect x = return x

-- From CSL docs:
-- "Title case conversion (with text-case set to “title”) for English-language
-- items is performed by:
--
-- For uppercase strings, the first character of each word remains capitalized.
-- All other letters are lowercased.
-- For lower or mixed case strings, the first character of each lowercase word
-- is capitalized. The case of words in mixed or uppercase stays the same.
-- In both cases, stop words are lowercased, unless they are the first or last
-- word in the string, or follow a colon. The stop words are “a”, “an”, “and”,
-- “as”, “at”, “but”, “by”, “down”, “for”, “from”, “in”, “into”, “nor”, “of”,
-- “on”, “onto”, “or”, “over”, “so”, “the”, “till”, “to”, “up”, “via”, “with”,
-- and “yet”.
titlecase :: [Inline] -> [Inline]
titlecase zs = evalState (caseTransform tc zs) SentenceBoundary
  where tc (Str (T.unpack -> (x:xs))) = do
          st <- get
          return $ case st of
                        LastWordBoundary ->
                          case (x:xs) of
                           s | not (isAscii x) -> Str $ T.pack s
                             | isShortWord s   -> Str $ T.pack s
                             | all isUpperOrPunct s   -> Str $ T.pack s
                             | isMixedCase s   -> Str $ T.pack s
                             | otherwise       -> Str $ T.pack (toUpper x:xs)
                        WordBoundary ->
                          case (x:xs) of
                           s | not (isAscii x) -> Str $ T.pack s
                             | all isUpperOrPunct s   -> Str $ T.pack s
                             | isShortWord s   -> Str $ T.pack (map toLower s)
                             | isMixedCase s   -> Str $ T.pack s
                             | otherwise       -> Str $ T.pack (toUpper x:xs)
                        SentenceBoundary ->
                           if isMixedCase (x:xs) || all isUpperOrPunct (x:xs)
                              then Str $ T.pack (x:xs)
                              else Str $ T.pack (toUpper x : xs)
                        _ -> Str $ T.pack (x:xs)
        tc (Span ("",["nocase"],[]) xs) = return $ Span ("",["nocase"],[]) xs
        tc x = return x
        isShortWord  s = map toLower s `Set.member` shortWords

shortWords :: Set.Set String
shortWords = Set.fromList
                 ["a","an","and","as","at","but","by","c","ca","d","de"
                 ,"down","et","for","from"
                 ,"in","into","nor","of","on","onto","or","over","so"
                 ,"the","till","to","up","van","von","via","with","yet"]

isMixedCase :: String -> Bool
isMixedCase xs = any isUpper xs && any isLower xs

isUpperOrPunct :: Char -> Bool
isUpperOrPunct c = isUpper c || isPunctuation c

data CaseTransformState = WordBoundary
                        | LastWordBoundary
                        | SentenceBoundary
                        | NoBoundary

caseTransform :: (Inline -> State CaseTransformState Inline) -> [Inline]
              -> State CaseTransformState [Inline]
caseTransform xform = fmap reverse . foldM go [] . splitUpStr
  where go acc s | s == Space || s == SoftBreak = do
               modify (\st ->
                 case st of
                      SentenceBoundary -> SentenceBoundary
                      _                -> WordBoundary)
               return $ Space : acc
        go acc LineBreak = do
               put WordBoundary
               return $ Space : acc
        go acc (Str (T.unpack -> [c]))
          | c `elem` (".?!:" :: String) = do
               put SentenceBoundary
               return $ Str (T.singleton c) : acc
          | c `elem` ("-/\x2013\x2014\160" :: String) = do
               put WordBoundary
               return $ Str (T.singleton c) : acc
          | isPunctuation c = return $ Str (T.singleton c) : acc -- leave state unchanged
        go acc (Str "") = return acc
        go acc (Str xs) = do
               res <- xform (Str xs)
               put NoBoundary
               return $ res : acc
        go acc (Span ("",["lastword"],[]) [x]) = do
               b <- get
               case b of
                    WordBoundary -> put LastWordBoundary
                    _            -> return ()
               go acc x
        go acc (Span ("",classes,[]) xs)
          | null classes || classes == ["nocase"] = do
               res <- xform (Span ("",classes,[]) xs)
               put NoBoundary
               return $ res : acc
        go acc (Quoted qt xs)    = (:acc) <$> (Quoted qt <$> caseTransform xform xs)
        go acc (Emph xs)         = (:acc) <$> (Emph <$> caseTransform xform xs)
        go acc (Strong xs)       = (:acc) <$> (Strong <$> caseTransform xform xs)
        go acc (Link at xs t)    = (:acc) <$> (Link at <$> caseTransform xform xs <*> pure t)
        go acc (Image at xs t)   = (:acc) <$> (Link at <$> caseTransform xform xs <*> pure t)
        go acc (Span attr xs)    = (:acc) <$> (Span attr <$> caseTransform xform xs)
        go acc x                 = return $ x : acc

splitWhen :: (Char -> Bool) -> Text -> [Text]
splitWhen f = filter (not . T.null) . T.split f

splitStrWhen :: (Char -> Bool) -> [Inline] -> [Inline]
splitStrWhen _ [] = []
splitStrWhen p (Str xs : ys) = go (T.unpack xs) ++ splitStrWhen p ys
  where go [] = []
        go s = case break p s of
                     ([],[])     -> []
                     (zs,[])     -> [Str $ T.pack zs]
                     ([],w:ws) -> Str (T.singleton w) : go ws
                     (zs,w:ws) -> Str (T.pack zs) : Str (T.singleton w) : go ws
splitStrWhen p (x : ys) = x : splitStrWhen p ys

-- | A generic processing function.
proc :: (Typeable a, Data b) => (a -> a) -> b -> b
proc f = everywhere (mkT f)

-- | A generic processing function: process a data structure in
-- top-down manner.
proc' :: (Typeable a, Data b) => (a -> a) -> b -> b
proc' f = everywhere' (mkT f)

-- | A generic monadic processing function.
procM :: (Monad m, Typeable a, Data b) => (a -> m a) -> b -> m b
procM f = everywhereM (mkM f)

-- | A generic query function.
query :: (Typeable a, Data b, Monoid m) => (a -> m) -> b -> m
query f = everything mappend (mempty `mkQ` f)

orIfNull :: [a] -> [a] -> [a]
orIfNull [] b = b
orIfNull a  _ = a

toRead :: Text -> Text
toRead t = case T.uncons t of
  Nothing     -> ""
  Just (s,ss) -> T.cons (toUpper s) . T.pack . camel . T.unpack $ ss
    where
      camel x
          | '-':y:ys <- x = toUpper y : camel ys
          | '_':y:ys <- x = toUpper y : camel ys
          |     y:ys <- x =         y : camel ys
          | otherwise     = []

inlinesToString :: [Inline] -> Text
inlinesToString = stringify

headInline :: [Inline] -> Maybe Char
headInline = fmap fst . T.uncons . stringify

lastInline :: [Inline] -> Maybe Char
lastInline = fmap snd . T.unsnoc . stringify

initInline :: [Inline] -> [Inline]
initInline [] = []
initInline [i]
    | Str          s <- i
    , not (T.null s)      = return $ Str         (T.init      s)
    | Emph        is <- i = return $ Emph        (initInline is)
    | Strong      is <- i = return $ Strong      (initInline is)
    | Superscript is <- i = return $ Superscript (initInline is)
    | Subscript   is <- i = return $ Subscript   (initInline is)
    | Quoted q    is <- i = return $ Quoted q    (initInline is)
    | SmallCaps   is <- i = return $ SmallCaps   (initInline is)
    | Strikeout   is <- i = return $ Strikeout   (initInline is)
    | Link   at is t <- i = return $ Link at     (initInline is) t
    | Span at     is <- i = return $ Span at     (initInline is)
    | otherwise           = []
initInline (i:xs) = i : initInline xs

tailInline :: [Inline] -> [Inline]
tailInline (Space:xs)     = xs
tailInline (SoftBreak:xs) = xs
tailInline xs             = tailFirstInlineStr xs

tailFirstInlineStr :: [Inline] -> [Inline]
tailFirstInlineStr = mapHeadInline (T.drop 1)

toCapital :: [Inline] -> [Inline]
toCapital ils@(Span (_,["nocase"],_) _:_) = ils
toCapital ils                             = mapHeadInline capitalize ils

mapHeadInline :: (Text -> Text) -> [Inline] -> [Inline]
mapHeadInline _ [] = []
mapHeadInline f (i:xs)
    | Str         "" <- i =                      mapHeadInline f xs
    | Str          s <- i = case f s of
                              "" -> xs
                              t  -> Str t : xs
    | Emph        is <- i = Emph        (mapHeadInline f is)      : xs
    | Strong      is <- i = Strong      (mapHeadInline f is)      : xs
    | Superscript is <- i = Superscript (mapHeadInline f is)      : xs
    | Subscript   is <- i = Subscript   (mapHeadInline f is)      : xs
    | Quoted q    is <- i = Quoted q    (mapHeadInline f is)      : xs
    | SmallCaps   is <- i = SmallCaps   (mapHeadInline f is)      : xs
    | Strikeout   is <- i = Strikeout   (mapHeadInline f is)      : xs
    | Link   at is t <- i = Link at     (mapHeadInline f is) t    : xs
    | Span     at is <- i = Span at     (mapHeadInline f is)      : xs
    | otherwise           = i : xs

findFile :: [FilePath] -> FilePath -> IO (Maybe FilePath)
findFile [] _ = return Nothing
findFile (p:ps) f
 | isAbsolute f = do
     exists <- doesFileExist f
     if exists
        then return (Just f)
        else return Nothing
 | otherwise = do
     exists <- doesFileExist (p </> f)
     if exists
        then return $ Just (p </> f)
        else findFile ps f

class AddYaml a where
  (&=) :: Text -> a -> [(Text, YamlBuilder)] -> [(Text, YamlBuilder)]

instance ToYaml a => AddYaml [a] where
  x &= y = \acc -> if null y
                      then acc
                      else (x Y..= y) : acc

instance ToYaml a => AddYaml (Maybe a) where
  x &= y = \acc -> case y of
                        Nothing -> acc
                        Just z  -> (x Y..= z) : acc

instance AddYaml Text where
  x &= y = \acc -> if T.null y
                      then acc
                      else (x Y..= y) : acc

instance AddYaml Bool where
  _ &= False = id
  x &= True = \acc -> (x Y..= Y.bool True) : acc

mapping' :: [[(Text, YamlBuilder)] -> [(Text, YamlBuilder)]] -> YamlBuilder
mapping' = Y.mapping . foldr ($) []

-- TODO: romanNumeral is defined in Text.Pandoc.Parsing, but it's
-- not exported there. Eventually we should remove this code duplication
-- by exporting something from pandoc.

parseRomanNumeral :: String -> Maybe Int
parseRomanNumeral s = case P.parse (pRomanNumeral <* P.eof) "" s of
                           Left _  -> Nothing
                           Right x -> Just x

-- | Parses a roman numeral (uppercase or lowercase), returns number.
pRomanNumeral :: P.Stream s m Char => P.ParsecT s st m Int
pRomanNumeral = do
    let lowercaseRomanDigits = ['i','v','x','l','c','d','m']
    let uppercaseRomanDigits = ['I','V','X','L','C','D','M']
    c <- P.lookAhead $ P.oneOf (lowercaseRomanDigits ++ uppercaseRomanDigits)
    let romanDigits = if isUpper c
                         then uppercaseRomanDigits
                         else lowercaseRomanDigits
    let [one, five, ten, fifty, hundred, fivehundred, thousand] =
          map P.char romanDigits
    thousands <- ((1000 *) . length) <$> P.many thousand
    ninehundreds <- P.option 0 $ P.try $ hundred >> thousand >> return 900
    fivehundreds <- ((500 *) . length) <$> P.many fivehundred
    fourhundreds <- P.option 0 $ P.try $ hundred >> fivehundred >> return 400
    hundreds <- ((100 *) . length) <$> P.many hundred
    nineties <- P.option 0 $ P.try $ ten >> hundred >> return 90
    fifties <- ((50 *) . length) <$> P.many fifty
    forties <- P.option 0 $ P.try $ ten >> fifty >> return 40
    tens <- ((10 *) . length) <$> P.many ten
    nines <- P.option 0 $ P.try $ one >> ten >> return 9
    fives <- ((5 *) . length) <$> P.many five
    fours <- P.option 0 $ P.try $ one >> five >> return 4
    ones <- length <$> P.many one
    let total = thousands + ninehundreds + fivehundreds + fourhundreds +
                hundreds + nineties + fifties + forties + tens + nines +
                fives + fours + ones
    if total == 0
       then Prelude.fail "not a roman numeral"
       else return total

isRange :: Text -> Bool
isRange = T.any (`elem` [',', '-', '\x2013'])

-- see issue 392 for motivation.  We want to treat
-- "J.G. Smith" and "J. G. Smith" the same.
addSpaceAfterPeriod :: [Inline] -> [Inline]
addSpaceAfterPeriod = go . splitStrWhen (=='.')
  where
    go [] = []
    go (Str (T.unpack -> [c]):Str ".":Str (T.unpack -> [d]):xs)
      | isLetter d
      , isLetter c
      , isUpper c
      , isUpper d   = Str (T.singleton c):Str ".":Space:go (Str (T.singleton d):xs)
    go (x:xs) = x:go xs
