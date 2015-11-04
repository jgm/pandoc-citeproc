{-# LANGUAGE ScopedTypeVariables, PatternGuards, FlexibleContexts #-}
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
  , mb
  , (.#?)
  , (.#:)
  , onBlocks
  , titlecase
  , unTitlecase
  , protectCase
  , splitStrWhen
  , proc
  , proc'
  , procM
  , query
  , betterThan
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
  , (&=)
  , mapping'
  , parseRomanNumeral
  ) where
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (toLower, toUpper, isLower, isUpper, isPunctuation,
                  isLetter, isAscii)
import qualified Data.Traversable
import Text.Pandoc.Shared (safeRead, stringify)
import Text.Pandoc.Walk (walk)
import Text.Pandoc
import Data.List.Split (wordsBy)
import Control.Monad.State
import Data.Generics ( Typeable, Data, everywhere, everywhereM, mkM,
                       everywhere', everything, mkT, mkQ )
import System.FilePath
import System.Directory (doesFileExist)
import qualified Data.Yaml.Builder as Y
import Data.Yaml.Builder (ToYaml(..), YamlBuilder)
import qualified Text.Parsec as P

#ifdef TRACE
import qualified Debug.Trace
import Text.Show.Pretty (ppShow)
#endif

tr' :: Show a => String -> a -> a
#ifdef TRACE
tr' note' x = Debug.Trace.trace ("=== " ++ note' ++ "\n" ++ ppShow x ++ "\n") x
#else
tr' _ x = x
#endif

readNum :: String -> Int
readNum s = case reads s of
              [(x,"")] -> x
              _        -> 0

-- | Conjoin strings, avoiding repeated punctuation.
(<^>) :: String -> String -> String
[] <^> sb         = sb
sa <^> []         = sa
sa <^> (s:xs)
  | s `elem` puncts && last sa `elem` puncts = sa ++ xs
  where puncts = ";:,. "
sa <^> sb         = sa ++ sb

capitalize :: String -> String
capitalize [] = []
capitalize (c:cs) = toUpper c : cs

isPunct :: Char -> Bool
isPunct c = c `elem` ".;?!"

camelize :: String -> String
camelize ('-':y:ys) = toUpper y : camelize ys
camelize ('_':y:ys) = toUpper y : camelize ys
camelize     (y:ys) =         y : camelize ys
camelize      _     = []

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
trim :: String -> String
trim = triml . trimr

triml :: String -> String
triml = dropWhile (`elem` " \r\n\t")

trimr :: String -> String
trimr = reverse . triml . reverse

-- | Parse JSON Boolean or Number as Bool.
parseBool :: Value -> Parser Bool
parseBool (Bool b)   = return b
parseBool (Number n) = case fromJSON (Number n) of
                            Success (0 :: Int) -> return False
                            Success _          -> return True
                            Error e            -> fail $ "Could not read boolean: " ++ e
parseBool _          = fail "Could not read boolean"

-- | Parse JSON value as String.
parseString :: Value -> Parser String
parseString (String s) = return $ T.unpack s
parseString (Number n) = case fromJSON (Number n) of
                            Success (x :: Int) -> return $ show x
                            Error _ -> case fromJSON (Number n) of
                                            Success (x :: Double) -> return $ show x
                                            Error e -> fail $ "Could not read string: " ++ e
parseString (Bool b)   = return $ map toLower $ show b
parseString v@(Array _)= inlinesToString `fmap` parseJSON v
parseString v          = fail $ "Could not read as string: " ++ show v

-- | Parse JSON value as Int.
parseInt :: Value -> Parser Int
parseInt (String s) = case safeRead (T.unpack s) of
                            Just n  -> return n
                            Nothing -> fail "Could not read Int"
parseInt (Number n) = case fromJSON (Number n) of
                            Success (x :: Int) -> return x
                            Error e -> fail $ "Could not read string: " ++ e
parseInt _          = fail "Could not read string"

mb :: Monad m => (b -> m a) -> (Maybe b -> m (Maybe a))
mb  = Data.Traversable.mapM

-- | Parse as a string (even if the value is a number).
(.#?) :: Object -> Text -> Parser (Maybe String)
x .#? y = (x .:? y) >>= mb parseString

(.#:) :: Object -> Text -> Parser String
x .#: y = (x .: y) >>= parseString

onBlocks :: ([Inline] -> [Inline]) -> [Block] -> [Block]
onBlocks f bs = walk f' bs
  where f' (Para ils)  = Para (f ils)
        f' (Plain ils) = Plain (f ils)
        f' x           = x

hasLowercaseWord :: [Inline] -> Bool
hasLowercaseWord = any startsWithLowercase . splitStrWhen isPunctuation
  where startsWithLowercase (Str (x:_)) = isLower x
        startsWithLowercase _           = False

splitUpStr :: [Inline] -> [Inline]
splitUpStr = splitStrWhen (\c -> isPunctuation c || c == '\160')

unTitlecase :: [Inline] -> [Inline]
unTitlecase zs = evalState (caseTransform untc zs) SentenceBoundary
  where untc w = do
          st <- get
          case (w, st) of
               (y, NoBoundary) -> return y
               (Str (x:xs), WordBoundary) | isUpper x ->
                 return $ Str (toLower x : xs)
               (Str (x:xs), SentenceBoundary) | isLower x ->
                 return $ Str (toUpper x : xs)
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

titlecase :: [Inline] -> [Inline]
titlecase zs = evalState (caseTransform tc zs) SentenceBoundary
  where tc (Str (x:xs)) = do
          st <- get
          return $ case st of
                        WordBoundary -> if isShortWord (x:xs) ||
                                             not (isAscii x && isLetter x)
                                           then Str (x:xs)
                                           else Str (toUpper x : xs)
                        SentenceBoundary -> Str (toUpper x : xs)
                        _ -> Str (x:xs)
        tc (Span ("",["nocase"],[]) xs) = return $ Span ("",["nocase"],[]) xs
        tc x = return x
        isShortWord  s = map toLower s `elem`
                      ["a","an","and","as","at","but","by","c","ca","d","de"
                      ,"down","et","for","from"
                      ,"in","into","nor","of","on","onto","or","over","so"
                      ,"the","till","to","up","van","von","via","with","yet"]

data CaseTransformState = WordBoundary | SentenceBoundary | NoBoundary

caseTransform :: (Inline -> State CaseTransformState Inline) -> [Inline]
              -> State CaseTransformState [Inline]
caseTransform xform = fmap reverse . foldM go [] . splitUpStr
  where go acc Space        = do
               modify (\st ->
                 case st of
                      SentenceBoundary -> SentenceBoundary
                      _                ->
                          case acc of
                                (Str [x]:_)
                                  | x `elem` "?!:"   -> SentenceBoundary
                                _                    -> WordBoundary)
               return $ Space : acc
        go acc LineBreak = do
               put WordBoundary
               return $ Space : acc
        go acc (Str [c])
          | c `elem` "-\2013\2014\160" = do
               put WordBoundary
               return $ Str [c] : acc
          | isPunctuation c = do
               -- leave state unchanged
               return $ Str [c] : acc
        go acc (Str []) = return acc
        go acc (Str xs) = do
               res <- xform (Str xs)
               put NoBoundary
               return $ res : acc
        go acc (Span ("",classes,[]) xs)
          | null classes || classes == ["nocase"] = do
               res <- xform (Span ("",classes,[]) xs)
               put NoBoundary
               return $ res : acc
        go acc (Quoted qt xs)    = (:acc) <$> (Quoted qt <$> caseTransform xform xs)
        go acc (Emph xs)         = (:acc) <$> (Emph <$> caseTransform xform xs)
        go acc (Strong xs)       = (:acc) <$> (Strong <$> caseTransform xform xs)
        go acc (Link xs t)       = (:acc) <$> (Link <$> caseTransform xform xs <*> pure t)
        go acc (Image xs t)      = (:acc) <$> (Link <$> caseTransform xform xs <*> pure t)
        go acc (Span attr xs)    = (:acc) <$> (Span attr <$> caseTransform xform xs)
        go acc x                 = return $ x : acc

splitStrWhen :: (Char -> Bool) -> [Inline] -> [Inline]
splitStrWhen _ [] = []
splitStrWhen p (Str xs : ys) = go xs ++ splitStrWhen p ys
  where go [] = []
        go s = case break p s of
                     ([],[])     -> []
                     (zs,[])     -> [Str zs]
                     ([],(w:ws)) -> Str [w] : go ws
                     (zs,(w:ws)) -> Str zs : Str [w] : go ws
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

betterThan :: [a] -> [a] -> [a]
betterThan [] b = b
betterThan a  _ = a

toRead :: String -> String
toRead    []  = []
toRead (s:ss) = toUpper s : camel ss
    where
      camel x
          | '-':y:ys <- x = toUpper y : camel ys
          | '_':y:ys <- x = toUpper y : camel ys
          |     y:ys <- x =         y : camel ys
          | otherwise     = []

inlinesToString :: [Inline] -> String
inlinesToString = stringify

headInline :: [Inline] -> String
headInline = take 1 . stringify

lastInline :: [Inline] -> String
lastInline xs = case stringify xs of
                      [] -> []
                      ys -> [last ys]

initInline :: [Inline] -> [Inline]
initInline [] = []
initInline (i:[])
    | Str          s <- i = return $ Str         (init'       s)
    | Emph        is <- i = return $ Emph        (initInline is)
    | Strong      is <- i = return $ Strong      (initInline is)
    | Superscript is <- i = return $ Superscript (initInline is)
    | Subscript   is <- i = return $ Subscript   (initInline is)
    | Quoted q    is <- i = return $ Quoted q    (initInline is)
    | SmallCaps   is <- i = return $ SmallCaps   (initInline is)
    | Strikeout   is <- i = return $ Strikeout   (initInline is)
    | Link      is t <- i = return $ Link        (initInline is) t
    | Span at     is <- i = return $ Span at     (initInline is)
    | otherwise           = []
initInline (i:xs) = i : initInline xs

tailInline :: [Inline] -> [Inline]
tailInline (Space:xs) = xs
tailInline xs         = removeEmpty $ tailFirstInlineStr xs
  where removeEmpty   = dropWhile (== Str "")

tailFirstInlineStr :: [Inline] -> [Inline]
tailFirstInlineStr = mapHeadInline (drop 1)

toCapital :: [Inline] -> [Inline]
toCapital ils@(Span (_,["nocase"],_) _:_) = ils
toCapital ils = mapHeadInline capitalize ils

mapHeadInline :: (String -> String) -> [Inline] -> [Inline]
mapHeadInline _ [] = []
mapHeadInline f (i:xs)
    | Str         [] <- i =                      mapHeadInline f xs
    | Str          s <- i = Str         (f                s)   : xs
    | Emph        is <- i = Emph        (mapHeadInline f is)   : xs
    | Strong      is <- i = Strong      (mapHeadInline f is)   : xs
    | Superscript is <- i = Superscript (mapHeadInline f is)   : xs
    | Subscript   is <- i = Subscript   (mapHeadInline f is)   : xs
    | Quoted q    is <- i = Quoted q    (mapHeadInline f is)   : xs
    | SmallCaps   is <- i = SmallCaps   (mapHeadInline f is)   : xs
    | Strikeout   is <- i = Strikeout   (mapHeadInline f is)   : xs
    | Link      is t <- i = Link        (mapHeadInline f is) t : xs
    | Span     at is <- i = Span at     (mapHeadInline f is)   : xs
    | otherwise           = i : xs

findFile :: [FilePath] -> FilePath -> IO (Maybe FilePath)
findFile [] _ = return Nothing
findFile (p:ps) f = do
  exists <- doesFileExist (p </> f)
  if exists
     then return $ Just (p </> f)
     else findFile ps f

(&=) :: (ToYaml a, Monoid a, Eq a)
     => Text -> a -> [(Text, YamlBuilder)] -> [(Text, YamlBuilder)]
x &= y = \acc -> if y == mempty
                    then acc
                    else (x Y..= y) : acc

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
    thousands <- P.many thousand >>= (return . (1000 *) . length)
    ninehundreds <- P.option 0 $ P.try $ hundred >> thousand >> return 900
    fivehundreds <- P.many fivehundred >>= (return . (500 *) . length)
    fourhundreds <- P.option 0 $ P.try $ hundred >> fivehundred >> return 400
    hundreds <- P.many hundred >>= (return . (100 *) . length)
    nineties <- P.option 0 $ P.try $ ten >> hundred >> return 90
    fifties <- P.many fifty >>= (return . (50 *) . length)
    forties <- P.option 0 $ P.try $ ten >> fifty >> return 40
    tens <- P.many ten >>= (return . (10 *) . length)
    nines <- P.option 0 $ P.try $ one >> ten >> return 9
    fives <- P.many five >>= (return . (5 *) . length)
    fours <- P.option 0 $ P.try $ one >> five >> return 4
    ones <- P.many one >>= (return . length)
    let total = thousands + ninehundreds + fivehundreds + fourhundreds +
                hundreds + nineties + fifties + forties + tens + nines +
                fives + fours + ones
    if total == 0
       then fail "not a roman numeral"
       else return total
