{-# LANGUAGE ScopedTypeVariables, PatternGuards #-}
module Text.CSL.Util
  ( safeRead
  , readNum
  , (<^>)
  , capitalize
  , camelize
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
  , readable
  , toShow
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
  ) where
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Applicative ((<$>), (<*>), pure)
import Data.Char (toLower, toUpper, isLower, isUpper, isPunctuation)
import qualified Data.Traversable
import Text.Pandoc.Shared (safeRead, stringify)
import Text.Pandoc.Walk (walk)
import Text.Pandoc
import Data.List.Split (wordsBy, whenElt, dropBlanks, split )
import Control.Monad.State
import Data.Monoid (Monoid, mappend, mempty)
import Data.Generics ( Typeable, Data, everywhere, everywhereM, mkM,
                       everywhere', everything, mkT, mkQ )
import qualified Debug.Trace

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
parseString _          = fail "Could not read string"

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
unTitlecase zs = evalState (caseTransform untc $ splitUpStr zs) False
  where untc (Str (x:xs))
          | isUpper x = Str (toLower x : xs)
        untc (Span ("",[],[]) xs)
          | hasLowercaseWord xs = Span ("",["nocase"],[]) xs
        untc x = x

protectCase :: [Inline] -> [Inline]
protectCase zs = evalState (caseTransform protect $ splitUpStr zs) False
  where protect (Span ("",[],[]) xs)
          | hasLowercaseWord xs = Span ("",["nocase"],[]) xs
        protect x = x

titlecase :: [Inline] -> [Inline]
titlecase zs = evalState (caseTransform tc $ splitUpStr zs) True
  where tc (Str (x:xs))
          | isLower x && not (isShortWord (x:xs)) = Str (toUpper x : xs)
          where isShortWord  s = s `elem`
                      ["a","an","and","as","at","but","by","d","de"
                      ,"down","for","from"
                      ,"in","into","nor","of","on","onto","or","over","so"
                      ,"the","till","to","up","van","von","via","with","yet"]
        tc (Span ("",["nocase"],[]) xs) = Span ("",["nocase"],[]) xs
        tc x = x

caseTransform :: (Inline -> Inline) -> [Inline] -> State Bool [Inline]
caseTransform xform = mapM go
  where go Space            = put True >> return Space
        go LineBreak        = put True >> return Space
        go (Str [])         = return $ Str []
        go (Str "’")        = return $ Str "’"
        go (Str [x])
          | isPunctuation x || x == '\160' = put True >> return (Str [x])
        go (Str (x:xs)) = do
               atWordBoundary <- get
               if atWordBoundary
                  then do
                    put False
                    return $ xform $ Str (x:xs)
                  else return $ Str (x:xs)
        go (Span ("",classes,[]) xs) | null classes || classes == ["nocase"] =
            do atWordBoundary <- get
               if atWordBoundary
                  then do
                    put False
                    return $ xform (Span ("",classes,[]) xs)
                  else return (Span ("",classes,[]) xs)
        go (Quoted qt xs)   = Quoted qt <$> caseTransform xform xs
        go (Emph xs)        = Emph <$> caseTransform xform xs
        go (Strong xs)      = Strong <$> caseTransform xform xs
        go (Link xs t)      = Link <$> caseTransform xform xs <*> pure t
        go (Image xs t)     = Link <$> caseTransform xform xs <*> pure t
        go (Span attr xs)   = Span attr <$> caseTransform xform xs
        go x = return x

splitStrWhen :: (Char -> Bool) -> [Inline] -> [Inline]
splitStrWhen _ [] = []
splitStrWhen p (Str xs : ys)
  | any p xs = map Str ((split . dropBlanks) (whenElt p) xs) ++ splitStrWhen p ys
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

readable :: (Read a, Show b) => (String -> a, b -> String)
readable =  (read . toRead, toShow . show)

toShow :: String -> String
toShow = foldr g [] . f
    where g    x xs  = if isUpper x then '-' : toLower x : xs else x : xs
          f (  x:xs) = toLower x : xs
          f       [] = []

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
toCapital = mapHeadInline capitalize

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

tr' :: Show a => String -> a -> a
tr' note' x = Debug.Trace.trace (note' ++ ": " ++ show x) x
