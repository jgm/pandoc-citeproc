{-# LANGUAGE ScopedTypeVariables #-}
module Text.CSL.Util
  ( safeRead
  , readNum
  , (<+>)
  , (<^>)
  , capitalize
  , head'
  , tail'
  , last'
  , words'
  , trim
  , parseBool
  , parseString
  , mb
  , (.#?)
  , (.#:)
  , onBlocks
  , titlecase
  , unTitlecase
  , protectCase
  , splitStrWhen
  ) where
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Applicative ((<$>), (<*>), pure)
import Data.Char (toLower, toUpper, isLower, isUpper, isPunctuation)
import qualified Data.Traversable
import Text.Pandoc.Shared (safeRead)
import Text.Pandoc.Walk (walk)
import Text.Pandoc
import Data.List.Split (wordsBy, whenElt, dropBlanks, split)
import Control.Monad.State

readNum :: String -> Int
readNum s = case reads s of
              [(x,"")] -> x
              _        -> 0

(<+>) :: String -> String -> String
[] <+> ss = ss
s  <+> [] = s
s  <+> ss =
  case reverse s of
       ('\'':rs) -> reverse rs ++ ('’':ss)
       _         -> s ++ (' ':ss)

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

head' :: [a] -> [a]
head' = take 1

tail' :: Eq a => [a] -> [a]
tail' = drop 1

last' :: [a] -> [a]
last' = foldl (\_ x -> [x]) []

-- | Like words, but doesn't break on nonbreaking spaces etc.
words' :: String -> [String]
words' = wordsBy (\c -> c == ' ' || c == '\t' || c == '\r' || c == '\n')

-- | Remove leading and trailing space (including newlines) from string.
trim :: String -> String
trim = triml . trimr
  where triml = dropWhile (`elem` " \r\n\t")
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
parseString _          = fail "Could not read string"

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

unTitlecase :: [Inline] -> [Inline]
unTitlecase = caseTransform untc
  where untc (Str (x:xs))
          | isUpper x = Str (toLower x : xs)
        untc (Span ("",[],[]) xs)
          | hasLowercaseWord xs = Span ("",["nocase"],[]) xs
        untc x = x

protectCase :: [Inline] -> [Inline]
protectCase = caseTransform protect
  where protect (Span ("",[],[]) xs)
          | hasLowercaseWord xs = Span ("",["nocase"],[]) xs
        protect x = x

titlecase :: [Inline] -> [Inline]
titlecase = caseTransform tc
  where tc (Str (x:xs))
          | isLower x && not (isShortWord (x:xs)) = Str (toUpper x : xs)
          where isShortWord  s = s `elem` ["a","an","and","as","at","but","by","down","for","from"
                                          ,"in","into","nor","of","on","onto","or","over","so"
                                          ,"the","till","to","up","via","with","yet"]
        tc (Span ("",[],[]) xs)
          | hasLowercaseWord xs = Span ("",["nocase"],[]) xs
        tc x = x

caseTransform :: (Inline -> Inline) -> [Inline] -> [Inline]
caseTransform xform xs = evalState (caseTransform' xform $ splitStrWhen isPunctuation xs) False

caseTransform' :: (Inline -> Inline) -> [Inline] -> State Bool [Inline]
caseTransform' xform = mapM go
  where go Space            = put True >> return Space
        go LineBreak        = put True >> return Space
        go (Str [x])
          | isPunctuation x = put True >> return (Str [x])
        go (Str (x:xs))
          | isUpper x = do
               atWordBoundary <- get
               if atWordBoundary
                  then do
                    put False
                    return $ xform $ Str (x:xs)
                  else return $ Str (x:xs)
        go (Span ("",classes,[]) xs) | null classes || classes == ["nocase"] = do
               atWordBoundary <- get
               if atWordBoundary
                  then do
                    put False
                    return $ xform (Span ("",[],[]) xs)
                  else return (Span ("",[],[]) xs)
        go (Quoted qt xs)   = Quoted qt <$> caseTransform' xform xs
        go (Emph xs)        = Emph <$> caseTransform' xform xs
        go (Strong xs)      = Strong <$> caseTransform' xform xs
        go (Link xs t)      = Link <$> caseTransform' xform xs <*> pure t
        go (Image xs t)     = Link <$> caseTransform' xform xs <*> pure t
        go (Span attr xs)   = Span attr <$> caseTransform' xform xs
        go x = return x

splitStrWhen :: (Char -> Bool) -> [Inline] -> [Inline]
splitStrWhen _ [] = []
splitStrWhen p (Str xs : ys)
  | any p xs = map Str ((split . dropBlanks) (whenElt p) xs) ++ splitStrWhen p ys
splitStrWhen p (x : ys) = x : splitStrWhen p ys

