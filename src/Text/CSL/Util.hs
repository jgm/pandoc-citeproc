{-# LANGUAGE ScopedTypeVariables #-}
module Text.CSL.Util
  ( safeRead
  , readNum
  , (<+>)
  , (<^>)
  , capitalize
  , head'
  , tail'
  , parseBool
  , parseString
  , mb
  , (.#?)
  , (.#:)
  ) where
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (toLower, toUpper)
import Data.Traversable (mapM)
import Text.Pandoc.Shared (safeRead)

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
  | s `elem` ";:,. " && last sa == s = sa ++ xs
sa <^> sb         = sa ++ sb

capitalize :: String -> String
capitalize [] = []
capitalize (c:cs) = toUpper c : cs

head' :: [a] -> [a]
head' = take 1

tail' :: Eq a => [a] -> [a]
tail' = drop 1

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

