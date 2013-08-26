module Main where
import Text.CSL.Input.Bibutils (readBiblioFile)
import Text.CSL.Pandoc ()
import Data.Char (chr)
import Data.Monoid
import Data.Yaml
import Control.Applicative
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Attoparsec.ByteString.Char8 as Attoparsec
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import System.Console.ParseArgs
import Control.Monad

main :: IO ()
main = do
  parsedArgs <- parseArgsIO ArgsComplete arguments
  when (gotArg parsedArgs Help) $
     usageError parsedArgs ""
  let mbbibfile = getArg parsedArgs BibFile
  refs <- maybe (usageError parsedArgs "Missing bibfile argument")
            readBiblioFile mbbibfile
  B.putStr $ unescapeTags $ encode refs

data Argument =
    Help | Version | BibFile
  deriving (Ord, Eq, Show)

arguments :: [Arg Argument]
arguments = [ Arg Version (Just 'v') (Just "version")
                 Nothing "print version"
            , Arg Help (Just 'h') (Just "help")
                 Nothing "print usage information"
            , Arg BibFile Nothing Nothing
                (argDataRequired "bibfile" ArgtypeString)
                "bibfile"
            ]

-- turn
-- id: ! "\u043F\u0443\u043D\u043A\u04423"
-- into
-- id: пункт3
unescapeTags :: B.ByteString -> B.ByteString
unescapeTags bs = case parseOnly (many $ tag <|> other) bs of
                       Left e  -> error e
                       Right r -> B.concat r

tag :: Attoparsec.Parser B.ByteString
tag = do
  _ <- string $ B8.pack ": ! \""
  cs <- manyTill (uchar <|> regchar) (char '"')
  return $ B8.pack ": \"" <> B.concat cs <> B8.pack "\""

other :: Attoparsec.Parser B.ByteString
other = Attoparsec.takeWhile1 (/=':') <|> Attoparsec.take 1

uchar :: Attoparsec.Parser B.ByteString
uchar = do
  _ <- string $ B8.pack "\\u"
  cs <- count 4 $ satisfy $ inClass "0-9a-fA-F"
  let n = read ('0':'x':cs)
  return $ encodeUtf8 $ T.pack [chr n]

regchar :: Attoparsec.Parser B.ByteString
regchar = (B8.pack . (:[])) <$> anyChar
