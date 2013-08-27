module Main where
import Text.CSL.Input.Bibutils (readBiblioString, BibFormat(..))
import Text.CSL.Pandoc ()
import Data.Char (chr, toLower)
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
import System.IO
import System.FilePath (takeExtension)

main :: IO ()
main = do
  parsedArgs <- parseArgsIO ArgsComplete arguments
  when (gotArg parsedArgs Help) $
     usageError parsedArgs ""
  bibstring <- hGetContents =<< getArgStdio parsedArgs BibFile ReadMode
  let bibformat = (getArg parsedArgs Format >>= readFormat)
              <|> (getArg parsedArgs BibFile >>= formatFromExtension)
  case bibformat of
       Nothing  -> usageError parsedArgs "Unknown format"
       Just f   -> readBiblioString f bibstring >>=
                     outputYamlBlock . unescapeTags . encode

outputYamlBlock :: B.ByteString -> IO ()
outputYamlBlock contents = do
  putStrLn "---\nreferences:"
  B.putStr contents
  putStrLn "..."

formatFromExtension :: FilePath -> Maybe BibFormat
formatFromExtension = readFormat . dropWhile (=='.') . takeExtension

data Argument =
    Help | BibFile | Format
  deriving (Ord, Eq, Show)

readFormat :: String -> Maybe BibFormat
readFormat = go . map toLower
  where go "mods"     = Just Mods
        go "biblatex" = Just BibLatex
        go "bib"      = Just BibLatex
        go "bibtex"   = Just Bibtex
        go "ris"      = Just Ris
        go "endnote"  = Just Endnote
        go "enl"      = Just Endnote
        go "endnotexml" = Just EndnotXml
        go "xml"      = Just EndnotXml
        go "wos"      = Just Isi
        go "isi"      = Just Isi
        go "medline"  = Just Medline
        go "copac"    = Just Copac
        go "json"     = Just Json
        go _          = Nothing

arguments :: [Arg Argument]
arguments = [ Arg Help (Just 'h') (Just "help")
                 Nothing "print usage information"
            , Arg Format (Just 'f') (Just "format")
                (argDataOptional "format" ArgtypeString)
                "format"
            , Arg BibFile Nothing Nothing
                (argDataOptional "bibfile" ArgtypeString)
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
