module Main where
import Text.CSL.Input.Bibutils (readBiblioString, BibFormat(..))
import Text.CSL.Reference (Reference(refId), Literal(..))
import Data.List (group, sort)
import Data.Char (chr, toLower)
import Data.Yaml.Builder (toByteString)
import Control.Applicative ((<|>), many)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B8
import Data.Attoparsec.ByteString.Char8 as Attoparsec
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Aeson.Encode.Pretty (encodePretty', Config(..))
import System.Console.GetOpt
import Control.Monad
import System.IO
import System.FilePath (takeExtension)
import System.Environment (getArgs)
import System.Exit
import Data.Version (showVersion)
import Paths_pandoc_citeproc (version)
import Text.CSL.Pandoc (processCites')
import Text.Pandoc.JSON hiding (Format)
import Text.Pandoc.Walk
import qualified Text.Pandoc.UTF8 as UTF8

main :: IO ()
main = do
  argv <- getArgs
  let (flags, args, errs) = getOpt Permute options argv
  let header = "Usage: pandoc-citeproc [options] [file..]"
  unless (null errs) $ do
    UTF8.hPutStrLn stderr $ usageInfo (unlines $ errs ++ [header]) options
    exitWith $ ExitFailure 1
  when (Version `elem` flags) $ do
    UTF8.putStrLn $ "pandoc-citeproc " ++ showVersion version
    exitWith ExitSuccess
  when (Help `elem` flags) $ do
    UTF8.putStrLn $ usageInfo header options
    exitWith ExitSuccess
  if Bib2YAML `elem` flags || Bib2JSON `elem` flags
     then do
       let mbformat = case [f | Format f <- flags] of
                           [x] -> readFormat x
                           _   -> Nothing
       bibformat <- case mbformat <|>
                         msum (map formatFromExtension args) of
                         Just f   -> return f
                         Nothing  -> do
                            UTF8.hPutStrLn stderr $ usageInfo
                              ("Unknown format\n" ++ header) options
                            exitWith $ ExitFailure 3
       bibstring <- case args of
                         []    -> UTF8.getContents
                         xs    -> mconcat <$> mapM UTF8.readFile xs
       readBiblioString bibformat bibstring >>=
         warnDuplicateKeys >>=
         if Bib2YAML `elem` flags
            then outputYamlBlock .
                 B8.intercalate (B.singleton 10) .
                 map (unescapeTags . toByteString . (:[]))
            else B8.putStrLn . unescapeUnicode . B.concat . BL.toChunks .
              encodePretty' Config{ confIndent = 2
                                  , confCompare = compare }
     else toJSONFilter doCites

formatFromExtension :: FilePath -> Maybe BibFormat
formatFromExtension = readFormat . dropWhile (=='.') . takeExtension

readFormat :: String -> Maybe BibFormat
readFormat = go . map toLower
  where go "biblatex" = Just BibLatex
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
        go "mods"     = Just Mods
        go "yaml"     = Just Yaml
        go _          = Nothing


doCites :: Pandoc -> IO Pandoc
doCites doc = do
  doc' <- processCites' doc
  let warnings = query findWarnings doc'
  mapM_ (UTF8.hPutStrLn stderr) warnings
  return doc'

findWarnings :: Inline -> [String]
findWarnings (Span (_,["citeproc-not-found"],[("data-reference-id",ref)]) _) =
  ["pandoc-citeproc: reference " ++ ref ++ " not found" | ref /= "*"]
findWarnings (Span (_,["citeproc-no-output"],_) _) =
  ["pandoc-citeproc: reference with no printed form"]
findWarnings _ = []

data Option =
    Help | Version | Convert | Format String | Bib2YAML | Bib2JSON
  deriving (Ord, Eq, Show)

options :: [OptDescr Option]
options =
  [ Option ['h'] ["help"] (NoArg Help) "show usage information"
  , Option ['V'] ["version"] (NoArg Version) "show program version"
  , Option ['y'] ["bib2yaml"] (NoArg Bib2YAML) "convert bibliography to YAML"
  , Option ['j'] ["bib2json"] (NoArg Bib2JSON) "convert bibliography to JSON"
  , Option ['f'] ["format"] (ReqArg Format "FORMAT") "bibliography format"
  ]

warnDuplicateKeys :: [Reference] -> IO [Reference]
warnDuplicateKeys refs = mapM_ warnDup dupKeys >> return refs
  where warnDup k = UTF8.hPutStrLn stderr $ "biblio2yaml: duplicate key " ++ k
        allKeys   = map (unLiteral . refId) refs
        dupKeys   = [x | (x:_:_) <- group (sort allKeys)]

outputYamlBlock :: B.ByteString -> IO ()
outputYamlBlock contents = do
  UTF8.putStrLn "---\nreferences:"
  B.putStr contents
  UTF8.putStrLn "..."

-- turn
-- id: ! "\u043F\u0443\u043D\u043A\u04423"
-- into
-- id: пункт3
unescapeTags :: B.ByteString -> B.ByteString
unescapeTags bs = case parseOnly (many $ tag <|> other) bs of
                       Left e  -> error e
                       Right r -> B.concat r

unescapeUnicode :: B.ByteString -> B.ByteString
unescapeUnicode bs = case parseOnly (many other) bs of
                       Left e  -> error e
                       Right r -> B.concat r

tag :: Attoparsec.Parser B.ByteString
tag = do
  _ <- string $ B8.pack ": ! "
  c <- char '\'' <|> char '"'
  cs <- manyTill (escaped c <|> other) (char c)
  return $ B8.pack ": " <> B8.singleton c <> B.concat cs <> B8.singleton c

escaped :: Char -> Attoparsec.Parser B.ByteString
escaped c = string $ B8.pack ['\\',c]

other :: Attoparsec.Parser B.ByteString
other = uchar <|> Attoparsec.takeWhile1 notspecial <|> regchar
  where notspecial = not . inClass ":!\\\"'"

uchar :: Attoparsec.Parser B.ByteString
uchar = do
  _ <- char '\\'
  num <- (2 <$ char 'x') <|> (4 <$ char 'u') <|> (8 <$ char 'U')
  cs <- count num $ satisfy $ inClass "0-9a-fA-F"
  let n = read ('0':'x':cs)
  return $ encodeUtf8 $ T.pack [chr n]

regchar :: Attoparsec.Parser B.ByteString
regchar = B8.singleton <$> anyChar
