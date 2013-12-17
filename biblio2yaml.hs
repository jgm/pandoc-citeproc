module Main where
import Text.CSL.Input.Bibutils (readBiblioString, BibFormat(..))
import Text.CSL.Style (Formatted(..))
import Text.Pandoc.Definition (Inline(Space,Str))
import Text.CSL.Reference (Reference(refId), Literal(..), Agent(..))
import Data.Generics ( everywhere, mkT )
import Data.List (group, sort)
import Data.Char (chr, toLower)
import Data.Monoid
import Data.Yaml
import Control.Applicative
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as BL8
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

main :: IO ()
main = do
  argv <- getArgs
  let (flags, args, errs) = getOpt Permute options argv
  let header = "Usage: biblio2yaml [OPTION..] [FILE]"
  unless (null errs && length args < 2) $ do
    hPutStrLn stderr $ usageInfo (unlines $ errs ++ [header]) options
    exitWith $ ExitFailure 1
  when (Version `elem` flags) $ do
    putStrLn $ "biblio2yaml " ++ showVersion version
    exitWith ExitSuccess
  when (Help `elem` flags) $ do
    putStrLn $ usageInfo header options
    exitWith ExitSuccess
  let mbformat = case [f | Format f <- flags] of
                      [x] -> readFormat x
                      _   -> Nothing
  bibstring <- case args of
                    (x:_) -> readFile x
                    []    -> getContents
  let bibformat = mbformat <|> msum (map formatFromExtension args)
  let output = if JsonOutput `elem` flags
                  then BL8.putStrLn .
                       encodePretty' Config{ confIndent = 2
                                           , confCompare = compare } .
                       everywhere (mkT compressName)
                  else outputYamlBlock . unescapeTags . encode
  case bibformat of
       Nothing  -> do
         hPutStrLn stderr $ usageInfo ("Unknown format\n" ++ header) options
         exitWith $ ExitFailure 3
       Just f   -> readBiblioString f bibstring >>= warnDuplicateKeys >>= output

warnDuplicateKeys :: [Reference] -> IO [Reference]
warnDuplicateKeys refs = mapM_ warnDup dupKeys >> return refs
  where warnDup k = hPutStrLn stderr $ "biblio2yaml: duplicate key " ++ k
        allKeys   = map (unLiteral . refId) refs
        dupKeys   = [x | (x:_:_) <- group (sort allKeys)]

outputYamlBlock :: B.ByteString -> IO ()
outputYamlBlock contents = do
  putStrLn "---\nreferences:"
  B.putStr contents
  putStrLn "..."

formatFromExtension :: FilePath -> Maybe BibFormat
formatFromExtension = readFormat . dropWhile (=='.') . takeExtension

data Option =
    Help | Version | Format String | JsonOutput
  deriving (Ord, Eq, Show)

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
        go _          = Nothing

options :: [OptDescr Option]
options =
  [ Option ['h'] ["help"] (NoArg Help) "show usage information"
  , Option ['V'] ["version"] (NoArg Version) "show program version"
  , Option ['f'] ["format"] (ReqArg Format "FORMAT") "bibliography format"
  , Option ['j'] ["json"] (NoArg JsonOutput) "output in json instead of yaml"
  ]

-- Compress particles and suffixes into given and last name,
-- as zotero JSON expects.  (We might also want to set parse-names = true.)
compressName :: Agent -> Agent
compressName ag = Agent{
    givenName       = gn
  , familyName      = fn
  , droppingPart    = mempty
  , nonDroppingPart = mempty
  , literal         = literal ag
  , nameSuffix      = mempty
  , commaSuffix     = False
  }
  where
  spcat (Formatted []) y = y
  spcat y (Formatted []) = y
  spcat x y = x <> Formatted [Space] <> y
  gn = case (givenName ag, nameSuffix ag, commaSuffix ag) of
            ([], _, _)            -> mempty
            (xs, Formatted [], _) -> xs
            (xs, ns, True)        -> init xs ++
                [last xs <> Formatted [Str ",!", Space] <> ns]
            (xs, ns, False)       -> xs ++ [ns]
  fn = spcat (nonDroppingPart ag) (familyName ag)

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
