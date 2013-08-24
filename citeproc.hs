module Main where
import Text.CSL (readBiblioFile)
import Text.CSL.Reference (Reference(..))
import Text.Pandoc.Definition
import Text.Pandoc.JSON
import Text.Pandoc.Walk
import Text.HTML.TagSoup.Entity (lookupEntity)

main :: IO ()
main = toJSONFilter processCites

processCites :: Pandoc -> IO Pandoc
processCites (Pandoc meta blocks) = do
  let inlineRefs = maybe [] id $ lookupMeta "references" meta >>= convertRefs
  bibRefs <- getBibRefs $ maybe (MetaList []) id
                        $ lookupMeta "bibliography" meta
  let refs = inlineRefs ++ bibRefs
  let meta' = walk (handleCite refs) meta
  return $ Pandoc meta' (walk (handleCite refs) blocks)

getStr :: Inline -> String
getStr (Str x) = x
getStr _ = ""

getBibRefs :: MetaValue -> IO [Reference]
getBibRefs (MetaList xs) = concat `fmap` mapM getBibRefs xs
getBibRefs (MetaInlines xs) =
  map unescapeRefId `fmap` readBiblioFile (query getStr xs)
getBibRefs (MetaString s) =
  map unescapeRefId `fmap` readBiblioFile s
getBibRefs _ = return []

-- unescape reference ids, which may contain XML entities, so
-- that we can do lookups with regular string equality
unescapeRefId :: Reference -> Reference
unescapeRefId ref = ref{ refId = decodeEntities (refId ref) }

decodeEntities :: String -> String
decodeEntities [] = []
decodeEntities ('&':xs) =
  let (ys,zs) = break (==';') xs
  in  case zs of
           ';':ws -> case lookupEntity ('&':ys ++ ";") of
                                       Just c  -> [c] ++ decodeEntities ws
                                       Nothing -> '&' : decodeEntities xs
           _      -> '&' : decodeEntities xs
decodeEntities (x:xs) = x : decodeEntities xs

handleCite :: [Reference] -> Inline -> Inline
handleCite refs (Cite cs ils) = Cite cs [Str "CITE"]
handleCite _ x = x

convertRefs :: MetaValue -> Maybe [Reference]
convertRefs _ = Nothing
