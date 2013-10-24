{-# LANGUAGE PatternGuards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.CSL.Input.Json
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Andrea Rossato <andrea.rossato@unitn.it>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A module for reading Json CSL data.
--
-----------------------------------------------------------------------------

module Text.CSL.Input.Json where

import Control.Arrow
import Control.Monad.State
import Data.Generics
import Data.Char (toLower, toUpper)
import Data.List
import Data.Ratio

import Text.JSON.Generic
import Text.JSON.String ( runGetJSON, readJSTopType )

import Text.CSL.Reference
import Text.CSL.Style

readJsonInput :: FilePath -> IO [Reference]
readJsonInput f = readJsonInputString `fmap` readFile f

readJsonInputString :: String -> [Reference]
readJsonInputString s
    = let jrefs  =  procJSObject editJsonInput $ readJsonString s
          refs r = case readJSData r of
                     Ok ref   -> ref
                     Error er -> error ("readJSData: " ++ er)
      in case jrefs of
           JSObject o -> map (refs . snd) $ fromJSObject o
           JSArray ar -> map (refs      ) $ ar
           _          -> error $ "citeproc: error in reading the Json bibliographic data."

readJsonFile :: FilePath -> IO JSValue
readJsonFile f = readJsonString `fmap` readFile f

readJsonString :: String -> JSValue
readJsonString =
  let rmCom = unlines . filter (\x -> not (" *" `isPrefixOf` x || "/*" `isPrefixOf` x)) . lines
  in  either error id . runGetJSON readJSTopType . rmCom

readJsonAbbrevFile :: FilePath -> IO Abbreviations
readJsonAbbrevFile f = readJsonAbbrev `fmap` readJsonFile f

readJsonAbbrev :: JSValue -> Abbreviations
readJsonAbbrev = undefined {-
    = map Abbrev . mapSndObj (mapSndObj (M.fromList . mapSndObj fromJString))
    where
      mapSndObj f = map (second f) . fromObj
-}

readJsonCitations :: JSValue -> [Cite]
readJsonCitations jv
    | JSArray (JSObject o:_) <- jv
    , Just    (JSArray   ar) <- lookup "citationItems" (fromJSObject o )
    , Just    (JSObject  o') <- lookup "properties"    (fromJSObject o )
    , idx                    <- lookup "noteIndex"     (fromJSObject o')
                = map (readCite $ readCitNum $ fmap toString idx) ar
    | otherwise = error ("error in reading CITATIONS:\n" ++ show jv)
    where
      readCitNum j
          | Just (JSString js) <- j = fromJSString js
          | otherwise               = []
      readCite :: String -> JSValue -> Cite
      readCite n c = case readJSData c of
                       Ok cite  -> cite { citeNoteNumber = n }
                       Error er -> error ("citations: " ++ er)

editJsonCiteItems :: (String, JSValue) -> (String, JSValue)
editJsonCiteItems (s,j)
    | "id"              <- s = ("citeId"        , toString j)
    | "label"           <- s = ("citeLabel"     , toString j)
    | "locator"         <- s = ("citeLocator"   , toString j)
    | "note-number"     <- s = ("citeNoteNumber", toString j)
    | "near-note"       <- s = ("nearNote"      , toJSBool j)
    | "prefix"          <- s = ("citePrefix"    , affixes  j)
    | "suffix"          <- s = ("citeSuffix"    , affixes  j)
    | "suppress-author" <- s = ("suppressAuthor", toJSBool j)
    | "author-only"     <- s = ("authorInText"  , toJSBool j)
    | "author-in-text"  <- s = ("authorInText"  , toJSBool j)
    | otherwise              = (s,j)
    where
      affixes v
          | JSString js <- v = JSString . toJSString . show . PlainText . fromJSString $ js
          | otherwise        = affixes $ toString v

editJsonInput :: (String, JSValue) -> (String, JSValue)
editJsonInput (s,j)
    | "dropping-particle"     <- s = ("droppingPart"   , j)
    | "non-dropping-particle" <- s = ("nonDroppingPart", j)
    | "comma-suffix"          <- s = ("commaSuffix", toJSBool j)
    | "id"                    <- s = ("refId"      , toString j)
    | "shortTitle"            <- s = ("titleShort" , j)
    | isRefDate s
    , JSObject js <- j = (camel s      , JSArray (editDate $ fromJSObject js))
    | "family"    <- s = ("familyName" , j)
    | "suffix"    <- s = ("nameSuffix" , j)
    | "URL"       <- s = ("url"        , j)
    | "edition"   <- s = ("edition"    , toString j)
    | "volume"    <- s = ("volume"     , toString j)
    | "issue"     <- s = ("issue"      , toString j)
    | "number"    <- s = ("number"     , toString j)
    | "page"      <- s = ("page"       , toString j)
    | "section"   <- s = ("section"    , toString j)
    | "given"     <- s
    , JSString js <- j = ("givenName"  , JSArray . map (JSString . toJSString) . words $ fromJSString js)
    | "type"      <- s
    , JSString js <- j = ("refType"    , JSString . toJSString . format . camel $ fromJSString js)
    | (c:cs)      <- s = (toLower c : camel cs , j)
    | otherwise        = (s,j)
    where
      camel x
          | '-':y:ys <- x = toUpper y : camel ys
          | '_':y:ys <- x = toUpper y : camel ys
          |     y:ys <- x =         y : camel ys
          | otherwise     = []

      format (x:xs) = toUpper x : xs
      format     [] = []

      zipDate x = zip (take (length x) ["year", "month", "day"]) . map toString $ x

      editDate x = let seas = case lookup "season" x of
                                Just o -> [("season",toString o)]
                                _      -> []
                       raw  = case lookup "raw" x of
                                Just o -> [("other",o)]
                                _      -> []
                       lit  = case lookup "literal" x of
                                Just o -> [("other",o)]
                                _      -> []
                       cir  = case lookup "circa" x of
                                Just o -> [("circa",toString o)]
                                _      -> []
                       rest = flip (++) (seas ++ lit ++ raw ++ cir)
                   in case lookup "dateParts" x of
                        Just (JSArray (JSArray x':[])) -> [JSObject . toJSObject . rest $ zipDate x']
                        Just (JSArray (JSArray x':
                                       JSArray y':[])) -> [JSObject . toJSObject        $ zipDate x'
                                                          ,JSObject . toJSObject        $ zipDate y']
                        _                              -> [JSObject . toJSObject $ rest []]

toString :: JSValue -> JSValue
toString x
    | JSString    js <- x = JSString js
    | JSRational _ n <- x = JSString . toJSString . show $ numerator n
    | otherwise = JSString . toJSString $ []

toJSBool :: JSValue -> JSValue
toJSBool x
    | JSBool       b <- x = JSBool b
    | JSRational _ n <- x = JSBool (numerator n /= 0)
    | JSString    js <- x = JSBool (fromJSString js /= [])
    | otherwise           = JSBool False

procJSObject :: ((String, JSValue) -> (String, JSValue)) -> JSValue -> JSValue
procJSObject f jv
    | JSObject o <- jv = JSObject . toJSObject . map f . map (second $ procJSObject f) . fromJSObject $ o
    | JSArray ar <- jv = JSArray  . map (procJSObject f) $ ar
    | otherwise        = jv

mapJSArray :: (JSValue -> JSValue) -> JSValue -> JSValue
mapJSArray f jv
    | JSArray ar <- jv = JSArray $ map (mapJSArray f) ar
    | otherwise        = f jv

isRefDate :: String -> Bool
isRefDate = flip elem [ "issued", "event-date", "accessed", "container", "original-date"]

readJSData :: (Data a) => JSValue -> Result a
readJSData j = undefined -- readType j
--              `ext1R` jList
--              `extR` (value :: Result String)
--              `extR` (value :: Result Affix )
--   where
--     value :: (JSON a) => Result a
--     value = readJSON j
-- 
--     jList :: (Data e) => Result [e]
--     jList = case j of
--               JSArray j' -> mapM readJSData j'
--               _          -> Error $ "fromJSON: Prelude.[] bad data: " ++ show j

-- | Build a datatype from a JSON object. Uses selectFields which
-- allows to provied default values for fields not present in the JSON
-- object. Useble with non algebraic datatype with record fields.
readType :: (Data a) => JSValue -> Result a
readType (JSObject ob) = construct
    where
      construct = selectFields (fromJSObject ob) (constrFields con) >>=
                  evalStateT (fromConstrM f con) . zip (constrFields con)

      resType :: Result a -> a
      resType _ = error "resType"

      typ = dataTypeOf $ resType construct
      con = indexConstr typ 1

      f :: (Data a) => StateT [(String,JSValue)] Result a
      f = do js <- get
             case js of
               j':js' -> do put js'
                            lift $ readJSData (snd j')
               []     -> lift $ Error ("construct: empty list")

readType j = fromJSON j

selectFields :: [(String, JSValue)] -> [String] -> Result [JSValue]
selectFields fjs = mapM sel
    where sel f = maybe (fb f) Ok $ lookup f fjs
          fb  f = maybe (Error $ "selectFields: no field " ++ f) Ok $ lookup f defaultJson

fromObj :: JSValue -> [(String, JSValue)]
fromObj (JSObject o) = fromJSObject o
fromObj _            = []

fromJString :: JSValue -> String
fromJString j
    | JSString x <- j = fromJSString x
    | otherwise       = []

defaultJson :: [(String, JSValue)]
defaultJson = fromObj (toJSON emptyReference) ++ fromObj emptyRefDate ++
              fromObj emptyPerson ++ fromObj emptyCite'
    where
      emptyRefDate = toJSON $ RefDate [] [] [] [] [] []
      emptyPerson  = toJSON $ Agent   [] [] [] [] [] [] False
      emptyCite'   = toJSON $ emptyCite
