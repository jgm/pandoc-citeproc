{-# LANGUAGE PatternGuards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.CSL.Pickle.Hexpat
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Andrea Rossato <andrea.rossato@unitn.it>
-- Stability   :  unstable
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Text.CSL.Pickle.Hexpat where

import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.UTF8 ( toString )
import Data.Maybe
import Text.XML.Expat.Tree hiding ( mkText, getText, getChildren )
import Text.XML.Expat.Format
import Text.XML.Expat.Proc

type Content = UNode String
type Attr    = (String, String)

showXML :: Content -> String
showXML = toString . format

getText :: [Content] -> Maybe String
getText [] = Nothing
getText (c:xs)
    | Text x <- c = Just (x ++ getAllText xs)
    | otherwise   = Nothing

getAllText :: [Content] -> String
getAllText [] = []
getAllText (c:xs)
    | Text cd <- c = cd ++ getAllText xs
    | otherwise    = []

dropFirstElem :: [Content] -> [Content]
dropFirstElem [] = []
dropFirstElem (x:xs)
    | Text {} <- x = dropFirstElem xs
    | otherwise    = xs

dropText :: [Content] -> [Content]
dropText [] = []
dropText a@(c:cs)
    | Text _ <- c = dropText cs
    | otherwise   = a

getChildren :: Content -> [Content]
getChildren c
    | Element _ _ x <- c = x
    | otherwise          = []

getElemName :: Content -> Maybe String
getElemName c
    | Element x _ _ <- c = Just x
    | otherwise          = Nothing

getAttName :: Attr -> String
getAttName = reverse . takeWhile (/= ':') . reverse . fst

getAttrl :: Content -> [Attr]
getAttrl c
    | Element _ x _ <- c = x
    | otherwise          = []

getAttrVal :: [Content] -> String
getAttrVal at
    | Text cd : _ <- at = cd
    | otherwise         = []

mkText :: String -> Content
mkText = Text

mkName :: String -> String
mkName = id

mkElement :: String -> [Attr] -> [Content] -> Content
mkElement n a c = Element n a c

mkAttribute :: String -> String -> Attr
mkAttribute n v = (n, v)

attrToCont :: Attr -> Content
attrToCont = Text . snd

qualifiedName :: String -> String
qualifiedName = id

onlyElems' :: [Content] -> [Content]
onlyElems' = onlyElems

parseXML' :: L.ByteString -> [Content]
parseXML' s
    = case parse defaultParseOptions s of
        (_, Just  e) -> error $ "error while reading the XML file: " ++ show e
        (x, Nothing) -> return x
