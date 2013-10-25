{-# LANGUAGE PatternGuards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.CSL.Pickle.Xml
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Andrea Rossato <andrea.rossato@unitn.it>
-- Stability   :  unstable
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Text.CSL.Pickle.Xml where

import Text.Pandoc.UTF8 ( toStringLazy )
import qualified Data.ByteString.Lazy as L
import Data.Maybe
import Text.XML.Light

showXML :: Content -> String
showXML = showContent

getText :: [Content] -> Maybe String
getText [] = Nothing
getText (c:_)
    | Text  x <- c = Just (showCData x)
    | otherwise    = Nothing

getChildren :: Content -> [Content]
getChildren c
    | Elem el <- c = elContent el
    | otherwise    = []

getElemName :: Content -> Maybe QName
getElemName c
    | Elem el <- c = Just (elName el)
    | otherwise    = Nothing

dropFirstElem :: [Content] -> [Content]
dropFirstElem [] = []
dropFirstElem (x:xs)
    | Text {} <- x = dropFirstElem xs
    | otherwise    = xs

dropText :: [Content] -> [Content]
dropText [] = []
dropText a@(x:xs)
    | Text {} <- x = dropFirstElem xs
    | otherwise    = a

getAttName :: Attr -> String
getAttName = qName . attrKey

getAttrl :: Content -> [Attr]
getAttrl c
    | Elem el <- c = elAttribs el
    | otherwise    = []

getAttrVal :: [Content] -> String
getAttrVal at
    | Text cd : _ <- at = cdData cd
    | otherwise         = []

mkText :: String -> Content
mkText s = Text $ blank_cdata { cdData = s }

attrToCont :: Attr -> Content
attrToCont a = Text $ blank_cdata { cdData = attrVal a }

mkName :: String -> QName
mkName n = blank_name {qName = n }

mkElement :: String -> [Attr] -> [Content] -> Content
mkElement n a c = Elem $ Element (mkName n) a c Nothing

mkAttribute :: String -> String -> Attr
mkAttribute n c = Attr (mkName n) c

qualifiedName :: QName -> String
qualifiedName qn = (fromMaybe [] $ qPrefix qn) ++ qName qn

onlyElems' :: [Content] -> [Content]
onlyElems' = map Elem . onlyElems

parseXML' :: L.ByteString -> [Content]
parseXML' s
    = case parseXML (toStringLazy s) of
        [] -> error $ "error while reading the XML string"
        x  -> x
