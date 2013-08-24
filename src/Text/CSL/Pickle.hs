{-# LANGUAGE PatternGuards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.CSL.Pickle
-- Copyright   :  (c) Uwe Schmidt Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Andrea Rossato <andrea.rossato@unitn.it>
-- Stability   :  unstable
-- Portability :  portable
--
-- This module is mostly copied from Text.XML.HXT.Arrow.Pickle.Xml
-- which is an adaptation of the pickler combinators developed by
-- Andrew Kennedy.
--
-- See: <http://research.microsoft.com/~akenn/fun/picklercombinators.pdf>
-----------------------------------------------------------------------------

module Text.CSL.Pickle where

import Control.Monad    ( unless        )
import Data.List        ( elemIndex     )
import Data.Maybe
import System.Directory ( doesFileExist )
import qualified Data.ByteString.Lazy as L

#ifdef USE_HEXPAT
import Text.CSL.Pickle.Hexpat
#else
import Text.CSL.Pickle.Xml
import Text.XML.Light
#endif

data St
    = St { attributes :: [Attr]
	 , contents   :: [Content]
	 }

data PU a
    = PU { appPickle   :: (a, St) -> St
	 , appUnPickle :: St -> (Maybe a, St)
         }

pickleXML :: PU a -> a -> String
pickleXML p v = concatMap showXML $ contents st
    where st = appPickle p (v, emptySt)

unpickleXML :: PU a -> [Content] -> Maybe a
unpickleXML p t
    = fst . appUnPickle p $ St { attributes = []
			       , contents   = t
			       }

emptySt	:: St
emptySt	=  St { attributes = []
	      , contents   = []
	      }

addAtt	:: Attr -> St -> St
addAtt x s = s {attributes = x : attributes s}

addCont	:: Content -> St -> St
addCont x s = s {contents = x : contents s}

dropCont :: St -> St
dropCont s = s { contents = dropFirstElem (contents s)}

getAtt :: String -> St -> Maybe Attr
getAtt name
    = listToMaybe . filter ((==) name .  getAttName) . attributes

getCont	:: St -> Maybe Content
getCont	= listToMaybe . contents

class XmlPickler a where
    xpickle :: PU a

instance XmlPickler Int where
    xpickle = xpPrim

instance XmlPickler Integer where
    xpickle = xpPrim

instance XmlPickler () where
    xpickle = xpUnit

instance XmlPickler a => XmlPickler [a] where
    xpickle = xpList xpickle

instance XmlPickler a => XmlPickler (Maybe a) where
    xpickle = xpOption xpickle

xpPrim	:: (Read a, Show a) => PU a
xpPrim
    = xpWrapMaybe (readMaybe, show) xpText
    where
    readMaybe :: Read a => String -> Maybe a
    readMaybe str
	= val (reads str)
	where
	val [(x,"")] = Just x
	val _        = Nothing

xpUnit :: PU ()
xpUnit = xpLift ()

xpZero :: PU a
xpZero
    =  PU { appPickle   = snd
	  , appUnPickle = \ s -> (Nothing, s)
          }

xpLift :: a -> PU a
xpLift x
    =  PU { appPickle   = snd
	  , appUnPickle = \ s -> (Just x, s)
          }

xpCondSeq :: PU b -> (b -> a) -> PU a -> (a -> PU b) -> PU b
xpCondSeq pd f pa k
    = PU { appPickle   = ( \ (b, s) ->
	                   let
			   a  = f b
			   pb = k a
			   in
			   appPickle pa (a, (appPickle pb (b, s)))
			 )
	 , appUnPickle = ( \ s ->
			   let
			   (a, s') = appUnPickle pa s
			   in
			   case a of
			   Nothing -> appUnPickle pd     s
			   Just a' -> appUnPickle (k a') s'
			 )
	 }

xpSeq :: (b -> a) -> PU a -> (a -> PU b) -> PU b
xpSeq = xpCondSeq xpZero

xpChoice :: PU b -> PU a -> (a -> PU b) -> PU b
xpChoice pb = xpCondSeq pb undefined

xpWrap	:: (a -> b, b -> a) -> PU a -> PU b
xpWrap (f, g) pa = xpSeq g pa (xpLift . f)

xpDefault :: (Eq a) => a -> PU a -> PU a
xpDefault df
    = xpWrap ( fromMaybe df
	     , \ x -> if x == df then Nothing else Just x
	     ) .
      xpOption

xpOption :: PU a -> PU (Maybe a)
xpOption pa
    = PU { appPickle   = ( \ (a, st) ->
			   case a of
			   Nothing -> st
			   Just x  -> appPickle pa (x, st)
			 )
	 , appUnPickle = appUnPickle $
	                 xpChoice (xpLift Nothing) pa (xpLift . Just)
	 }


xpAlt	:: (a -> Int) -> [PU a] -> PU a
xpAlt tag ps
    = PU { appPickle   = ( \ (a, st) ->
			   let
			   pa = ps !! (tag a)
			   in
			   appPickle pa (a, st)
			 )
	 , appUnPickle = appUnPickle $
	                 ( case ps of
			   []     -> xpZero
			   pa:ps1 -> xpChoice (xpAlt tag ps1) pa xpLift
			 )
	 }

xpList	:: PU a -> PU [a]
xpList pa
    = PU { appPickle   = ( \ (a, st) ->
			   case a of
			   []  -> st
			   _:_ -> appPickle pc (a, st)
			 )
	 , appUnPickle = appUnPickle $
                         xpChoice (xpLift []) pa
	                   (\ x -> xpSeq id (xpList pa) (\xs -> xpLift (x:xs)))
	 }
      where
      pc = xpSeq head  pa       (\ x ->
	   xpSeq tail (xpList pa) (\ xs ->
	   xpLift (x:xs)))


xpLiftMaybe :: Maybe a -> PU a
xpLiftMaybe = maybe xpZero xpLift

xpWrapMaybe :: (a -> Maybe b, b -> a) -> PU a -> PU b
xpWrapMaybe (i, j) pa	= xpSeq j pa (xpLiftMaybe . i)

xpPair :: PU a -> PU b -> PU (a, b)
xpPair pa pb
    = ( xpSeq fst pa (\ a ->
        xpSeq snd pb (\ b ->
        xpLift (a,b)))
      )

xpTriple :: PU a -> PU b -> PU c -> PU (a, b, c)
xpTriple pa pb pc
    = xpWrap (toTriple, fromTriple) (xpPair pa (xpPair pb pc))
    where
    toTriple   ~(a, ~(b, c)) = (a,  b, c )
    fromTriple ~(a,   b, c ) = (a, (b, c))

xp4Tuple :: PU a -> PU b -> PU c -> PU d -> PU (a, b, c, d)
xp4Tuple pa pb pc pd
    = xpWrap (toQuad, fromQuad) (xpPair pa (xpPair pb (xpPair pc pd)))
    where
    toQuad   ~(a, ~(b, ~(c, d))) = (a,  b,  c, d  )
    fromQuad ~(a,   b,   c, d  ) = (a, (b, (c, d)))

xp5Tuple :: PU a -> PU b -> PU c -> PU d -> PU e -> PU (a, b, c, d, e)
xp5Tuple pa pb pc pd pe
    = xpWrap (toQuint, fromQuint) (xpPair pa (xpPair pb (xpPair pc (xpPair pd pe))))
    where
    toQuint   ~(a, ~(b, ~(c, ~(d, e)))) = (a,  b,  c,  d, e   )
    fromQuint ~(a,   b,   c,   d, e   ) = (a, (b, (c, (d, e))))

xp6Tuple :: PU a -> PU b -> PU c -> PU d -> PU e -> PU f -> PU (a, b, c, d, e, f)
xp6Tuple pa pb pc pd pe pf
    = xpWrap (toSix, fromSix) (xpPair pa (xpPair pb (xpPair pc (xpPair pd (xpPair pe pf)))))
    where
    toSix   ~(a, ~(b, ~(c, ~(d, ~(e, f))))) = (a,  b,  c,  d,  e, f    )
    fromSix ~(a,   b,   c,   d,   e, f    ) = (a, (b, (c, (d, (e, f)))))

--------------------------------------------------------------------------------

xpText :: PU String
xpText
    = PU { appPickle   = \ (s, st) -> addCont (mkText s) st
	 , appUnPickle = \ st -> fromMaybe (Nothing, st) (unpickleString st)
	 }
    where
    unpickleString st
	= do
	  s <- getText (contents st)
	  return (Just (unescape s), st {contents = dropText $ contents st})

xpText0 :: PU String
xpText0
    = xpWrap (fromMaybe "", emptyToNothing) $ xpOption $ xpText
    where
    emptyToNothing "" = Nothing
    emptyToNothing x  = Just x

xpElem	:: String -> PU a -> PU a
xpElem name pa
    = PU { appPickle   = ( \ (a, st) ->
			   let
	                   st' = appPickle pa (a, emptySt)
			   in
			   addCont (mkElement name (attributes st') (contents st')) st
			 )
	 , appUnPickle = \ st -> fromMaybe (Nothing, st) (unpickleElement st)
	 }
      where
      unpickleElement st
          = do
            e <- listToMaybe . onlyElems' . contents $ st
            n <- getElemName e
            if qualifiedName n /= name
              then fail "element name does not match"
              else do
                al  <- Just $ getAttrl e
                res <- fst . appUnPickle pa $ St {attributes = al, contents = getChildren e}
                return (Just res, dropCont st)

-- | A pickler for interleaved elements.
xpIElem	:: String -> PU a -> PU a
xpIElem name pa
    = PU { appPickle   = ( \ (a, st) ->
			   let
	                   st' = appPickle pa (a, emptySt)
			   in
			   addCont (mkElement name (attributes st') (contents st')) st
			 )
	 , appUnPickle = \ st -> fromMaybe (Nothing, st) (unpickleElement st)
	 }
      where
      unpickleElement st
          = do
            let t = onlyElems' . contents $ st
            ns <- mapM getElemName t
            case elemIndex name (map qualifiedName ns) of
              Nothing -> fail "element name does not match"
              Just i  -> do
                let cs = getChildren (t !! i)
                al <- Just $ getAttrl (t !! i)
                res <- fst . appUnPickle pa $ St {attributes = al, contents = cs}
                return (Just res, st {contents = take i t ++ drop (i + 1) t})

xpAttr	:: String -> PU a -> PU a
xpAttr name pa
    = PU { appPickle   = ( \ (a, st) ->
			   let
			   st' = appPickle pa (a, emptySt)
			   in
			   addAtt (mkAttribute name $ getAttrVal $ contents st') st
			 )
	 , appUnPickle = \ st -> fromMaybe (Nothing, st) (unpickleAttr st)
	 }
      where
      unpickleAttr st
	  = do
	    a <- getAtt name st
	    res <- fst . appUnPickle pa $ St { attributes = []
                                             , contents   = [attrToCont a]}
	    return (Just res, st)

xpElemWithAttrValue :: String -> String -> String -> PU a -> PU a
xpElemWithAttrValue n a v = xpIElem n . xpAddFixedAttr a v

xpAttrFixed	:: String -> String -> PU ()
xpAttrFixed name val
    = ( xpWrapMaybe ( \ v -> if v == val then Just () else Nothing
		    , const val
		    ) $
	xpAttr name xpText
      )

xpAddFixedAttr	:: String -> String -> PU a -> PU a
xpAddFixedAttr name val pa
    = xpWrap ( snd
	     , (,) ()
	     ) $
      xpPair (xpAttrFixed name val) pa

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a,b,c) = f a b c

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (a,b,c,d) = f a b c d

uncurry5 :: (a -> b -> c -> d -> e -> f) -> (a, b, c, d, e) -> f
uncurry5 f (a,b,c,d,e) = f a b c d e

unescape :: String -> String
unescape [] = []
unescape ('&':'l':'t':    ';':xs) = "<" ++ unescape xs
unescape ('&':'g':'t':    ';':xs) = ">" ++ unescape xs
unescape ('&':'a':'m':'p':';':xs) = "&" ++ unescape xs
unescape (x:                  xs) = x    : unescape xs

readXmlString :: Show a => PU a -> L.ByteString -> a
readXmlString xp s
    = case unpickleXML xp $ parseXML' s of
        Just a -> a
        _      -> error "error while parsing the XML string"

readXmlFile :: Show a => PU a -> FilePath -> IO a
readXmlFile xp f = readXmlString xp `fmap` readFile' f

readFile' :: FilePath -> IO L.ByteString
readFile' f = do
  flip unless (error $ f ++ " file does not exist") =<< doesFileExist f
  L.readFile f
