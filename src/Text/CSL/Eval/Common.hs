{-# LANGUAGE PatternGuards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.CSL.Eval.Common
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Andrea Rossato <andrea.rossato@unitn.it>
-- Stability   :  unstable
-- Portability :  unportable
--
-- The CSL implementation
--
-----------------------------------------------------------------------------

module Text.CSL.Eval.Common where

import Control.Arrow ( (&&&), (>>>) )
import Control.Monad.State
import Data.Char ( toLower )
import Data.List ( elemIndex )
import qualified Data.Map as M
import Data.Maybe

import Text.Pandoc.Shared ( stringify )
import Text.CSL.Reference
import Text.CSL.Style

import Debug.Trace

data EvalState
    = EvalState
      { ref      :: ReferenceMap
      , env      :: Environment
      , debug    :: [String]
      , mode     :: EvalMode
      , disamb   :: Bool
      , consume  :: Bool
      , authSub  :: [String]
      , consumed :: [String]
      , edtrans  :: Bool
      , etal     :: [[Output]]
      , contNum  :: [Agent]
      , lastName :: [Output]
      } deriving ( Show )

data Environment
    = Env
      { cite    :: Cite
      , terms   :: [CslTerm]
      , macros  :: [MacroMap]
      , dates   :: [Element]
      , options :: [Option]
      , names   :: [Element]
      , abbrevs :: Abbreviations
      } deriving ( Show )

data EvalMode
    = EvalSorting Cite
    | EvalCite    Cite
    | EvalBiblio  Cite -- for the reference position
      deriving ( Show, Eq )

isSorting :: EvalMode -> Bool
isSorting m = case m of EvalSorting _ -> True; _ -> False

-- | With the variable name and the variable value search for an
-- abbreviation or return an empty string.
getAbbreviation :: Abbreviations -> String -> String -> String
getAbbreviation (Abbreviations as) s v
    = maybe [] id $ M.lookup "default" as >>=
                    M.lookup (if s `elem` numericVars then "number" else s) >>=
                    M.lookup v

-- | If the first parameter is 'True' the plural form will be retrieved.
getTerm :: Bool -> Form -> String -> State EvalState String
getTerm b f s = maybe [] g . findTerm s f' <$> gets (terms  . env) -- FIXME: vedere i fallback
    where g  = if b then termPlural else termSingular
          f' = case f of NotSet -> Long; _ -> f

getStringVar :: String -> State EvalState String
getStringVar
    = getVar [] getStringValue

getDateVar :: String -> State EvalState [RefDate]
getDateVar
    = getVar [] getDateValue
    where getDateValue = maybe [] id . fromValue

getLocVar :: State EvalState (String,String)
getLocVar = gets (env >>> cite >>> citeLabel &&& citeLocator)

getVar :: a -> (Value -> a) -> String -> State EvalState a
getVar a f s
    = withRefMap $ maybe a f . lookup (formatVariable s)

getAgents :: String -> State EvalState [Agent]
getAgents s
    = do
      mv <- withRefMap (lookup s)
      case mv of
        Just v -> case fromValue v of
                    Just x -> consumeVariable s >> return x
                    _      -> return []
        _      -> return []

getAgents' :: String -> State EvalState [Agent]
getAgents' s
    = do
      mv <- withRefMap (lookup s)
      case mv of
        Just v -> case fromValue v of
                    Just x -> return x
                    _      -> return []
        _      -> return []

getStringValue :: Value -> String
getStringValue val =
  -- The second clause handles the case where we have a Formatted
  -- but need a String.  This is currently needed for "page".  It's a bit
  -- hackish; we should probably change the type in Reference for
  -- page to String.
  case fromValue val `mplus` ((stringify . unFormatted) `fmap` fromValue val)
       `mplus` (unLiteral `fmap` fromValue val) of
       Just v   -> v
       Nothing  -> Debug.Trace.trace ("Expecting string value, got " ++
                       show val) []

getOptionVal :: String -> [Option] -> String
getOptionVal s = fromMaybe [] . lookup s

isOptionSet :: String -> [Option] -> Bool
isOptionSet s = maybe False (not . null) . lookup s

isTitleVar, isTitleShortVar :: String -> Bool
isTitleVar         = flip elem ["title", "container-title", "collection-title"]
isTitleShortVar    = flip elem ["title-short", "container-title-short"]

getTitleShort :: String -> State EvalState String
getTitleShort s = do let s' = take (length s - 6) s  -- drop '-short'
                     v <- getStringVar s'
                     abbrs <- gets (abbrevs . env)
                     return $ getAbbreviation abbrs s' v

isVarSet :: String -> State EvalState Bool
isVarSet s
    | isTitleShortVar s = do r <- getVar False isValueSet s
                             if r then return r
                                  else return . not . null =<< getTitleShort s
    | otherwise = if s /= "locator"
                  then getVar False isValueSet s
                  else getLocVar >>= return . (/=) "" . snd

withRefMap :: (ReferenceMap -> a) -> State EvalState a
withRefMap f = return . f =<< gets ref

-- | Convert variable to lower case, translating underscores ("_") to dashes ("-")
formatVariable :: String -> String
formatVariable = foldr f []
    where f x xs = if x == '_' then '-' : xs else toLower x : xs

consumeVariable :: String -> State EvalState ()
consumeVariable s
    = do b <- gets consume
         when b $ modify $ \st -> st { consumed = s : consumed st }

consuming :: State EvalState a -> State EvalState a
consuming f = setConsume >> f >>= \a -> doConsume >> unsetConsume >> return a
    where setConsume   = modify $ \s -> s {consume = True, consumed = [] }
          unsetConsume = modify $ \s -> s {consume = False }
          doConsume    = do sl <- gets consumed
                            modify $ \st -> st { ref = remove (ref st) sl }
          doRemove s (k,v) = if isValueSet v then [(formatVariable s,Value Empty)] else [(k,v)]
          remove rm sl
              | (s:ss) <- sl = case elemIndex (formatVariable s) (map fst rm) of
                                 Just  i -> let nrm = take i rm ++
                                                      doRemove s (rm !! i) ++
                                                      drop (i + 1) rm
                                            in  remove nrm ss
                                 Nothing ->     remove  rm ss
              | otherwise    = rm

when' :: Monad m => m Bool -> m [a] -> m [a]
when' p f = whenElse p f (return [])

whenElse :: Monad m => m Bool -> m a -> m a -> m a
whenElse b f g = b >>= \ bool -> if bool then f else g

concatMapM :: (Monad m, Functor m, Eq b) => (a -> m [b]) -> [a] -> m [b]
concatMapM f l = concat . filter (/=[]) <$> mapM f l

{-
trace ::  String -> State EvalState ()
trace d = modify $ \s -> s { debug = d : debug s }
-}
