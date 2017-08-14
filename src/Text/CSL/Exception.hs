{-# LANGUAGE StandaloneDeriving #-}
module Text.CSL.Exception (CiteprocException(..)) where
import Control.Exception (Exception)

data CiteprocException =
       ErrorParsingReferences String
     | CouldNotFindAbbrevFile String
     | CouldNotFindBibFile    String
     deriving Show

instance Exception CiteprocException

