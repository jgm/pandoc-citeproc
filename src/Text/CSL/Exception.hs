{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Text.CSL.Exception (CiteprocException(..)) where
import Control.Exception (Exception)
import Data.Data
import Data.Typeable

data CiteprocException =
       ErrorParsingReferences String
     | CouldNotFindAbbrevFile String
     | CouldNotFindBibFile    String
     | ErrorReadingBibFile    String String
     | ErrorReadingBib        String
     | ErrorSplittingDate
     | MacroNotFound          String
     | DependentStyleHasItselfAsParent String
     deriving (Show, Data, Typeable)

instance Exception CiteprocException

