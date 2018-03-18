{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Text.CSL.Exception (CiteprocException(..), renderError) where
import Prelude
import           Control.Exception (Exception)
import           Data.Data

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

renderError :: CiteprocException -> String
renderError (ErrorParsingReferences s) =
  "Error parsing references: " ++ s
renderError (CouldNotFindAbbrevFile s) =
  "Could not find abbreviation file: " ++ s
renderError (CouldNotFindBibFile s) =
  "Could not find bibliography file: " ++ s
renderError (ErrorReadingBibFile f s) =
  "Error reading bibliography " ++ f ++ " " ++ s
renderError (ErrorReadingBib s) =
  "Error reading bibliography " ++ s
renderError ErrorSplittingDate =
  "Error splitting date"
renderError (MacroNotFound s) =
  "Macro not found: " ++ s
renderError (DependentStyleHasItselfAsParent s) =
  "Dependent style " ++ s ++ " has itself as parent"
