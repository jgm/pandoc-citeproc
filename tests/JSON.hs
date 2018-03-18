{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- ToJSON/FromJSON instances for Style
module JSON where
import Prelude
import           Data.Aeson
import qualified Data.ByteString.Lazy as L
import qualified Data.Text.Encoding   as T
import           Text.CSL.Parser
import           Text.CSL.Style

instance FromJSON Style where
  parseJSON (String s) = return $ parseCSL' $ L.fromChunks [T.encodeUtf8 s]
  parseJSON _          = fail "Could not parse Style"
