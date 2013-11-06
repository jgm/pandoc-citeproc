{-# OPTIONS_GHC -fno-warn-orphans #-}
-- ToJSON/FromJSON instances for Style
module JSON where
import Data.Aeson
import Text.CSL.Style
import Text.CSL.Parser
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as L

instance FromJSON Style where
  parseJSON (String s) = return $ parseCSL' $ L.fromChunks [T.encodeUtf8 s]
  parseJSON _ = fail "Could not parse Style"
