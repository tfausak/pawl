-- | The @Scaling ⇆ Json@ codec, split out of the former
-- Pawl.Codec.All (#481).
module Pawl.Codec.Scaling where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Json as Json
import Pawl.Json.Value (Value)
import qualified Pawl.Types.Scaling as Scaling

scalingToJson :: Scaling.Scaling -> Value
scalingToJson s = case s of
  Scaling.Multiply n -> Json.tagged (Text.pack "Multiply") (Just (Json.natTo n))
  Scaling.AddMore n -> Json.tagged (Text.pack "AddMore") (Just (Json.natTo n))

jsonToScaling :: Value -> Either Text Scaling.Scaling
jsonToScaling value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("Multiply", Just v) -> fmap Scaling.Multiply (Json.natFrom v)
    ("AddMore", Just v) -> fmap Scaling.AddMore (Json.natFrom v)
    _ -> Left (Text.pack "unknown Scaling: " <> t)
