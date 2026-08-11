module Pawl.Codec.Scaling where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.Scaling as Scaling

toJson :: Scaling.Scaling -> Value.Value
toJson s = case s of
  Scaling.Multiply n -> Common.tagged "Multiply" . Just $ Common.encodeNatural n
  Scaling.AddMore n -> Common.tagged "AddMore" . Just $ Common.encodeNatural n
  Scaling.Halve -> Common.nullary "Halve"

fromJson :: Value.Value -> Either Text.Text Scaling.Scaling
fromJson value = do
  (t, mv) <- Common.asTagged value
  case (t, mv) of
    ("Multiply", Just v) -> Scaling.Multiply <$> Common.decodeNatural v
    ("AddMore", Just v) -> Scaling.AddMore <$> Common.decodeNatural v
    ("Halve", Nothing) -> Right Scaling.Halve
    _ -> Left . Text.pack $ "unknown Scaling: " <> t
