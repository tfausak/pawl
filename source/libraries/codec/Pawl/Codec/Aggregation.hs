module Pawl.Codec.Aggregation where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.Aggregation as Aggregation

-- | No longer wholly nullary: Greatest carries the per-member quantity it
-- reads. Parametric in that quantity, exactly as 'Aggregation.Aggregation' is,
-- so the codec reaches the payload only through @codec@ -- which is what lets
-- this module sit below @Pawl.Codec.Quantity@ rather than in a cycle with it.
toJson :: (q -> Value.Value) -> Aggregation.Aggregation q -> Value.Value
toJson codec a = case a of
  Aggregation.Objects -> Common.nullary "Objects"
  Aggregation.DistinctCardTypes -> Common.nullary "DistinctCardTypes"
  Aggregation.Greatest q -> Common.tagged "Greatest" . Just $ codec q

fromJson :: (Value.Value -> Either Text.Text q) -> Value.Value -> Either Text.Text (Aggregation.Aggregation q)
fromJson decode value = do
  (t, mv) <- Common.asTagged value
  case (t, mv) of
    ("Objects", Nothing) -> Right Aggregation.Objects
    ("DistinctCardTypes", Nothing) -> Right Aggregation.DistinctCardTypes
    ("Greatest", Just v) -> Aggregation.Greatest <$> decode v
    _ -> Left . Text.pack $ "unknown Aggregation: " <> t
