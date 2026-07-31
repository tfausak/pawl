-- | The @Aggregation ⇆ Json@ codec, split out of the former
-- Pawl.Codec.All (#481).
module Pawl.Codec.Aggregation where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Json as Json
import Pawl.Json.Value (Value)
import qualified Pawl.Types.Aggregation as Aggregation

-- No longer wholly nullary, and so no longer Json.decodeNullary's shape:
-- Greatest carries the per-member quantity it reads. Parametric in that
-- quantity, exactly as Pawl.Types.Aggregation is, so the codec reaches the
-- payload only through `codec` -- which is what lets this module sit below
-- Pawl.Codec.Quantity rather than in a cycle with it (#481).
aggregationToJson :: (q -> Value) -> Aggregation.Aggregation q -> Value
aggregationToJson codec a = case a of
  Aggregation.Objects -> Json.nullary (Text.pack "Objects")
  Aggregation.DistinctCardTypes -> Json.nullary (Text.pack "DistinctCardTypes")
  Aggregation.Greatest q -> Json.tagged (Text.pack "Greatest") (Just (codec q))

jsonToAggregation :: (Value -> Either Text q) -> Value -> Either Text (Aggregation.Aggregation q)
jsonToAggregation decode value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("Objects", Nothing) -> Right Aggregation.Objects
    ("DistinctCardTypes", Nothing) -> Right Aggregation.DistinctCardTypes
    ("Greatest", Just v) -> Aggregation.Greatest <$> decode v
    _ -> Left (Text.pack "unknown Aggregation: " <> t)
