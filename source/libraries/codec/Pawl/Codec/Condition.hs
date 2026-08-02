module Pawl.Codec.Condition where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Comparison as Comparison
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.Json.Array as Array
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.Condition as Condition

-- | Both sides go through Quantity.toJson, and that is BACKWARD COMPATIBLE with
-- the Count-on-the-left shape rather than merely similar to it: Quantity.toJson's
-- Count arm delegates to Count.toJson and emits no wrapper of its own, so a
-- `Quantity.Count c` is byte-for-byte the JSON `Count.toJson c` used to produce.
-- Every committed card file that carries a condition therefore round-trips
-- untouched.
toJson :: Condition.Condition -> Value.Value
toJson (Condition.MkCondition m cmp q) =
  Common.tagged "Condition" . Just . Common.array $ [Quantity.toJson m, Comparison.toJson cmp, Quantity.toJson q]

fromJson :: Value.Value -> Either Text.Text Condition.Condition
fromJson value = do
  (t, mv) <- Common.asTagged value
  case (t, mv) of
    ("Condition", Just (Value.Array (Array.MkArray [m, cmp, q]))) -> Condition.MkCondition <$> Quantity.fromJson m <*> Comparison.fromJson cmp <*> Quantity.fromJson q
    _ -> Left . Text.pack $ "unknown Condition: " <> t
