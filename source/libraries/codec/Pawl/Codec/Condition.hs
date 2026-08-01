-- | The @Condition ⇆ Json@ codec (#481).
module Pawl.Codec.Condition where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Comparison as Comparison
import qualified Pawl.Codec.Json as Json
import qualified Pawl.Codec.Quantity as Quantity
import Pawl.Json.Array (Array (MkArray))
import Pawl.Json.Value (Value (Array))
import qualified Pawl.Types.Condition as Condition.Type

-- Both sides go through Quantity.toJson, and that is BACKWARD COMPATIBLE with
-- the Count-on-the-left shape rather than merely similar to it: Quantity.toJson's
-- Count arm delegates to Count.toJson and emits no wrapper of its own, so a
-- `Quantity.Count c` is byte-for-byte the JSON `Count.toJson c` used to produce.
-- Every committed card file that carries a condition therefore round-trips
-- untouched.
conditionToJson :: Condition.Type.Condition -> Value
conditionToJson (Condition.Type.MkCondition m cmp q) =
  Json.tagged (Text.pack "Condition") (Just (Array (MkArray [Quantity.toJson m, Comparison.toJson cmp, Quantity.toJson q])))

jsonToCondition :: Value -> Either Text Condition.Type.Condition
jsonToCondition value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("Condition", Just (Array (MkArray [m, cmp, q]))) -> Condition.Type.MkCondition <$> Quantity.fromJson m <*> Comparison.fromJson cmp <*> Quantity.fromJson q
    _ -> Left (Text.pack "unknown Condition: " <> t)
