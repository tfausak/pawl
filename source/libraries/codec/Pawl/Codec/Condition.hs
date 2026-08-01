-- | The @Condition ⇆ Json@ codec (#481).
module Pawl.Codec.Condition where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Comparison as Comparison
import qualified Pawl.Codec.Json as Json
import Pawl.Codec.Quantity (jsonToQuantity, quantityToJson)
import Pawl.Json.Array (Array (MkArray))
import Pawl.Json.Value (Value (Array))
import qualified Pawl.Types.Condition as Condition.Type

-- Both sides go through quantityToJson, and that is BACKWARD COMPATIBLE with
-- the Count-on-the-left shape rather than merely similar to it: quantityToJson's
-- Count arm delegates to countToJson and emits no wrapper of its own, so a
-- `Quantity.Count c` is byte-for-byte the JSON `countToJson c` used to produce.
-- Every committed card file that carries a condition therefore round-trips
-- untouched.
conditionToJson :: Condition.Type.Condition -> Value
conditionToJson (Condition.Type.MkCondition m cmp q) =
  Json.tagged (Text.pack "Condition") (Just (Array (MkArray [quantityToJson m, Comparison.toJson cmp, quantityToJson q])))

jsonToCondition :: Value -> Either Text Condition.Type.Condition
jsonToCondition value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("Condition", Just (Array (MkArray [m, cmp, q]))) -> Condition.Type.MkCondition <$> jsonToQuantity m <*> Comparison.fromJson cmp <*> jsonToQuantity q
    _ -> Left (Text.pack "unknown Condition: " <> t)
