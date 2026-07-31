-- | The @Quantity ⇆ Json@ codec (#481).
module Pawl.Codec.Quantity where

import Data.Text (Text)
import qualified Data.Text as Text
import Pawl.Codec.Count (countToJson, jsonToCount)
import qualified Pawl.Codec.Json as Json
import Pawl.Codec.SlotName (jsonToSlotName, slotNameToJson)
import Pawl.Json.Array (Array (MkArray))
import Pawl.Json.Value (Value (Array))
import qualified Pawl.Types.Quantity as Quantity

-- Quantity.Count's arm is `countToJson c` directly, NOT re-wrapped in another
-- "Count" tag: countToJson already tags its own output "Count" (it is shared
-- with Condition's embedding of a Count), and the two types happen to use the
-- SAME tag name at two different levels. Re-wrapping would double-tag
-- ({"type":"Count","value":{"type":"Count","value":[...]}}) -- guarded by the
-- CodecSpec round-trip test.
quantityToJson :: Quantity.Quantity -> Value
quantityToJson q = case q of
  Quantity.Literal n -> Json.tagged (Text.pack "Literal") (Just (Json.jInt n))
  Quantity.ManaValue -> Json.nullary (Text.pack "ManaValue")
  Quantity.Power -> Json.nullary (Text.pack "Power")
  Quantity.X -> Json.nullary (Text.pack "X")
  Quantity.InSlot s -> Json.tagged (Text.pack "InSlot") (Just (slotNameToJson s))
  Quantity.Star -> Json.nullary (Text.pack "Star")
  Quantity.Plus a b -> Json.tagged (Text.pack "Plus") (Just (Array (MkArray [quantityToJson a, quantityToJson b])))
  Quantity.Count c -> countToJson quantityToJson c

jsonToQuantity :: Value -> Either Text Quantity.Quantity
jsonToQuantity value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("Literal", Just v) -> Quantity.Literal <$> Json.asInteger v
    ("ManaValue", _) -> Right Quantity.ManaValue
    ("Power", _) -> Right Quantity.Power
    ("X", _) -> Right Quantity.X
    ("InSlot", Just v) -> Quantity.InSlot <$> jsonToSlotName v
    ("Star", _) -> Right Quantity.Star
    ("Plus", Just (Array (MkArray [x, y]))) -> Quantity.Plus <$> jsonToQuantity x <*> jsonToQuantity y
    -- jsonToCount re-derives the tag from the WHOLE value (see the comment on
    -- quantityToJson) rather than from `mv`, which has already had it
    -- stripped.
    ("Count", _) -> Quantity.Count <$> jsonToCount jsonToQuantity value
    _ -> Left (Text.pack "unknown Quantity: " <> t)

jsonToQuantityPair :: Value -> Either Text (Quantity.Quantity, Quantity.Quantity)
jsonToQuantityPair value = case value of
  Array (MkArray [p, t]) -> do
    p_ <- jsonToQuantity p
    t_ <- jsonToQuantity t
    pure (p_, t_)
  _ -> Left (Text.pack "expected a [power, toughness] quantity pair")
