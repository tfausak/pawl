module Pawl.Codec.Quantity where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Count as Count
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.Json.Array as Array
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.Quantity as Quantity

-- | Quantity.Count's arm is @Count.toJson toJson c@ directly, NOT re-wrapped in
-- another "Count" tag: 'Count.toJson' already tags its own output "Count" (it is
-- shared with Condition's embedding of a Count), and the two types happen to use
-- the SAME tag name at two different levels. Re-wrapping would double-tag
-- (@{"type":"Count","value":{"type":"Count","value":[...]}}@) -- guarded by
-- Pawl.Codec.QuantitySpec's "Count shares Count's own tag, not double-tagged"
-- test.
toJson :: Quantity.Quantity -> Value.Value
toJson q = case q of
  Quantity.Literal n -> Common.tagged "Literal" . Just $ Common.integer n
  Quantity.ManaValue -> Common.nullary "ManaValue"
  Quantity.Power -> Common.nullary "Power"
  Quantity.X -> Common.nullary "X"
  Quantity.InSlot s -> Common.tagged "InSlot" . Just $ SlotName.toJson s
  Quantity.Star -> Common.nullary "Star"
  Quantity.Plus a b -> Common.tagged "Plus" . Just . Common.array $ [toJson a, toJson b]
  Quantity.Count c -> Count.toJson toJson c

fromJson :: Value.Value -> Either Text.Text Quantity.Quantity
fromJson value = do
  (t, mv) <- Common.asTagged value
  case (t, mv) of
    ("Literal", Just v) -> Quantity.Literal <$> Common.asInteger v
    ("ManaValue", _) -> Right Quantity.ManaValue
    ("Power", _) -> Right Quantity.Power
    ("X", _) -> Right Quantity.X
    ("InSlot", Just v) -> Quantity.InSlot <$> SlotName.fromJson v
    ("Star", _) -> Right Quantity.Star
    ("Plus", Just (Value.Array (Array.MkArray [x, y]))) -> Quantity.Plus <$> fromJson x <*> fromJson y
    -- Count.fromJson re-derives the tag from the WHOLE value (see the comment on
    -- toJson) rather than from `mv`, which has already had it stripped.
    ("Count", _) -> Quantity.Count <$> Count.fromJson fromJson value
    _ -> Left . Text.pack $ "unknown Quantity: " <> t

fromJsonPair :: Value.Value -> Either Text.Text (Quantity.Quantity, Quantity.Quantity)
fromJsonPair value = case value of
  Value.Array (Array.MkArray [p, t]) -> do
    p_ <- fromJson p
    t_ <- fromJson t
    pure (p_, t_)
  _ -> Left $ Text.pack "expected a [power, toughness] quantity pair"
