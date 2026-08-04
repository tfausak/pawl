module Pawl.Codec.Quantity where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Count as Count
import qualified Pawl.Codec.ManaCount as ManaCount
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.Json.Array as Array
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.Quantity as Quantity

-- | Quantity.Count's arm is tagged HERE, like every other arm. Pawl.Codec.Count
-- writes a bare object, so the tag that picks this arm has to come from the
-- dispatching type, which is this one.
toJson :: Quantity.Quantity -> Value.Value
toJson q = case q of
  Quantity.Literal n -> Common.tagged "Literal" . Just $ Common.integer n
  Quantity.ManaValue -> Common.nullary "ManaValue"
  Quantity.Power -> Common.nullary "Power"
  Quantity.X -> Common.nullary "X"
  Quantity.InSlot s -> Common.tagged "InSlot" . Just $ SlotName.toJson s
  Quantity.Star -> Common.nullary "Star"
  Quantity.Plus a b -> Common.tagged "Plus" . Just . Common.array $ [toJson a, toJson b]
  Quantity.Count c -> Common.tagged "Count" . Just $ Count.toJson toJson c
  Quantity.ManaCount c -> Common.tagged "ManaCount" . Just $ ManaCount.toJson c

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
    ("Count", Just v) -> Quantity.Count <$> Count.fromJson fromJson v
    ("ManaCount", Just v) -> Quantity.ManaCount <$> ManaCount.fromJson v
    _ -> Left . Text.pack $ "unknown Quantity: " <> t

fromJsonPair :: Value.Value -> Either Text.Text (Quantity.Quantity, Quantity.Quantity)
fromJsonPair value = case value of
  Value.Array (Array.MkArray [p, t]) -> do
    p_ <- fromJson p
    t_ <- fromJson t
    pure (p_, t_)
  _ -> Left $ Text.pack "expected a [power, toughness] quantity pair"
