module Pawl.Codec.Quantity where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Count as Count
import qualified Pawl.Codec.CounterKind as CounterKind
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.ManaCount as ManaCount
import qualified Pawl.Codec.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Codec.PlayerRef as PlayerRef
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
  Quantity.Toughness -> Common.nullary "Toughness"
  Quantity.InSlot s -> Common.tagged "InSlot" . Just $ SlotName.toJson s
  Quantity.Star -> Common.nullary "Star"
  Quantity.Plus a b -> Common.tagged "Plus" . Just . Common.array $ [toJson a, toJson b]
  Quantity.Count c -> Common.tagged "Count" . Just $ Count.toJson toJson c
  Quantity.ManaCount c -> Common.tagged "ManaCount" . Just $ ManaCount.toJson c
  Quantity.LifeTotal p -> Common.tagged "LifeTotal" . Just $ PlayerRef.toJson p
  Quantity.Speed p -> Common.tagged "Speed" . Just $ PlayerRef.toJson p
  -- CR 725.1's designation, with only a PlayerRef on the wire: the answer is a
  -- 0/1 rather than a stored number, so there is nothing beside the reference.
  Quantity.IsMonarch p -> Common.tagged "IsMonarch" . Just $ PlayerRef.toJson p
  Quantity.PlayerCounters p k -> Common.tagged "PlayerCounters" . Just . Common.array $ [PlayerRef.toJson p, PlayerCounterKind.toJson k]
  -- CR 122.1's OBJECT reading: only a kind on the wire, since the object is
  -- whichever one the quantity is evaluated against (Pawl.Types.Quantity).
  Quantity.ObjectCounters k -> Common.tagged "ObjectCounters" . Just $ CounterKind.toJson Keyword.toJson k
  -- CR 702.112b's designation, with nothing on the wire: the object is whichever
  -- one the quantity is evaluated against, and the answer is a 0/1 rather than a
  -- stored number, so there is neither a reference nor a payload.
  Quantity.IsRenowned -> Common.nullary "IsRenowned"
  Quantity.IsMonstrous -> Common.nullary "IsMonstrous"
  -- CR 508.3b's record, with only a PlayerRef on the wire: what is counted comes
  -- from the combat record rather than from anything the card names.
  Quantity.OpponentsAttacked p -> Common.tagged "OpponentsAttacked" . Just $ PlayerRef.toJson p
  -- CR 509.1h's declaration read against the object the quantity is aimed at, so
  -- there is nothing on the wire at all -- Power's shape rather than
  -- ObjectCounters'.
  Quantity.BlockersBeyondFirst -> Common.nullary "BlockersBeyondFirst"
  -- A slot and a whole Quantity, in that order: which object to aim at, then what
  -- to read off it.
  Quantity.AgainstSlot s q_ -> Common.tagged "AgainstSlot" . Just . Common.array $ [SlotName.toJson s, toJson q_]

fromJson :: Value.Value -> Either Text.Text Quantity.Quantity
fromJson value = do
  (t, mv) <- Common.asTagged value
  case (t, mv) of
    ("Literal", Just v) -> Quantity.Literal <$> Common.asInteger v
    ("ManaValue", _) -> Right Quantity.ManaValue
    ("Power", _) -> Right Quantity.Power
    ("Toughness", _) -> Right Quantity.Toughness
    ("InSlot", Just v) -> Quantity.InSlot <$> SlotName.fromJson v
    ("Star", _) -> Right Quantity.Star
    ("Plus", Just (Value.Array (Array.MkArray [x, y]))) -> Quantity.Plus <$> fromJson x <*> fromJson y
    ("Count", Just v) -> Quantity.Count <$> Count.fromJson fromJson v
    ("ManaCount", Just v) -> Quantity.ManaCount <$> ManaCount.fromJson v
    ("LifeTotal", Just v) -> Quantity.LifeTotal <$> PlayerRef.fromJson v
    ("Speed", Just v) -> Quantity.Speed <$> PlayerRef.fromJson v
    ("IsMonarch", Just v) -> Quantity.IsMonarch <$> PlayerRef.fromJson v
    ("PlayerCounters", Just (Value.Array (Array.MkArray [p, k]))) -> Quantity.PlayerCounters <$> PlayerRef.fromJson p <*> PlayerCounterKind.fromJson k
    ("ObjectCounters", Just v) -> Quantity.ObjectCounters <$> CounterKind.fromJson Keyword.fromJson v
    ("IsRenowned", _) -> Right Quantity.IsRenowned
    ("IsMonstrous", _) -> Right Quantity.IsMonstrous
    ("OpponentsAttacked", Just v) -> Quantity.OpponentsAttacked <$> PlayerRef.fromJson v
    ("BlockersBeyondFirst", _) -> Right Quantity.BlockersBeyondFirst
    ("AgainstSlot", Just (Value.Array (Array.MkArray [s, q]))) -> Quantity.AgainstSlot <$> SlotName.fromJson s <*> fromJson q
    _ -> Left . Text.pack $ "unknown Quantity: " <> t

fromJsonPair :: Value.Value -> Either Text.Text (Quantity.Quantity, Quantity.Quantity)
fromJsonPair value = case value of
  Value.Array (Array.MkArray [p, t]) -> do
    p_ <- fromJson p
    t_ <- fromJson t
    pure (p_, t_)
  _ -> Left $ Text.pack "expected a [power, toughness] quantity pair"
