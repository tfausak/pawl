module Pawl.Codec.Quantity where

import qualified Pawl.Codec.Count as Count
import qualified Pawl.Codec.CounterKind as CounterKind
import qualified Pawl.Codec.Designation as Designation
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.ManaCount as ManaCount
import qualified Pawl.Codec.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Quantity as Quantity

-- | Quantity.Count's arm is tagged HERE, like every other arm. Pawl.Codec.Count
-- writes a bare object, so the tag that picks this arm has to come from the
-- dispatching type, which is this one.
--
-- RECURSIVE, and directly so: @Plus@, @Negate@, @AgainstSlot@ and the @Count@
-- arm all name 'codec' itself, where the loose pair named its own 'toJson'. That
-- ties the same knot Pawl.Codec.Filter already ties at the value level, and
-- Pawl.JsonSchema.Define.define breaks it at the schema level by registering the
-- name before running the body -- so the schema emits a @$ref@ on re-entry rather
-- than diverging.
--
-- Converting this converts 'Pawl.Codec.Count' and 'Pawl.Codec.Aggregation' with
-- it: both are parametric in the quantity and cannot take a @Codec q@ until
-- there is one to give them (#1306).
codec :: Codec.Codec Quantity.Quantity
codec =
  Arm.tagged
    encode
    [ Arm.payload "Literal" Common.integer Quantity.Literal,
      Arm.nullary "ManaValue" Quantity.ManaValue,
      Arm.nullary "Power" Quantity.Power,
      Arm.nullary "Toughness" Quantity.Toughness,
      Arm.payload "InSlot" SlotName.codec Quantity.InSlot,
      Arm.nullary "Star" Quantity.Star,
      Arm.payload "Plus" pairCodec (uncurry Quantity.Plus),
      -- CR 107.1b's negative game value: one whole Quantity on the wire, since a
      -- minus sign carries nothing of its own.
      Arm.payload "Negate" codec Quantity.Negate,
      Arm.payload "Count" (Count.codec codec) Quantity.Count,
      Arm.payload "ManaCount" ManaCount.codec Quantity.ManaCount,
      Arm.payload "LifeTotal" PlayerRef.codec Quantity.LifeTotal,
      Arm.payload "Speed" PlayerRef.codec Quantity.Speed,
      -- CR 725.1's designation, with only a PlayerRef on the wire: the answer is
      -- a 0/1 rather than a stored number, so there is nothing beside the
      -- reference.
      Arm.payload "IsMonarch" PlayerRef.codec Quantity.IsMonarch,
      Arm.payload "PlayerCounters" (Common.tuple PlayerRef.codec PlayerCounterKind.codec) (uncurry Quantity.PlayerCounters),
      -- CR 122.1's OBJECT reading: only a kind on the wire, since the object is
      -- whichever one the quantity is evaluated against (Pawl.Types.Quantity).
      Arm.payload "ObjectCounters" (CounterKind.codec Keyword.codec) Quantity.ObjectCounters,
      -- CR 702.112b's designation, read against the object the quantity is aimed
      -- at, so only the designation is on the wire.
      Arm.payload "HasDesignation" Designation.codec Quantity.HasDesignation,
      Arm.nullary "WasKicked" Quantity.WasKicked,
      -- CR 508.3b's record, with only a PlayerRef on the wire: what is counted
      -- comes from the combat record rather than from anything the card names.
      Arm.payload "OpponentsAttacked" PlayerRef.codec Quantity.OpponentsAttacked,
      -- CR 701.9a's tally, with only a PlayerRef on the wire for
      -- OpponentsAttacked's reason: what is counted comes from the event log
      -- rather than from anything the card names, and the turn is the log's
      -- extent rather than a window a card could state.
      Arm.payload "CardsDiscardedThisTurn" PlayerRef.codec Quantity.CardsDiscardedThisTurn,
      -- CR 509.1h's declaration read against the object the quantity is aimed at,
      -- so there is nothing on the wire at all -- Power's shape rather than
      -- ObjectCounters'.
      Arm.nullary "BlockersBeyondFirst" Quantity.BlockersBeyondFirst,
      -- A slot and a whole Quantity, in that order: which object to aim at, then
      -- what to read off it.
      Arm.payload "AgainstSlot" (Common.tuple SlotName.codec codec) (uncurry Quantity.AgainstSlot)
    ]
  where
    encode q = case q of
      Quantity.Literal n -> Common.tagged "Literal" . Just $ Value.integer n
      Quantity.ManaValue -> Common.nullary "ManaValue"
      Quantity.Power -> Common.nullary "Power"
      Quantity.Toughness -> Common.nullary "Toughness"
      Quantity.InSlot s -> Common.tagged "InSlot" . Just $ Codec.encode SlotName.codec s
      Quantity.Star -> Common.nullary "Star"
      Quantity.Plus a b -> Common.tagged "Plus" . Just . Value.array $ [encode a, encode b]
      Quantity.Negate a -> Common.tagged "Negate" . Just $ encode a
      Quantity.Count c -> Common.tagged "Count" . Just $ Codec.encode (Count.codec codec) c
      Quantity.ManaCount c -> Common.tagged "ManaCount" . Just $ Codec.encode ManaCount.codec c
      Quantity.LifeTotal p -> Common.tagged "LifeTotal" . Just $ Codec.encode PlayerRef.codec p
      Quantity.Speed p -> Common.tagged "Speed" . Just $ Codec.encode PlayerRef.codec p
      Quantity.IsMonarch p -> Common.tagged "IsMonarch" . Just $ Codec.encode PlayerRef.codec p
      Quantity.PlayerCounters p k ->
        Common.tagged "PlayerCounters" . Just . Value.array $
          [Codec.encode PlayerRef.codec p, Codec.encode PlayerCounterKind.codec k]
      Quantity.ObjectCounters k -> Common.tagged "ObjectCounters" . Just $ Codec.encode (CounterKind.codec Keyword.codec) k
      Quantity.HasDesignation d -> Common.tagged "HasDesignation" . Just $ Codec.encode Designation.codec d
      Quantity.WasKicked -> Common.nullary "WasKicked"
      Quantity.OpponentsAttacked p -> Common.tagged "OpponentsAttacked" . Just $ Codec.encode PlayerRef.codec p
      Quantity.CardsDiscardedThisTurn p -> Common.tagged "CardsDiscardedThisTurn" . Just $ Codec.encode PlayerRef.codec p
      Quantity.BlockersBeyondFirst -> Common.nullary "BlockersBeyondFirst"
      Quantity.AgainstSlot s q_ ->
        Common.tagged "AgainstSlot" . Just . Value.array $
          [Codec.encode SlotName.codec s, encode q_]

-- | CR 208.2's characteristic-defining [power, toughness] pair, which
-- Pawl.Codec.ProjectedCharacteristics stores.
pairCodec :: Codec.Codec (Quantity.Quantity, Quantity.Quantity)
pairCodec = Common.tuple codec codec
