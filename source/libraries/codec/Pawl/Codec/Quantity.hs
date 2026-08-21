module Pawl.Codec.Quantity where

import qualified Pawl.Codec.AgainstSlot as AgainstSlot
import qualified Pawl.Codec.Count as Count
import qualified Pawl.Codec.CounterKind as CounterKind
import qualified Pawl.Codec.Designation as Designation
import qualified Pawl.Codec.Halved as Halved
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.ManaCount as ManaCount
import qualified Pawl.Codec.PlayerCounterTally as PlayerCounterTally
import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.Codec.Plus as Plus
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Quantity as Quantity

-- | Quantity.Count's arm is tagged HERE, like every other arm. Pawl.Codec.Count
-- writes a bare object, so the tag that picks this arm has to come from the
-- dispatching type, which is this one.
--
-- RECURSIVE, and directly so: @Negate@ names 'codec' itself, and @Plus@,
-- @Halved@, @AgainstSlot@ and @Count@ each pass it to a payload codec that is
-- parametric in the quantity for exactly that reason. That ties the same knot
-- Pawl.Codec.Filter already ties at the value level, and
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
    [ Arm.payload "Literal" Common.integer Quantity.Literal (\x -> case x of Quantity.Literal y -> Just y; _ -> Nothing),
      Arm.nullary "ManaValue" Quantity.ManaValue,
      Arm.nullary "Power" Quantity.Power,
      Arm.nullary "Toughness" Quantity.Toughness,
      Arm.payload "InSlot" SlotName.codec Quantity.InSlot (\x -> case x of Quantity.InSlot y -> Just y; _ -> Nothing),
      Arm.nullary "Star" Quantity.Star,
      Arm.payload "Plus" (Plus.codec codec) Quantity.Plus (\x -> case x of Quantity.Plus y -> Just y; _ -> Nothing),
      -- CR 107.1a's rounding first, then what is halved: the direction is the
      -- card's word and the payload is the value it applies to.
      Arm.payload "Halved" (Halved.codec codec) Quantity.Halved (\x -> case x of Quantity.Halved y -> Just y; _ -> Nothing),
      -- CR 107.1b's negative game value: one whole Quantity on the wire, since a
      -- minus sign carries nothing of its own.
      Arm.payload "Negate" codec Quantity.Negate (\x -> case x of Quantity.Negate y -> Just y; _ -> Nothing),
      Arm.payload "Count" (Count.codec codec) Quantity.Count (\x -> case x of Quantity.Count y -> Just y; _ -> Nothing),
      Arm.payload "ManaCount" ManaCount.codec Quantity.ManaCount (\x -> case x of Quantity.ManaCount y -> Just y; _ -> Nothing),
      Arm.payload "LifeTotal" PlayerRef.codec Quantity.LifeTotal (\x -> case x of Quantity.LifeTotal y -> Just y; _ -> Nothing),
      Arm.payload "Speed" PlayerRef.codec Quantity.Speed (\x -> case x of Quantity.Speed y -> Just y; _ -> Nothing),
      -- CR 725.1's designation, with only a PlayerRef on the wire: the answer is
      -- a 0/1 rather than a stored number, so there is nothing beside the
      -- reference.
      Arm.payload "IsMonarch" PlayerRef.codec Quantity.IsMonarch (\x -> case x of Quantity.IsMonarch y -> Just y; _ -> Nothing),
      Arm.payload "IsStartingPlayer" PlayerRef.codec Quantity.IsStartingPlayer (\x -> case x of Quantity.IsStartingPlayer y -> Just y; _ -> Nothing),
      Arm.payload "PlayerCounters" PlayerCounterTally.codec Quantity.PlayerCounters (\x -> case x of Quantity.PlayerCounters y -> Just y; _ -> Nothing),
      -- CR 122.1's OBJECT reading: only a kind on the wire, since the object is
      -- whichever one the quantity is evaluated against (Pawl.Types.Quantity).
      Arm.payload "ObjectCounters" (CounterKind.codec Keyword.codec) Quantity.ObjectCounters (\x -> case x of Quantity.ObjectCounters y -> Just y; _ -> Nothing),
      -- CR 702.112b's designation, read against the object the quantity is aimed
      -- at, so only the designation is on the wire.
      Arm.payload "HasDesignation" Designation.codec Quantity.HasDesignation (\x -> case x of Quantity.HasDesignation y -> Just y; _ -> Nothing),
      Arm.nullary "WasKicked" Quantity.WasKicked,
      -- CR 107.4h's third sentence, with nothing on the wire either: which tag is
      -- asked about is the constructor (Pawl.Types.Quantity), and the object is
      -- whichever one the quantity is evaluated against.
      Arm.nullary "SnowWasSpent" Quantity.SnowWasSpent,
      Arm.nullary "ClassLevel" Quantity.ClassLevel,
      -- CR 508.3b's record, with only a PlayerRef on the wire: what is counted
      -- comes from the combat record rather than from anything the card names.
      Arm.payload "OpponentsAttacked" PlayerRef.codec Quantity.OpponentsAttacked (\x -> case x of Quantity.OpponentsAttacked y -> Just y; _ -> Nothing),
      -- CR 701.9a's tally, with only a PlayerRef on the wire for
      -- OpponentsAttacked's reason: what is counted comes from the event log
      -- rather than from anything the card names, and the turn is the log's
      -- extent rather than a window a card could state.
      Arm.payload "CardsDiscardedThisTurn" PlayerRef.codec Quantity.CardsDiscardedThisTurn (\x -> case x of Quantity.CardsDiscardedThisTurn y -> Just y; _ -> Nothing),
      -- CR 120.1's damage, with only a PlayerRef on the wire for
      -- CardsDiscardedThisTurn's reason above. The threshold that turns the count
      -- into "an opponent was dealt damage" is the Comparison's, not this arm's.
      Arm.payload "PlayersDealtDamageThisTurn" PlayerRef.codec Quantity.PlayersDealtDamageThisTurn (\x -> case x of Quantity.PlayersDealtDamageThisTurn y -> Just y; _ -> Nothing),
      -- CR 400.7's entry read against the object the quantity is aimed at, so
      -- there is nothing on the wire: the turn is the log's extent rather than a
      -- window a card could state, as for CardsDiscardedThisTurn above.
      Arm.nullary "EnteredThisTurn" Quantity.EnteredThisTurn,
      -- CR 509.1h's declaration read against the object the quantity is aimed at,
      -- so there is nothing on the wire at all -- Power's shape rather than
      -- ObjectCounters'.
      Arm.nullary "BlockersBeyondFirst" Quantity.BlockersBeyondFirst,
      -- A slot and a whole Quantity, in that order: which object to aim at, then
      -- what to read off it.
      Arm.payload "AgainstSlot" (AgainstSlot.codec codec) Quantity.AgainstSlot (\x -> case x of Quantity.AgainstSlot y -> Just y; _ -> Nothing)
    ]
