{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.QuantitySpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Aggregation as Aggregation
import qualified Pawl.Types.Count as Count
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.Scope as Scope
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Quantity" $ do
  Spec.it s "Literal" $
    Common.assertJsonCodec
      s
      Quantity.toJson
      Quantity.fromJson
      (Quantity.Literal 5)
      """ {"type":"Literal","value":5} """
  Spec.it s "ManaValue" $
    Common.assertJsonCodec
      s
      Quantity.toJson
      Quantity.fromJson
      Quantity.ManaValue
      """ {"type":"ManaValue"} """
  -- CR 208.1. Nullary like ManaValue, and NOT to be confused with the Power
  -- newtype, which wraps a printed power/toughness box.
  Spec.it s "Power" $
    Common.assertJsonCodec
      s
      Quantity.toJson
      Quantity.fromJson
      Quantity.Power
      """ {"type":"Power"} """
  -- A number an earlier effect of the same resolution bound into a slot. Unlike
  -- X it carries the slot name on the wire, nested under Plus here since
  -- composition is where a recursive decoder loses a payload.
  Spec.it s "InSlot, bare and nested" $ do
    let slot = SlotName.MkSlotName (Text.pack "destroyed")
    Common.assertJsonCodec
      s
      Quantity.toJson
      Quantity.fromJson
      (Quantity.InSlot slot)
      """ {"type":"InSlot","value":"destroyed"} """
    Common.assertJsonCodec
      s
      Quantity.toJson
      Quantity.fromJson
      (Quantity.Plus (Quantity.Literal 1) (Quantity.InSlot slot))
      """ {"type":"Plus","value":[{"type":"Literal","value":1},{"type":"InSlot","value":"destroyed"}]} """
  Spec.it s "Star" $
    Common.assertJsonCodec
      s
      Quantity.toJson
      Quantity.fromJson
      Quantity.Star
      """ {"type":"Star"} """
  Spec.it s "Plus" $
    Common.assertJsonCodec
      s
      Quantity.toJson
      Quantity.fromJson
      (Quantity.Plus (Quantity.Literal 1) Quantity.Star)
      """ {"type":"Plus","value":[{"type":"Literal","value":1},{"type":"Star"}]} """
  -- Quantity.Count's arm is tagged here and nowhere else: Pawl.Codec.Count
  -- writes a bare object, so a Count payload can never be double-tagged.
  Spec.it s "Count is tagged by Quantity, and the payload is Count's bare object" $
    Common.assertJsonCodec
      s
      Quantity.toJson
      Quantity.fromJson
      ( Quantity.Count
          ( Count.MkCount
              (Scope.InZone Zone.Graveyard PlayerRef.EachPlayer)
              (Filter.And [])
              Aggregation.DistinctCardTypes
          )
      )
      """ {"type":"Count","value":{"scope":{"type":"InZone","value":[{"type":"Graveyard"},{"type":"EachPlayer"}]},"filter":{"type":"And","value":[]},"aggregation":{"type":"DistinctCardTypes"}}} """
  -- Greatest's payload is a whole Quantity rather than a nullary tag, so a
  -- per-member quantity that is itself a Count has to round-trip.
  Spec.it s "Greatest round-trips a nested Count payload" $
    Common.assertJsonCodec
      s
      Quantity.toJson
      Quantity.fromJson
      ( Quantity.Count
          ( Count.MkCount
              (Scope.InZone Zone.Battlefield PlayerRef.EachPlayer)
              (Filter.And [])
              ( Aggregation.Greatest
                  ( Quantity.Count
                      ( Count.MkCount
                          (Scope.InZone Zone.Graveyard PlayerRef.EachPlayer)
                          (Filter.And [])
                          Aggregation.DistinctCardTypes
                      )
                  )
              )
          )
      )
      """ {"type":"Count","value":{"scope":{"type":"InZone","value":[{"type":"Battlefield"},{"type":"EachPlayer"}]},"filter":{"type":"And","value":[]},"aggregation":{"type":"Greatest","value":{"type":"Count","value":{"scope":{"type":"InZone","value":[{"type":"Graveyard"},{"type":"EachPlayer"}]},"filter":{"type":"And","value":[]},"aggregation":{"type":"DistinctCardTypes"}}}}}} """
  -- CR 119.1, with the PlayerRef on the wire saying whose. Serra Avatar's "your"
  -- is the Relative arm; the InSlot arm below is the one a recursive decoder
  -- could lose a payload through, so both are round-tripped.
  Spec.it s "LifeTotal, relative and from a slot" $ do
    Common.assertJsonCodec
      s
      Quantity.toJson
      Quantity.fromJson
      (Quantity.LifeTotal (PlayerRef.Relative PlayerRelation.You))
      """ {"type":"LifeTotal","value":{"type":"Relative","value":{"type":"You"}}} """
    Common.assertJsonCodec
      s
      Quantity.toJson
      Quantity.fromJson
      (Quantity.LifeTotal (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))))
      """ {"type":"LifeTotal","value":{"type":"InSlot","value":"target"}} """
  -- CR 725.1, with a PlayerRef and nothing else on the wire: the arm answers a
  -- 0/1 rather than carrying a number. Dawnglade Regent's "you're the monarch"
  -- is the Relative arm; the InSlot arm beside it is the one a recursive decoder
  -- could lose a payload through, so both are round-tripped, as LifeTotal's are.
  Spec.it s "IsMonarch, relative and from a slot" $ do
    Common.assertJsonCodec
      s
      Quantity.toJson
      Quantity.fromJson
      (Quantity.IsMonarch (PlayerRef.Relative PlayerRelation.You))
      """ {"type":"IsMonarch","value":{"type":"Relative","value":{"type":"You"}}} """
    Common.assertJsonCodec
      s
      Quantity.toJson
      Quantity.fromJson
      (Quantity.IsMonarch (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))))
      """ {"type":"IsMonarch","value":{"type":"InSlot","value":"target"}} """
  -- CR 122.1, with BOTH halves on the wire: a PlayerRef saying whose and a
  -- PlayerCounterKind saying which. Rule 728.1's reading is the Relative one.
  Spec.it s "PlayerCounters, relative and from a slot" $ do
    Common.assertJsonCodec
      s
      Quantity.toJson
      Quantity.fromJson
      (Quantity.PlayerCounters (PlayerRef.Relative PlayerRelation.You) PlayerCounterKind.Rad)
      """ {"type":"PlayerCounters","value":[{"type":"Relative","value":{"type":"You"}},{"type":"Rad"}]} """
    Common.assertJsonCodec
      s
      Quantity.toJson
      Quantity.fromJson
      (Quantity.PlayerCounters (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) PlayerCounterKind.Experience)
      """ {"type":"PlayerCounters","value":[{"type":"InSlot","value":"target"},{"type":"Experience"}]} """
  -- CR 122.1's OBJECT reading, with only a CounterKind on the wire: the object
  -- is whichever one the quantity is evaluated against, so there is no reference
  -- beside the kind. The payload-bearing CounterKind arm (CR 122.1b's keyword
  -- counter) is round-tripped too, since that is the one a recursive decoder
  -- could lose.
  Spec.it s "ObjectCounters, a plain kind and a payload-bearing one" $ do
    Common.assertJsonCodec
      s
      Quantity.toJson
      Quantity.fromJson
      (Quantity.ObjectCounters CounterKind.PlusOnePlusOne)
      """ {"type":"ObjectCounters","value":{"type":"PlusOnePlusOne"}} """
    Common.assertJsonCodec
      s
      Quantity.toJson
      Quantity.fromJson
      (Quantity.ObjectCounters (CounterKind.Keyword Keyword.Flying))
      """ {"type":"ObjectCounters","value":{"type":"Keyword","value":{"type":"Flying"}}} """
  Spec.describe s "fromJsonPair" . Spec.it s "the [power, toughness] characteristicPT pair" $
    Common.assertFromJson
      s
      Quantity.fromJsonPair
      "[{\"type\":\"Literal\",\"value\":1},{\"type\":\"Star\"}]"
      (Quantity.Literal 1, Quantity.Star)
