{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.QuantitySpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Aggregation as Aggregation
import qualified Pawl.Types.Count as Count
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.PlayerRef as PlayerRef
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
  -- CR 208.1, Ghitu Fire-Eater's "damage equal to its power". Nullary like
  -- ManaValue, and NOT to be confused with the Power newtype, which wraps a
  -- printed power/toughness box.
  Spec.it s "Power" $
    Common.assertJsonCodec
      s
      Quantity.toJson
      Quantity.fromJson
      Quantity.Power
      """ {"type":"Power"} """
  Spec.it s "X" $
    Common.assertJsonCodec
      s
      Quantity.toJson
      Quantity.fromJson
      Quantity.X
      """ {"type":"X"} """
  -- Bane of Progress' "for each permanent destroyed this way": a number an
  -- earlier effect of the same resolution bound into a slot. Unlike X, it
  -- carries the slot name on the wire -- nested under Plus, since composition
  -- is where a recursive decoder loses a payload.
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
  -- writes a bare object, so this pins the one "Count" tag in the encoding --
  -- the arm reads like every other arm, and a Count payload can never be
  -- double-tagged because only one level writes a tag at all.
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
  -- The arm that proves the Greatest payload is a whole Quantity rather than a
  -- nullary tag: a Greatest whose per-member quantity is itself a Count
  -- round-trips, which is the recursion Pawl.Types.Quantity's parameter exists
  -- to permit.
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
  Spec.describe s "fromJsonPair" . Spec.it s "the [power, toughness] characteristicPT pair" $
    Common.assertFromJson
      s
      Quantity.fromJsonPair
      "[{\"type\":\"Literal\",\"value\":1},{\"type\":\"Star\"}]"
      (Quantity.Literal 1, Quantity.Star)
