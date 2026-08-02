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
    Common.assertJsonCodec s Quantity.toJson Quantity.fromJson (Quantity.Literal 5) "{\"type\":\"Literal\",\"value\":5}"
  Spec.it s "ManaValue" $
    Common.assertJsonCodec s Quantity.toJson Quantity.fromJson Quantity.ManaValue "{\"type\":\"ManaValue\"}"
  -- CR 208.1, Ghitu Fire-Eater's "damage equal to its power". Nullary like
  -- ManaValue, and NOT to be confused with the Power newtype, which wraps a
  -- printed power/toughness box.
  Spec.it s "Power" $
    Common.assertJsonCodec s Quantity.toJson Quantity.fromJson Quantity.Power "{\"type\":\"Power\"}"
  Spec.it s "X" $
    Common.assertJsonCodec s Quantity.toJson Quantity.fromJson Quantity.X "{\"type\":\"X\"}"
  -- Bane of Progress' "for each permanent destroyed this way": a number an
  -- earlier effect of the same resolution bound into a slot. Unlike X, it
  -- carries the slot name on the wire -- nested under Plus, since composition
  -- is where a recursive decoder loses a payload.
  Spec.it s "InSlot, bare and nested" $ do
    let slot = SlotName.MkSlotName (Text.pack "destroyed")
    Common.assertJsonCodec s Quantity.toJson Quantity.fromJson (Quantity.InSlot slot) "{\"type\":\"InSlot\",\"value\":\"destroyed\"}"
    Common.assertJsonCodec
      s
      Quantity.toJson
      Quantity.fromJson
      (Quantity.Plus (Quantity.Literal 1) (Quantity.InSlot slot))
      "{\"type\":\"Plus\",\"value\":[{\"type\":\"Literal\",\"value\":1},{\"type\":\"InSlot\",\"value\":\"destroyed\"}]}"
  Spec.it s "Star" $
    Common.assertJsonCodec s Quantity.toJson Quantity.fromJson Quantity.Star "{\"type\":\"Star\"}"
  Spec.it s "Plus" $
    Common.assertJsonCodec
      s
      Quantity.toJson
      Quantity.fromJson
      (Quantity.Plus (Quantity.Literal 1) Quantity.Star)
      "{\"type\":\"Plus\",\"value\":[{\"type\":\"Literal\",\"value\":1},{\"type\":\"Star\"}]}"
  -- Task 5: Quantity.Count's arm shares Count's own "Count" tag rather than
  -- wrapping it in a second one, so `Quantity.Count c` is byte-for-byte the
  -- JSON `Count.toJson Quantity.toJson c` used to produce.
  Spec.it s "Count shares Count's own tag, not double-tagged" $
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
      "{\"type\":\"Count\",\"value\":[{\"type\":\"InZone\",\"value\":[{\"type\":\"Graveyard\"},{\"type\":\"EachPlayer\"}]},{\"type\":\"And\",\"value\":[]},{\"type\":\"DistinctCardTypes\"}]}"
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
      "{\"type\":\"Count\",\"value\":[{\"type\":\"InZone\",\"value\":[{\"type\":\"Battlefield\"},{\"type\":\"EachPlayer\"}]},{\"type\":\"And\",\"value\":[]},{\"type\":\"Greatest\",\"value\":{\"type\":\"Count\",\"value\":[{\"type\":\"InZone\",\"value\":[{\"type\":\"Graveyard\"},{\"type\":\"EachPlayer\"}]},{\"type\":\"And\",\"value\":[]},{\"type\":\"DistinctCardTypes\"}]}}]}"
  Spec.describe s "fromJsonPair" . Spec.it s "the [power, toughness] characteristicPT pair" $
    Common.assertFromJson
      s
      Quantity.fromJsonPair
      "[{\"type\":\"Literal\",\"value\":1},{\"type\":\"Star\"}]"
      (Quantity.Literal 1, Quantity.Star)
