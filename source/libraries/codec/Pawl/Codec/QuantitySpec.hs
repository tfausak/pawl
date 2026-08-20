module Pawl.Codec.QuantitySpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AgainstSlot as AgainstSlot
import qualified Pawl.Types.Aggregation as Aggregation
import qualified Pawl.Types.Count as Count
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Designation as Designation
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Halved as Halved
import qualified Pawl.Types.InZone as InZone
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.PlayerCounterTally as PlayerCounterTally
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Plus as Plus
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.Rounding as Rounding
import qualified Pawl.Types.Scope as Scope
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Quantity" $ do
  Spec.it s "Literal" $
    Common.assertCodec
      s
      Quantity.codec
      (Quantity.Literal 5)
      " {\"type\":\"Literal\",\"value\":5} "
  Spec.it s "ManaValue" $
    Common.assertCodec
      s
      Quantity.codec
      Quantity.ManaValue
      " {\"type\":\"ManaValue\"} "
  -- CR 208.1. Nullary like ManaValue, and NOT to be confused with the Power
  -- newtype, which wraps a printed power/toughness box.
  Spec.it s "Power" $
    Common.assertCodec
      s
      Quantity.codec
      Quantity.Power
      " {\"type\":\"Power\"} "
  -- CR 208.1's other half, and Power's sibling on the wire as in the type.
  Spec.it s "Toughness" $
    Common.assertCodec
      s
      Quantity.codec
      Quantity.Toughness
      " {\"type\":\"Toughness\"} "
  -- A number an earlier effect of the same resolution bound into a slot. Unlike
  -- X it carries the slot name on the wire, nested under Plus here since
  -- composition is where a recursive decoder loses a payload.
  Spec.it s "InSlot, bare and nested" $ do
    let slot = SlotName.MkSlotName (Text.pack "destroyed")
    Common.assertCodec
      s
      Quantity.codec
      (Quantity.InSlot slot)
      " {\"type\":\"InSlot\",\"value\":\"destroyed\"} "
    Common.assertCodec
      s
      Quantity.codec
      (Quantity.Plus (Plus.MkPlus (Quantity.Literal 1) (Quantity.InSlot slot)))
      " {\"type\":\"Plus\",\"value\":{\"left\":{\"type\":\"Literal\",\"value\":1},\"right\":{\"type\":\"InSlot\",\"value\":\"destroyed\"}}} "
  Spec.it s "Star" $
    Common.assertCodec
      s
      Quantity.codec
      Quantity.Star
      " {\"type\":\"Star\"} "
  Spec.it s "Plus" $
    Common.assertCodec
      s
      Quantity.codec
      (Quantity.Plus (Plus.MkPlus (Quantity.Literal 1) Quantity.Star))
      " {\"type\":\"Plus\",\"value\":{\"left\":{\"type\":\"Literal\",\"value\":1},\"right\":{\"type\":\"Star\"}}} "
  -- Toxic Deluge's -X on the wire: one whole Quantity under the tag, not a pair.
  -- The second case nests a NEGATIVE Literal under it -- the other way this type
  -- says a negative number -- so a decoder that folded the two into one could not
  -- round-trip it.
  Spec.it s "Negate, over a slot read and over a literal" $ do
    let slot = SlotName.MkSlotName (Text.pack "X")
    Common.assertCodec
      s
      Quantity.codec
      (Quantity.Negate (Quantity.InSlot slot))
      " {\"type\":\"Negate\",\"value\":{\"type\":\"InSlot\",\"value\":\"X\"}} "
    Common.assertCodec
      s
      Quantity.codec
      (Quantity.Negate (Quantity.Literal (-2)))
      " {\"type\":\"Negate\",\"value\":{\"type\":\"Literal\",\"value\":-2}} "
  -- Quantity.Count's arm is tagged here and nowhere else: Pawl.Codec.Count
  -- writes a bare object, so a Count payload can never be double-tagged.
  Spec.it s "Count is tagged by Quantity, and the payload is Count's bare object" $
    Common.assertCodec
      s
      Quantity.codec
      ( Quantity.Count
          ( Count.MkCount
              (Scope.InZone (InZone.MkInZone Zone.Graveyard PlayerRef.EachPlayer))
              (Filter.And [])
              Aggregation.DistinctCardTypes
          )
      )
      " {\"type\":\"Count\",\"value\":{\"scope\":{\"type\":\"InZone\",\"value\":{\"zone\":{\"type\":\"Graveyard\"},\"player\":{\"type\":\"EachPlayer\"}}},\"filter\":{\"type\":\"And\",\"value\":[]},\"aggregation\":{\"type\":\"DistinctCardTypes\"}}} "
  -- Greatest's payload is a whole Quantity rather than a nullary tag, so a
  -- per-member quantity that is itself a Count has to round-trip.
  Spec.it s "Greatest round-trips a nested Count payload" $
    Common.assertCodec
      s
      Quantity.codec
      ( Quantity.Count
          ( Count.MkCount
              (Scope.InZone (InZone.MkInZone Zone.Battlefield PlayerRef.EachPlayer))
              (Filter.And [])
              ( Aggregation.Greatest
                  ( Quantity.Count
                      ( Count.MkCount
                          (Scope.InZone (InZone.MkInZone Zone.Graveyard PlayerRef.EachPlayer))
                          (Filter.And [])
                          Aggregation.DistinctCardTypes
                      )
                  )
              )
          )
      )
      " {\"type\":\"Count\",\"value\":{\"scope\":{\"type\":\"InZone\",\"value\":{\"zone\":{\"type\":\"Battlefield\"},\"player\":{\"type\":\"EachPlayer\"}}},\"filter\":{\"type\":\"And\",\"value\":[]},\"aggregation\":{\"type\":\"Greatest\",\"value\":{\"type\":\"Count\",\"value\":{\"scope\":{\"type\":\"InZone\",\"value\":{\"zone\":{\"type\":\"Graveyard\"},\"player\":{\"type\":\"EachPlayer\"}}},\"filter\":{\"type\":\"And\",\"value\":[]},\"aggregation\":{\"type\":\"DistinctCardTypes\"}}}}}} "
  -- CR 119.1, with the PlayerRef on the wire saying whose. Serra Avatar's "your"
  -- is the Relative arm; the InSlot arm below is the one a recursive decoder
  -- could lose a payload through, so both are round-tripped.
  Spec.it s "LifeTotal, relative and from a slot" $ do
    Common.assertCodec
      s
      Quantity.codec
      (Quantity.LifeTotal (PlayerRef.Relative PlayerRelation.You))
      " {\"type\":\"LifeTotal\",\"value\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}}} "
    Common.assertCodec
      s
      Quantity.codec
      (Quantity.LifeTotal (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))))
      " {\"type\":\"LifeTotal\",\"value\":{\"type\":\"InSlot\",\"value\":\"target\"}} "
  -- CR 725.1, with a PlayerRef and nothing else on the wire: the arm answers a
  -- 0/1 rather than carrying a number. Dawnglade Regent's "you're the monarch"
  -- is the Relative arm; the InSlot arm beside it is the one a recursive decoder
  -- could lose a payload through, so both are round-tripped, as LifeTotal's are.
  Spec.it s "IsMonarch, relative and from a slot" $ do
    Common.assertCodec
      s
      Quantity.codec
      (Quantity.IsMonarch (PlayerRef.Relative PlayerRelation.You))
      " {\"type\":\"IsMonarch\",\"value\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}}} "
    Common.assertCodec
      s
      Quantity.codec
      (Quantity.IsMonarch (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))))
      " {\"type\":\"IsMonarch\",\"value\":{\"type\":\"InSlot\",\"value\":\"target\"}} "
  -- CR 103.1, IsMonarch's shape and round-tripped the same two ways: Gemstone
  -- Caverns' gate is the Relative arm, and the InSlot arm beside it is the one a
  -- recursive decoder could lose a payload through.
  Spec.it s "IsStartingPlayer, relative and from a slot" $ do
    Common.assertCodec
      s
      Quantity.codec
      (Quantity.IsStartingPlayer (PlayerRef.Relative PlayerRelation.You))
      " {\"type\":\"IsStartingPlayer\",\"value\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}}} "
    Common.assertCodec
      s
      Quantity.codec
      (Quantity.IsStartingPlayer (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))))
      " {\"type\":\"IsStartingPlayer\",\"value\":{\"type\":\"InSlot\",\"value\":\"target\"}} "
  -- CR 702.112b, CR 701.37b and CR 701.60b: WHICH designation is on the wire and
  -- nothing else, the object it is asked of riding the evaluation rather than a
  -- reference -- so this is Power's shape rather than IsMonarch's.
  Spec.it s "HasDesignation Renowned" $
    Common.assertCodec
      s
      Quantity.codec
      (Quantity.HasDesignation Designation.Renowned)
      " {\"type\":\"HasDesignation\",\"value\":{\"type\":\"Renowned\"}} "
  Spec.it s "HasDesignation Monstrous" $
    Common.assertCodec
      s
      Quantity.codec
      (Quantity.HasDesignation Designation.Monstrous)
      " {\"type\":\"HasDesignation\",\"value\":{\"type\":\"Monstrous\"}} "
  Spec.it s "HasDesignation Suspected" $
    Common.assertCodec
      s
      Quantity.codec
      (Quantity.HasDesignation Designation.Suspected)
      " {\"type\":\"HasDesignation\",\"value\":{\"type\":\"Suspected\"}} "
  -- CR 702.33d, with nothing on the wire: a spell's kicked flag is not a member of
  -- Pawl.Types.Designation, so it keeps a tag of its own.
  Spec.it s "WasKicked" $
    Common.assertCodec
      s
      Quantity.codec
      Quantity.WasKicked
      " {\"type\":\"WasKicked\"} "
  -- CR 122.1, with BOTH halves on the wire: a PlayerRef saying whose and a
  -- PlayerCounterKind saying which. Rule 728.1's reading is the Relative one.
  Spec.it s "PlayerCounters, relative and from a slot" $ do
    Common.assertCodec
      s
      Quantity.codec
      (Quantity.PlayerCounters (PlayerCounterTally.MkPlayerCounterTally (PlayerRef.Relative PlayerRelation.You) PlayerCounterKind.Rad))
      " {\"type\":\"PlayerCounters\",\"value\":{\"player\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}},\"kind\":{\"type\":\"Rad\"}}} "
    Common.assertCodec
      s
      Quantity.codec
      (Quantity.PlayerCounters (PlayerCounterTally.MkPlayerCounterTally (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) PlayerCounterKind.Experience))
      " {\"type\":\"PlayerCounters\",\"value\":{\"player\":{\"type\":\"InSlot\",\"value\":\"target\"},\"kind\":{\"type\":\"Experience\"}}} "
  -- CR 122.1's OBJECT reading, with only a CounterKind on the wire: the object
  -- is whichever one the quantity is evaluated against, so there is no reference
  -- beside the kind. The payload-bearing CounterKind arm (CR 122.1b's keyword
  -- counter) is round-tripped too, since that is the one a recursive decoder
  -- could lose.
  Spec.it s "ObjectCounters, a plain kind and a payload-bearing one" $ do
    Common.assertCodec
      s
      Quantity.codec
      (Quantity.ObjectCounters CounterKind.PlusOnePlusOne)
      " {\"type\":\"ObjectCounters\",\"value\":{\"type\":\"PlusOnePlusOne\"}} "
    Common.assertCodec
      s
      Quantity.codec
      (Quantity.ObjectCounters (CounterKind.Keyword Keyword.Flying))
      " {\"type\":\"ObjectCounters\",\"value\":{\"type\":\"Keyword\",\"value\":{\"type\":\"Flying\"}}} "
  -- CR 508.3b, with a PlayerRef and nothing else on the wire: what is counted is
  -- the combat record. Rule 702.121a's melee is the Relative arm; the InSlot arm
  -- beside it is the one a recursive decoder could lose a payload through, as
  -- LifeTotal's and IsMonarch's are.
  Spec.it s "OpponentsAttacked, relative and from a slot" $ do
    Common.assertCodec
      s
      Quantity.codec
      (Quantity.OpponentsAttacked (PlayerRef.Relative PlayerRelation.You))
      " {\"type\":\"OpponentsAttacked\",\"value\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}}} "
    Common.assertCodec
      s
      Quantity.codec
      (Quantity.OpponentsAttacked (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))))
      " {\"type\":\"OpponentsAttacked\",\"value\":{\"type\":\"InSlot\",\"value\":\"target\"}} "
  -- CR 701.9a, with a PlayerRef and nothing else on the wire: what is counted is
  -- the turn-scoped event log. Asmoranomardicadaistinaculdacar's is the Relative
  -- arm; the InSlot arm beside it is the one a recursive decoder could lose a
  -- payload through, as OpponentsAttacked's is.
  Spec.it s "CardsDiscardedThisTurn, relative and from a slot" $ do
    Common.assertCodec
      s
      Quantity.codec
      (Quantity.CardsDiscardedThisTurn (PlayerRef.Relative PlayerRelation.You))
      " {\"type\":\"CardsDiscardedThisTurn\",\"value\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}}} "
    Common.assertCodec
      s
      Quantity.codec
      (Quantity.CardsDiscardedThisTurn (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))))
      " {\"type\":\"CardsDiscardedThisTurn\",\"value\":{\"type\":\"InSlot\",\"value\":\"target\"}} "
  -- CR 120.1, on CardsDiscardedThisTurn's terms: a PlayerRef and nothing else,
  -- with the same recursive-decoder pair. Furious Spinesplitter's is the Relative
  -- arm, over Opponent rather than You.
  Spec.it s "PlayersDealtDamageThisTurn, relative and from a slot" $ do
    Common.assertCodec
      s
      Quantity.codec
      (Quantity.PlayersDealtDamageThisTurn (PlayerRef.Relative PlayerRelation.Opponent))
      " {\"type\":\"PlayersDealtDamageThisTurn\",\"value\":{\"type\":\"Relative\",\"value\":{\"type\":\"Opponent\"}}} "
    Common.assertCodec
      s
      Quantity.codec
      (Quantity.PlayersDealtDamageThisTurn (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))))
      " {\"type\":\"PlayersDealtDamageThisTurn\",\"value\":{\"type\":\"InSlot\",\"value\":\"target\"}} "
  -- CR 400.7 with NOTHING on the wire either: the object is the one the quantity
  -- is evaluated against, and "this turn" is the event log's own extent.
  Spec.it s "EnteredThisTurn is nullary" $
    Common.assertCodec
      s
      Quantity.codec
      Quantity.EnteredThisTurn
      " {\"type\":\"EnteredThisTurn\"} "
  -- CR 509.1h with NOTHING on the wire: the object is the one the quantity is
  -- evaluated against, so this is a bare tag like Power and ManaValue.
  Spec.it s "BlockersBeyondFirst is nullary" $
    Common.assertCodec
      s
      Quantity.codec
      Quantity.BlockersBeyondFirst
      " {\"type\":\"BlockersBeyondFirst\"} "
  -- A slot and a payload on the wire. Nested once, since a recursive decoder is
  -- where a payload gets lost -- and the inner arm is one whose OWN answer the
  -- slot moves, which is the whole point of the constructor.
  Spec.it s "AgainstSlot, bare and nested" $ do
    let slot = SlotName.MkSlotName (Text.pack "creature")
    Common.assertCodec
      s
      Quantity.codec
      (Quantity.AgainstSlot (AgainstSlot.MkAgainstSlot slot Quantity.Power))
      " {\"type\":\"AgainstSlot\",\"value\":{\"slot\":\"creature\",\"quantity\":{\"type\":\"Power\"}}} "
    Common.assertCodec
      s
      Quantity.codec
      (Quantity.Plus (Plus.MkPlus (Quantity.Literal 1) (Quantity.AgainstSlot (AgainstSlot.MkAgainstSlot slot (Quantity.ObjectCounters CounterKind.PlusOnePlusOne)))))
      " {\"type\":\"Plus\",\"value\":{\"left\":{\"type\":\"Literal\",\"value\":1},\"right\":{\"type\":\"AgainstSlot\",\"value\":{\"slot\":\"creature\",\"quantity\":{\"type\":\"ObjectCounters\",\"value\":{\"type\":\"PlusOnePlusOne\"}}}}}} "
  -- CR 107.1a's direction and the value it applies to, in that order. Nested
  -- once for AgainstSlot's reason -- a recursive decoder is where a payload gets
  -- lost -- and over a Count, which is the shape both producers print.
  Spec.it s "Halved, both directions" $ do
    Common.assertCodec
      s
      Quantity.codec
      (Quantity.Halved (Halved.MkHalved Rounding.Down (Quantity.Literal 5)))
      " {\"type\":\"Halved\",\"value\":{\"rounding\":{\"type\":\"Down\"},\"quantity\":{\"type\":\"Literal\",\"value\":5}}} "
    Common.assertCodec
      s
      Quantity.codec
      (Quantity.Halved (Halved.MkHalved Rounding.Up (Quantity.LifeTotal PlayerRef.Candidate)))
      " {\"type\":\"Halved\",\"value\":{\"rounding\":{\"type\":\"Up\"},\"quantity\":{\"type\":\"LifeTotal\",\"value\":{\"type\":\"Candidate\"}}}} "
  -- Forcing the schema is what proves the RECURSIVE definition terminates, and
  -- it proves more of that now than it used to: Negate names `codec` itself
  -- while Plus, Halved, AgainstSlot and Count each hand it to a payload codec in
  -- another module, so the loop runs through four siblings. assertHasSchema
  -- renders the whole tree rather than just its outer tag, so a definition that
  -- failed to emit a $ref on re-entry would hang here rather than pass.
  Spec.it s "has a schema" $ Common.assertHasSchema s Quantity.codec
