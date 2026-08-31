module Pawl.Codec.MoveCountersSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.MoveCounters as MoveCounters
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AgainstSlot as AgainstSlot
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.MoveCounters as MoveCounters
import qualified Pawl.Types.MovedKinds as MovedKinds
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.MoveCounters" $ do
  -- CR 122.5's pair: the objects counters leave and the objects they land on,
  -- both ObjectRefs and so both spelled the same way. Agent's Toolkit's "move a counter"
  -- names no kind, moves one and is not looked back at, so both defaults hold and
  -- this is the wire form data/cards/agents-toolkit.json writes.
  Spec.it s "MkMoveCounters, no kind named" $
    Common.assertCodec
      s
      MoveCounters.codec
      ( MoveCounters.MkMoveCounters
          { MoveCounters.from = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "self")),
            MoveCounters.kinds = MovedKinds.Chosen (Quantity.Literal 1),
            MoveCounters.slot = Nothing,
            MoveCounters.to = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "became"))
          }
      )
      " {\"from\":{\"type\":\"InSlot\",\"value\":\"self\"},\"to\":{\"type\":\"InSlot\",\"value\":\"became\"}} "
  -- Explorer's Cache's "move a +1/+1 counter" names one.
  Spec.it s "MkMoveCounters, a named kind" $
    Common.assertCodec
      s
      MoveCounters.codec
      ( MoveCounters.MkMoveCounters
          { MoveCounters.from = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "self")),
            MoveCounters.kinds = MovedKinds.Named CounterKind.PlusOnePlusOne (Quantity.Literal 1),
            MoveCounters.slot = Nothing,
            MoveCounters.to = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))
          }
      )
      " {\"from\":{\"type\":\"InSlot\",\"value\":\"self\"},\"kinds\":{\"type\":\"Named\",\"value\":{\"count\":{\"type\":\"Literal\",\"value\":1},\"kind\":{\"type\":\"PlusOnePlusOne\"}}},\"to\":{\"type\":\"InSlot\",\"value\":\"target\"}} "
  -- Fate Transfer's "move all counters": no kind named and none asked for, since
  -- every kind crosses. This is the wire form data/cards/fate-transfer.json
  -- writes.
  Spec.it s "MkMoveCounters, every kind" $
    Common.assertCodec
      s
      MoveCounters.codec
      ( MoveCounters.MkMoveCounters
          { MoveCounters.from = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "from")),
            MoveCounters.kinds = MovedKinds.Every,
            MoveCounters.slot = Nothing,
            MoveCounters.to = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "to"))
          }
      )
      " {\"from\":{\"type\":\"InSlot\",\"value\":\"from\"},\"kinds\":{\"type\":\"Every\"},\"to\":{\"type\":\"InSlot\",\"value\":\"to\"}} "
  -- Black Panther, Wakandan King's "move all +1/+1 counters ... If one or more
  -- +1/+1 counters are moved this way": the count is the tally of that kind on the
  -- object a slot names, and `slot` is where what actually moved is written back.
  -- This is the wire form data/cards/black-panther-wakandan-king.json writes.
  Spec.it s "MkMoveCounters, a whole tally moved and counted" $
    Common.assertCodec
      s
      MoveCounters.codec
      ( MoveCounters.MkMoveCounters
          { MoveCounters.from = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "land")),
            MoveCounters.kinds =
              MovedKinds.Named
                CounterKind.PlusOnePlusOne
                ( Quantity.AgainstSlot
                    ( AgainstSlot.MkAgainstSlot
                        { AgainstSlot.slot = SlotName.MkSlotName (Text.pack "land"),
                          AgainstSlot.quantity = Quantity.ObjectCounters CounterKind.PlusOnePlusOne
                        }
                    )
                ),
            MoveCounters.slot = Just (SlotName.MkSlotName (Text.pack "moved")),
            MoveCounters.to = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "creature"))
          }
      )
      " {\"from\":{\"type\":\"InSlot\",\"value\":\"land\"},\"kinds\":{\"type\":\"Named\",\"value\":{\"count\":{\"type\":\"AgainstSlot\",\"value\":{\"quantity\":{\"type\":\"ObjectCounters\",\"value\":{\"type\":\"PlusOnePlusOne\"}},\"slot\":\"land\"}},\"kind\":{\"type\":\"PlusOnePlusOne\"}}},\"slot\":\"moved\",\"to\":{\"type\":\"InSlot\",\"value\":\"creature\"}} "
  -- Spike Cannibal's "move all +1/+1 counters from all creatures onto it": the
  -- first side is a battlefield sweep rather than a slot, and the kind is named
  -- where the count is not. This is the wire form data/cards/spike-cannibal.json
  -- writes, and the case that FORCES an EveryOfKind arm on Pawl.Codec.MovedKinds
  -- -- Arm.tagged compiles with no arm and answers Nothing on encode (#2262).
  Spec.it s "MkMoveCounters, a whole tally off a group" $
    Common.assertCodec
      s
      MoveCounters.codec
      ( MoveCounters.MkMoveCounters
          { MoveCounters.from = ObjectRef.EachMatching (Filter.HasCardType CardType.Creature),
            MoveCounters.kinds = MovedKinds.EveryOfKind CounterKind.PlusOnePlusOne,
            MoveCounters.slot = Nothing,
            MoveCounters.to = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "self"))
          }
      )
      " {\"from\":{\"type\":\"EachMatching\",\"value\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}},\"kinds\":{\"type\":\"EveryOfKind\",\"value\":{\"type\":\"PlusOnePlusOne\"}},\"to\":{\"type\":\"InSlot\",\"value\":\"self\"}} "
  -- Forgotten Ancient's "move any number of +1/+1 counters from this creature
  -- onto other creatures": the SECOND side is the battlefield sweep this time,
  -- which is the case that forces the destination to be an ObjectRef rather than
  -- a slot. This is the wire form data/cards/forgotten-ancient.json writes.
  Spec.it s "MkMoveCounters, a group of destinations" $
    Common.assertCodec
      s
      MoveCounters.codec
      ( MoveCounters.MkMoveCounters
          { MoveCounters.from = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "self")),
            MoveCounters.kinds = MovedKinds.AnyNumberOfKind CounterKind.PlusOnePlusOne,
            MoveCounters.slot = Nothing,
            MoveCounters.to = ObjectRef.EachMatching (Filter.And [Filter.HasCardType CardType.Creature, Filter.Not Filter.IsSource])
          }
      )
      " {\"from\":{\"type\":\"InSlot\",\"value\":\"self\"},\"kinds\":{\"type\":\"AnyNumberOfKind\",\"value\":{\"type\":\"PlusOnePlusOne\"}},\"to\":{\"type\":\"EachMatching\",\"value\":{\"type\":\"And\",\"value\":[{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}},{\"type\":\"Not\",\"value\":{\"type\":\"IsSource\"}}]}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s MoveCounters.codec
