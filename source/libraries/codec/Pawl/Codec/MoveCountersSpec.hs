module Pawl.Codec.MoveCountersSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.MoveCounters as MoveCounters
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AgainstSlot as AgainstSlot
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.MoveCounters as MoveCounters
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.MoveCounters" $ do
  -- CR 122.5's pair: the object counters leave and the object they land on.
  -- Agent's Toolkit's "move a counter" names no kind, moves one and is not looked
  -- back at, so all three defaults hold and this is the wire form
  -- data/cards/agents-toolkit.json writes.
  Spec.it s "MkMoveCounters, no kind named" $
    Common.assertCodec
      s
      MoveCounters.codec
      ( MoveCounters.MkMoveCounters
          { MoveCounters.from = SlotName.MkSlotName (Text.pack "self"),
            MoveCounters.kind = Nothing,
            MoveCounters.quantity = Quantity.Literal 1,
            MoveCounters.slot = Nothing,
            MoveCounters.to = SlotName.MkSlotName (Text.pack "became")
          }
      )
      " {\"from\":\"self\",\"to\":\"became\"} "
  -- Explorer's Cache's "move a +1/+1 counter" names one.
  Spec.it s "MkMoveCounters, a named kind" $
    Common.assertCodec
      s
      MoveCounters.codec
      ( MoveCounters.MkMoveCounters
          { MoveCounters.from = SlotName.MkSlotName (Text.pack "self"),
            MoveCounters.kind = Just CounterKind.PlusOnePlusOne,
            MoveCounters.quantity = Quantity.Literal 1,
            MoveCounters.slot = Nothing,
            MoveCounters.to = SlotName.MkSlotName (Text.pack "target")
          }
      )
      " {\"from\":\"self\",\"kind\":{\"type\":\"PlusOnePlusOne\"},\"to\":\"target\"} "
  -- Black Panther, Wakandan King's "move all +1/+1 counters ... If one or more
  -- +1/+1 counters are moved this way": the count is the tally of that kind on the
  -- object the `from` slot names, and `slot` is where what actually moved is
  -- written back. This is the wire form data/cards/black-panther-wakandan-king.json
  -- writes.
  Spec.it s "MkMoveCounters, a whole tally moved and counted" $
    Common.assertCodec
      s
      MoveCounters.codec
      ( MoveCounters.MkMoveCounters
          { MoveCounters.from = SlotName.MkSlotName (Text.pack "land"),
            MoveCounters.kind = Just CounterKind.PlusOnePlusOne,
            MoveCounters.quantity =
              Quantity.AgainstSlot
                ( AgainstSlot.MkAgainstSlot
                    { AgainstSlot.slot = SlotName.MkSlotName (Text.pack "land"),
                      AgainstSlot.quantity = Quantity.ObjectCounters CounterKind.PlusOnePlusOne
                    }
                ),
            MoveCounters.slot = Just (SlotName.MkSlotName (Text.pack "moved")),
            MoveCounters.to = SlotName.MkSlotName (Text.pack "creature")
          }
      )
      " {\"from\":\"land\",\"kind\":{\"type\":\"PlusOnePlusOne\"},\"quantity\":{\"type\":\"AgainstSlot\",\"value\":{\"quantity\":{\"type\":\"ObjectCounters\",\"value\":{\"type\":\"PlusOnePlusOne\"}},\"slot\":\"land\"}},\"slot\":\"moved\",\"to\":\"creature\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s MoveCounters.codec
