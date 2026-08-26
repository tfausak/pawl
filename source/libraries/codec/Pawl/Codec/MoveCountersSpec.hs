module Pawl.Codec.MoveCountersSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.MoveCounters as MoveCounters
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.MoveCounters as MoveCounters
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.MoveCounters" $ do
  -- CR 122.5's pair: the object a counter leaves and the object it lands on.
  -- Agent's Toolkit's "move a counter" names no kind, and this is the wire form
  -- data/cards/agents-toolkit.json writes.
  Spec.it s "MkMoveCounters, no kind named" $
    Common.assertCodec
      s
      MoveCounters.codec
      ( MoveCounters.MkMoveCounters
          { MoveCounters.from = SlotName.MkSlotName (Text.pack "self"),
            MoveCounters.kind = Nothing,
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
            MoveCounters.to = SlotName.MkSlotName (Text.pack "target")
          }
      )
      " {\"from\":\"self\",\"kind\":{\"type\":\"PlusOnePlusOne\"},\"to\":\"target\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s MoveCounters.codec
