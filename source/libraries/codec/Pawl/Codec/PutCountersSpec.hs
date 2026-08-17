module Pawl.Codec.PutCountersSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.PutCounters as PutCounters
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.PutCounters as PutCounters
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.PutCounters" $ do
  -- An ObjectRef and not a bare slot, which is what lets Renegade Krasis' swept
  -- set be written (CR 115.10a).
  Spec.it s "MkPutCounters, all three keys" $
    Common.assertCodec
      s
      PutCounters.codec
      ( PutCounters.MkPutCounters
          { PutCounters.kind = CounterKind.PlusOnePlusOne,
            PutCounters.quantity = Quantity.Literal 1,
            PutCounters.ref = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))
          }
      )
      " {\"kind\":{\"type\":\"PlusOnePlusOne\"},\"quantity\":{\"type\":\"Literal\",\"value\":1},\"ref\":{\"type\":\"InSlot\",\"value\":\"target\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s PutCounters.codec
