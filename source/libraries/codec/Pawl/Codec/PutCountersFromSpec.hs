module Pawl.Codec.PutCountersFromSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.PutCountersFrom as PutCountersFrom
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.PutCountersFrom as PutCountersFrom
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.PutCountersFrom" $ do
  -- No kind and no count: CR 122.8's whole per-kind tally is what crosses, so
  -- the two keys are the object read and the objects written to.
  Spec.it s "MkPutCountersFrom, both keys" $
    Common.assertCodec
      s
      PutCountersFrom.codec
      ( PutCountersFrom.MkPutCountersFrom
          { PutCountersFrom.from = SlotName.MkSlotName (Text.pack "self"),
            PutCountersFrom.ref = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))
          }
      )
      " {\"from\":\"self\",\"ref\":{\"type\":\"InSlot\",\"value\":\"target\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s PutCountersFrom.codec
