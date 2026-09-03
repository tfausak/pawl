module Pawl.Codec.CopyStackObjectSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.CopyStackObject as CopyStackObject
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CopyStackObject as CopyStackObject
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CopyStackObject" $ do
  -- CR 707.10 alone: the copy keeps the original's targets, so the key that
  -- says otherwise is absent.
  Spec.it s "MkCopyStackObject, the original's targets: the offer is omitted" $
    Common.assertCodec
      s
      CopyStackObject.codec
      (CopyStackObject.MkCopyStackObject (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "spell"))) False)
      " {\"ref\":{\"type\":\"InSlot\",\"value\":\"spell\"}} "
  -- CR 707.10c, Twincast's second sentence.
  Spec.it s "MkCopyStackObject, new targets offered: it is written" $
    Common.assertCodec
      s
      CopyStackObject.codec
      (CopyStackObject.MkCopyStackObject (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "spell"))) True)
      " {\"newTargets\":true,\"ref\":{\"type\":\"InSlot\",\"value\":\"spell\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s CopyStackObject.codec
