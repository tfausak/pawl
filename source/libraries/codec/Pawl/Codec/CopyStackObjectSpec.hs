module Pawl.Codec.CopyStackObjectSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.CopyStackObject as CopyStackObject
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CopyStackObject as CopyStackObject
import qualified Pawl.Types.CopyTargets as CopyTargets
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CopyStackObject" $ do
  -- CR 707.10 alone: the copy keeps the original's targets, so the key that
  -- says otherwise is absent.
  Spec.it s "MkCopyStackObject, the original's targets: the key is omitted" $
    Common.assertCodec
      s
      CopyStackObject.codec
      (CopyStackObject.MkCopyStackObject (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "spell"))) CopyTargets.Copied)
      " {\"ref\":{\"type\":\"InSlot\",\"value\":\"spell\"}} "
  -- CR 707.10c, Twincast's second sentence.
  Spec.it s "MkCopyStackObject, new targets offered: it is written" $
    Common.assertCodec
      s
      CopyStackObject.codec
      (CopyStackObject.MkCopyStackObject (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "spell"))) CopyTargets.ChosenByController)
      " {\"ref\":{\"type\":\"InSlot\",\"value\":\"spell\"},\"targets\":{\"type\":\"ChosenByController\"}} "
  -- CR 707.10d, Zada, Hedron Grinder's second sentence.
  Spec.it s "MkCopyStackObject, one copy per candidate: the candidates' ref rides in the payload" $
    Common.assertCodec
      s
      CopyStackObject.codec
      (CopyStackObject.MkCopyStackObject (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "spell"))) (CopyTargets.ForEach (ObjectRef.EachMatching (Filter.ControlledBy PlayerRelation.You))))
      " {\"ref\":{\"type\":\"InSlot\",\"value\":\"spell\"},\"targets\":{\"type\":\"ForEach\",\"value\":{\"type\":\"EachMatching\",\"value\":{\"type\":\"ControlledBy\",\"value\":{\"type\":\"You\"}}}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s CopyStackObject.codec
