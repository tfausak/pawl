module Pawl.Codec.CopyStackObjectSpec where

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Codec.CopyStackObject as CopyStackObject
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CopyException as CopyException
import qualified Pawl.Types.CopyStackObject as CopyStackObject
import qualified Pawl.Types.CopyTargets as CopyTargets
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Supertype as Supertype

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CopyStackObject" $ do
  -- CR 707.10 alone: the copy keeps the original's targets, so the key that
  -- says otherwise is absent.
  Spec.it s "MkCopyStackObject, the original's targets: the key is omitted" $
    Common.assertCodec
      s
      (CopyStackObject.codec Common.text)
      (CopyStackObject.MkCopyStackObject (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "spell"))) CopyTargets.Copied CopyStackObject.defaultQuantity CopyStackObject.defaultCopier [])
      " {\"ref\":{\"type\":\"InSlot\",\"value\":\"spell\"}} "
  -- CR 707.10c, Twincast's second sentence.
  Spec.it s "MkCopyStackObject, new targets offered: it is written" $
    Common.assertCodec
      s
      (CopyStackObject.codec Common.text)
      (CopyStackObject.MkCopyStackObject (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "spell"))) CopyTargets.ChosenByController CopyStackObject.defaultQuantity CopyStackObject.defaultCopier [])
      " {\"ref\":{\"type\":\"InSlot\",\"value\":\"spell\"},\"targets\":{\"type\":\"ChosenByController\"}} "
  -- CR 707.10d, Zada, Hedron Grinder's second sentence.
  Spec.it s "MkCopyStackObject, one copy per candidate: the candidates' ref rides in the payload" $
    Common.assertCodec
      s
      (CopyStackObject.codec Common.text)
      (CopyStackObject.MkCopyStackObject (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "spell"))) (CopyTargets.ForEach (ObjectRef.EachMatching (Filter.ControlledBy PlayerRelation.You))) CopyStackObject.defaultQuantity CopyStackObject.defaultCopier [])
      " {\"ref\":{\"type\":\"InSlot\",\"value\":\"spell\"},\"targets\":{\"type\":\"ForEach\",\"value\":{\"type\":\"EachMatching\",\"value\":{\"type\":\"ControlledBy\",\"value\":{\"type\":\"You\"}}}}} "
  -- CR 702.40a, storm's "copy it for each": a count other than one is written.
  Spec.it s "MkCopyStackObject, a count: it is written" $
    Common.assertCodec
      s
      (CopyStackObject.codec Common.text)
      (CopyStackObject.MkCopyStackObject (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "spell"))) CopyTargets.ChosenByController Quantity.SpellsCastBefore CopyStackObject.defaultCopier [])
      " {\"ref\":{\"type\":\"InSlot\",\"value\":\"spell\"},\"targets\":{\"type\":\"ChosenByController\"},\"quantity\":{\"type\":\"SpellsCastBefore\"}} "
  -- CR 707.10, Meletis Charlatan's "the controller of target ... spell copies it".
  Spec.it s "MkCopyStackObject, another player copies it: the copier is written" $
    Common.assertCodec
      s
      (CopyStackObject.codec Common.text)
      (CopyStackObject.MkCopyStackObject (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "spell"))) CopyTargets.ChosenByController CopyStackObject.defaultQuantity (PlayerRef.ControllerOfBound (SlotName.MkSlotName (Text.pack "spell"))) [])
      " {\"ref\":{\"type\":\"InSlot\",\"value\":\"spell\"},\"targets\":{\"type\":\"ChosenByController\"},\"copier\":{\"type\":\"ControllerOfBound\",\"value\":\"spell\"}} "
  -- CR 707.9b, Double Major's "except it isn't legendary".
  Spec.it s "MkCopyStackObject, an exception: it is written" $
    Common.assertCodec
      s
      (CopyStackObject.codec Common.text)
      (CopyStackObject.MkCopyStackObject (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "spell"))) CopyTargets.Copied CopyStackObject.defaultQuantity CopyStackObject.defaultCopier [CopyException.RemoveSupertypes (Set.singleton Supertype.Legendary)])
      " {\"ref\":{\"type\":\"InSlot\",\"value\":\"spell\"},\"exceptions\":[{\"type\":\"RemoveSupertypes\",\"value\":[{\"type\":\"Legendary\"}]}]} "
  Spec.it s "has a schema" $ Common.assertHasSchema s (CopyStackObject.codec Common.text)
