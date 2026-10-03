module Pawl.Codec.TargetChooserSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.TargetChooser as TargetChooser
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.TargetChooser as TargetChooser

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.TargetChooser" $ do
  Spec.it s "Relative" $
    Common.assertCodec
      s
      TargetChooser.codec
      (TargetChooser.Relative PlayerRelation.Opponent)
      " {\"type\":\"Relative\",\"value\":{\"type\":\"Opponent\"}} "
  Spec.it s "InSlot" $
    Common.assertCodec
      s
      TargetChooser.codec
      (TargetChooser.InSlot (SlotName.MkSlotName (Text.pack "thatAttackingPlayer")))
      " {\"type\":\"InSlot\",\"value\":\"thatAttackingPlayer\"} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s TargetChooser.codec
