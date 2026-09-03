module Pawl.Codec.SacrificeEffectSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.SacrificeEffect as SacrificeEffect
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.SacrificeEffect as SacrificeEffect
import qualified Pawl.Types.Sacrificer as Sacrificer
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.SacrificeEffect" $ do
  -- CR 701.21a's ordinary form, which every card in the pool but Golgothian
  -- Sylex prints: the sacrificer is the default, so no key is written.
  Spec.it s "the effect's controller writes no sacrificer" $
    Common.assertCodec
      s
      SacrificeEffect.codec
      SacrificeEffect.MkSacrificeEffect {SacrificeEffect.ref = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target")), SacrificeEffect.sacrificer = Sacrificer.EffectController}
      " {\"ref\":{\"type\":\"InSlot\",\"value\":\"target\"}} "
  -- CR 701.54c's form, which is what makes the field more than a constant.
  Spec.it s "the permanent's controller is written" $
    Common.assertCodec
      s
      SacrificeEffect.codec
      SacrificeEffect.MkSacrificeEffect {SacrificeEffect.ref = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "thatBlocker")), SacrificeEffect.sacrificer = Sacrificer.PermanentController}
      " {\"ref\":{\"type\":\"InSlot\",\"value\":\"thatBlocker\"},\"sacrificer\":{\"type\":\"PermanentController\"}} "
  -- Golgothian Sylex's form: the sweep a bare SlotName could not express, with
  -- the other sacrificer beside it.
  Spec.it s "a filtered sweep round-trips" $
    Common.assertCodec
      s
      SacrificeEffect.codec
      SacrificeEffect.MkSacrificeEffect {SacrificeEffect.ref = ObjectRef.EachMatching (Filter.Not Filter.IsToken), SacrificeEffect.sacrificer = Sacrificer.PermanentController}
      " {\"ref\":{\"type\":\"EachMatching\",\"value\":{\"type\":\"Not\",\"value\":{\"type\":\"IsToken\"}}},\"sacrificer\":{\"type\":\"PermanentController\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s SacrificeEffect.codec
