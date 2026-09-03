module Pawl.Codec.SlotSacrificeSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.SlotSacrifice as SlotSacrifice
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Sacrificer as Sacrificer
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.SlotSacrifice as SlotSacrifice

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.SlotSacrifice" $ do
  -- CR 701.21a's ordinary form, which every card in the pool prints: the
  -- sacrificer is the default, so no key is written.
  Spec.it s "the effect's controller writes no sacrificer" $
    Common.assertCodec
      s
      SlotSacrifice.codec
      SlotSacrifice.MkSlotSacrifice {SlotSacrifice.slot = SlotName.MkSlotName (Text.pack "target"), SlotSacrifice.sacrificer = Sacrificer.EffectController}
      " {\"slot\":\"target\"} "
  -- CR 701.54c's form, which is what makes the field more than a constant.
  Spec.it s "the permanent's controller is written" $
    Common.assertCodec
      s
      SlotSacrifice.codec
      SlotSacrifice.MkSlotSacrifice {SlotSacrifice.slot = SlotName.MkSlotName (Text.pack "thatBlocker"), SlotSacrifice.sacrificer = Sacrificer.PermanentController}
      " {\"slot\":\"thatBlocker\",\"sacrificer\":{\"type\":\"PermanentController\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s SlotSacrifice.codec
