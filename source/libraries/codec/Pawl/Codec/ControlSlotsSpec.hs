module Pawl.Codec.ControlSlotsSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.ControlSlots as ControlSlots
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ControlSlots as ControlSlots
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ControlSlots" $ do
  -- Distinct names, so a codec swapping the two sides disagrees.
  Spec.it s "MkControlSlots" $
    Common.assertCodec
      s
      ControlSlots.codec
      (ControlSlots.MkControlSlots (SlotName.MkSlotName (Text.pack "first")) (SlotName.MkSlotName (Text.pack "second")))
      " {\"first\":\"first\",\"second\":\"second\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ControlSlots.codec
