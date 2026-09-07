module Pawl.Codec.DesignateSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.Designate as Designate
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Designate as Designate
import qualified Pawl.Types.Designation as Designation
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Designate" $ do
  -- CR 701.60 / CR 702.112: give the slot's object this designation.
  Spec.it s "MkDesignate, the two required keys" $
    Common.assertCodec
      s
      Designate.codec
      ( Designate.MkDesignate
          { Designate.designation = Designation.Suspected,
            Designate.slot = SlotName.MkSlotName (Text.pack "self"),
            Designate.value = Nothing
          }
      )
      " {\"designation\":{\"type\":\"Suspected\"},\"slot\":\"self\"} "
  -- CR 701.37c's X, which only a "Monstrosity X" writes.
  Spec.it s "MkDesignate carries the value the mark was set with" $
    Common.assertCodec
      s
      Designate.codec
      ( Designate.MkDesignate
          { Designate.designation = Designation.Monstrous,
            Designate.slot = SlotName.MkSlotName (Text.pack "self"),
            Designate.value = Just (Quantity.InSlot (SlotName.MkSlotName (Text.pack "X")))
          }
      )
      " {\"designation\":{\"type\":\"Monstrous\"},\"slot\":\"self\",\"value\":{\"type\":\"InSlot\",\"value\":\"X\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s Designate.codec
