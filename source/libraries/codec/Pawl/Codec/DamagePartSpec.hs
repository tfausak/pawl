module Pawl.Codec.DamagePartSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.DamagePart as DamagePart
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.DamagePart as DamagePart
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.DamagePart" $ do
  -- One recipient description and the amount each of its recipients gets.
  Spec.it s "MkDamagePart" $
    Common.assertCodec
      s
      DamagePart.codec
      ( DamagePart.MkDamagePart
          { DamagePart.ref = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target")),
            DamagePart.quantity = Quantity.Literal 4
          }
      )
      " {\"ref\":{\"type\":\"InSlot\",\"value\":\"target\"},\"quantity\":{\"type\":\"Literal\",\"value\":4}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s DamagePart.codec
