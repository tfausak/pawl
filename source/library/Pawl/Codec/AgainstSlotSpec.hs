module Pawl.Codec.AgainstSlotSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.AgainstSlot as AgainstSlot
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AgainstSlot as AgainstSlot
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.SlotName as SlotName

-- | Instantiated at 'Quantity.Quantity', the only concrete instantiation
-- anywhere: 'Pawl.Codec.Quantity' passes its own recursive codec in.
codec :: Codec.Codec (AgainstSlot.AgainstSlot Quantity.Quantity)
codec = AgainstSlot.codec Quantity.codec

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.AgainstSlot" $ do
  -- Which object to aim at, then what to read off it.
  Spec.it s "MkAgainstSlot" $
    Common.assertCodec
      s
      codec
      ( AgainstSlot.MkAgainstSlot
          { AgainstSlot.slot = SlotName.MkSlotName (Text.pack "target"),
            AgainstSlot.quantity = Quantity.Toughness
          }
      )
      " {\"slot\":\"target\",\"quantity\":{\"type\":\"Toughness\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s codec
