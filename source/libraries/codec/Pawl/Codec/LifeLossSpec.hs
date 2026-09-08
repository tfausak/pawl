module Pawl.Codec.LifeLossSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.LifeLoss as LifeLoss
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.LifeLoss as LifeLoss
import qualified Pawl.Types.LifeLossCause as LifeLossCause
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.LifeLoss" $ do
  -- CR 119.3 is what a printing means, so the key is omitted: this is byte for
  -- byte what data/cards/ held when the payload was a PlayerQuantity.
  Spec.it s "an ordinary effect's loss omits the cause" $
    Common.assertCodec
      s
      LifeLoss.codec
      LifeLoss.MkLifeLoss
        { LifeLoss.player = PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target")),
          LifeLoss.quantity = Quantity.Literal 2,
          LifeLoss.cause = LifeLossCause.ByEffect
        }
      " {\"player\":{\"type\":\"InSlot\",\"value\":\"target\"},\"quantity\":{\"type\":\"Literal\",\"value\":2}} "
  -- CR 728.1a, which only Pawl.Engine.Rad mints.
  Spec.it s "rule 728.1's loss writes its cause" $
    Common.assertCodec
      s
      LifeLoss.codec
      LifeLoss.MkLifeLoss
        { LifeLoss.player = PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target")),
          LifeLoss.quantity = Quantity.Literal 1,
          LifeLoss.cause = LifeLossCause.ByRadiation
        }
      " {\"player\":{\"type\":\"InSlot\",\"value\":\"target\"},\"quantity\":{\"type\":\"Literal\",\"value\":1},\"cause\":{\"type\":\"ByRadiation\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s LifeLoss.codec
