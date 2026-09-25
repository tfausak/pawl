module Pawl.Codec.GiveControlSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.GiveControl as GiveControl
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.GiveControl as GiveControl
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.GiveControl" $ do
  Spec.it s "MkGiveControl, both keys" $
    Common.assertCodec
      s
      GiveControl.codec
      ( GiveControl.MkGiveControl
          { GiveControl.player = PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "teammate")),
            GiveControl.ref = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))
          }
      )
      " {\"player\":{\"type\":\"InSlot\",\"value\":\"teammate\"},\"ref\":{\"type\":\"InSlot\",\"value\":\"target\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s GiveControl.codec
