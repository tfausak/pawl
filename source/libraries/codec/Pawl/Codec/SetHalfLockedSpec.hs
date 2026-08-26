module Pawl.Codec.SetHalfLockedSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.SetHalfLocked as SetHalfLocked
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.SetHalfLocked as SetHalfLocked
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.SetHalfLocked" $ do
  -- CR 709.5g: lock a half of the slot's permanent.
  Spec.it s "MkSetHalfLocked, the lock setting" $
    Common.assertCodec
      s
      SetHalfLocked.codec
      ( SetHalfLocked.MkSetHalfLocked
          { SetHalfLocked.locked = True,
            SetHalfLocked.slot = SlotName.MkSlotName (Text.pack "target")
          }
      )
      " {\"locked\":true,\"slot\":\"target\"} "
  -- CR 709.5f: and unlock one. Both settings, since the wire tells them apart by
  -- one key and a card that wrote the wrong one reads as the other rule.
  Spec.it s "MkSetHalfLocked, the unlock setting" $
    Common.assertCodec
      s
      SetHalfLocked.codec
      ( SetHalfLocked.MkSetHalfLocked
          { SetHalfLocked.locked = False,
            SetHalfLocked.slot = SlotName.MkSlotName (Text.pack "self")
          }
      )
      " {\"locked\":false,\"slot\":\"self\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s SetHalfLocked.codec
