module Pawl.Codec.ChoosePlayerSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.ChoosePlayer as ChoosePlayer
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ChoosePlayer as ChoosePlayer
import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ChoosePlayer" $ do
  -- CR 608.2d's two printed sentences, so the pair catches a codec that read
  -- the scope off anything but the wire.
  Spec.it s "MkChoosePlayer, an opponent (Skullwinder)" $
    Common.assertCodec
      s
      ChoosePlayer.codec
      ( ChoosePlayer.MkChoosePlayer
          { ChoosePlayer.scope = PlayerScope.Opponents,
            ChoosePlayer.slot = SlotName.MkSlotName (Text.pack "opponent")
          }
      )
      " {\"scope\":{\"type\":\"Opponents\"},\"slot\":\"opponent\"} "
  Spec.it s "MkChoosePlayer, a player (Stadium Vendors)" $
    Common.assertCodec
      s
      ChoosePlayer.codec
      ( ChoosePlayer.MkChoosePlayer
          { ChoosePlayer.scope = PlayerScope.EachPlayer,
            ChoosePlayer.slot = SlotName.MkSlotName (Text.pack "chosen")
          }
      )
      " {\"scope\":{\"type\":\"EachPlayer\"},\"slot\":\"chosen\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ChoosePlayer.codec
