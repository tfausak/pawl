module Pawl.Codec.ControlPlayerSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.ControlPlayer as ControlPlayer
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ControlPlayer as ControlPlayer
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ControlPlayer" $ do
  Spec.it s "MkControlPlayer, restriction elided" $
    Common.assertCodec
      s
      ControlPlayer.codec
      ( ControlPlayer.MkControlPlayer
          { ControlPlayer.slot = SlotName.MkSlotName (Text.pack "target"),
            ControlPlayer.manaFromLandsOnly = False
          }
      )
      " {\"slot\":\"target\"} "
  -- Word of Command's clause, and the only producer that writes the key.
  Spec.it s "MkControlPlayer, restriction written" $
    Common.assertCodec
      s
      ControlPlayer.codec
      ( ControlPlayer.MkControlPlayer
          { ControlPlayer.slot = SlotName.MkSlotName (Text.pack "opponent"),
            ControlPlayer.manaFromLandsOnly = True
          }
      )
      " {\"slot\":\"opponent\",\"manaFromLandsOnly\":true} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ControlPlayer.codec
