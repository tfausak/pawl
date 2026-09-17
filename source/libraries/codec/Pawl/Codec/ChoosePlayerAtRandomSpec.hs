module Pawl.Codec.ChoosePlayerAtRandomSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.ChoosePlayerAtRandom as ChoosePlayerAtRandom
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ChoosePlayerAtRandom as ChoosePlayerAtRandom
import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ChoosePlayerAtRandom" $ do
  -- The two printed sentences, so the pair catches a codec that read the scope
  -- off anything but the wire.
  Spec.it s "MkChoosePlayerAtRandom, an opponent (Ruhan of the Fomori)" $
    Common.assertCodec
      s
      ChoosePlayerAtRandom.codec
      ( ChoosePlayerAtRandom.MkChoosePlayerAtRandom
          { ChoosePlayerAtRandom.scope = PlayerScope.Opponents,
            ChoosePlayerAtRandom.slot = SlotName.MkSlotName (Text.pack "opponent")
          }
      )
      " {\"scope\":{\"type\":\"Opponents\"},\"slot\":\"opponent\"} "
  Spec.it s "MkChoosePlayerAtRandom, a player (Strax, Sontaran Nurse)" $
    Common.assertCodec
      s
      ChoosePlayerAtRandom.codec
      ( ChoosePlayerAtRandom.MkChoosePlayerAtRandom
          { ChoosePlayerAtRandom.scope = PlayerScope.EachPlayer,
            ChoosePlayerAtRandom.slot = SlotName.MkSlotName (Text.pack "chosen")
          }
      )
      " {\"scope\":{\"type\":\"EachPlayer\"},\"slot\":\"chosen\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ChoosePlayerAtRandom.codec
