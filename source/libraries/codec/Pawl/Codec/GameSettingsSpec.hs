module Pawl.Codec.GameSettingsSpec where

import qualified Pawl.Codec.GameSettings as GameSettings
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.GameSettings as GameSettings

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.GameSettings" $ do
  -- CR 103: a two-player game, which uses no option at all -- CR 802.1 being
  -- an option a multiplayer game adds.
  Spec.it s "a game started with no options" $
    Common.assertCodec
      s
      GameSettings.codec
      GameSettings.MkGameSettings {GameSettings.brawl = False, GameSettings.attackMultiplePlayers = False}
      " {\"brawl\":false,\"attackMultiplePlayers\":false} "
  -- CR 903.12a: the same record with the Brawl option turned on.
  Spec.it s "a Brawl game" $
    Common.assertCodec
      s
      GameSettings.codec
      GameSettings.MkGameSettings {GameSettings.brawl = True, GameSettings.attackMultiplePlayers = False}
      " {\"brawl\":true,\"attackMultiplePlayers\":false} "
  -- CR 802.1: and the option every game pawl starts uses.
  Spec.it s "a game using the attack multiple players option" $
    Common.assertCodec
      s
      GameSettings.codec
      GameSettings.MkGameSettings {GameSettings.brawl = False, GameSettings.attackMultiplePlayers = True}
      " {\"brawl\":false,\"attackMultiplePlayers\":true} "
