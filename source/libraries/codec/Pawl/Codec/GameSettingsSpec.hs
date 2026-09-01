module Pawl.Codec.GameSettingsSpec where

import qualified Pawl.Codec.GameSettings as GameSettings
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AttackOption as AttackOption
import qualified Pawl.Types.GameSettings as GameSettings

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.GameSettings" $ do
  -- CR 103: a two-player game, which uses no option at all -- CR 800.2's
  -- options being ones a multiplayer game adds.
  Spec.it s "a game started with no options" $
    Common.assertCodec
      s
      GameSettings.codec
      GameSettings.MkGameSettings {GameSettings.brawl = False, GameSettings.attackOption = Nothing}
      " {\"brawl\":false,\"attackOption\":null} "
  -- CR 903.12a: the same record with the Brawl option turned on.
  Spec.it s "a Brawl game" $
    Common.assertCodec
      s
      GameSettings.codec
      GameSettings.MkGameSettings {GameSettings.brawl = True, GameSettings.attackOption = Nothing}
      " {\"brawl\":true,\"attackOption\":null} "
  -- CR 802.1: and the option every game pawl starts uses.
  Spec.it s "a game using the attack multiple players option" $
    Common.assertCodec
      s
      GameSettings.codec
      GameSettings.MkGameSettings {GameSettings.brawl = False, GameSettings.attackOption = Just AttackOption.MultiplePlayers}
      " {\"brawl\":false,\"attackOption\":{\"type\":\"MultiplePlayers\"}} "
  -- CR 803.1a: the option CR 807.2b makes the Grand Melee default. Its sibling
  -- Rightward is Pawl.Codec.AttackOptionSpec's business; what is this record's
  -- is that the field carries a NAMED option rather than a flag.
  Spec.it s "a game using the attack left option" $
    Common.assertCodec
      s
      GameSettings.codec
      GameSettings.MkGameSettings {GameSettings.brawl = False, GameSettings.attackOption = Just AttackOption.Leftward}
      " {\"brawl\":false,\"attackOption\":{\"type\":\"Leftward\"}} "
