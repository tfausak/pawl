module Pawl.Codec.GameSettingsSpec where

import qualified Pawl.Codec.GameSettings as GameSettings
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.GameSettings as GameSettings

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.GameSettings" $ do
  -- CR 103: an ordinary game, which uses no option at all.
  Spec.it s "a game started with no options" $
    Common.assertCodec
      s
      GameSettings.codec
      GameSettings.MkGameSettings {GameSettings.brawl = False}
      " {\"brawl\":false} "
  -- CR 903.12a: the same record with the one option this type carries turned on.
  Spec.it s "a Brawl game" $
    Common.assertCodec
      s
      GameSettings.codec
      GameSettings.MkGameSettings {GameSettings.brawl = True}
      " {\"brawl\":true} "
