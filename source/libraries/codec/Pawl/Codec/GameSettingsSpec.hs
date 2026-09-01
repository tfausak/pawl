module Pawl.Codec.GameSettingsSpec where

import qualified Data.Map.Strict as Map
import qualified Pawl.Codec.GameSettings as GameSettings
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.GameSettings as GameSettings
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.TeamId as TeamId
import qualified Pawl.Types.Teams as Teams

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.GameSettings" $ do
  -- CR 103: a two-player game, which uses no option at all -- CR 802.1 being
  -- an option a multiplayer game adds.
  Spec.it s "a game started with no options" $
    Common.assertCodec
      s
      GameSettings.codec
      GameSettings.MkGameSettings {GameSettings.brawl = False, GameSettings.attackMultiplePlayers = False, GameSettings.teams = Teams.none}
      " {\"brawl\":false,\"attackMultiplePlayers\":false,\"teams\":{}} "
  -- CR 903.12a: the same record with the Brawl option turned on.
  Spec.it s "a Brawl game" $
    Common.assertCodec
      s
      GameSettings.codec
      GameSettings.MkGameSettings {GameSettings.brawl = True, GameSettings.attackMultiplePlayers = False, GameSettings.teams = Teams.none}
      " {\"brawl\":true,\"attackMultiplePlayers\":false,\"teams\":{}} "
  -- CR 802.1: and the option every game pawl starts uses.
  Spec.it s "a game using the attack multiple players option" $
    Common.assertCodec
      s
      GameSettings.codec
      GameSettings.MkGameSettings {GameSettings.brawl = False, GameSettings.attackMultiplePlayers = True, GameSettings.teams = Teams.none}
      " {\"brawl\":false,\"attackMultiplePlayers\":true,\"teams\":{}} "
  -- CR 808.1: a Team vs. Team game between two teams of two.
  Spec.it s "a game played between teams" $
    Common.assertCodec
      s
      GameSettings.codec
      GameSettings.MkGameSettings
        { GameSettings.brawl = False,
          GameSettings.attackMultiplePlayers = True,
          GameSettings.teams =
            Teams.MkTeams
              ( Map.fromList
                  [ (PlayerId.MkPlayerId 0, TeamId.MkTeamId 0),
                    (PlayerId.MkPlayerId 1, TeamId.MkTeamId 1),
                    (PlayerId.MkPlayerId 2, TeamId.MkTeamId 0),
                    (PlayerId.MkPlayerId 3, TeamId.MkTeamId 1)
                  ]
              )
        }
      " {\"brawl\":false,\"attackMultiplePlayers\":true,\"teams\":{\"0\":0,\"1\":1,\"2\":0,\"3\":1}} "
