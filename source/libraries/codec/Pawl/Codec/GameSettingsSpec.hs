module Pawl.Codec.GameSettingsSpec where

import qualified Data.Map.Strict as Map
import qualified Pawl.Codec.GameSettings as GameSettings
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AttackOption as AttackOption
import qualified Pawl.Types.Emperors as Emperors
import qualified Pawl.Types.GameSettings as GameSettings
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.RangeOfInfluence as RangeOfInfluence
import qualified Pawl.Types.TeamId as TeamId
import qualified Pawl.Types.Teams as Teams

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.GameSettings" $ do
  -- CR 103: a two-player game, which uses no option at all -- CR 800.2's
  -- options being ones a multiplayer game adds.
  Spec.it s "a game started with no options" $
    Common.assertCodec
      s
      GameSettings.codec
      GameSettings.MkGameSettings {GameSettings.brawl = False, GameSettings.attackOption = Nothing, GameSettings.teams = Teams.none, GameSettings.sharedTeamTurns = False, GameSettings.sharedTeamLife = False, GameSettings.rangeOfInfluence = RangeOfInfluence.unlimited, GameSettings.deployCreatures = False, GameSettings.emperors = Emperors.none, GameSettings.twoHeadedGiant = False}
      " {\"brawl\":false,\"attackOption\":null,\"teams\":{},\"sharedTeamTurns\":false,\"sharedTeamLife\":false,\"rangeOfInfluence\":{},\"deployCreatures\":false,\"emperors\":{},\"twoHeadedGiant\":false} "
  -- CR 903.12a: the same record with the Brawl option turned on.
  Spec.it s "a Brawl game" $
    Common.assertCodec
      s
      GameSettings.codec
      GameSettings.MkGameSettings {GameSettings.brawl = True, GameSettings.attackOption = Nothing, GameSettings.teams = Teams.none, GameSettings.sharedTeamTurns = False, GameSettings.sharedTeamLife = False, GameSettings.rangeOfInfluence = RangeOfInfluence.unlimited, GameSettings.deployCreatures = False, GameSettings.emperors = Emperors.none, GameSettings.twoHeadedGiant = False}
      " {\"brawl\":true,\"attackOption\":null,\"teams\":{},\"sharedTeamTurns\":false,\"sharedTeamLife\":false,\"rangeOfInfluence\":{},\"deployCreatures\":false,\"emperors\":{},\"twoHeadedGiant\":false} "
  -- CR 802.1: and the option every game pawl starts uses.
  Spec.it s "a game using the attack multiple players option" $
    Common.assertCodec
      s
      GameSettings.codec
      GameSettings.MkGameSettings {GameSettings.brawl = False, GameSettings.attackOption = Just AttackOption.MultiplePlayers, GameSettings.teams = Teams.none, GameSettings.sharedTeamTurns = False, GameSettings.sharedTeamLife = False, GameSettings.rangeOfInfluence = RangeOfInfluence.unlimited, GameSettings.deployCreatures = False, GameSettings.emperors = Emperors.none, GameSettings.twoHeadedGiant = False}
      " {\"brawl\":false,\"attackOption\":{\"type\":\"MultiplePlayers\"},\"teams\":{},\"sharedTeamTurns\":false,\"sharedTeamLife\":false,\"rangeOfInfluence\":{},\"deployCreatures\":false,\"emperors\":{},\"twoHeadedGiant\":false} "
  -- CR 803.1a: the option CR 807.2b makes the Grand Melee default. Its sibling
  -- Rightward is Pawl.Codec.AttackOptionSpec's business; what is this record's
  -- is that the field carries a NAMED option rather than a flag.
  Spec.it s "a game using the attack left option" $
    Common.assertCodec
      s
      GameSettings.codec
      GameSettings.MkGameSettings {GameSettings.brawl = False, GameSettings.attackOption = Just AttackOption.Leftward, GameSettings.teams = Teams.none, GameSettings.sharedTeamTurns = False, GameSettings.sharedTeamLife = False, GameSettings.rangeOfInfluence = RangeOfInfluence.unlimited, GameSettings.deployCreatures = False, GameSettings.emperors = Emperors.none, GameSettings.twoHeadedGiant = False}
      " {\"brawl\":false,\"attackOption\":{\"type\":\"Leftward\"},\"teams\":{},\"sharedTeamTurns\":false,\"sharedTeamLife\":false,\"rangeOfInfluence\":{},\"deployCreatures\":false,\"emperors\":{},\"twoHeadedGiant\":false} "
  -- CR 810.1 / 810.2 / 810.4 / 804.1: a Two-Headed Giant game, two teams of two
  -- each taking its turns together and sharing one life total, with the deploy
  -- creatures option.
  Spec.it s "a game played between teams" $
    Common.assertCodec
      s
      GameSettings.codec
      GameSettings.MkGameSettings
        { GameSettings.brawl = False,
          GameSettings.attackOption = Just AttackOption.MultiplePlayers,
          GameSettings.teams =
            Teams.MkTeams
              ( Map.fromList
                  [ (PlayerId.MkPlayerId 0, TeamId.MkTeamId 0),
                    (PlayerId.MkPlayerId 1, TeamId.MkTeamId 1),
                    (PlayerId.MkPlayerId 2, TeamId.MkTeamId 0),
                    (PlayerId.MkPlayerId 3, TeamId.MkTeamId 1)
                  ]
              ),
          GameSettings.sharedTeamTurns = True,
          GameSettings.sharedTeamLife = True,
          GameSettings.rangeOfInfluence = RangeOfInfluence.unlimited,
          GameSettings.deployCreatures = True,
          GameSettings.emperors = Emperors.none,
          GameSettings.twoHeadedGiant = True
        }
      " {\"brawl\":false,\"attackOption\":{\"type\":\"MultiplePlayers\"},\"teams\":{\"0\":0,\"1\":1,\"2\":0,\"3\":1},\"sharedTeamTurns\":true,\"sharedTeamLife\":true,\"rangeOfInfluence\":{},\"deployCreatures\":true,\"emperors\":{},\"twoHeadedGiant\":true} "
  -- CR 801.2a: the limited range of influence option, one seat for two players.
  Spec.it s "a game using a limited range of influence" $
    Common.assertCodec
      s
      GameSettings.codec
      GameSettings.MkGameSettings
        { GameSettings.brawl = False,
          GameSettings.attackOption = Just AttackOption.MultiplePlayers,
          GameSettings.teams = Teams.none,
          GameSettings.sharedTeamTurns = False,
          GameSettings.sharedTeamLife = False,
          GameSettings.rangeOfInfluence = RangeOfInfluence.MkRangeOfInfluence (Map.fromList [(PlayerId.MkPlayerId 0, 1), (PlayerId.MkPlayerId 1, 1)]),
          GameSettings.deployCreatures = False,
          GameSettings.emperors = Emperors.none,
          GameSettings.twoHeadedGiant = False
        }
      " {\"brawl\":false,\"attackOption\":{\"type\":\"MultiplePlayers\"},\"teams\":{},\"sharedTeamTurns\":false,\"sharedTeamLife\":false,\"rangeOfInfluence\":{\"0\":1,\"1\":1},\"deployCreatures\":false,\"emperors\":{},\"twoHeadedGiant\":false} "
  -- CR 809.2 / 809.3: an Emperor game, two teams of three with each team's
  -- emperor in its middle seat.
  Spec.it s "an Emperor game" $
    Common.assertCodec
      s
      GameSettings.codec
      GameSettings.MkGameSettings
        { GameSettings.brawl = False,
          GameSettings.attackOption = Just AttackOption.Adjacent,
          GameSettings.teams = Teams.MkTeams (Map.fromList [(PlayerId.MkPlayerId p, TeamId.MkTeamId (if p < 3 then 0 else 1)) | p <- [0 .. 5]]),
          GameSettings.sharedTeamTurns = False,
          GameSettings.sharedTeamLife = False,
          GameSettings.rangeOfInfluence = RangeOfInfluence.MkRangeOfInfluence (Map.fromList [(PlayerId.MkPlayerId p, if p == 1 || p == 4 then 2 else 1) | p <- [0 .. 5]]),
          GameSettings.deployCreatures = True,
          GameSettings.emperors = Emperors.MkEmperors (Map.fromList [(TeamId.MkTeamId 0, PlayerId.MkPlayerId 1), (TeamId.MkTeamId 1, PlayerId.MkPlayerId 4)]),
          GameSettings.twoHeadedGiant = False
        }
      " {\"brawl\":false,\"attackOption\":{\"type\":\"Adjacent\"},\"teams\":{\"0\":0,\"1\":0,\"2\":0,\"3\":1,\"4\":1,\"5\":1},\"sharedTeamTurns\":false,\"sharedTeamLife\":false,\"rangeOfInfluence\":{\"0\":1,\"1\":2,\"2\":1,\"3\":1,\"4\":2,\"5\":1},\"deployCreatures\":true,\"emperors\":{\"0\":1,\"1\":4},\"twoHeadedGiant\":false} "
