module Pawl.Codec.BoardSpec where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Pawl.Codec.Board as Board
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AttackOption as AttackOption.Type
import qualified Pawl.Types.Board as Board.Type
import qualified Pawl.Types.CombatStep as CombatStep.Type
import qualified Pawl.Types.Label as Label.Type
import qualified Pawl.Types.Phase as Phase.Type
import qualified Pawl.Types.Seat as Seat.Type

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Board" $ do
  Spec.it s "the step is its flat name" $
    Common.assertCodec
      s
      Board.codec
      Board.Type.MkBoard
        { Board.Type.seats = seat "alice" NonEmpty.:| [seat "bob"],
          Board.Type.active = Label.Type.MkLabel (Text.pack "alice"),
          Board.Type.turn = 1,
          Board.Type.phase = Phase.Type.Combat CombatStep.Type.DeclareAttackers,
          Board.Type.monarch = Nothing,
          Board.Type.attackOption = Just AttackOption.Type.MultiplePlayers,
          Board.Type.brawl = False,
          Board.Type.sharedTeamTurns = False,
          Board.Type.sharedTeamLife = False,
          Board.Type.deployCreatures = False,
          Board.Type.twoHeadedGiant = False,
          Board.Type.alternatingTeams = False,
          Board.Type.ante = False
        }
      " {\"seats\":[{\"name\":\"alice\"},{\"name\":\"bob\"}],\"active\":\"alice\",\"step\":\"DeclareAttackers\"} "
  Spec.it s "a monarch" $
    Common.assertCodec
      s
      Board.codec
      Board.Type.MkBoard
        { Board.Type.seats = seat "alice" NonEmpty.:| [seat "bob"],
          Board.Type.active = Label.Type.MkLabel (Text.pack "alice"),
          Board.Type.turn = 1,
          Board.Type.phase = Phase.Type.PrecombatMain,
          Board.Type.monarch = Just (Label.Type.MkLabel (Text.pack "bob")),
          Board.Type.attackOption = Just AttackOption.Type.MultiplePlayers,
          Board.Type.brawl = False,
          Board.Type.sharedTeamTurns = False,
          Board.Type.sharedTeamLife = False,
          Board.Type.deployCreatures = False,
          Board.Type.twoHeadedGiant = False,
          Board.Type.alternatingTeams = False,
          Board.Type.ante = False
        }
      " {\"seats\":[{\"name\":\"alice\"},{\"name\":\"bob\"}],\"active\":\"alice\",\"step\":\"PrecombatMain\",\"monarch\":\"bob\"} "
  Spec.it s "an attack option, and none" $ do
    Common.assertCodec
      s
      Board.codec
      Board.Type.MkBoard
        { Board.Type.seats = seat "alice" NonEmpty.:| [seat "bob"],
          Board.Type.active = Label.Type.MkLabel (Text.pack "alice"),
          Board.Type.turn = 1,
          Board.Type.phase = Phase.Type.PrecombatMain,
          Board.Type.monarch = Nothing,
          Board.Type.attackOption = Just AttackOption.Type.Leftward,
          Board.Type.brawl = False,
          Board.Type.sharedTeamTurns = False,
          Board.Type.sharedTeamLife = False,
          Board.Type.deployCreatures = False,
          Board.Type.twoHeadedGiant = False,
          Board.Type.alternatingTeams = False,
          Board.Type.ante = False
        }
      " {\"seats\":[{\"name\":\"alice\"},{\"name\":\"bob\"}],\"active\":\"alice\",\"step\":\"PrecombatMain\",\"attackOption\":\"Leftward\"} "
    Common.assertCodec
      s
      Board.codec
      Board.Type.MkBoard
        { Board.Type.seats = seat "alice" NonEmpty.:| [seat "bob"],
          Board.Type.active = Label.Type.MkLabel (Text.pack "alice"),
          Board.Type.turn = 1,
          Board.Type.phase = Phase.Type.PrecombatMain,
          Board.Type.monarch = Nothing,
          Board.Type.attackOption = Nothing,
          Board.Type.brawl = False,
          Board.Type.sharedTeamTurns = False,
          Board.Type.sharedTeamLife = False,
          Board.Type.deployCreatures = False,
          Board.Type.twoHeadedGiant = False,
          Board.Type.alternatingTeams = False,
          Board.Type.ante = False
        }
      " {\"seats\":[{\"name\":\"alice\"},{\"name\":\"bob\"}],\"active\":\"alice\",\"step\":\"PrecombatMain\",\"attackOption\":null} "
  Spec.it s "game-wide options" $
    Common.assertCodec
      s
      Board.codec
      Board.Type.MkBoard
        { Board.Type.seats = seat "alice" NonEmpty.:| [seat "bob"],
          Board.Type.active = Label.Type.MkLabel (Text.pack "alice"),
          Board.Type.turn = 1,
          Board.Type.phase = Phase.Type.PrecombatMain,
          Board.Type.monarch = Nothing,
          Board.Type.attackOption = Just AttackOption.Type.MultiplePlayers,
          Board.Type.brawl = True,
          Board.Type.sharedTeamTurns = True,
          Board.Type.sharedTeamLife = True,
          Board.Type.deployCreatures = True,
          Board.Type.twoHeadedGiant = True,
          Board.Type.alternatingTeams = False,
          Board.Type.ante = False
        }
      " {\"seats\":[{\"name\":\"alice\"},{\"name\":\"bob\"}],\"active\":\"alice\",\"step\":\"PrecombatMain\",\"brawl\":true,\"sharedTeamTurns\":true,\"sharedTeamLife\":true,\"deployCreatures\":true,\"twoHeadedGiant\":true} "
  Spec.it s "a later turn" $
    Common.assertCodec
      s
      Board.codec
      Board.Type.MkBoard
        { Board.Type.seats = seat "alice" NonEmpty.:| [seat "bob"],
          Board.Type.active = Label.Type.MkLabel (Text.pack "bob"),
          Board.Type.turn = 2,
          Board.Type.phase = Phase.Type.PrecombatMain,
          Board.Type.monarch = Nothing,
          Board.Type.attackOption = Just AttackOption.Type.MultiplePlayers,
          Board.Type.brawl = False,
          Board.Type.sharedTeamTurns = False,
          Board.Type.sharedTeamLife = False,
          Board.Type.deployCreatures = False,
          Board.Type.twoHeadedGiant = False,
          Board.Type.alternatingTeams = False,
          Board.Type.ante = False
        }
      " {\"seats\":[{\"name\":\"alice\"},{\"name\":\"bob\"}],\"active\":\"bob\",\"turn\":2,\"step\":\"PrecombatMain\"} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s Board.codec

seat :: String -> Seat.Type.Seat
seat name =
  Seat.Type.MkSeat
    { Seat.Type.name = Label.Type.MkLabel (Text.pack name),
      Seat.Type.life = 20,
      Seat.Type.counters = Map.empty,
      Seat.Type.team = Nothing,
      Seat.Type.range = Nothing,
      Seat.Type.emperor = False,
      Seat.Type.manaPool = [],
      Seat.Type.battlefield = Seq.empty,
      Seat.Type.hand = Seq.empty,
      Seat.Type.graveyard = Seq.empty,
      Seat.Type.library = Seq.empty,
      Seat.Type.exile = Seq.empty,
      Seat.Type.command = Seq.empty,
      Seat.Type.ante = Seq.empty
    }
