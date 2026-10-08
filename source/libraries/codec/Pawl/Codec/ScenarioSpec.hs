module Pawl.Codec.ScenarioSpec where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Pawl.Codec.Scenario as Scenario
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AttackOption as AttackOption.Type
import qualified Pawl.Types.Board as Board.Type
import qualified Pawl.Types.Label as Label.Type
import qualified Pawl.Types.Phase as Phase.Type
import qualified Pawl.Types.Scenario as Scenario.Type
import qualified Pawl.Types.Seat as Seat.Type

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Scenario" $ do
  Spec.it s "an empty timeline and no final checks are left out" $
    Common.assertCodec
      s
      Scenario.codec
      Scenario.Type.MkScenario
        { Scenario.Type.description = Text.pack "nothing happens",
          Scenario.Type.board =
            Board.Type.MkBoard
              { Board.Type.seats = Seat.Type.MkSeat (Label.Type.MkLabel (Text.pack "alice")) 20 Map.empty Nothing Nothing False [] Seq.empty Seq.empty Seq.empty Seq.empty Seq.empty Seq.empty NonEmpty.:| [],
                Board.Type.active = Label.Type.MkLabel (Text.pack "alice"),
                Board.Type.turn = 1,
                Board.Type.phase = Phase.Type.PrecombatMain,
                Board.Type.monarch = Nothing,
                Board.Type.attackOption = Just AttackOption.Type.MultiplePlayers,
                Board.Type.brawl = False,
                Board.Type.sharedTeamTurns = False,
                Board.Type.sharedTeamLife = False,
                Board.Type.deployCreatures = False,
                Board.Type.twoHeadedGiant = False,
                Board.Type.alternatingTeams = False
              },
          Scenario.Type.timeline = Seq.empty,
          Scenario.Type.final = Seq.empty
        }
      " {\"description\":\"nothing happens\",\"board\":{\"seats\":[{\"name\":\"alice\"}],\"active\":\"alice\",\"step\":\"PrecombatMain\"}} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s Scenario.codec
