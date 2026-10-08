{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Board where

import qualified Pawl.Codec.Label as Label
import qualified Pawl.Codec.Phase as Phase
import qualified Pawl.Codec.Seat as Seat
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.AttackOption as AttackOption
import qualified Pawl.Types.Board as Board

codec :: Codec.Codec Board.Board
codec = Fields.object $ do
  seats <- Fields.required "seats" (Common.nonEmpty Seat.codec) Board.seats
  active <- Fields.required "active" Label.codec Board.active
  turn <- Fields.defaulted "turn" 1 Common.natural Board.turn
  phase <- Fields.required "step" Phase.flat Board.phase
  monarch <- Fields.defaulted "monarch" Nothing (Common.maybe Label.codec) Board.monarch
  attackOption <- Fields.defaulted "attackOption" (Just AttackOption.MultiplePlayers) (Common.maybe Arm.keyedEnum) Board.attackOption
  brawl <- Fields.defaulted "brawl" False Common.boolean Board.brawl
  sharedTeamTurns <- Fields.defaulted "sharedTeamTurns" False Common.boolean Board.sharedTeamTurns
  sharedTeamLife <- Fields.defaulted "sharedTeamLife" False Common.boolean Board.sharedTeamLife
  deployCreatures <- Fields.defaulted "deployCreatures" False Common.boolean Board.deployCreatures
  twoHeadedGiant <- Fields.defaulted "twoHeadedGiant" False Common.boolean Board.twoHeadedGiant
  alternatingTeams <- Fields.defaulted "alternatingTeams" False Common.boolean Board.alternatingTeams
  ante <- Fields.defaulted "ante" False Common.boolean Board.ante
  pure
    Board.MkBoard
      { Board.seats = seats,
        Board.active = active,
        Board.turn = turn,
        Board.phase = phase,
        Board.monarch = monarch,
        Board.attackOption = attackOption,
        Board.brawl = brawl,
        Board.sharedTeamTurns = sharedTeamTurns,
        Board.sharedTeamLife = sharedTeamLife,
        Board.deployCreatures = deployCreatures,
        Board.twoHeadedGiant = twoHeadedGiant,
        Board.alternatingTeams = alternatingTeams,
        Board.ante = ante
      }
