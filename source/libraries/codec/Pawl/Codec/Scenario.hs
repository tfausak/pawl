{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Scenario where

import qualified Data.Sequence as Seq
import qualified Pawl.Codec.Board as Board
import qualified Pawl.Codec.Check as Check
import qualified Pawl.Codec.Timed as Timed
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Scenario as Scenario

codec :: Codec.Codec Scenario.Scenario
codec = Fields.object $ do
  description <- Fields.required "description" Common.text Scenario.description
  board <- Fields.required "board" Board.codec Scenario.board
  timeline <- Fields.defaulted "timeline" Seq.empty (Common.seq Timed.codec) Scenario.timeline
  final <- Fields.defaulted "final" Seq.empty (Common.seq Check.codec) Scenario.final
  pure
    Scenario.MkScenario
      { Scenario.description = description,
        Scenario.board = board,
        Scenario.timeline = timeline,
        Scenario.final = final
      }
