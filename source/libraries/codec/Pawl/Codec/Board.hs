{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Board where

import qualified Pawl.Codec.Label as Label
import qualified Pawl.Codec.Phase as Phase
import qualified Pawl.Codec.Seat as Seat
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Board as Board

codec :: Codec.Codec Board.Board
codec = Fields.object $ do
  seats <- Fields.required "seats" (Common.nonEmpty Seat.codec) Board.seats
  active <- Fields.required "active" Label.codec Board.active
  phase <- Fields.required "step" Phase.flat Board.phase
  pure
    Board.MkBoard
      { Board.seats = seats,
        Board.active = active,
        Board.phase = phase
      }
