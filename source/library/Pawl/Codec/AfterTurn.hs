{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.AfterTurn where

import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.AfterTurn as AfterTurn

-- | A bare object keyed by the record's field names, like Pawl.Codec.While
-- beside it.
codec :: Codec.Codec AfterTurn.AfterTurn
codec = Fields.object $ do
  player <- Fields.required "player" PlayerId.codec AfterTurn.player
  turn <- Fields.required "turn" Common.natural AfterTurn.turn
  pure AfterTurn.MkAfterTurn {AfterTurn.player = player, AfterTurn.turn = turn}
