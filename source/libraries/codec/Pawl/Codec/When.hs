{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.When where

import qualified Pawl.Codec.Label as Label
import qualified Pawl.Codec.Phase as Phase
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.When as When

codec :: Codec.Codec When.When
codec = Fields.object fields

-- | The keys alone, which Pawl.Codec.Timed writes into its own object.
fields :: Fields.Fields When.When When.When
fields = do
  turn <- Fields.required "turn" Common.natural When.turn
  phase <- Fields.required "step" Phase.flat When.phase
  player <- Fields.required "player" Label.codec When.player
  pure When.MkWhen {When.turn = turn, When.phase = phase, When.player = player}
