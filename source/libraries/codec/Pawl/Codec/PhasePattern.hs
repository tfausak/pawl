module Pawl.Codec.PhasePattern where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.PhaseSelector as PhaseSelector
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.PhasePattern as PhasePattern

-- | `whosePhase` is runtime-only -- a player-scoped skip is baked by Resolve's
-- SkipNextPhase arm, not authored on a card -- but this codec is structural
-- over the record and so accepts one from card JSON. Nothing needs a baked
-- pattern to survive a round trip: neither Pawl.Types.ActiveReplacement nor
-- GameState has a codec at all (#126). A corpus lint is what keeps the pool
-- honest instead, the same treatment SetController's PlayerId gets.
toJson :: PhasePattern.PhasePattern -> Value.Value
toJson p =
  Common.object
    ( Common.requiredPair "whichPhase" PhaseSelector.toJson (PhasePattern.whichPhase p)
        <> Common.optionalPair "whosePhase" Nothing (Common.encodeMaybe PlayerId.toJson) (PhasePattern.whosePhase p)
    )

fromJson :: Value.Value -> Either Text.Text PhasePattern.PhasePattern
fromJson value = do
  ps <- Common.asObject value
  p <- Common.field "whichPhase" ps >>= PhaseSelector.fromJson
  w <- Common.defaultedField "whosePhase" Nothing (Common.decodeMaybe PlayerId.fromJson) ps
  pure PhasePattern.MkPhasePattern {PhasePattern.whichPhase = p, PhasePattern.whosePhase = w}
