module Pawl.Codec.PhasePattern where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.PhaseSelector as PhaseSelector
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.PhasePattern as PhasePattern

-- | `whosePhase` is runtime-only -- a player-scoped skip is baked by Resolve's
-- SkipNextPhase arm, not authored on a card -- but the codec must stay total for
-- the round trip a stored ActiveReplacement needs, so it accepts one from card
-- JSON. Pawl.CardSpec's "no card authors a player-scoped phase skip" is what
-- keeps the pool honest. Same treatment, and same reason, as SetController's
-- PlayerId above.
toJson :: PhasePattern.PhasePattern -> Value.Value
toJson p =
  Common.object
    [ Common.pair "whichPhase" (PhaseSelector.toJson (PhasePattern.whichPhase p)),
      Common.pair "whosePhase" (Common.encodeMaybe PlayerId.toJson (PhasePattern.whosePhase p))
    ]

fromJson :: Value.Value -> Either Text.Text PhasePattern.PhasePattern
fromJson value = do
  ps <- Common.asObject value
  p <- Common.field "whichPhase" ps >>= PhaseSelector.fromJson
  w <- Common.field "whosePhase" ps >>= Common.decodeMaybe PlayerId.fromJson
  pure PhasePattern.MkPhasePattern {PhasePattern.whichPhase = p, PhasePattern.whosePhase = w}
