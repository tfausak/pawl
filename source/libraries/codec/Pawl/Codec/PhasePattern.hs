-- | The @PhasePattern ⇆ Json@ codec (#481).
module Pawl.Codec.PhasePattern where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Json as Json
import Pawl.Codec.PhaseSelector (jsonToPhaseSelector, phaseSelectorToJson)
import Pawl.Codec.PlayerId (jsonToPlayerId, playerIdToJson)
import Pawl.Json.Value (Value)
import qualified Pawl.Types.PhasePattern as PhasePattern

-- `whosePhase` is meant to be runtime-only -- a player-scoped skip is baked by
-- Resolve's SkipNextPhase arm, not authored on a card -- but the codec must stay
-- total, so this accepts one from card JSON and a lint owes the pool the check
-- (#437). Same treatment, and same reason, as SetController's PlayerId above.
phasePatternToJson :: PhasePattern.PhasePattern -> Value
phasePatternToJson p =
  Json.jObject
    [ (Text.pack "whichPhase", phaseSelectorToJson (PhasePattern.whichPhase p)),
      (Text.pack "whosePhase", Json.maybeTo playerIdToJson (PhasePattern.whosePhase p))
    ]

jsonToPhasePattern :: Value -> Either Text PhasePattern.PhasePattern
jsonToPhasePattern value = do
  ps <- Json.asObject value
  p <- Json.field (Text.pack "whichPhase") ps >>= jsonToPhaseSelector
  w <- Json.field (Text.pack "whosePhase") ps >>= Json.maybeFrom jsonToPlayerId
  pure PhasePattern.MkPhasePattern {PhasePattern.whichPhase = p, PhasePattern.whosePhase = w}
