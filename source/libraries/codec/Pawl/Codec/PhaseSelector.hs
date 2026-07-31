-- | The @PhaseSelector ⇆ Json@ codec (#481).
module Pawl.Codec.PhaseSelector where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Json as Json
import Pawl.Codec.Phase (jsonToPhase, phaseToJson)
import Pawl.Json.Value (Value)
import qualified Pawl.Types.PhaseSelector as PhaseSelector

phaseSelectorToJson :: PhaseSelector.PhaseSelector -> Value
phaseSelectorToJson selector = case selector of
  PhaseSelector.Step p -> Json.tagged (Text.pack "Step") (Just (phaseToJson p))
  PhaseSelector.BeginningPhase -> Json.nullary (Text.pack "BeginningPhase")
  PhaseSelector.CombatPhase -> Json.nullary (Text.pack "CombatPhase")
  PhaseSelector.EndingPhase -> Json.nullary (Text.pack "EndingPhase")

jsonToPhaseSelector :: Value -> Either Text PhaseSelector.PhaseSelector
jsonToPhaseSelector value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("Step", Just v) -> PhaseSelector.Step <$> jsonToPhase v
    ("BeginningPhase", _) -> Right PhaseSelector.BeginningPhase
    ("CombatPhase", _) -> Right PhaseSelector.CombatPhase
    ("EndingPhase", _) -> Right PhaseSelector.EndingPhase
    _ -> Left (Text.pack "unknown PhaseSelector: " <> t)
