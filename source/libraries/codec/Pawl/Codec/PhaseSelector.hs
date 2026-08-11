module Pawl.Codec.PhaseSelector where

import qualified Data.Text as Text
import qualified Pawl.Codec.Phase as Phase
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.PhaseSelector as PhaseSelector

toJson :: PhaseSelector.PhaseSelector -> Value.Value
toJson selector = case selector of
  PhaseSelector.Step p -> Common.tagged "Step" . Just $ Phase.toJson p
  PhaseSelector.BeginningPhase -> Common.nullary "BeginningPhase"
  PhaseSelector.CombatPhase -> Common.nullary "CombatPhase"
  PhaseSelector.EndingPhase -> Common.nullary "EndingPhase"

fromJson :: Value.Value -> Either Text.Text PhaseSelector.PhaseSelector
fromJson value = do
  (t, mv) <- Common.asTagged value
  case (t, mv) of
    ("Step", Just v) -> PhaseSelector.Step <$> Phase.fromJson v
    ("BeginningPhase", _) -> Right PhaseSelector.BeginningPhase
    ("CombatPhase", _) -> Right PhaseSelector.CombatPhase
    ("EndingPhase", _) -> Right PhaseSelector.EndingPhase
    _ -> Left . Text.pack $ "unknown PhaseSelector: " <> t
