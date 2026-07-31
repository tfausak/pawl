-- | The @Phase ⇆ Json@ codec, split out of the former
-- Pawl.Codec.All (#481).
module Pawl.Codec.Phase where

import Data.Text (Text)
import qualified Data.Text as Text
import Pawl.Codec.BeginningStep (beginningStepToJson, jsonToBeginningStep)
import Pawl.Codec.CombatStep (combatStepToJson, jsonToCombatStep)
import Pawl.Codec.EndingStep (endingStepToJson, jsonToEndingStep)
import qualified Pawl.Codec.Json as Json
import Pawl.Json.Value (Value)
import qualified Pawl.Types.Phase as Phase

phaseToJson :: Phase.Phase -> Value
phaseToJson p = case p of
  Phase.Beginning s -> Json.tagged (Text.pack "Beginning") (Just (beginningStepToJson s))
  Phase.PrecombatMain -> Json.nullary (Text.pack "PrecombatMain")
  Phase.Combat s -> Json.tagged (Text.pack "Combat") (Just (combatStepToJson s))
  Phase.PostcombatMain -> Json.nullary (Text.pack "PostcombatMain")
  Phase.Ending s -> Json.tagged (Text.pack "Ending") (Just (endingStepToJson s))

jsonToPhase :: Value -> Either Text Phase.Phase
jsonToPhase value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("Beginning", Just v) -> Phase.Beginning <$> jsonToBeginningStep v
    ("PrecombatMain", _) -> Right Phase.PrecombatMain
    ("Combat", Just v) -> Phase.Combat <$> jsonToCombatStep v
    ("PostcombatMain", _) -> Right Phase.PostcombatMain
    ("Ending", Just v) -> Phase.Ending <$> jsonToEndingStep v
    _ -> Left (Text.pack "unknown Phase: " <> t)
