-- | The @Phase ⇆ Json@ codec (#481).
module Pawl.Codec.Phase where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.BeginningStep as BeginningStep
import qualified Pawl.Codec.CombatStep as CombatStep
import qualified Pawl.Codec.EndingStep as EndingStep
import qualified Pawl.Codec.Json as Json
import Pawl.Json.Value (Value)
import qualified Pawl.Types.Phase as Phase

phaseToJson :: Phase.Phase -> Value
phaseToJson p = case p of
  Phase.Beginning s -> Json.tagged (Text.pack "Beginning") (Just (BeginningStep.toJson s))
  Phase.PrecombatMain -> Json.nullary (Text.pack "PrecombatMain")
  Phase.Combat s -> Json.tagged (Text.pack "Combat") (Just (CombatStep.toJson s))
  Phase.PostcombatMain -> Json.nullary (Text.pack "PostcombatMain")
  Phase.Ending s -> Json.tagged (Text.pack "Ending") (Just (EndingStep.toJson s))

jsonToPhase :: Value -> Either Text Phase.Phase
jsonToPhase value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("Beginning", Just v) -> Phase.Beginning <$> BeginningStep.fromJson v
    ("PrecombatMain", _) -> Right Phase.PrecombatMain
    ("Combat", Just v) -> Phase.Combat <$> CombatStep.fromJson v
    ("PostcombatMain", _) -> Right Phase.PostcombatMain
    ("Ending", Just v) -> Phase.Ending <$> EndingStep.fromJson v
    _ -> Left (Text.pack "unknown Phase: " <> t)
