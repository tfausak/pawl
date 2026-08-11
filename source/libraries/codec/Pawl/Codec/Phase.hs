module Pawl.Codec.Phase where

import qualified Data.Text as Text
import qualified Pawl.Codec.BeginningStep as BeginningStep
import qualified Pawl.Codec.CombatStep as CombatStep
import qualified Pawl.Codec.EndingStep as EndingStep
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Phase as Phase

toJson :: Phase.Phase -> Value.Value
toJson p = case p of
  Phase.Beginning st -> Common.tagged "Beginning" . Just $ BeginningStep.toJson st
  Phase.PrecombatMain -> Common.nullary "PrecombatMain"
  Phase.Combat st -> Common.tagged "Combat" . Just $ CombatStep.toJson st
  Phase.PostcombatMain -> Common.nullary "PostcombatMain"
  Phase.Ending st -> Common.tagged "Ending" . Just $ EndingStep.toJson st

fromJson :: Value.Value -> Either Text.Text Phase.Phase
fromJson value = do
  (t, mv) <- Common.asTagged value
  case (t, mv) of
    ("Beginning", Just v) -> Phase.Beginning <$> BeginningStep.fromJson v
    ("PrecombatMain", _) -> Right Phase.PrecombatMain
    ("Combat", Just v) -> Phase.Combat <$> CombatStep.fromJson v
    ("PostcombatMain", _) -> Right Phase.PostcombatMain
    ("Ending", Just v) -> Phase.Ending <$> EndingStep.fromJson v
    _ -> Left . Text.pack $ "unknown Phase: " <> t
