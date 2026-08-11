module Pawl.Codec.Phase where

import qualified Data.Text as Text
import qualified Pawl.Codec.BeginningStep as BeginningStep
import qualified Pawl.Codec.CombatStep as CombatStep
import qualified Pawl.Codec.EndingStep as EndingStep
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Phase as Phase

toJson :: Phase.Phase -> Value.Value
toJson p = case p of
  Phase.Beginning st -> Common.tagged "Beginning" . Just $ Codec.encode BeginningStep.codec st
  Phase.PrecombatMain -> Common.nullary "PrecombatMain"
  Phase.Combat st -> Common.tagged "Combat" . Just $ Codec.encode CombatStep.codec st
  Phase.PostcombatMain -> Common.nullary "PostcombatMain"
  Phase.Ending st -> Common.tagged "Ending" . Just $ Codec.encode EndingStep.codec st

fromJson :: Value.Value -> Either Text.Text Phase.Phase
fromJson value = do
  (t, mv) <- Common.asTagged value
  case (t, mv) of
    ("Beginning", Just v) -> Phase.Beginning <$> Codec.decode BeginningStep.codec v
    ("PrecombatMain", _) -> Right Phase.PrecombatMain
    ("Combat", Just v) -> Phase.Combat <$> Codec.decode CombatStep.codec v
    ("PostcombatMain", _) -> Right Phase.PostcombatMain
    ("Ending", Just v) -> Phase.Ending <$> Codec.decode EndingStep.codec v
    _ -> Left . Text.pack $ "unknown Phase: " <> t
