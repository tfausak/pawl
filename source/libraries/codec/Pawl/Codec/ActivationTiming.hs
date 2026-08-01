module Pawl.Codec.ActivationTiming where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Phase as Phase
import qualified Pawl.Codec.TurnScope as TurnScope
import qualified Pawl.Json.Array as Array
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.ActivationTiming as ActivationTiming

-- | Tagged rather than bare-nullary since CR 500.1's DuringPhase carries a phase,
-- the shape CostComponent's toJson takes. AnyTime and SorcerySpeed still render as
-- the bare tag they always did.
--
-- DuringPhase's payload is a PAIR -- the phase and the turn scope -- written as
-- a two-element array, which is the shape TriggerCondition's toJson already gives
-- TriggerCondition.StepBegins for the same two components. There is deliberately
-- no scope-less form: the axis is not a default a card may omit, so a bare
-- phase payload is a decode failure rather than a silent EachTurn.
toJson :: ActivationTiming.ActivationTiming -> Value.Value
toJson t = case t of
  ActivationTiming.AnyTime -> Common.nullary "AnyTime"
  ActivationTiming.SorcerySpeed -> Common.nullary "SorcerySpeed"
  ActivationTiming.DuringPhase p sc -> Common.tagged "DuringPhase" . Just . Common.array $ [Phase.toJson p, TurnScope.toJson sc]

fromJson :: Value.Value -> Either Text.Text ActivationTiming.ActivationTiming
fromJson value = do
  (t, mv) <- Common.asTagged value
  case (t, mv) of
    ("AnyTime", _) -> Right ActivationTiming.AnyTime
    ("SorcerySpeed", _) -> Right ActivationTiming.SorcerySpeed
    ("DuringPhase", Just (Value.Array (Array.MkArray [p, sc]))) -> ActivationTiming.DuringPhase <$> Phase.fromJson p <*> TurnScope.fromJson sc
    _ -> Left . Text.pack $ "unknown ActivationTiming: " <> t
