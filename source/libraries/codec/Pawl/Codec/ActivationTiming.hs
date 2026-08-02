module Pawl.Codec.ActivationTiming where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.PhaseSelector as PhaseSelector
import qualified Pawl.Codec.TurnScope as TurnScope
import qualified Pawl.Json.Array as Array
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.ActivationTiming as ActivationTiming

-- | Tagged rather than bare-nullary since CR 500.1's DuringPhase carries a window,
-- the shape CostComponent's toJson takes. AnyTime and SorcerySpeed still render as
-- the bare tag they always did.
--
-- DuringPhase's payload is a PAIR -- the window and the turn scope -- written as
-- a two-element array, which is the shape TriggerCondition's toJson already gives
-- TriggerCondition.StepBegins for the same two components. There is deliberately
-- no scope-less form: the axis is not a default a card may omit, so a bare
-- window payload is a decode failure rather than a silent EachTurn.
--
-- The window is a PhaseSelector, so a rider naming a step nests it inside that
-- type's own `Step` tag -- Desert's is {"type": "Step", "value": {"type":
-- "Combat", ...}} where it used to be the bare phase. Widening in place rather
-- than accepting the old spelling too: the pool is data pawl ships and is
-- rewritten with the type (no-api-stability), and a codec that quietly took
-- both would leave two spellings of one window, which is the thing
-- Pawl.Types.PhaseSelector exists to prevent.
toJson :: ActivationTiming.ActivationTiming -> Value.Value
toJson t = case t of
  ActivationTiming.AnyTime -> Common.nullary "AnyTime"
  ActivationTiming.SorcerySpeed -> Common.nullary "SorcerySpeed"
  ActivationTiming.DuringPhase sel sc -> Common.tagged "DuringPhase" . Just . Common.array $ [PhaseSelector.toJson sel, TurnScope.toJson sc]

fromJson :: Value.Value -> Either Text.Text ActivationTiming.ActivationTiming
fromJson value = do
  (t, mv) <- Common.asTagged value
  case (t, mv) of
    ("AnyTime", _) -> Right ActivationTiming.AnyTime
    ("SorcerySpeed", _) -> Right ActivationTiming.SorcerySpeed
    ("DuringPhase", Just (Value.Array (Array.MkArray [sel, sc]))) -> ActivationTiming.DuringPhase <$> PhaseSelector.fromJson sel <*> TurnScope.fromJson sc
    _ -> Left . Text.pack $ "unknown ActivationTiming: " <> t
