-- | The @ActivationTiming ⇆ Json@ codec (#481).
module Pawl.Codec.ActivationTiming where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Json as Json
import Pawl.Codec.PhaseSelector (jsonToPhaseSelector, phaseSelectorToJson)
import Pawl.Codec.TurnScope (jsonToTurnScope, turnScopeToJson)
import Pawl.Json.Array (Array (MkArray))
import Pawl.Json.Value (Value (Array))
import qualified Pawl.Types.ActivationTiming as ActivationTiming

-- Tagged rather than bare-nullary since CR 500.1's DuringPhase carries a window,
-- the shape costComponentToJson takes. AnyTime and SorcerySpeed still render as
-- the bare tag they always did.
--
-- DuringPhase's payload is a PAIR -- the window and the turn scope -- written as
-- a two-element array, which is the shape triggerConditionToJson already gives
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
activationTimingToJson :: ActivationTiming.ActivationTiming -> Value
activationTimingToJson t = case t of
  ActivationTiming.AnyTime -> Json.nullary (Text.pack "AnyTime")
  ActivationTiming.SorcerySpeed -> Json.nullary (Text.pack "SorcerySpeed")
  ActivationTiming.DuringPhase sel sc -> Json.tagged (Text.pack "DuringPhase") (Just (Array (MkArray [phaseSelectorToJson sel, turnScopeToJson sc])))

jsonToActivationTiming :: Value -> Either Text ActivationTiming.ActivationTiming
jsonToActivationTiming value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("AnyTime", _) -> Right ActivationTiming.AnyTime
    ("SorcerySpeed", _) -> Right ActivationTiming.SorcerySpeed
    ("DuringPhase", Just (Array (MkArray [sel, sc]))) -> ActivationTiming.DuringPhase <$> jsonToPhaseSelector sel <*> jsonToTurnScope sc
    _ -> Left (Text.pack "unknown ActivationTiming: " <> t)
