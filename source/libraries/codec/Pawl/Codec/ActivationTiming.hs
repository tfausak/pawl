-- | The @ActivationTiming ⇆ Json@ codec (#481).
module Pawl.Codec.ActivationTiming where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Json as Json
import Pawl.Codec.Phase (jsonToPhase, phaseToJson)
import qualified Pawl.Codec.TurnScope as TurnScope
import Pawl.Json.Array (Array (MkArray))
import Pawl.Json.Value (Value (Array))
import qualified Pawl.Types.ActivationTiming as ActivationTiming

-- Tagged rather than bare-nullary since CR 500.1's DuringPhase carries a phase,
-- the shape costComponentToJson takes. AnyTime and SorcerySpeed still render as
-- the bare tag they always did.
--
-- DuringPhase's payload is a PAIR -- the phase and the turn scope -- written as
-- a two-element array, which is the shape triggerConditionToJson already gives
-- TriggerCondition.StepBegins for the same two components. There is deliberately
-- no scope-less form: the axis is not a default a card may omit, so a bare
-- phase payload is a decode failure rather than a silent EachTurn.
activationTimingToJson :: ActivationTiming.ActivationTiming -> Value
activationTimingToJson t = case t of
  ActivationTiming.AnyTime -> Json.nullary (Text.pack "AnyTime")
  ActivationTiming.SorcerySpeed -> Json.nullary (Text.pack "SorcerySpeed")
  ActivationTiming.DuringPhase p sc -> Json.tagged (Text.pack "DuringPhase") (Just (Array (MkArray [phaseToJson p, TurnScope.toJson sc])))

jsonToActivationTiming :: Value -> Either Text ActivationTiming.ActivationTiming
jsonToActivationTiming value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("AnyTime", _) -> Right ActivationTiming.AnyTime
    ("SorcerySpeed", _) -> Right ActivationTiming.SorcerySpeed
    ("DuringPhase", Just (Array (MkArray [p, sc]))) -> ActivationTiming.DuringPhase <$> jsonToPhase p <*> TurnScope.fromJson sc
    _ -> Left (Text.pack "unknown ActivationTiming: " <> t)
