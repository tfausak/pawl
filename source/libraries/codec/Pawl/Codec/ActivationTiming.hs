module Pawl.Codec.ActivationTiming where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.PhaseSelector as PhaseSelector
import qualified Pawl.Codec.TurnScope as TurnScope
import qualified Pawl.Json.Array as Array
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.ActivationTiming as ActivationTiming

-- | Tagged rather than bare-nullary since CR 500.1's DuringPhase carries a
-- window; AnyTime and SorcerySpeed still render as bare tags.
--
-- DuringPhase's payload is a PAIR -- the window and the turn scope -- written
-- as a two-element array. There is deliberately no scope-less form: the axis is
-- not a default a card may omit, so a bare window payload is a decode failure
-- rather than a silent EachTurn.
--
-- The window is a PhaseSelector, so a rider naming a step nests it inside that
-- type's own `Step` tag. The codec does not also accept the older bare-phase
-- spelling: two spellings of one window is the thing Pawl.Types.PhaseSelector
-- exists to prevent.
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
