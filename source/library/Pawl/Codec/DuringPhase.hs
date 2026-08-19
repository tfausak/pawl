{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.DuringPhase where

import qualified Pawl.Codec.PhaseSelector as PhaseSelector
import qualified Pawl.Codec.TurnScope as TurnScope
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.DuringPhase as DuringPhase

-- | A bare object keyed by the record's field names, replacing the two-element
-- array this payload used to be.
--
-- The window is a PhaseSelector, so a rider naming a step nests it inside that
-- type's own `Step` tag. The codec does not also accept the older bare-phase
-- spelling: two spellings of one window is the thing Pawl.Types.PhaseSelector
-- exists to prevent.
codec :: Codec.Codec DuringPhase.DuringPhase
codec = Fields.object $ do
  window <- Fields.required "window" PhaseSelector.codec DuringPhase.window
  scope <- Fields.required "scope" TurnScope.codec DuringPhase.scope
  pure DuringPhase.MkDuringPhase {DuringPhase.window = window, DuringPhase.scope = scope}
