module Pawl.Types.ExtraPhase where

-- | CR 500.8: one phase an effect adds to a turn. The rule does not fix WHICH,
-- and printed cards vary -- Relentless Assault adds a combat and a main phase,
-- Aurelia, the Warleader only a combat phase.
--
-- A whole PHASE, never a step: Pawl.Engine.Turn.expandExtraPhase inserts
-- CR 506.1's five steps for ExtraCombat and CR 505.2's stepless main phase for
-- ExtraMain, so the payload says what the card says and what a phase is made of
-- stays in Pawl.Engine.Turn.
data ExtraPhase
  = -- | CR 506.1's five steps.
    ExtraCombat
  | -- | CR 505.1a: an added main phase is a POSTCOMBAT main phase.
    ExtraMain
  deriving (Eq, Ord, Show)
