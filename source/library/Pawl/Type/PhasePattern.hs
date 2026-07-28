module Pawl.Type.PhasePattern where

import Pawl.Type.Phase (Phase)

-- CR 614.1b / 500.11: which step-or-phase beginnings a SKIP intercepts. Eon Hub
-- is (Beginning Upkeep).
--
-- Pawl.Type.Phase is one type over both the CR 500.1 phases and their steps, and
-- GameState.remaining is a Seq of exactly these, so naming a step and naming a
-- STEPLESS phase (CR 500.1's two main phases) are the same act. A phase that HAS
-- steps is not: CR 500.1's beginning, combat and ending phases exist in this type
-- only as their steps, so "skip your next combat phase" (Stonehorn Dignitary)
-- would have to name five of them at once. No producer today, and the shape that
-- fixes it -- a phase-level pattern, or a Phase value standing for the whole
-- phase -- is a choice a card should make, not this file.
--
-- No WHOSE field. Eon Hub's "PLAYERS skip their upkeep steps" is symmetric, and
-- so is Stasis's "players skip their untap steps" -- every skip pawl can express
-- applies to every player, so the pattern has no relation to read against the
-- event's PlayerId. A controller-relative skip ("skip YOUR next draw step") is
-- what would add one; it has no producer either, and per the note on
-- Pawl.Type.ReplacementEffect a field appears when a card needs it rather than
-- as speculative structure.
newtype PhasePattern = MkPhasePattern
  { whichPhase :: Phase
  }
  deriving (Eq, Ord, Show)
