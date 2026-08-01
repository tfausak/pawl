module Pawl.Types.PhaseSelector where

import Pawl.Types.Phase (Phase)

-- | CR 500.11 / 614.10: which step-or-phase a skip names, and which one
-- Pawl.Engine.Engine offers up as it is about to begin. The two sides of CR 614.1b are
-- compared by EQUALITY on this type, so a pattern that names a phase can never
-- be confused with one that names a step of it.
--
-- Pawl.Types.Phase is one type over CR 500.1's five phases and their steps, and
-- GameState.remaining is a Seq of exactly those, so a Phase value is always a
-- SCHEDULE ENTRY: one step, or a phase that has none. That is what makes `Step`
-- the arm carrying it, and it is enough for Eon Hub's upkeep step, Fatigue's
-- draw step and for either main phase -- CR 505.2, "the main phase has no
-- steps", makes naming that phase and naming its one entry the same act.
--
-- The other three arms are CR 500.1's stepped phases: "the beginning, combat, and
-- ending phases are further broken down into steps, which proceed in order".
-- Stonehorn Dignitary's "skips their next combat phase" names ALL of CR 506.1's
-- five steps and none of them individually, which no Phase value can say.
--
-- Three arms rather than one `WholePhase PhaseKind`, and no arm for either main
-- phase: this way every namable window has exactly ONE spelling. A separate kind
-- type would let `WholePhase PrecombatMainPhase` and `Step PrecombatMain` mean
-- the same thing, and every reader would owe both a case.
--
-- Vocabulary on a finished axis, which is why all three stepped phases are here
-- while only the combat one has a producer in the pool: CR 500.1 fixes the set at
-- three, and Pawl.Engine.Turn.phaseBeginningAt has to answer for every Phase regardless.
data PhaseSelector
  = Step Phase
  | BeginningPhase
  | CombatPhase
  | EndingPhase
  deriving (Eq, Ord, Show)
