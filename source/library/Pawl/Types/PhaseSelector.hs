module Pawl.Types.PhaseSelector where

import qualified Pawl.Types.Phase as Phase

-- | CR 500.11 / 614.10: which step-or-phase a skip names, and which one
-- Pawl.Engine.Engine offers up as it is about to begin. The two sides of CR 614.1b are
-- compared by EQUALITY on this type, so a pattern that names a phase can never
-- be confused with one that names a step of it.
--
-- A Pawl.Types.Phase value is always a SCHEDULE ENTRY -- one step, or a phase
-- that has none -- since GameState.remaining is a Seq of exactly those. That is
-- what `Step` carries, and CR 505.2 makes it enough for either main phase, naming
-- the phase and naming its one entry being the same act.
--
-- The other three arms are CR 500.1's stepped phases. Stonehorn Dignitary's
-- "skips their next combat phase" names ALL of CR 506.1's five steps and none of
-- them individually, which no Phase value can say. Three arms rather than one
-- `WholePhase PhaseKind`, and none for either main phase, so that every namable
-- window has exactly ONE spelling and no reader owes two cases for it.
--
-- Vocabulary on a finished axis: CR 500.1 fixes the set at three, so all three
-- are here though only the combat one has a producer in the pool.
data PhaseSelector
  = Step Phase.Phase
  | BeginningPhase
  | CombatPhase
  | EndingPhase
  deriving (Eq, Ord, Show)
