module Pawl.Type.PhasePattern where

import Pawl.Type.Phase (Phase)
import Pawl.Type.PlayerId (PlayerId)

-- CR 614.1b / 500.11: which step-or-phase beginnings a SKIP intercepts. Eon Hub
-- is (Beginning Upkeep) for everybody; Fatigue is (Beginning DrawStep) for the
-- one player its resolution named.
--
-- Pawl.Type.Phase is one type over both the CR 500.1 phases and their steps, and
-- GameState.remaining is a Seq of exactly these, so naming a step and naming a
-- STEPLESS phase (CR 500.1's two main phases) are the same act. A phase that HAS
-- steps is not: CR 500.1's beginning, combat and ending phases exist in this type
-- only as their steps, so "skip your next combat phase" (Stonehorn Dignitary)
-- would have to name five of them at once. The shape that fixes it -- a
-- phase-level pattern, or a Phase value standing for the whole phase -- is a
-- choice a card should make, not this file (#337).
--
-- Also not expressible: a skip scoped to one identified turn (#334).
--
-- `whosePhase` is CR 614.1's "does this instance apply" question asked about a
-- PLAYER, and Nothing is not a missing answer -- it is EVERY player. Eon Hub's
-- "PLAYERS skip their upkeep steps" is symmetric, and so is Stasis's "players
-- skip their untap steps", so a pattern printed on a permanent writes Nothing
-- and the event's PlayerId goes unread. Just is Fatigue's "TARGET PLAYER skips
-- their next draw step": the skipped step is the named player's, which for a
-- step of the turn means it applies only on that player's own turn
-- (Pawl.Replacement.applies compares it against ProposedEvent.WouldBeginPhase's
-- active player).
--
-- The PlayerId is BAKED at resolution, by Resolve.applyEffect's SkipNextPhase
-- arm, exactly as Modification.SetController's is by GainControl -- card data
-- cannot name a player, so this is the only way one gets in. Meant to be
-- runtime-only, and nothing ENFORCES that: the codec round-trips it, so card
-- JSON could author a Just, which is meaningless (#437).
data PhasePattern = MkPhasePattern
  { whichPhase :: Phase,
    whosePhase :: Maybe PlayerId
  }
  deriving (Eq, Ord, Show)
