module Pawl.Types.PhasePattern where

import Pawl.Types.PhaseSelector (PhaseSelector)
import Pawl.Types.PlayerId (PlayerId)

-- CR 614.1b / 500.11: which step-or-phase beginnings a SKIP intercepts. Eon Hub
-- is (Step (Beginning Upkeep)) for everybody; Fatigue is
-- (Step (Beginning DrawStep)) for the one player its resolution named; Stonehorn
-- Dignitary is CombatPhase for that player -- the whole of CR 506.1's five steps
-- and none of them individually.
--
-- WHICH windows a PhaseSelector can name, and why a bare Phase cannot name them
-- all, is Pawl.Types.PhaseSelector's own question.
--
-- Not expressible: a skip scoped to one identified turn (#334).
--
-- `whosePhase` is CR 614.1's "does this instance apply" question asked about a
-- PLAYER, and Nothing is not a missing answer -- it is EVERY player. Eon Hub's
-- "PLAYERS skip their upkeep steps" is symmetric, and so is Stasis's "players
-- skip their untap steps", so a pattern printed on a permanent writes Nothing
-- and the event's PlayerId goes unread. Just is Fatigue's "TARGET PLAYER skips
-- their next draw step": the skipped step is the named player's, which for a
-- step of the turn means it applies only on that player's own turn
-- (Pawl.Engine.Replacement.applies compares it against ProposedEvent.WouldBeginPhase's
-- active player).
--
-- The PlayerId is BAKED at resolution, by Resolve.applyEffect's SkipNextPhase
-- arm, exactly as Modification.SetController's is by GainControl -- card data
-- cannot name a player, so this is the only way one gets in. Meant to be
-- runtime-only, and nothing ENFORCES that: the codec round-trips it, so card
-- JSON could author a Just, which is meaningless (#437).
data PhasePattern = MkPhasePattern
  { whichPhase :: PhaseSelector,
    whosePhase :: Maybe PlayerId
  }
  deriving (Eq, Ord, Show)
