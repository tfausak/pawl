module Pawl.Types.PhasePattern where

import qualified Pawl.Types.PhaseSelector as PhaseSelector
import qualified Pawl.Types.PlayerId as PlayerId

-- | CR 614.1b / 500.11: which step-or-phase beginnings a SKIP intercepts. Eon Hub
-- is (Step (Beginning Upkeep)) for everybody; Stonehorn Dignitary is CombatPhase
-- for one player -- the whole of CR 506.1's five steps and none of them
-- individually.
--
-- `whosePhase` is CR 614.1's "does this instance apply" question asked about a
-- PLAYER, and Nothing means EVERY player (Stasis, Eon Hub), not a missing
-- answer. Just is Fatigue's "target player skips their next draw step", which
-- for a step of the turn means it applies only on that player's own turn.
--
-- The PlayerId is BAKED by the engine, never authored: card data cannot name a
-- player, exactly as it cannot name Modification.SetController's. The TYPE does
-- not enforce that, since ReplacementEffect.PhaseR carries both the authored and
-- the baked halves; Pawl.CardSpec's "no card authors a player-scoped phase skip"
-- rejects a Just in card JSON instead, the same treatment SetController's baked
-- PlayerId gets (#199).
data PhasePattern = MkPhasePattern
  { whichPhase :: PhaseSelector.PhaseSelector,
    whosePhase :: Maybe PlayerId.PlayerId
  }
  deriving (Eq, Ord, Show)
