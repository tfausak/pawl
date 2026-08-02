module Pawl.Types.PhasePattern where

import qualified Pawl.Types.PhaseSelector as PhaseSelector
import qualified Pawl.Types.PlayerId as PlayerId

-- | CR 614.1b / 500.11: which step-or-phase beginnings a SKIP intercepts. Eon Hub
-- is (Step (Beginning Upkeep)) for everybody; Fatigue is
-- (Step (Beginning DrawStep)) for the one player its resolution named; Stonehorn
-- Dignitary is CombatPhase for that player -- the whole of CR 506.1's five steps
-- and none of them individually.
--
-- WHICH windows a PhaseSelector can name, and why a bare Phase cannot name them
-- all, is Pawl.Types.PhaseSelector's own question.
--
-- A skip scoped to ONE identified turn is not said here either, and does not need
-- to be: Savor the Moment's "skip the untap step of that turn" rides on the
-- pending turn itself (Pawl.Types.ExtraTurn) and becomes an ordinary pattern of
-- this shape -- selector plus taker -- only once that turn begins, by which point
-- "that turn" and "this turn" are the same turn
-- (Pawl.Engine.Replacement.installTurnSkips).
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
-- The PlayerId is BAKED by the engine, never authored: card data cannot name a
-- player, exactly as it cannot name Modification.SetController's. Two bakers, and
-- they differ in WHEN rather than in what they write -- Resolve.applyEffect's
-- SkipNextPhase arm at resolution, and Pawl.Engine.Replacement.installTurnSkips as
-- an extra turn begins (see above). Runtime-only, and the TYPE does not enforce
-- that: this pattern reaches card data only inside ReplacementEffect.PhaseR, and
-- that sum is the carrier for BOTH halves -- Card.replacementEffects, which a
-- card authors, and ActiveReplacement.effect, which the engine bakes. Making a
-- Just unrepresentable card-side would mean splitting or parameterizing
-- ReplacementEffect, not merely this record. Pawl.CardSpec's "no card authors a
-- player-scoped phase skip" is what rejects a Just written into card JSON
-- instead -- the same treatment, for the same structural reason, that
-- Modification.SetController's baked PlayerId gets (#199).
data PhasePattern = MkPhasePattern
  { whichPhase :: PhaseSelector.PhaseSelector,
    whosePhase :: Maybe PlayerId.PlayerId
  }
  deriving (Eq, Ord, Show)
