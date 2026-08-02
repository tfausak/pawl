module Pawl.Types.ProposedEvent where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PhaseSelector as PhaseSelector
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.ZoneChange as ZoneChange

-- | CR 614.6: an event as it WOULD happen -- the thing a replacement effect
-- rewrites. Deliberately distinct from Pawl.Types.GameEvent: a GameEvent is
-- HISTORY (it carries a CR 608.2h last-known-information snapshot and exists only
-- after the fact), while a ProposedEvent exists only while it is being replaced,
-- and the one that survives the CR 616.1 loop is the one that actually happens.
--
-- WouldEnter is raised only for BATTLEFIELD entries (CR 614.1c-d apply nowhere
-- else) and is NESTED inside whatever caused the entry -- CR 616.1g's containment
-- ("one effect may apply to an event, and another to an event contained within
-- the first"), expressed as call nesting rather than as a field.
--
-- It carries an ObjectId and NOTHING ELSE, including no would-be controller for
-- CR 616.1b to rewrite, because the engine materializes the entering permanent
-- before the loop runs (CR 614.12's "as it would exist on the battlefield", see
-- Pawl.Engine.Replacement.runEntry). Every property of the entry that a
-- replacement can modify therefore lives on the OBJECT -- the copy snapshot, the
-- entry choice, the counters, and CR 110.2's default controller -- and each
-- rewrite writes there, which is also what makes CR 616.2 fall out: the loop's
-- next iteration re-collects against a board that already shows the change.
--
-- WouldBeginPhase is the one arm that is not about an OBJECT: CR 614.1b's skips
-- replace a step, a phase or a turn, none of which any object owns. Its PlayerId
-- is the active player -- the player whose step it is, and so CR 616.1's
-- "affected player" for the choice among applicable skips.
--
-- Seven arms, not the ~40 replaceable event classes the rules define: each of the
-- rest is one more arm plus the funnel that raises it -- vocabulary on a finished
-- axis, which is what "the closed half can genuinely be finished" means here.
data ProposedEvent
  = WouldChangeZone ZoneChange.ZoneChange
  | WouldEnter ObjectId.ObjectId
  | WouldDealDamage DamageEvent.DamageEvent
  | -- | CR 701.8 / 701.19c: a permanent would be destroyed. The Regenerability is
    -- the destruction's own, not the permanent's: it says whether a CR 701.19a
    -- regeneration shield may be applied to THIS destruction, which is where
    -- Terror's "It can't be regenerated" lives.
    WouldBeDestroyed ObjectId.ObjectId Regenerability.Regenerability
  | WouldPutCounters ObjectId.ObjectId CounterKind.CounterKind Natural.Natural
  | WouldCreateTokens PlayerId.PlayerId Card.Card Natural.Natural
  | -- | CR 500.11 / 614.10: a step or phase would begin, on this player's turn.
    -- Raised by Engine.runStep before anything about the step is observable, so
    -- a skip that takes it leaves no trace -- CR 614.10's "once a step, phase, or
    -- turn has started, it can no longer be skipped" read as the moment this
    -- event exists.
    --
    -- A PhaseSelector, not a Phase, because CR 500.11 lets a skip name a whole
    -- PHASE and CR 500.1's beginning, combat and ending phases are more than one
    -- schedule entry each. Engine.runStep therefore raises this TWICE at the
    -- first step of such a phase -- once for the phase, once for the step -- and
    -- CR 614.10's "once a step, phase, or turn has started" is what keeps the
    -- phase question to that one moment.
    WouldBeginPhase PhaseSelector.PhaseSelector PlayerId.PlayerId
  deriving (Eq, Show)
