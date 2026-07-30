module Pawl.Types.ProposedEvent where

import Numeric.Natural (Natural)
import Pawl.Types.Card (Card)
import Pawl.Types.CounterKind (CounterKind)
import Pawl.Types.DamageEvent (DamageEvent)
import Pawl.Types.ObjectId (ObjectId)
import Pawl.Types.Phase (Phase)
import Pawl.Types.PlayerId (PlayerId)
import Pawl.Types.Regenerability (Regenerability)
import Pawl.Types.ZoneChange (ZoneChange)

-- CR 614.6: an event as it WOULD happen -- the thing a replacement effect
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
-- WouldBeginPhase is the one arm that is not about an OBJECT: CR 614.1b's skips
-- replace a step, a phase or a turn, none of which any object owns. Its PlayerId
-- is the active player -- the player whose step it is, and so CR 616.1's
-- "affected player" for the choice among applicable skips.
--
-- Seven arms, not the ~40 replaceable event classes the rules define: each of the
-- rest is one more arm plus the funnel that raises it -- vocabulary on a finished
-- axis, which is what "the closed half can genuinely be finished" means here.
data ProposedEvent
  = WouldChangeZone ZoneChange
  | WouldEnter ObjectId
  | WouldDealDamage DamageEvent
  | -- CR 701.8 / 701.19c: a permanent would be destroyed. The Regenerability is
    -- the destruction's own, not the permanent's: it says whether a CR 701.19a
    -- regeneration shield may be applied to THIS destruction, which is where
    -- Terror's "It can't be regenerated" lives.
    WouldBeDestroyed ObjectId Regenerability
  | WouldPutCounters ObjectId CounterKind Natural
  | WouldCreateTokens PlayerId Card Natural
  | -- CR 500.11 / 614.10: a step or phase would begin, on this player's turn.
    -- Raised by Engine.runStep before anything about the step is observable, so
    -- a skip that takes it leaves no trace -- CR 614.10's "once a step, phase, or
    -- turn has started, it can no longer be skipped" read as the moment this
    -- event exists.
    WouldBeginPhase Phase PlayerId
  deriving (Eq, Show)
