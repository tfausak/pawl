module Pawl.Types.ProposedEvent where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.CounterCause as CounterCause
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.DestructionCause as DestructionCause
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PhaseSelector as PhaseSelector
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
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
-- else) and is NESTED inside whatever caused the entry -- CR 616.1g's
-- containment, expressed as call nesting rather than as a field.
--
-- It carries an ObjectId and NOTHING ELSE, including no would-be controller for
-- CR 616.1b to rewrite, because the engine materializes the entering permanent
-- before the loop runs (CR 614.12). Every property of the entry a replacement can
-- modify therefore lives on the OBJECT -- the copy snapshot, the entry choice,
-- the counters, and CR 110.2's default controller -- and each rewrite writes
-- there, which is also what makes CR 616.2 fall out: the loop's next iteration
-- re-collects against a board that already shows the change.
--
-- WouldBeginPhase is the one arm that is not about an OBJECT: CR 614.1b's skips
-- replace a step, a phase or a turn, none of which any object owns. Its PlayerId
-- is the active player, and so CR 616.1's "affected player" for the choice among
-- applicable skips.
--
-- WouldTurnFaceUp carries an ObjectId and nothing else for WouldEnter's reason:
-- CR 708.11 has the ability applied WHILE the permanent is turning over, and
-- Pawl.Engine.FaceDown.turnFaceUp has already written the face-up status by the
-- time it raises this -- so every property a CR 614.1e replacement can modify is
-- read off, and written to, the object.
--
-- Nine arms, not every replaceable event class the rules define: each of the
-- rest is one more arm plus the funnel that raises it.
data ProposedEvent
  = WouldChangeZone ZoneChange.ZoneChange
  | WouldEnter ObjectId.ObjectId
  | WouldDealDamage DamageEvent.DamageEvent
  | -- | CR 701.8 / 701.19c: a permanent would be destroyed. The Regenerability is
    -- the destruction's own, not the permanent's: it says whether a CR 701.19a
    -- regeneration shield may be applied to THIS destruction, which is where
    -- Terror's "It can't be regenerated" lives.
    --
    -- The DestructionCause beside it is the same shape of fact one rule further
    -- on: CR 122.1c's replacement reaches a destruction only when an EFFECT is
    -- what would destroy the permanent, where regeneration reaches CR 704.5g's
    -- lethal-damage destruction too. Both fields gate which candidates are
    -- offered the event and neither is a characteristic of the object.
    WouldBeDestroyed ObjectId.ObjectId Regenerability.Regenerability DestructionCause.DestructionCause
  | -- | CR 122.6: counters would be put on a PERMANENT. The CounterCause is who
    -- is putting them and whether an effect is doing it -- what CR 614.16 and
    -- Vorinclex, Monstrous Raider narrow by, and the one ProposedEvent field that
    -- is about the event's PROVENANCE rather than its content. It rides the event
    -- rather than being consumed before the loop because the two clauses disagree
    -- about which causes they reach, so only a row can decide.
    WouldPutCounters CounterCause.CounterCause ObjectId.ObjectId (CounterKind.CounterKind Keyword.Keyword) Natural.Natural
  | -- | CR 122.1 / 122.6: counters would be put on a PLAYER.
    --
    -- A separate arm from WouldPutCounters rather than one arm over a recipient
    -- sum, because the two recipients take disjoint kinds: CR 122.1's object
    -- kinds and its player ones share no constructor, which is why
    -- Pawl.Types.PlayerCounterKind is its own type. One arm would have to admit
    -- a +1/+1 counter on a player.
    WouldPutPlayerCounters CounterCause.CounterCause PlayerId.PlayerId PlayerCounterKind.PlayerCounterKind Natural.Natural
  | WouldCreateTokens PlayerId.PlayerId Card.Card Natural.Natural
  | -- | CR 500.11 / 614.10: a step or phase would begin, on this player's turn.
    -- Raised by Engine.runStep before anything about the step is observable, so
    -- a skip that takes it leaves no trace -- CR 614.10 stops a step, phase or
    -- turn being skipped once it has started, read as the moment this event
    -- exists.
    --
    -- A PhaseSelector, not a Phase, because CR 500.11 lets a skip name a whole
    -- PHASE and CR 500.1's beginning, combat and ending phases are more than one
    -- schedule entry each. Engine.runStep therefore raises this TWICE at the
    -- first step of such a phase -- once for the phase, once for the step.
    WouldBeginPhase PhaseSelector.PhaseSelector PlayerId.PlayerId
  | -- | CR 614.1e / 708.11: a face-down permanent is being turned face up.
    -- Raised by Pawl.Engine.FaceDown.turnFaceUp, the only place in the engine
    -- that turns anything face up, and the one funnel CR 116.2b's special action
    -- goes through.
    WouldTurnFaceUp ObjectId.ObjectId
  deriving (Eq, Ord, Show)
