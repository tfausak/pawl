module Pawl.Types.ProposedEvent where

import qualified Data.Sequence as Seq
import qualified Numeric.Natural as Natural
import qualified Pawl.Types.CounterCause as CounterCause
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.DestructionCause as DestructionCause
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.LifeLossCause as LifeLossCause
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PhaseSelector as PhaseSelector
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.TokenLot as TokenLot
import qualified Pawl.Types.TurnUpProcedure as TurnUpProcedure
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
-- WouldTurnFaceUp carries the object for WouldEnter's reason: CR 708.11 has the
-- ability applied WHILE the permanent is turning over, and
-- Pawl.Engine.FaceDown.performTurnFaceUp has already written the face-up status
-- by the time it raises this -- so every property a CR 614.1e replacement can
-- modify is read off, and written to, the object.
--
-- It carries the PROCEDURE beside it, which no other arm needs, because CR
-- 702.37b's rewrite is conditional on WHICH COST WAS PAID -- "put a +1/+1 counter
-- on it if its megamorph cost was paid to turn it face up" -- and CR 701.40c gives
-- a manifested megamorph card a second, cheaper way over that pays no megamorph
-- cost at all. Nothing on the object records which road it came by, so the event
-- has to.
--
-- MAYBE the procedure, because CR 708.7's two procedures are CR 116.2b special
-- actions and an Effect.TurnFaceUp is neither: it takes no procedure, shows
-- nothing and pays nothing. Nothing is what a reader gating on a paid cost -- CR
-- 702.37b's megamorph counter, the only such reader -- correctly declines.
--
-- Thirteen arms, not every replaceable event class the rules define: each of the rest
-- is one more arm plus the funnel that raises it.
data ProposedEvent
  = WouldChangeZone ZoneChange.ZoneChange
  | WouldEnter ObjectId.ObjectId
  | WouldDealDamage DamageEvent.DamageEvent
  | -- | CR 701.8 / 701.19c: a permanent would be destroyed. The Regenerability is
    -- the destruction's own, not the permanent's: it says whether a CR 701.19a
    -- regeneration shield may be applied to THIS destruction, which is where
    -- Terror's "It can't be regenerated" lives. CR 701.19c's lasting prohibition
    -- (Hurr Jackal) reaches the same field, Event.resolveDestruction folding a
    -- standing Pawl.Types.ActiveUnregeneratable in before the event is proposed.
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
  | -- | CR 111.1: this player would create these tokens, lot by lot. A sequence
    -- of lots rather than one card and a count so that a CR 614.1a append
    -- (Pawl.Types.TokenR.plus) stays inside the ONE event a later row scales.
    WouldCreateTokens PlayerId.PlayerId (Seq.Seq TokenLot.TokenLot)
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
    -- Raised by Pawl.Engine.FaceDown.performTurnFaceUp, the only place in the
    -- engine that turns anything face up -- the one funnel CR 116.2b's special
    -- action and Effect.TurnFaceUp both go through.
    WouldTurnFaceUp ObjectId.ObjectId (Maybe TurnUpProcedure.TurnUpProcedure)
  | -- | CR 701.26b / 122.1d: a permanent would become untapped. Raised by
    -- Pawl.Engine.Event.resolveUntap, the one funnel CR 502.3's turn-based
    -- action, Effect.Untap and CR 107.6's untap symbol in a cost all go through.
    --
    -- The ObjectId and nothing else: rule 701.26b's action has no other content
    -- -- no source, no amount, no destination -- and the one property a
    -- replacement can change about it is whether it happens at all.
    --
    -- Rule 701.26b's second sentence ("only tapped permanents can be untapped")
    -- is the funnel's guard rather than a field here, so this event is never
    -- proposed for an already-untapped permanent and a stun counter is never
    -- spent on one.
    WouldUntap ObjectId.ObjectId
  | -- | CR 119.3 / 120.4c: a player would lose life. Two funnels raise it, one
    -- per cause: Pawl.Engine.Damage.applyDamage at CR 120.4c's result-processing
    -- step, and Pawl.Engine.Resolve's Effect.LoseLife arm.
    --
    -- The LifeLossCause is the same shape of fact WouldPutCounters' CounterCause
    -- is, and for its reason: nothing about the loss itself says where it came
    -- from, and Worship's ruling makes a card split on it ("loss of life bypasses
    -- Worship"). It gates which candidates are offered the event and is not a
    -- property of the player.
    --
    -- The Natural is the life that WOULD be lost, never the resulting total: a
    -- rewrite that states a total (Pawl.Types.LifeLossRewrite's LeaveAtLeast)
    -- reads the player's current life off the board and answers in this
    -- field's currency, so CR 616.2's next iteration sees a smaller loss rather
    -- than a differently-shaped event.
    WouldLoseLife LifeLossCause.LifeLossCause PlayerId.PlayerId Natural.Natural
  | -- | CR 121.1 / 614.11: a player would draw a card. Raised by
    -- Pawl.Engine.Event.drawCardReturning, the one funnel CR 121.1's turn-based
    -- draw, an opening hand and an Effect.Draw all go through -- and raised
    -- BEFORE the library is looked at, which is rule 614.11's "applied even if no
    -- cards could be drawn because there are no cards in the affected player's
    -- library".
    --
    -- ONE draw and no count: CR 121.2 makes an instruction to draw several cards
    -- that many individual card draws, and this funnel runs once per draw. The
    -- INSTRUCTION is WouldDrawCards below, a different event class that CR 616.1g
    -- settles first.
    --
    -- The PlayerId and nothing else: rule 121.1's action has no source, no amount
    -- and no destination a replacement could rewrite, and the card it would move
    -- is not chosen until the draw happens.
    WouldDraw PlayerId.PlayerId
  | -- | CR 121.2a: a player would be instructed to draw this many cards. Raised by
    -- Pawl.Engine.Resolve's Effect.Draw arm once per drawer, after the quantity is
    -- evaluated and before any of CR 121.2's individual draws -- rule 121.2a's
    -- "this modification occurs before considering any of the individual card
    -- draws", and rule 616.1g's containment: the instruction is settled here, then
    -- each draw it leaves raises its own WouldDraw.
    --
    -- The count is the number the instruction NAMES, never a library or a hand
    -- size, so a row that resizes it answers in the same currency and CR 616.2's
    -- next iteration sees a differently-numbered instruction rather than a
    -- differently-shaped event.
    --
    -- Effect.Draw is the only funnel that raises it, which is CR 121.2a's own
    -- scope: rule 121.1's other two roads are the draw step's turn-based action
    -- and CR 103.5's opening hand. The first draws one card, which is no
    -- instruction to draw multiple; the second runs before any replacement effect
    -- can exist, so the two readings are indistinguishable there.
    WouldDrawCards PlayerId.PlayerId Natural.Natural
  deriving (Eq, Ord, Show)
