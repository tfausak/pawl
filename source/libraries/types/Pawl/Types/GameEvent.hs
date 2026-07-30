module Pawl.Types.GameEvent where

import Pawl.Types.DamageEvent (DamageEvent)
import Pawl.Types.ObjectId (ObjectId)
import Pawl.Types.Phase (Phase)
import Pawl.Types.PlayerId (PlayerId)
import Pawl.Types.ProjectedCharacteristics (ProjectedCharacteristics)
import Pawl.Types.ZoneChange (ZoneChange)

-- CR 608.2i: one entry of the turn-scoped record of what happened. "Some effects
-- look back in time and require information about previous game states and
-- actions rather than considering the current game state" -- so entries are
-- APPENDED by the change-and-emit funnels and never removed by a reader. Each
-- reader keeps its own watermark into GameState.events; the log itself is cleared
-- only at turn handoff.
data GameEvent
  = -- CR 400.7: an object moved between zones. The ZoneChange is the RESOLVED
    -- (post-replacement) event, carrying the RESULTING object's id.
    --
    -- The ProjectedCharacteristics is the moved object as it last existed in the
    -- zone it LEFT (CR 608.2h: "if it's no longer in that zone ... the effect uses
    -- the object's last known information"). A snapshot, never a re-derivation
    -- from the printed card: a land animated into a creature DIED as a creature,
    -- and a token has no printed card at all (CR 111.3).
    --
    -- Strict (!): the snapshot is taken as of THIS zone change, not as of
    -- whenever a reader eventually forces it. An unforced field would be a thunk
    -- closing over the entire pre-move GameState, appended to a log that lives
    -- for a whole turn -- retaining a turn's worth of superseded states instead
    -- of the one small value this constructor is meant to carry.
    Moved ZoneChange !ProjectedCharacteristics
  | -- CR 120 / 510: damage was dealt. The record the CR 704.5h deathtouch
    -- state-based action reads, watermarked rather than drained.
    DamageDealt DamageEvent
  | -- CR 603.2b: a phase or step began, on whose turn (the active player). What
    -- both an "at the beginning of each end step" step trigger and a CR 603.7
    -- delayed ability match against -- the second consumer is
    -- Pawl.Event.delayedPending (Tidal Wave's "sacrifice it at the beginning of
    -- the next end step" depends on it).
    StepBegan Phase PlayerId
  | -- CR 601.2i: a player cast a spell. The event Rule of Law counts, and the
    -- reason the count is a fold over P4's whole turn log rather than a
    -- per-effect watermark: its ruling looks at "the entire turn ... even if
    -- Rule of Law wasn't on the battlefield when that spell was cast".
    --
    -- The CAST is the event, not the resolution -- its second ruling ("If you
    -- cast a spell that was countered, you can't cast another spell during the
    -- same turn") is what fixes that.
    SpellCast PlayerId
  | -- CR 725.1: a player became the monarch. What Palace Jailer's exile duration
    -- keys off, and the substrate for any future "whenever a player becomes the
    -- monarch" trigger.
    BecameMonarch PlayerId
  | -- CR 702.29c: a card was cycled -- "discard[ed] ... to pay an activation cost
    -- of a cycling ability". Recorded by Pawl.Cost as that cost component is
    -- paid, which is what makes the trigger fire off the COST rather than off the
    -- ability resolving.
    --
    -- The ObjectId is the incarnation the card became, not the one that was in
    -- the hand: CR 400.7 mints a new object as it moves, and CR 702.29c's
    -- abilities "trigger from whatever zone the card winds up in after it's
    -- cycled" -- so the graveyard object is the one bearing the ability that
    -- triggers.
    --
    -- Distinct from the Moved event the same discard also records. CR 702.29d
    -- ("abilities that trigger whenever a player cycles or discards a card ...
    -- trigger only once when a card is cycled") is what says the two are one
    -- event with two descriptions rather than two events. No card in the pool
    -- triggers on discarding, so nothing yet has to reconcile them; Bartered Cow
    -- is the card that will, and #319 is where that reconciliation is owed.
    Cycled ObjectId
  | -- CR 508.2b: an attacker was DECLARED -- one entry per creature the active
    -- player chose in CR 508.1's turn-based action. What "whenever this creature
    -- attacks" matches (CR 508.3a).
    --
    -- The declaration is the event, and that is the whole point of recording it
    -- rather than reading Combat.attackers. CR 508.3a's last sentence -- "such
    -- abilities won't trigger if a creature is put onto the battlefield
    -- attacking" -- makes the two indistinguishable in the combat record and
    -- distinct here: only Pawl.Combat.declareAttackers appends this, and
    -- Pawl.Combat.putOntoBattlefieldAttacking deliberately does not.
    AttackerDeclared ObjectId
  | -- CR 701.20a: a player revealed a card -- "show that card to all players for
    -- a brief time."
    --
    -- A reveal is the one game action whose entire content is INFORMATION, so
    -- the log is where it has to live: CR 701.20b says revealing does not move
    -- the card, and nothing about the object changes, so an engine that did not
    -- record the reveal would be bit-for-bit identical to one that never
    -- performed it. What the reveal changes is what the players know, and this
    -- log is the public record of what happened (CR 608.2i).
    --
    -- Carries the CARD's characteristics rather than an ObjectId, because what a
    -- reveal discloses is a card and not an identity -- and because the id is
    -- routinely dead by the time anything reads the event. Every reveal in the
    -- pool today is a search's "reveal it, and put it into your hand", where CR
    -- 400.7 mints a new object for the card one step later, so the id recorded
    -- here would name an object that has already ceased -- which is the problem
    -- GameEvent.Moved's snapshot field exists to solve. An id joins this payload
    -- when a card needs to refer back to "that card" it revealed.
    --
    -- Strict (!) for GameEvent.Moved's reason: the snapshot is taken as of THIS
    -- reveal, and an unforced field would retain the whole GameState it was
    -- projected from for as long as the turn's log lives.
    --
    -- This is the MOMENTARY reveal only. CR 701.20a's lasting cases -- a card
    -- revealed to pay a cost, which "remains revealed ... until the time it
    -- leaves the stack", and a card that stays revealed while a triggered
    -- ability it caused is on the stack -- need a per-object flag that no card
    -- in the pool asks for (#185, #282).
    Revealed PlayerId !ProjectedCharacteristics
  deriving (Eq, Ord, Show)
