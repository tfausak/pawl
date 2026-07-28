module Pawl.Type.GameEvent where

import Pawl.Type.DamageEvent (DamageEvent)
import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.Phase (Phase)
import Pawl.Type.PlayerId (PlayerId)
import Pawl.Type.ProjectedCharacteristics (ProjectedCharacteristics)
import Pawl.Type.ZoneChange (ZoneChange)

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
    -- event with two descriptions rather than two events; no card in the pool
    -- triggers on discarding, so nothing yet has to reconcile them (#314).
    Cycled ObjectId
  deriving (Eq, Ord, Show)
