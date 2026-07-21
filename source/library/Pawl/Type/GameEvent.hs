module Pawl.Type.GameEvent where

import Pawl.Type.DamageEvent (DamageEvent)
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
  | -- CR 603.2b: a phase or step began, on whose turn (the active player). What a
    -- "at the beginning of each end step" trigger matches against (and, once P4
    -- Task 6 adds delayed abilities, what a CR 603.7 delayed ability will match
    -- against too -- that consumer does not exist yet).
    StepBegan Phase PlayerId
  deriving (Eq, Ord, Show)
