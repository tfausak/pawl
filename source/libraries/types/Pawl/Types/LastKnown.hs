module Pawl.Types.LastKnown where

import Pawl.Types.PlayerId (PlayerId)
import Pawl.Types.ProjectedCharacteristics (ProjectedCharacteristics)

-- CR 608.2h: what an object WAS, filed under the id it had while it existed --
-- "if it's no longer in that zone ... the effect uses the object's last known
-- information". Written by the one zone-change funnel
-- (Pawl.Event.changeZoneAttaching) as the object ceases, from the same
-- pre-move state the GameEvent.Moved snapshot is taken against.
--
-- A record of TWO things rather than the characteristics alone, because CR
-- 613.1b control is not one of them: CR 109.3 lists an object's characteristics
-- and then says outright that "characteristics don't include ... an object's
-- owner or controller", so control has no home in ProjectedCharacteristics --
-- while "who controlled it" is exactly what CR 603.3a asks of a triggered
-- ability whose source is already gone.
--
-- Both fields STRICT (!), for GameEvent.Moved's reason: entries are keyed by an
-- id that no longer exists and are never pruned, so an unforced field would be a
-- thunk closing over the whole pre-move GameState, retained for the rest of the
-- game instead of the one small value this record is meant to carry.
data LastKnown = MkLastKnown
  { characteristics :: !ProjectedCharacteristics,
    -- CR 110.2 / 613.1b: the PROJECTED controller as the object left, which is
    -- not its owner -- "a permanent's controller is, by default, the player
    -- under whose control it entered the battlefield", and layer 2 can move it
    -- since. A permanent stolen by Control Magic was controlled by the thief
    -- right up to the moment it died, and CR 603.3a hands that player its
    -- trigger.
    controller :: !PlayerId
  }
  deriving (Eq, Show)
