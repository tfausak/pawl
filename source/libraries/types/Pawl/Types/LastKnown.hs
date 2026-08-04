module Pawl.Types.LastKnown where

import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.ProjectedCharacteristics as ProjectedCharacteristics
import qualified Pawl.Types.Source as Source

-- | CR 608.2h: what an object WAS, filed under the id it had while it existed.
-- Written by the one zone-change funnel (Pawl.Engine.Event.changeZoneAttaching)
-- as the object ceases, from the same pre-move state the GameEvent.Moved
-- snapshot is taken against.
--
-- Three things rather than the characteristics alone, because two of the
-- questions CR 608.2h is asked have no home in ProjectedCharacteristics. Control
-- is not a characteristic (CR 109.3), yet "who controlled it" is what CR 603.3a
-- asks of a triggered ability whose source is gone. Neither is the object's
-- SOURCE: the projection folds characteristics, and CR 603.7's delayed-ability
-- declarations are read straight from the card, so an ArmDelayedTrigger whose
-- source has just exiled itself has nowhere else to find the ability it names.
--
-- All three fields STRICT (!): entries are keyed by an id that no longer exists
-- and are never pruned, so an unforced field would be a thunk retaining the whole
-- pre-move GameState for the rest of the game.
data LastKnown = MkLastKnown
  { characteristics :: !ProjectedCharacteristics.ProjectedCharacteristics,
    -- | CR 110.2 / 613.1b: the PROJECTED controller as the object left, which is
    -- not its owner -- layer 2 can have moved it. A permanent stolen by Control
    -- Magic was controlled by the thief right up to the moment it died, and
    -- CR 603.3a hands that player its trigger.
    controller :: !PlayerId.PlayerId,
    -- | CR 608.2h: what KIND of object it was and the card behind it -- the same
    -- Object.source the live object carried, copied as it ceased.
    source :: !Source.Source
  }
  deriving (Eq, Show)
