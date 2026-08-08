module Pawl.Types.LastKnown where

import qualified Data.Map.Strict as Map
import qualified Numeric.Natural as Natural
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.ProjectedCharacteristics as ProjectedCharacteristics
import qualified Pawl.Types.Source as Source

-- | CR 608.2h: what an object WAS, filed under the id it had while it existed.
-- Written by the one zone-change funnel (Pawl.Engine.Event.changeZoneAttaching)
-- as the object ceases, from the same pre-move state the GameEvent.Moved
-- snapshot is taken against.
--
-- Four things rather than the characteristics alone, because three of the
-- questions CR 608.2h is asked have no home in ProjectedCharacteristics. Control
-- is not a characteristic (CR 109.3), yet "who controlled it" is what CR 603.3a
-- asks of a triggered ability whose source is gone. Neither is the object's
-- SOURCE: the projection folds characteristics, and CR 603.7's delayed-ability
-- declarations are read straight from the card, so an ArmDelayedTrigger whose
-- source has just exiled itself has nowhere else to find the ability it names.
-- Nor are COUNTERS -- CR 109.3's list has none -- and unlike the other two the
-- projection actively CONSUMES them (CR 613.4c), so the record has to be taken
-- beside it rather than out of it.
--
-- All four fields STRICT (!): entries are keyed by an id that no longer exists
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
    source :: !Source.Source,
    -- | CR 122.1: the counters that were on it, per kind -- the same
    -- Object.counters the live object carried. What Promising Duskmage's "if it
    -- had a +1/+1 counter on it" asks after.
    --
    -- It cannot be recovered from `characteristics` and is not redundant with it:
    -- CR 613.4c applies a +1/+1 counter at layer 7c, so the projection above
    -- records 3/4 where the printed box said 2/3 and says nothing at all about
    -- what produced the difference. A +1/+1 counter and a still-live pump effect
    -- land in the same sublayer and leave the same power behind; only this field
    -- tells them apart, and Promising Duskmage's clause is about exactly that
    -- difference.
    --
    -- Counters are not characteristics -- CR 109.3's list has none -- so this
    -- sits beside the projection for the reason `controller` does rather than
    -- inside it.
    counters :: !(Map.Map CounterKind.CounterKind Natural.Natural)
  }
  deriving (Eq, Show)
