module Pawl.Types.PendingTrigger where

import qualified Data.Map.Strict as Map
import qualified Pawl.Types.Binding as Binding
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.TriggerSource as TriggerSource
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility

-- | CR 603.3: an ability that has TRIGGERED but is not yet on the stack. Gathered
-- by Pawl.Engine.Event (and, for the sourceless ones, by Pawl.Engine.Monarch for
-- CR 725.2's pair and Pawl.Engine.Speed for CR 702.179d's) at the CR 117.5
-- boundary, ordered and placed by Pawl.Engine.Engine.
--
-- `source` is what the ability hangs on -- the object it belongs to (CR 113.7),
-- or nothing at all for an inherent ability the rulebook rather than a card
-- states (CR 725.2, CR 702.179d). Both kinds
-- share this type so that CR 603.3b can order them as one batch;
-- `controller` is who controls the ability (CR 603.3a / CR 725.2). `bindings` is
-- the environment the trigger arrives with: what CR 603.7 captured when a
-- delayed ability was armed -- how "it" and "that card" (CR 603.7c) are
-- remembered, and how CR 725.2's crown steal remembers the damaging creature --
-- or, for an event-matched one, what Event.eventBindings read off the event
-- itself (CR 603.6a's entrant, under Binding.became). Empty for a state trigger
-- (CR 603.8), which has no event to read; the SOURCE binding is stamped later
-- either way, by Engine.placeOne at placement.
data PendingTrigger = MkPendingTrigger
  { source :: TriggerSource.TriggerSource,
    controller :: PlayerId.PlayerId,
    ability :: TriggeredAbility.TriggeredAbility Card.Card,
    bindings :: Map.Map SlotName.SlotName Binding.Binding
  }
  deriving (Eq, Ord, Show)
