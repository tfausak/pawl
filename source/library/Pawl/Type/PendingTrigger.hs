module Pawl.Type.PendingTrigger where

import Data.Map.Strict (Map)
import Pawl.Type.Binding (Binding)
import Pawl.Type.Card (Card)
import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.PlayerId (PlayerId)
import Pawl.Type.SlotName (SlotName)
import Pawl.Type.TriggeredAbility (TriggeredAbility)

-- CR 603.3: an ability that has TRIGGERED but is not yet on the stack. Gathered
-- by Pawl.Event at the CR 117.5 boundary, ordered and placed by Pawl.Engine.
--
-- `source` is the object the ability belongs to (CR 608.2g's effect source);
-- `controller` is who controls the ability (CR 603.3a). `bindings` is the
-- environment CAPTURED when a CR 603.7 delayed ability was armed -- how "it" and
-- "that card" (CR 603.7c) are remembered. Empty for an event- or state-matched
-- trigger, whose source binding Engine.placeOne stamps at placement instead.
data PendingTrigger = MkPendingTrigger
  { source :: ObjectId,
    controller :: PlayerId,
    ability :: TriggeredAbility Card,
    bindings :: Map SlotName Binding
  }
  deriving (Eq, Show)
