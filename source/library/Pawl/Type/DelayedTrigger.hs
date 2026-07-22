module Pawl.Type.DelayedTrigger where

import Data.Map.Strict (Map)
import Pawl.Type.Binding (Binding)
import Pawl.Type.Card (Card)
import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.PlayerId (PlayerId)
import Pawl.Type.SlotName (SlotName)
import Pawl.Type.TriggeredAbility (TriggeredAbility)

-- CR 603.7: a delayed triggered ability that has been created and is waiting for
-- its trigger event. A concrete `TriggeredAbility Card`, exactly as
-- Source.OfTrigger already carries one.
--
-- `controller` is the player who controlled the SPELL OR ABILITY that created it,
-- as that spell or ability RESOLVED (CR 603.7d-f) -- baked in at arming, never
-- re-derived. `bindings` is the environment captured at that moment, which is how
-- "it" and "that card" (CR 603.7c) survive the resolution that armed the ability.
--
-- An entry is removed as it fires (CR 603.7b: "only once, the next time its
-- trigger event occurs"). A STATED-DURATION delayed ability ("this turn") would
-- fire repeatedly instead; stated durations are not modelled (#52).
data DelayedTrigger = MkDelayedTrigger
  { ability :: TriggeredAbility Card,
    source :: ObjectId,
    controller :: PlayerId,
    bindings :: Map SlotName Binding
  }
  deriving (Eq, Show)
