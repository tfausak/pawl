module Pawl.Type.TriggeredAbility where

import Pawl.Type.Modal (Modal)
import Pawl.Type.TriggerCondition (TriggerCondition)

-- CR 603.1 / 700.2b / 603.3c: "[condition], [effect]", now modal-capable. Card-free/
-- parametric (M4c). On the stack it shares Resolve's executor with an activated
-- ability. Effects live in Mode.effects :: Seq (M4g's interim [Effect] retired).
data TriggeredAbility card = MkTriggeredAbility
  { condition :: TriggerCondition,
    modal :: Modal card
  }
  deriving (Eq, Ord, Show)
