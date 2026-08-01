module Pawl.Types.TriggeredAbility where

import Pawl.Types.Condition (Condition)
import Pawl.Types.Modal (Modal)
import Pawl.Types.TriggerCondition (TriggerCondition)

-- | CR 603.1 / 700.2b / 603.3c: "[condition], [effect]", now modal-capable. Card-free/
-- parametric (M4c). On the stack it shares Resolve's executor with an activated
-- ability. Effects live in Mode.effects :: Seq (M4g's interim [Effect] retired).
data TriggeredAbility card = MkTriggeredAbility
  { condition :: TriggerCondition,
    modal :: Modal card,
    -- | CR 603.4: an intervening "if" clause. The SAME predicate vocabulary a CR
    -- 603.8 state trigger uses, with two customers: checked when the trigger event
    -- occurs (the ability does not trigger AT ALL if it is false) and checked
    -- AGAIN on resolution (CR 608.2a removes the ability from the stack if it has
    -- become false). Nothing for every ability without one.
    intervening :: Maybe Condition
  }
  deriving (Eq, Ord, Show)
