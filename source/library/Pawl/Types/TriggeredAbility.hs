module Pawl.Types.TriggeredAbility where

import qualified Pawl.Types.Condition as Condition
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.TriggerLimit as TriggerLimit

-- | CR 603.1 / 700.2b / 603.3c: "[condition], [effect]", modal-capable and
-- parametric in `card` for the reason ActivatedAbility is. On the stack it shares
-- Resolve's executor with an activated ability.
data TriggeredAbility card = MkTriggeredAbility
  { condition :: TriggerCondition.TriggerCondition,
    modal :: Modal.Modal card,
    -- | CR 603.4: an intervening "if" clause. The SAME predicate vocabulary a CR
    -- 603.8 state trigger uses, with two customers: checked when the trigger event
    -- occurs (the ability does not trigger AT ALL if it is false) and checked
    -- AGAIN on resolution (CR 608.2a removes the ability from the stack if it has
    -- become false). Nothing for every ability without one.
    intervening :: Maybe Condition.Condition,
    -- | Whispering Wizard's "This ability triggers only once each turn". A rider
    -- on the ABILITY rather than a narrowing of the event, which is why it sits
    -- here and not in the TriggerCondition beside it: the condition still
    -- matches every occurrence, and the rider says the ability declines all but
    -- the first. Unlimited for every ability that prints no such rider.
    --
    -- Enforced at Pawl.Engine.Engine.withinTurnLimit, over the turn-scoped log
    -- rather than a stored flag.
    limit :: TriggerLimit.TriggerLimit
  }
  deriving (Eq, Ord, Show)
