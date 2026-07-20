module Pawl.Type.TriggeredAbility where

import Data.Map.Strict (Map)
import Pawl.Type.Effect (Effect)
import Pawl.Type.SlotName (SlotName)
import Pawl.Type.TargetSpec (TargetSpec)
import Pawl.Type.TriggerCondition (TriggerCondition)

-- CR 603.1: "[condition], [effect]". Reuses the Effect vocabulary and the
-- slot/target machinery of a spell. Differs from ActivatedAbility only in
-- carrying a trigger condition instead of a cost; on the stack the two share one
-- executor (Resolve.resolveEffects).
-- Parametric in `card` for the same reason as Effect/ActivatedAbility: its
-- effects are `[Effect card]`. Card ties the knot at `TriggeredAbility Card`.
data TriggeredAbility card = MkTriggeredAbility
  { condition :: TriggerCondition,
    effects :: [Effect card],
    targetSpecs :: Map SlotName TargetSpec
  }
  deriving (Eq, Ord, Show)
