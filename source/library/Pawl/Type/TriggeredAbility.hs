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
data TriggeredAbility = MkTriggeredAbility
  { condition :: TriggerCondition,
    effects :: [Effect],
    targetSpecs :: Map SlotName TargetSpec
  }
  deriving (Eq, Ord, Show)
