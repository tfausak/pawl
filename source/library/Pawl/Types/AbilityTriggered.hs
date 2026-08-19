module Pawl.Types.AbilityTriggered where

import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.TriggerCondition as TriggerCondition

-- | CR 603.2: an ability triggered -- its source, its controller, and the
-- condition that fired it.
data AbilityTriggered = MkAbilityTriggered
  { source :: ObjectId.ObjectId,
    controller :: PlayerId.PlayerId,
    condition :: TriggerCondition.TriggerCondition
  }
  deriving (Eq, Ord, Show)
