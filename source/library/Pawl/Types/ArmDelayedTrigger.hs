module Pawl.Types.ArmDelayedTrigger where

import qualified Pawl.Types.AbilityName as AbilityName
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.Onset as Onset

-- | CR 603.7: which delayed triggered ability to arm, and the temporal envelope
-- to arm it inside -- when it becomes armed, and how long it stays armed.
data ArmDelayedTrigger = MkArmDelayedTrigger
  { name :: AbilityName.AbilityName,
    -- | CR 603.7a's floor, which is Immediately for everything but Meandering
    -- Towershell.
    onset :: Onset.Onset,
    -- | CR 603.7b's stated duration. Nothing is that rule's default -- once
    -- only, at the next trigger event -- spelled as an absence because the rule
    -- words it that way.
    duration :: Maybe Duration.Duration
  }
  deriving (Eq, Ord, Show)
