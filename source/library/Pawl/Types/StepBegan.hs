module Pawl.Types.StepBegan where

import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerId as PlayerId

-- | CR 500.1: a step or phase began, and whose turn it is.
data StepBegan = MkStepBegan
  { phase :: Phase.Phase,
    player :: PlayerId.PlayerId
  }
  deriving (Eq, Ord, Show)
