module Pawl.Types.Phase where

import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.EndingStep as EndingStep

data Phase
  = Beginning BeginningStep.BeginningStep
  | PrecombatMain
  | Combat CombatStep.CombatStep
  | PostcombatMain
  | Ending EndingStep.EndingStep
  deriving (Eq, Ord, Show)
