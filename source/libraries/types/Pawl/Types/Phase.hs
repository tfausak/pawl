module Pawl.Types.Phase where

import Pawl.Types.BeginningStep (BeginningStep)
import Pawl.Types.CombatStep (CombatStep)
import Pawl.Types.EndingStep (EndingStep)

data Phase
  = Beginning BeginningStep
  | PrecombatMain
  | Combat CombatStep
  | PostcombatMain
  | Ending EndingStep
  deriving (Eq, Ord, Show)
