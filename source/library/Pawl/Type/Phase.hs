module Pawl.Type.Phase where

import Pawl.Type.BeginningStep (BeginningStep)
import Pawl.Type.CombatStep (CombatStep)
import Pawl.Type.EndingStep (EndingStep)

data Phase
  = Beginning BeginningStep
  | PrecombatMain
  | Combat CombatStep
  | PostcombatMain
  | Ending EndingStep
  deriving (Eq, Ord, Show)
