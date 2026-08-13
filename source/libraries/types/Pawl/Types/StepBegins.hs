module Pawl.Types.StepBegins where

import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.TurnScope as TurnScope

-- | CR 603.2's turn-structure trigger: which step or phase beginning fires the
-- ability, and whose turns count.
data StepBegins = MkStepBegins
  { phase :: Phase.Phase,
    -- | CR 603.2a's "your" versus an unscoped "each". Not a default a card may
    -- omit -- "at the beginning of the end step" and "at the beginning of YOUR
    -- end step" are different cards.
    scope :: TurnScope.TurnScope
  }
  deriving (Eq, Ord, Show)
