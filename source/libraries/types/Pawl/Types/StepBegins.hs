module Pawl.Types.StepBegins where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.TurnScope as TurnScope

-- | CR 603.2's turn-structure trigger: which step or phase beginning fires the
-- ability, and whose turns count.
data StepBegins = MkStepBegins
  { phase :: Phase.Phase,
    -- | CR 505.1b's ordinal, counting the main phases that have begun this turn:
    -- Just 2 for a card printed "your second main phase", Nothing for one that
    -- names the phase instead ("each postcombat main phase"). The two are
    -- different cards in a turn holding an extra main phase, where CR 505.1a
    -- makes every main phase after the first a postcombat one.
    ordinal :: Maybe Natural.Natural,
    -- | CR 603.2a's "your" versus an unscoped "each". Not a default a card may
    -- omit -- "at the beginning of the end step" and "at the beginning of YOUR
    -- end step" are different cards.
    scope :: TurnScope.TurnScope
  }
  deriving (Eq, Ord, Show)
