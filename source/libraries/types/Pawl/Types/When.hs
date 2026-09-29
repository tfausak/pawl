module Pawl.Types.When where

import Numeric.Natural (Natural)
import qualified Pawl.Types.Label as Label
import qualified Pawl.Types.Phase as Phase

-- | The moment a timeline entry waits for: a turn, a step, and the seat that
-- DECIDES, which CR 723.5 makes the controller of a controlled player.
data When = MkWhen
  { turn :: Natural,
    phase :: Phase.Phase,
    player :: Label.Label
  }
  deriving (Eq, Ord, Show)
