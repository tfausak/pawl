module Pawl.Types.Drew where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.PlayerId as PlayerId

-- | CR 121.1: a player drew a card, and WHICH draw of the turn it was --
-- Underworld Dreams' "second card" reads this rather than a running total.
data Drew = MkDrew
  { player :: PlayerId.PlayerId,
    nth :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
