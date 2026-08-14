module Pawl.Types.MovedBetween where

import qualified Pawl.Types.Zone as Zone

-- | The zone pair an event shape matches: where the object left, and where it
-- arrived.

-- BOTH fields are a Zone and they are NOT interchangeable, so they are named
-- rather than positional: a swap would match the move backwards.
data MovedBetween = MkMovedBetween
  { from :: Zone.Zone,
    to :: Zone.Zone
  }
  deriving (Eq, Ord, Show)
