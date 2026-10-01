module Pawl.Types.PlayerCountersAre where

import Numeric.Natural (Natural)
import qualified Pawl.Types.Label as Label
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind

-- | CR 122.1: how many counters of one kind a player has.
data PlayerCountersAre = MkPlayerCountersAre
  { player :: Label.Label,
    kind :: PlayerCounterKind.PlayerCounterKind,
    count :: Natural
  }
  deriving (Eq, Ord, Show)
