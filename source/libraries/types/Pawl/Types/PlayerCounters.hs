module Pawl.Types.PlayerCounters where

import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.Quantity as Quantity

-- | CR 122: "these players, this kind, this many" -- the payload of
-- Pawl.Types.Effect's GainPlayerCounters arm (#1305). RemovePlayerCounters
-- shared it until the removal alone needed a tally, and has its own
-- (Pawl.Types.RemovePlayerCounters); the two are separate CONSTRUCTORS for the
-- reason LoseLife and GainLife are.
data PlayerCounters = MkPlayerCounters
  { player :: PlayerRef.PlayerRef,
    kind :: PlayerCounterKind.PlayerCounterKind,
    quantity :: Quantity.Quantity
  }
  deriving (Eq, Ord, Show)
