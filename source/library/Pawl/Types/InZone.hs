module Pawl.Types.InZone where

import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.Zone as Zone

-- | CR 400.1's scope for a count: which zone to look in, and whose copy of it.
data InZone = MkInZone
  { zone :: Zone.Zone,
    player :: PlayerRef.PlayerRef
  }
  deriving (Eq, Ord, Show)
