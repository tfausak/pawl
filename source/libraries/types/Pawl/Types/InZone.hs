module Pawl.Types.InZone where

import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.Zone as Zone

-- | CR 400.1's scope for a count: which zone to look in, and whose copy of it.
--
-- The rule's two halves are not symmetric, and the invariant that follows is
-- Pawl.Codec.InZone's: a library, a hand and a graveyard are each one player's,
-- so any reference names a copy of one, while the other zones are shared by all
-- players and have no per-player copy to name -- their reference can only be
-- PlayerRef.EachPlayer. The flat shape is kept, and the check made at the
-- decoder, because splitting the type would not close it either: a shared/
-- per-player split could still say "the shared hand".
data InZone = MkInZone
  { zone :: Zone.Zone,
    player :: PlayerRef.PlayerRef
  }
  deriving (Eq, Ord, Show)
