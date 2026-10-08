module Pawl.Types.SetOwner where

import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.PlayerRef as PlayerRef

-- | CR 108.3 / 407.3: the one player `player` names becomes the owner of each
-- object `ref` names -- Darkpact's "You own target card in the ante".
data SetOwner = MkSetOwner
  { -- | The new owner; a ref naming no player, or several, changes nothing.
    player :: PlayerRef.PlayerRef,
    -- | What changes owner.
    ref :: ObjectRef.ObjectRef
  }
  deriving (Eq, Ord, Show)
