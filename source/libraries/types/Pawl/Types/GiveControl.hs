module Pawl.Types.GiveControl where

import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.PlayerRef as PlayerRef

-- | "That player gains control of these objects" -- the payload of
-- Pawl.Types.Effect's GiveControl arm, CR 804.2's "target teammate gains control
-- of this creature". Indefinite, as CR 804.2 is.
data GiveControl = MkGiveControl
  { -- | The new controller, who must be exactly one player.
    player :: PlayerRef.PlayerRef,
    ref :: ObjectRef.ObjectRef
  }
  deriving (Eq, Ord, Show)
