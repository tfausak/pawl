module Pawl.Types.AttackerBlocked where

import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId

-- | CR 509.1h: an attacking creature became blocked, and the player it is
-- attacking. The defending player is CARRIED rather than derived because
-- Pawl.Engine.Event.eventBindings takes no game state (CR 508.5).
data AttackerBlocked = MkAttackerBlocked
  { attacker :: ObjectId.ObjectId,
    defender :: PlayerId.PlayerId
  }
  deriving (Eq, Ord, Show)
