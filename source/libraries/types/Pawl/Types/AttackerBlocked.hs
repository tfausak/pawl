module Pawl.Types.AttackerBlocked where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId

-- | CR 509.1h: an attacking creature became blocked, and the player it is
-- attacking. The defending player is CARRIED rather than derived because
-- Pawl.Engine.Event.eventBindings takes no game state (CR 508.5).
data AttackerBlocked = MkAttackerBlocked
  { attacker :: ObjectId.ObjectId,
    defender :: PlayerId.PlayerId,
    -- | CR 509.3e's comparand at the moment of THIS becoming: how many creatures
    -- were blocking the attacker as it became a blocked creature. The
    -- declaration's whole set (CR 509.1h) on that road; exactly one on CR
    -- 509.3c's arrival road, that rule recording this event only for an
    -- attacker no creature was blocking yet; and zero for CR 509.1h's escape
    -- clause, where an effect makes a creature blocked by nothing at all.
    --
    -- Carried rather than counted off Combat.blockers when the condition is
    -- scanned, which is a STALE read on the arrival road: arrivals that came
    -- after this one are already in the entry by then (CR 509.2a), so the
    -- becoming would be measured against a later board.
    blockers :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
