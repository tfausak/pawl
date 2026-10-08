module Pawl.Types.AfterObjectTurn where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.ObjectId as ObjectId

-- | CR 611.2a's "during its controller's next turn" before the turn is known:
-- the object whose controller it names, and the turn the duration began on.
-- Pawl.Types.AfterTurn with the seat left open until a turn of that object's
-- controller begins (Pawl.Engine.Expiry.dropAtTurnOf).
data AfterObjectTurn = MkAfterObjectTurn
  { object :: ObjectId.ObjectId,
    turn :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
