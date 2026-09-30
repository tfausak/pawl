module Pawl.Types.TurnedFaceUp where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.ObjectId as ObjectId

-- | CR 708.7: a face-down permanent was turned face up -- which permanent, and
-- the X chosen for the cost that turned it (CR 107.3d).
--
-- The X is Nothing when the road up paid no cost with an X in it, and Just 0
-- when the player chose zero. Carried on the event rather than read off the
-- permanent, since CR 702.37f and CR 702.168e give it to the permanent's
-- turned-face-up trigger, and CR 117.5's scan may run after state-based actions
-- have already taken the permanent away.
data TurnedFaceUp = MkTurnedFaceUp
  { object :: ObjectId.ObjectId,
    announcedX :: Maybe Natural.Natural
  }
  deriving (Eq, Ord, Show)
