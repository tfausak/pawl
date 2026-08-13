module Pawl.Types.ControlChanged where

import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId

-- | CR 613.1b: a permanent's controller changed -- the permanent, who controlled
-- it when the game last looked, and who controls it now.

-- BOTH players are a PlayerId and they are NOT interchangeable, so they are named
-- rather than positional: a swap would report the change backwards, and
-- Pawl.Engine.Event.sampleControl only mints the event when the two differ.
data ControlChanged = MkControlChanged
  { object :: ObjectId.ObjectId,
    before :: PlayerId.PlayerId,
    after :: PlayerId.PlayerId
  }
  deriving (Eq, Ord, Show)
