module Pawl.Types.ReturnWatch where

import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Zone as Zone

-- | CR 610.3: one object moved "until" an event, and what the game must
-- remember to move it back. Keyed by the incarnation the move minted (CR 400.7),
-- the way GameState.exiledUntilMonarch is keyed one rule over.
--
-- Board state rather than a field on the moved Object, for that register's
-- reason: it is a relation between two ids that outlives neither, and CR 400.7
-- would strip it from an object that moved again anyway.
data ReturnWatch = MkReturnWatch
  { -- | The object whose leaving the battlefield ends the duration -- the
    -- ability's source, read as the incarnation that was on the battlefield when
    -- the move happened, so a permanent that leaves and comes back (CR 400.7) is
    -- a new object and does not keep the watch armed.
    source :: ObjectId.ObjectId,
    -- | CR 610.3's "previous zone": where the second one-shot effect puts the
    -- object back. Recorded at the move rather than assumed, since the rule
    -- names the zone the object came from.
    zone :: Zone.Zone
  }
  deriving (Eq, Ord, Show)
