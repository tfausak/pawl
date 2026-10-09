module Pawl.Types.ReturnWatch where

import qualified Pawl.Types.ReturnEnding as ReturnEnding
import qualified Pawl.Types.Zone as Zone

-- | CR 610.3: one object moved "until" an event, and what the game must
-- remember to move it back. Keyed by the incarnation the move minted (CR 400.7).
--
-- Board state rather than a field on the moved Object: it is a relation that
-- outlives the move's source, and CR 400.7 would strip it from an object that
-- moved again anyway.
data ReturnWatch = MkReturnWatch
  { -- | The specified event whose happening ends the duration.
    ending :: ReturnEnding.ReturnEnding,
    -- | CR 610.3's "previous zone": where the second one-shot effect puts the
    -- object back. Recorded at the move rather than assumed, since the rule
    -- names the zone the object came from.
    zone :: Zone.Zone
  }
  deriving (Eq, Ord, Show)
