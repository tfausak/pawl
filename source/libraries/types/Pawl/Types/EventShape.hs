module Pawl.Types.EventShape where

import qualified Pawl.Types.Zone as Zone

-- | Which recorded events a history count folds over. GameState.events is cleared
-- at the turn change (Pawl.Engine.Engine), an engine choice made under CR 608.2i
-- because every history-reading card in the pool asks "this turn" -- so the
-- log's extent IS the window and none is carried here.
--
-- Only MovedBetween exists: every other GameEvent constructor is recorded in the
-- log with no EventShape arm, so a count cannot fold over any of them (#162).
data EventShape
  = -- | CR 700.4: "dies" is MovedBetween Battlefield Graveyard.
    MovedBetween Zone.Zone Zone.Zone
  deriving (Eq, Ord, Show)
