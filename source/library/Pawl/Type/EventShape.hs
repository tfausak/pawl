module Pawl.Type.EventShape where

import Pawl.Type.Zone (Zone)

-- Which recorded events a history count folds over. GameState.events is cleared
-- at the turn change (Pawl.Engine), an engine choice made under CR 608.2i
-- because every history-reading card in the pool asks "this turn" -- so the
-- log's extent IS the window and none is carried here.
data EventShape
  = -- CR 700.4: "dies" means "is put into a graveyard from the battlefield",
    -- which is MovedBetween Battlefield Graveyard.
    MovedBetween Zone Zone
  deriving (Eq, Ord, Show)
