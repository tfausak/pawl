module Pawl.Types.Scope where

import Pawl.Types.EventShape (EventShape)
import Pawl.Types.PlayerRef (PlayerRef)
import Pawl.Types.Zone (Zone)

-- | What a Pawl.Types.Count folds over: a zone's current residents, or the event
-- log. Two domains rather than one because the second reads CR 608.2h
-- last-known information from a stored snapshot, not a live object.
data Scope
  = InZone Zone PlayerRef
  | -- | CR 608.2i: effects that look back in time.
    InHistory EventShape
  deriving (Eq, Ord, Show)
