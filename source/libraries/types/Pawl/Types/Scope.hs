module Pawl.Types.Scope where

import qualified Pawl.Types.EventShape as EventShape
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.Zone as Zone

-- | What a Pawl.Types.Count folds over: a zone's current residents, or the event
-- log. Two domains rather than one because the second reads CR 608.2h
-- last-known information from a stored snapshot, not a live object.
data Scope
  = InZone Zone.Zone PlayerRef.PlayerRef
  | -- | CR 608.2i: effects that look back in time.
    InHistory EventShape.EventShape
  deriving (Eq, Ord, Show)
