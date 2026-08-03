module Pawl.Types.Scope where

import qualified Pawl.Types.EventShape as EventShape
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.Zone as Zone

-- | What a Pawl.Types.Count folds over: a zone's current residents, or the event
-- log. Two domains rather than one because the second reads CR 608.2h
-- last-known information from a stored snapshot, not a live object.
--
-- A MANA POOL is deliberately not a third arm, and the temptation to add one is
-- worth naming: CR 400.1 lists seven zones and the pool is none of them (CR 106.4
-- gives it its own existence, attached to a player), and a Count's Filter and
-- Aggregation have nothing to say about a mana unit anyway. Pawl.Types.ManaCount
-- is the parallel axis, and its haddock carries the argument in full.
data Scope
  = InZone Zone.Zone PlayerRef.PlayerRef
  | -- | CR 608.2i: effects that look back in time.
    InHistory EventShape.EventShape
  deriving (Eq, Ord, Show)
