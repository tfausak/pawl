module Pawl.Types.Scope where

import qualified Pawl.Types.EventShape as EventShape
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.Zone as Zone

-- | What a Pawl.Types.Count folds over: a zone's current residents, or the event
-- log. Two domains rather than one because the second reads CR 608.2h
-- last-known information from a stored snapshot, not a live object.
--
-- A MANA POOL is deliberately not a third arm: the pool is none of CR 400.1's
-- zones (CR 106.4 attaches it to a player instead), and a Count's Filter and
-- Aggregation have nothing to say about a mana unit. Pawl.Types.ManaCount is the
-- parallel axis, and its haddock carries the argument in full.
data Scope
  = InZone Zone.Zone PlayerRef.PlayerRef
  | -- | CR 608.2i: effects that look back in time.
    InHistory EventShape.EventShape
  deriving (Eq, Ord, Show)
