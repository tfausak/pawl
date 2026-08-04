module Pawl.Types.ManaCount where

import qualified Pawl.Types.ManaFilter as ManaFilter
import qualified Pawl.Types.PlayerRef as PlayerRef

-- | A number derived from a mana pool: whose pool to look in, and which of the
-- units in it to keep. Omnath, Locus of Mana's "for each unspent green mana you
-- have".
--
-- A PARALLEL AXIS to Pawl.Types.Count, not a third Scope constructor. Two things
-- stop the pool joining Count's fold: the pool is not one of CR 400.1's zones,
-- so Scope.InZone cannot name it (CR 106.4 attaches it to a player instead); and
-- neither Pawl.Types.Filter nor Pawl.Types.Aggregation fits a mana unit, since
-- two of Aggregation's three arms are not questions a unit can be asked. There is
-- no aggregation field because counting the survivors is all a pool-reading card
-- asks.
--
-- Reuses Pawl.Types.PlayerRef, resolved by the same Pawl.Engine.Count.playersFor
-- a Scope.InZone is, so the two readings cannot disagree.
--
-- Read LIVE, per projection, never stored or sampled: CR 605.3a/605.3b let a mana
-- ability resolve immediately without the stack, so the pool changes with no
-- state-based action and no priority pass in between.
--
-- `filter` shadows the Prelude's, keeping the field named after its type.
data ManaCount = MkManaCount
  { player :: PlayerRef.PlayerRef,
    filter :: ManaFilter.ManaFilter
  }
  deriving (Eq, Ord, Show)
