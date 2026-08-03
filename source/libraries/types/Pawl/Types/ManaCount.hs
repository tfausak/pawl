module Pawl.Types.ManaCount where

import qualified Pawl.Types.ManaFilter as ManaFilter
import qualified Pawl.Types.PlayerRef as PlayerRef

-- | A number derived from a mana pool: whose pool to look in, and which of the
-- units in it to keep. Omnath, Locus of Mana's "for each unspent green mana you
-- have".
--
-- A PARALLEL AXIS to Pawl.Types.Count, not a third Scope constructor and not a
-- second shape of Count. Two things stop the pool joining Count's fold:
--
--   * The pool is not a ZONE. CR 400.1 lists seven (library, hand, battlefield,
--     graveyard, stack, exile, command) plus the older ante zone, and the mana
--     pool is none of them; CR 106.4 gives it its own existence, attached to a
--     player. So Scope.InZone cannot name it.
--   * A Count keeps its candidates by a Pawl.Types.Filter and aggregates them by
--     a Pawl.Types.Aggregation, and neither fits a Pawl.Types.ManaUnit. The
--     Filter reason is Pawl.Types.ManaFilter's haddock. The Aggregation reason is
--     that two of its three arms -- DistinctCardTypes (CR 208.2a, Tarmogoyf) and
--     Greatest (a per-object Quantity) -- are not questions a mana unit can be
--     asked at all, so a shared type would carry a field two thirds of whose
--     values are nonsense here. Counting the survivors is the whole of what a
--     pool-reading card asks, so there is no aggregation field.
--
-- Reuses Pawl.Types.PlayerRef, which is a reference to a PLAYER and so is
-- orthogonal to what is being counted about them; CR 106.4 attaches the pool to
-- a player exactly as CR 400.1 attaches a library. Resolved by the same
-- Pawl.Engine.Count.playersFor a Scope.InZone is, so the two readings of a
-- PlayerRef cannot disagree.
--
-- Read LIVE, per projection, never stored or sampled: CR 605.3a lets a player
-- activate a mana ability whenever they have priority and CR 605.3b has it
-- resolve immediately without using the stack, so the pool changes with no
-- state-based action (CR 704.3) and no priority pass in between. Pinned by
-- Pawl.PowerToughnessSpec's "CR 106.4 the count is live".
--
-- `filter` shadows the Prelude's, which costs nothing here and keeps the field
-- named after its type -- the convention Pawl.Types.Count's three fields take.
data ManaCount = MkManaCount
  { player :: PlayerRef.PlayerRef,
    filter :: ManaFilter.ManaFilter
  }
  deriving (Eq, Ord, Show)
