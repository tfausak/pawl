module Pawl.Types.Count where

import qualified Pawl.Types.Aggregation as Aggregation
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Scope as Scope

-- | A number derived from game state: a `scope` to fold over, a `filter` to keep
-- each candidate by, and an `aggregation` that turns the survivors into a
-- number. First-order and analyzable -- the filter is data, never a predicate
-- function -- and evaluated by one generic fold (Pawl.Engine.Count.evaluate)
-- that never learns which effect or card produced it.
--
-- Counts objects or PLAYERS, depending on the Scope (CR 109.1 makes those
-- disjoint). A count over a MANA POOL is neither: that is
-- Pawl.Types.ManaCount, a parallel type rather than a third shape of this one,
-- and its haddock says why neither the Scope, the Filter nor the Aggregation
-- here can reach a mana unit.
--
-- The `quantity` parameter is passed straight through to the Aggregation, where
-- the reason for it is written. Every customer but Pawl.Types.Quantity
-- instantiates it as `Count Quantity`.
--
-- `filter` shadows the Prelude's, keeping the three fields named after their
-- types.
data Count quantity = MkCount
  { scope :: Scope.Scope,
    filter :: Filter.Filter Keyword.Keyword,
    aggregation :: Aggregation.Aggregation quantity
  }
  deriving (Eq, Ord, Show)
