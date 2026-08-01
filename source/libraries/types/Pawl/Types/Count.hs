module Pawl.Types.Count where

import Pawl.Types.Aggregation (Aggregation)
import Pawl.Types.Filter (Filter)
import Pawl.Types.Keyword (Keyword)
import Pawl.Types.Scope (Scope)

-- A number derived from game state: a scope to fold over, a per-object predicate
-- to keep by, and an aggregation. First-order and analyzable -- never a
-- predicate function -- and evaluated by one generic fold (Pawl.Engine.Count.evaluate)
-- that never learns which effect or card produced it.
--
-- This replaced a hand-carved variant per card (the retired
-- Pawl.Types.CountSpec), which is the identity-casing the project's central
-- invariant forbids.
--
-- The `quantity` parameter is passed straight through to the Aggregation, which
-- is where it is actually used and where the reason for it is written. Every
-- customer but Pawl.Types.Quantity instantiates it concretely as
-- `Count Quantity`.
data Count quantity = MkCount Scope (Filter Keyword) (Aggregation quantity)
  deriving (Eq, Ord, Show)
