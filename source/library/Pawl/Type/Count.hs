module Pawl.Type.Count where

import Pawl.Type.Aggregation (Aggregation)
import Pawl.Type.Filter (Filter)
import Pawl.Type.Scope (Scope)

-- A number derived from game state: a scope to fold over, a per-object predicate
-- to keep by, and an aggregation. First-order and analyzable -- never a
-- predicate function -- and evaluated by one generic fold (Pawl.Count.evaluate)
-- that never learns which effect or card produced it.
--
-- This replaced a hand-carved variant per card (the retired
-- Pawl.Type.CountSpec), which is the identity-casing the project's central
-- invariant forbids.
data Count = MkCount Scope Filter Aggregation
  deriving (Eq, Ord, Show)
