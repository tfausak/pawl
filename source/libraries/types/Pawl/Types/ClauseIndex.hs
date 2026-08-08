module Pawl.Types.ClauseIndex where

import qualified Numeric.Natural as Natural

-- | CR 608.2e: a clause is one of the "multiple steps or actions, denoted by
-- separate sentences or clauses" inside a single mode. Like a mode and unlike a
-- target slot, a clause has no label to conjure, so it is referenced by its
-- ORDINAL within its mode -- and the ordinal is load-bearing, since CR 608.2c
-- has the controller follow the instructions "in the order written".
--
-- A newtype rather than a bare Natural, so the reference is typed and cannot be
-- swapped with a Pawl.Types.ModeIndex: a prompt naming a gated clause carries
-- both, and they index different things.
newtype ClauseIndex = MkClauseIndex
  { unwrap :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
