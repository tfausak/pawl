module Pawl.Types.Comparison where

-- | How a Pawl.Types.Condition relates its count to its threshold. A sum type
-- rather than a bare Bool-shaped "is zero": the pool's cards are all Exactly 0
-- today, and AtLeast/AtMost are what a threshold card needs.
--
-- AtLeast and AtMost have no producer: every condition in the pool is
-- Exactly (#158).
data Comparison
  = Exactly
  | AtLeast
  | AtMost
  deriving (Eq, Ord, Show)
