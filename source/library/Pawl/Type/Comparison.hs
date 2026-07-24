module Pawl.Type.Comparison where

-- How a Pawl.Type.Condition relates its count to its threshold. A sum type
-- rather than a bare Bool-shaped "is zero": the pool's cards are all Exactly 0
-- today, and AtLeast/AtMost are what a threshold card needs.
data Comparison
  = Exactly
  | AtLeast
  | AtMost
  deriving (Eq, Ord, Show)
