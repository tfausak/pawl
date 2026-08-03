module Pawl.Types.Comparison where

-- | How a Pawl.Types.Condition relates its measured side to its threshold. A
-- sum type rather than a bare Bool-shaped "is zero": most of the pool's
-- conditions are Exactly, and a threshold card needs AtLeast or AtMost.
--
-- AtLeast's producer is Galvanic Blast's metalcraft clause -- "if you control
-- three or more artifacts", the pool's first nonzero threshold.
--
-- AtMost still has no producer: no condition in the pool bounds a count from
-- above (#158).
data Comparison
  = Exactly
  | AtLeast
  | AtMost
  deriving (Eq, Ord, Show)
