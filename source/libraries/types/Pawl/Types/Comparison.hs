module Pawl.Types.Comparison where

-- | How a Pawl.Types.Condition relates its measured side to its threshold. A
-- sum type rather than a bare Bool-shaped "is zero": most of the pool's
-- conditions are Exactly, and a threshold card needs AtLeast or AtMost.
--
-- AtLeast's producer is Galvanic Blast's metalcraft clause -- "if you control
-- three or more artifacts", the pool's first nonzero threshold.
--
-- AtMost's first producer was minted by the rules core rather than printed on a
-- card: CR 702.179d's "if your speed is less than 4", which Pawl.Engine.Speed
-- states as AtMost 3. The Ten Rings is the printed one -- CR 603.4's "if you
-- have fewer than ten cards in hand", stated as AtMost 9, a strict inequality
-- over a count being the bound one below it.
data Comparison
  = Exactly
  | AtLeast
  | AtMost
  deriving (Bounded, Enum, Eq, Ord, Show)
