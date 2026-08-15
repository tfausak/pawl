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
-- have fewer than ten cards in hand", stated as AtMost 9.
--
-- No STRICT arm, and none is owed: every quantity these compare is an integer,
-- so a strict inequality is the adjacent bound -- "fewer than ten" is AtMost 9
-- and "greater than n" is AtLeast (n + 1), which is how Meren of Clan Nel Toth
-- spells the "otherwise" half of its end step. Both halves of that convention
-- live here so a card writing one can find the other; adding a fourth and fifth
-- arm would give every reader two spellings of one condition to keep in step.
data Comparison
  = Exactly
  | AtLeast
  | AtMost
  deriving (Bounded, Enum, Eq, Ord, Show)
