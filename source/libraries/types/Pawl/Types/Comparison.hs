module Pawl.Types.Comparison where

-- | How a measured number relates to its threshold: a Pawl.Types.Condition's
-- two Quantities, or a Pawl.Types.Filter Measures atom's candidate measure and
-- operand. Pawl.Engine.Filter.compares is the one reader.
--
-- AtLeast's producer is Galvanic Blast's metalcraft clause -- "if you control
-- three or more artifacts", the pool's first nonzero threshold.
--
-- AtMost's first producer was minted by the rules core rather than printed on a
-- card: CR 702.179d's "if your speed is less than 4", which Pawl.Engine.Speed
-- states as AtMost 3. The Ten Rings is the printed one -- CR 603.4's "if you
-- have fewer than ten cards in hand", stated as AtMost 9.
--
-- The STRICT arms are for an operand that is another object's number, where no
-- adjacent literal can be written: CR 702.134a's mentor ("power less than this
-- creature's power"). Against a literal, a card writes the adjacent bound
-- instead -- "fewer than ten" is AtMost 9 and "greater than n" is AtLeast (n +
-- 1), as Meren of Clan Nel Toth's end step does.
data Comparison
  = Exactly
  | AtLeast
  | AtMost
  | LessThan
  | GreaterThan
  deriving (Bounded, Enum, Eq, Ord, Show)
