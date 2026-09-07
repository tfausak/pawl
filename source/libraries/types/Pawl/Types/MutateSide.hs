module Pawl.Types.MutateSide where

-- | CR 702.140c: which side of the target creature a mutating creature spell is
-- put on -- "the spell's controller chooses whether the spell is put on top of
-- the creature or on the bottom". CR 730.2a is what makes the answer observable:
-- the topmost component supplies the merged permanent's characteristics.
--
-- A named sum rather than a Bool, the posture every player-facing yes-or-no in
-- this engine takes (Pawl.Types.EntwineDecision), so a transcript reads as the
-- decision it records.
--
-- Two arms and no third: rule 702.140c offers a two-way choice, and CR 730.2's
-- "place that object on top of or under that permanent" says the same, however
-- many components the permanent already has -- a merge names a side of the whole
-- permanent rather than a position in its stack.
data MutateSide
  = Over
  | Under
  deriving (Bounded, Enum, Eq, Ord, Show)
