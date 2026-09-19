module Pawl.Types.TimeTravelChoice where

-- | CR 701.56a's per-object half of time travel: "put a time counter on it or
-- remove a time counter from it".
--
-- A TYPE rather than a Bool because the rule's two branches are named, and
-- because the set of chosen objects and the direction each one takes are one
-- answer: rule 701.56a chooses "any number" of objects and then decides each
-- separately, so absence from the answer is the third option and neither
-- constructor can stand for it.
data TimeTravelChoice
  = -- | CR 701.56a: put a time counter on the object.
    Add
  | -- | CR 701.56a: remove a time counter from the object.
    Remove
  deriving (Eq, Ord, Show)
