module Pawl.Type.Aggregation where

-- How a Pawl.Type.Count turns its matched set into a number. A real axis, not
-- always "length": CR 208.2a's Tarmogoyf counts the distinct card TYPES among
-- the cards in every graveyard, which is the size of a union.
data Aggregation
  = Objects
  | DistinctCardTypes
  deriving (Eq, Ord, Show)
