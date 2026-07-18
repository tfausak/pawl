module Pawl.Type.Timestamp where

import Numeric.Natural (Natural)

-- CR 613.7: within a layer, continuous effects apply in timestamp order. A
-- monotonic counter (GameState.nextTimestamp) stamps every object when it is
-- created -- CR 613.7b: a permanent's static-ability timestamp is when the
-- object entered -- and every stored continuous effect when it begins. One
-- comparable sequence is what lets a static ability (Humility) and a stored
-- effect (Serpent's Gift) order against each other in layer 6.
--
-- A dedicated newtype, not the object id reused: id is identity, this is
-- entry-order. Both are monotone today, but conflating them is a pun this rule
-- rejects.
newtype Timestamp = MkTimestamp Natural
  deriving (Eq, Ord, Show)
