module Pawl.Types.Timestamp where

import qualified Numeric.Natural as Natural

-- | CR 613.7: within a layer, continuous effects apply in timestamp order. A
-- monotonic counter (GameState.nextTimestamp) stamps every object as it enters a
-- zone (CR 613.7d, and CR 613.7a gives a permanent's static-ability effect the
-- same stamp), every stored continuous effect as it begins, and every counter as
-- it is put on (CR 613.7c). One comparable
-- sequence is what lets a static ability (Humility) and a stored effect
-- (Serpent's Gift) order against each other in layer 6.
--
-- A dedicated newtype, not the object id reused: id is identity, this is
-- entry-order. Both are monotone today, but conflating them is a pun this rule
-- rejects.
newtype Timestamp = MkTimestamp
  { unwrap :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
