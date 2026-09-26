module Pawl.Types.Saddling where

import qualified Data.Set as Set
import qualified Pawl.Types.ObjectId as ObjectId

-- | CR 702.171c's "saddles" relation for one saddle activation: the Mount, and
-- the creatures tapped to pay that activation's cost.
--
-- Pawl.Types.Crewing's shape, and a record of its own so that a creature that
-- saddled a Mount never reads as one that crewed a Vehicle.
data Saddling = MkSaddling
  { mount :: ObjectId.ObjectId,
    -- | CR 702.171c: the creatures tapped to pay the cost of THAT saddle ability.
    saddledBy :: Set.Set ObjectId.ObjectId
  }
  deriving (Eq, Ord, Show)
