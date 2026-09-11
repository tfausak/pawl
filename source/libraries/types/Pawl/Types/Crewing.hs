module Pawl.Types.Crewing where

import qualified Data.Set as Set
import qualified Pawl.Types.ObjectId as ObjectId

-- | CR 702.122c's "crewed by" relation for one crew activation: the Vehicle,
-- and the creatures tapped to pay that activation's cost.

-- The two fields are different questions -- the relation names the Vehicle on
-- one side and the creatures on the other -- so they are named rather than
-- positional.
data Crewing = MkCrewing
  { vehicle :: ObjectId.ObjectId,
    -- | CR 702.122b: the creatures tapped to pay the cost of THAT crew ability,
    -- which is also the set rule 702.122e's rider asks about. Empty when the cost
    -- carried no TapForTotalPower component, which no printed crew ability does.
    crewedBy :: Set.Set ObjectId.ObjectId
  }
  deriving (Eq, Ord, Show)
