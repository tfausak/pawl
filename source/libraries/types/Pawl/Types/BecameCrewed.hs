module Pawl.Types.BecameCrewed where

import qualified Data.Set as Set
import qualified Pawl.Types.ObjectId as ObjectId

-- | CR 702.122e's crewing: which Vehicle became crewed, and which creatures
-- crewed it.

-- The two fields are different questions -- CR 702.122c's "crewed by" relation
-- names the Vehicle on one side and the creatures tapped to pay that
-- activation's cost on the other -- so they are named rather than positional.
data BecameCrewed = MkBecameCrewed
  { vehicle :: ObjectId.ObjectId,
    -- | CR 702.122b: the creatures tapped to pay the cost of THAT crew ability,
    -- which is also the set rule 702.122e's rider asks about. Empty when the cost
    -- carried no TapForTotalPower component, which no printed crew ability does.
    crewedBy :: Set.Set ObjectId.ObjectId
  }
  deriving (Eq, Ord, Show)
