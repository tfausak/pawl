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
    -- which is the set rule 702.122e's rider would read. Empty is reachable --
    -- a crew cost paid with no TapForTotalPower component left is not a shape
    -- the pool prints, but nothing in the type forbids it.
    crewedBy :: Set.Set ObjectId.ObjectId
  }
  deriving (Eq, Ord, Show)
