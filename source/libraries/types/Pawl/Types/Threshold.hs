module Pawl.Types.Threshold where

import qualified Data.Map.Strict as Map
import qualified Pawl.Types.ObjectId as ObjectId

-- | A TOTAL the objects one selection takes must reach between them, rather than
-- a number of them: CR 702.122a's "total power N or greater" and CR 701.59a's
-- "total mana value N or greater". What a Pawl.Types.Claim carrying one
-- contends for is WHICH objects, since only some of them add up.
data Threshold = MkThreshold
  { -- | The N a selection's amounts must reach.
    total :: Integer,
    -- | What each object in the claim's pool adds towards it.
    amounts :: Map.Map ObjectId.ObjectId Integer
  }
  deriving (Eq, Ord, Show)
