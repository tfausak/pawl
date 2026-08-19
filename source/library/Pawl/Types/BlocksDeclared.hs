module Pawl.Types.BlocksDeclared where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.ObjectId as ObjectId

-- | CR 509.1i read the other way: a blocking creature, and how many attackers it
-- blocked in that declaration.
data BlocksDeclared = MkBlocksDeclared
  { blocker :: ObjectId.ObjectId,
    count :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
