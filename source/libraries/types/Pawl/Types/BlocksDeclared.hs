module Pawl.Types.BlocksDeclared where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.ObjectId as ObjectId

-- | CR 509.1i read the other way: a blocking creature, and how many attackers it
-- blocked in that declaration, or blocks now that an effect made it block anew
-- (CR 509.3a).
data BlocksDeclared = MkBlocksDeclared
  { blocker :: ObjectId.ObjectId,
    count :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
