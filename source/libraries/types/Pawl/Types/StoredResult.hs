module Pawl.Types.StoredResult where

import qualified Numeric.Natural as Natural

-- | CR 706.8a: a die result stored on a permanent -- the kind of die rolled,
-- which CR 706.1a's N describes whole, and the result, the stored result's
-- "value".
data StoredResult = MkStoredResult
  { sides :: Natural.Natural,
    value :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
