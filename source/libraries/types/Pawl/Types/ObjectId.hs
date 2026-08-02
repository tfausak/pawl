module Pawl.Types.ObjectId where

import qualified Numeric.Natural as Natural

newtype ObjectId = MkObjectId
  { unwrap :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
