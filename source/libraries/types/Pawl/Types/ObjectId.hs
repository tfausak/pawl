module Pawl.Types.ObjectId where

import Numeric.Natural (Natural)

newtype ObjectId = MkObjectId
  { unwrap :: Natural
  }
  deriving (Eq, Ord, Show)
