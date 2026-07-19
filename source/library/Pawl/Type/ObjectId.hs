module Pawl.Type.ObjectId where

import Numeric.Natural (Natural)

newtype ObjectId = MkObjectId Natural
  deriving (Eq, Ord, Show)
