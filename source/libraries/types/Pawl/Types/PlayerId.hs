module Pawl.Types.PlayerId where

import Numeric.Natural (Natural)

newtype PlayerId = MkPlayerId
  { unwrap :: Natural
  }
  deriving (Eq, Ord, Show)
