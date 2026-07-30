module Pawl.Types.PlayerId where

import Numeric.Natural (Natural)

newtype PlayerId = MkPlayerId Natural
  deriving (Eq, Ord, Show)
