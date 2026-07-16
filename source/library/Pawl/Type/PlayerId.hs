module Pawl.Type.PlayerId where

import Numeric.Natural (Natural)

newtype PlayerId = MkPlayerId Natural
  deriving (Eq, Ord, Show)
