module Pawl.Types.PlayerId where

import qualified Numeric.Natural as Natural

newtype PlayerId = MkPlayerId
  { unwrap :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
