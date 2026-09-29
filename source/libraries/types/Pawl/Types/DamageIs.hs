module Pawl.Types.DamageIs where

import Numeric.Natural (Natural)
import qualified Pawl.Types.Reference as Reference

-- | CR 120.6: the damage marked on a permanent.
data DamageIs = MkDamageIs
  { object :: Reference.Reference,
    damage :: Natural
  }
  deriving (Eq, Ord, Show)
