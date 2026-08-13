module Pawl.Types.DealDamage where

import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.Quantity as Quantity

-- | CR 119.3: deal this much damage to the objects or players the ObjectRef names.
data DealDamage = MkDealDamage
  { ref :: ObjectRef.ObjectRef,
    quantity :: Quantity.Quantity
  }
  deriving (Eq, Ord, Show)
