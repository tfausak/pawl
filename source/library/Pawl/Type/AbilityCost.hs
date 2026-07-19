module Pawl.Type.AbilityCost where

import Pawl.Type.AdditionalCost (AdditionalCost)

-- The cost of an activated ability (CR 602.1). A `mana :: Maybe ManaCost` field
-- is the named future addition (no M3e gate has a mana symbol in its ability
-- cost); for now only the non-mana costs. A newtype today; becomes `data` when
-- that second field lands.
newtype AbilityCost = MkAbilityCost
  { additional :: [AdditionalCost]
  }
  deriving (Eq, Ord, Show)
