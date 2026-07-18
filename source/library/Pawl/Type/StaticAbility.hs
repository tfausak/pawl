module Pawl.Type.StaticAbility where

import Pawl.Type.Affected (Affected)
import Pawl.Type.Modification (Modification)

-- A card's printed static continuous ability (CR 604.3). Gathered live from every
-- battlefield permanent by the projection, with the permanent's own timestamp
-- (CR 613.7b). Humility declares two: (AllCreatures, LoseAllAbilities) and
-- (AllCreatures, SetBasePowerToughness 1 1).
data StaticAbility = MkStaticAbility
  { affected :: Affected,
    modification :: Modification
  }
  deriving (Eq, Ord, Show)
