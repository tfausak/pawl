module Pawl.Type.StaticAbility where

import Pawl.Type.Affected (Affected)
import Pawl.Type.Modification (Modification)

-- A card's printed static continuous ability (CR 604.1/604.2: a static ability
-- creates a continuous effect active while its permanent is on the battlefield).
-- Gathered live from every battlefield permanent by the projection, with the
-- permanent's own timestamp (CR 613.7a: a static ability's continuous effect has
-- the same timestamp as the object it is on). Humility declares two:
-- (AllCreatures, LoseAllAbilities) and
-- (AllCreatures, SetBasePowerToughness 1 1).
data StaticAbility = MkStaticAbility
  { affected :: Affected,
    modification :: Modification
  }
  deriving (Eq, Ord, Show)
