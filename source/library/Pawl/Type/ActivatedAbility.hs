module Pawl.Type.ActivatedAbility where

import Pawl.Type.AbilityCost (AbilityCost)
import Pawl.Type.Modal (Modal)

-- CR 602.1 / 700.2 / 602.2b: "[cost]: [effect]", now modal-capable. VALUE-typed:
-- Action.Activate carries the value and validates by membership
-- (Projection.abilitiesOf), never an index. Parametric in `card` (M4c): a concrete
-- Modal Card would drag Card in and cycle; Card ties the knot at Modal Card. The
-- effects now live in Mode.effects :: Seq (Effect card) -- M4g's interim [Effect]
-- divergence, retired.
data ActivatedAbility card = MkActivatedAbility
  { cost :: AbilityCost,
    modal :: Modal card
  }
  deriving (Eq, Ord, Show)
