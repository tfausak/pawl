module Pawl.Types.ActivatedAbility where

import qualified Pawl.Types.ActivationTiming as ActivationTiming
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Modal as Modal

-- | CR 602.1 / 700.2 / 602.2b: "[cost]: [effect]", modal-capable. VALUE-typed:
-- Action.Activate carries the value and validates by membership
-- (Projection.abilitiesOf), never an index. Parametric in `card` because a
-- concrete Modal Card would cycle with Card, which ties the knot at Modal Card.
--
-- An activation cost is a Pawl.Types.Cost, the same type a spell's cost takes
-- (CR 118.1).
data ActivatedAbility card = MkActivatedAbility
  { cost :: Cost.Cost Keyword.Keyword,
    modal :: Modal.Modal card,
    -- | CR 307.5: any timing rider the ability carries. AnyTime for every ability
    -- without one, which is all of them but equip.
    timing :: ActivationTiming.ActivationTiming
  }
  deriving (Eq, Ord, Show)
