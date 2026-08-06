module Pawl.Types.ActivatedAbility where

import qualified Pawl.Types.ActivationTiming as ActivationTiming
import qualified Pawl.Types.Condition as Condition
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
    timing :: ActivationTiming.ActivationTiming,
    -- | The "as long as" clause of a static ability that GRANTS this one, or
    -- Nothing for an ability the object simply has, which is nearly all of them.
    --
    -- CR 702.178a is the producer: "Max speed — [Ability]" means "as long as your
    -- speed is 4, this object has '[Ability]'". A grant whose grantee is the
    -- granting object itself and whose granted ability is printed right there is
    -- indistinguishable from the printed ability plus its own gate, so pawl
    -- carries the gate on the ability rather than minting a layer-6 grant --
    -- Pawl.Types.Modification has no arm that grants an activated ability, and
    -- giving it one would make it parametric in `card`.
    --
    -- The SAME shape as Pawl.Types.StaticAbility.condition, which spells CR
    -- 604.2's "as long as" for a continuous effect, and the same rule about what
    -- it is not: not a Pawl.Types.Duration, because CR 611.2c's "for as long as"
    -- ends a stored effect once, while this is re-asked on every read
    -- (Pawl.Engine.Projection.abilitiesGiven) and so takes the ability away and
    -- gives it back as the board moves, with no resolution in between.
    --
    -- Read AFTER the layer fold rather than at the projection seed: the seed's
    -- view determines nothing (#156), and CR 604.2's own gate -- the object being
    -- on the battlefield with the ability -- is the fold's job, which this only
    -- narrows.
    --
    -- CR 702.178b's zone clause is why abilitiesGiven is not the only reader: "if
    -- an ability granted by a max speed ability states which zones it functions
    -- from, the max speed ability that grants that ability functions from those
    -- zones". Pawl.Engine.Activate.graveyardAbilitiesOf asks the same gate of a
    -- card in a GRAVEYARD, for an ability whose cost or effect names that zone
    -- (CR 113.6m).
    condition :: Maybe Condition.Condition
  }
  deriving (Eq, Ord, Show)
