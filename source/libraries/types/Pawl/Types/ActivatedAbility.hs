module Pawl.Types.ActivatedAbility where

import qualified Pawl.Types.ActivationRestriction as ActivationRestriction
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
    -- | CR 602.5: every clause of the "activate only ..." rider the ability
    -- prints, ALL of which must hold. Empty for an ability without one, which is
    -- all of them but equip and a handful of printed windows.
    restrictions :: [ActivationRestriction.ActivationRestriction],
    -- | The "as long as" clause of a static ability that GRANTS this one, or
    -- Nothing for an ability the object simply has, which is nearly all of them.
    --
    -- CR 702.178a is the producer: "Max speed — [Ability]" means "as long as your
    -- speed is 4, this object has '[Ability]'". A grant whose grantee is the
    -- granting object itself and whose granted ability is printed right there is
    -- indistinguishable from the printed ability plus its own gate, so pawl
    -- carries the gate on the ability rather than minting a layer-6 grant.
    -- Pawl.Types.Modification's GainActivatedAbility is the layer-6 grant, and
    -- it is for the OTHER case: an ability handed to a DIFFERENT object, which
    -- is not indistinguishable from anything -- CR 113.7a moves the source and
    -- CR 602.2b the controller.
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
