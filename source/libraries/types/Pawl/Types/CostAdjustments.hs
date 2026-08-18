module Pawl.Types.CostAdjustments where

import Numeric.Natural (Natural)
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.CostScale as CostScale
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ManaCost as ManaCost

-- | CR 601.2f's adjustments to one cost: the increases that apply to it, the
-- reductions that apply to it -- each with the floor its own effect states --
-- and the non-mana components other effects add to it. Gathered by
-- Pawl.Engine.Cost (spellAdjustments, activationAdjustments) and consumed by
-- Pawl.Engine.Cost.applyAdjustments and Pawl.Engine.Cost.plusComponents;
-- nothing else builds one.
--
-- The increases and the reductions are kept APART, never summed into one signed
-- delta, for Pawl.Types.PlayerEffect.ReduceSpellCost's stated reason: CR 601.2f
-- applies every increase before any reduction, and CR 118.7a gives a reduction a
-- restriction an increase does not have. They do not even have the same shape --
-- an increase is an amount of generic mana, and a reduction is a ManaCost with a
-- floor of its own.
data CostAdjustments = MkCostAdjustments
  { increases :: [Natural],
    -- | Each reduction PAIRED WITH ITS OWN FLOOR: the fewest mana that effect
    -- may leave in the cost -- Heartstone's "This effect can't reduce the mana in
    -- that cost to less than one mana", which is card text CR 101.1 lets override
    -- the rules rather than a rule of its own.
    --
    -- PAIRED rather than one floor over the pool, because the sentence says "this
    -- effect": a floored reduction beside an unfloored one leaves a different
    -- total depending on which of the two the floor is checked against, and only
    -- the reduction that states it is bound by it. Pawl.Engine.Cost.applyAdjustments
    -- folds them one at a time for that reason.
    --
    -- Zero is "no floor", and needs no Maybe to say so: CR 601.2f already floors
    -- every total at {0}, so a floor of zero constrains nothing. It is what every
    -- SPELL cost carries -- no printed spell-cost reducer states this sentence --
    -- and what an activation-cost reducer without the sentence (Blossoming
    -- Tortoise) carries too.
    --
    -- A floor never RAISES a cost that was already below it, which is
    -- Heartstone's own ruling ("It will not add a {1} to abilities with no
    -- generic mana in their activation cost"): the clamp is on what that
    -- reduction took, not on the cost.
    reductions :: [(ManaCost.ManaCost, Natural)],
    -- | CR 601.2f's other half of "cost increases", the one that is not mana at
    -- all: the additional non-mana components an effect adds to the cost
    -- (Brutal Suppression's @Sacrifice a land@). Appended to the cost's own
    -- components by Pawl.Engine.Cost.plusComponents, so an added component is
    -- paid, gated and ordered by exactly the machinery a printed one is --
    -- except an added LOYALTY component (Carth the Lion's @[+1]@), which CR 606.5
    -- merges into the printed one instead of leaving beside it.
    --
    -- A LIST rather than one component, matching Pawl.Types.Cost.components:
    -- several effects can add to one cost at once, so even a vocabulary where
    -- each effect adds a single component has to accumulate here.
    --
    -- Each component PAIRED WITH ITS OWN SCALE, the way the reductions above are
    -- paired with their own floors and for the same reason: "for each black mana
    -- symbol in their mana costs" is one effect's sentence, so a scaled addition
    -- beside an unscaled one must expand by its own count. The scale is not
    -- resolved here because a gatherer is handed an OBJECT and never a cost;
    -- Pawl.Engine.Cost.plusComponents holds the cost and cashes it there.
    --
    -- SEPARATE from `increases` above rather than folded into it, for the reason
    -- that field is separate from `reductions`: the two do not have the same
    -- shape, and CR 601.2f orders the mana arithmetic (increases, then
    -- reductions, then the floor) in a way that a non-mana component takes no
    -- part in -- nothing reduces a "sacrifice a land" away.
    --
    -- Written on BOTH sides of CR 601.2f: an activation cost's additions (Brutal
    -- Suppression, by CR 602.2b) and a spell's (Drought). A spell's own PRINTED
    -- additional costs are not among them -- those are card text and arrive
    -- through Pawl.Engine.Cost.plus at CR 601.2b instead.
    components :: [(CostScale.CostScale, CostComponent.CostComponent Keyword.Keyword)]
  }
  deriving (Eq, Ord, Show)
