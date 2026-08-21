module Pawl.Types.CostAdjustments where

import Numeric.Natural (Natural)
import qualified Pawl.Types.AppliedReduction as AppliedReduction
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.CostScale as CostScale
import qualified Pawl.Types.Keyword as Keyword

-- | CR 601.2f's adjustments to one cost: the increases that apply to it, the
-- reductions that apply to it -- each with the restrictions its own effect
-- states -- and the non-mana components other effects add to it. Gathered by
-- Pawl.Engine.Cost (spellAdjustments, activationAdjustments) and consumed by
-- Pawl.Engine.Cost.applyAdjustments and Pawl.Engine.Cost.plusComponents;
-- nothing else builds one.
--
-- The increases and the reductions are kept APART, never summed into one signed
-- delta, for Pawl.Types.PlayerEffect.ReduceSpellCost's stated reason: CR 601.2f
-- applies every increase before any reduction, and CR 118.7a gives a reduction a
-- restriction an increase does not have. They do not even have the same shape --
-- an increase is an amount of generic mana, and a reduction is an
-- AppliedReduction.
data CostAdjustments = MkCostAdjustments
  { increases :: [Natural],
    -- | Each reduction WITH THE RESTRICTIONS ITS OWN EFFECT STATES -- the floor
    -- (Heartstone) and the coloured-mana confinement (Edgewalker), both of them
    -- card text CR 101.1 lets override the rules. See
    -- Pawl.Types.AppliedReduction for what each means.
    --
    -- PER REDUCTION rather than over the pool, because each sentence says "this
    -- effect": a floored reduction beside an unfloored one leaves a different
    -- total depending on which of the two the floor is checked against, and only
    -- the reduction that states it is bound by it. Pawl.Engine.Cost.applyAdjustments
    -- folds them one at a time for that reason.
    reductions :: [AppliedReduction.AppliedReduction],
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
