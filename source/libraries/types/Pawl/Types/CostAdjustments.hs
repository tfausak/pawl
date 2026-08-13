module Pawl.Types.CostAdjustments where

import Numeric.Natural (Natural)
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ManaCost as ManaCost

-- | CR 601.2f's adjustments to one cost: the increases that apply to it, the
-- reductions that apply to it, the floor the reducing effects impose on what is
-- left, and the non-mana components other effects add to it. Gathered by
-- Pawl.Engine.Cost (spellAdjustments, activationAdjustments) and consumed by
-- Pawl.Engine.Cost.applyAdjustments and Pawl.Engine.Cost.plusComponents;
-- nothing else builds one.
--
-- The increases and the reductions are kept APART, never summed into one signed
-- delta, for Pawl.Types.PlayerEffect.ReduceSpellCost's stated reason: CR 601.2f
-- applies every increase before any reduction, and CR 118.7a gives a reduction a
-- restriction an increase does not have. They do not even have the same shape --
-- an increase is an amount of generic mana and a reduction is a ManaCost.
data CostAdjustments = MkCostAdjustments
  { increases :: [Natural],
    reductions :: [ManaCost.ManaCost],
    -- | The fewest mana the reductions may leave in the cost -- Heartstone's
    -- "This effect can't reduce the mana in that cost to less than one mana",
    -- which is card text CR 101.1 lets override the rules rather than a rule of
    -- its own. The MAXIMUM over the applying effects' own floors, since a floor
    -- one effect states binds whatever another says.
    --
    -- Zero is "no floor", and needs no Maybe to say so: CR 601.2f already floors
    -- every total at {0}, so a floor of zero constrains nothing. It is what every
    -- SPELL cost carries -- no printed spell-cost reducer states this sentence --
    -- and what an activation-cost reducer without the sentence (Hero of Iroas)
    -- would carry too.
    --
    -- A floor never RAISES a cost that was already below it, which is
    -- Heartstone's own ruling ("It will not add a {1} to abilities with no
    -- generic mana in their activation cost"): the clamp is on what the
    -- reductions took, not on the cost.
    minimumMana :: Natural,
    -- | CR 601.2f's other half of "cost increases", the one that is not mana at
    -- all: the additional non-mana components an effect adds to the cost
    -- (Brutal Suppression's @Sacrifice a land@). Appended to the cost's own
    -- components by Pawl.Engine.Cost.plusComponents, so an added component is
    -- paid, gated and ordered by exactly the machinery a printed one is.
    --
    -- A LIST rather than one component, matching Pawl.Types.Cost.components:
    -- several effects can add to one cost at once, so even a vocabulary where
    -- each effect adds a single component has to accumulate here.
    --
    -- SEPARATE from `increases` above rather than folded into it, for the reason
    -- that field is separate from `reductions`: the two do not have the same
    -- shape, and CR 601.2f orders the mana arithmetic (increases, then
    -- reductions, then the floor) in a way that a non-mana component takes no
    -- part in -- nothing reduces a "sacrifice a land" away.
    --
    -- Empty for a SPELL's cost. CR 601.2f's additional costs for a spell arrive
    -- through Pawl.Engine.Cost.plus instead, off the spell's own card text (CR
    -- 601.2b's alternative and additional costs), and no gathered player effect
    -- adds one.
    components :: [CostComponent.CostComponent Keyword.Keyword]
  }
  deriving (Eq, Ord, Show)
