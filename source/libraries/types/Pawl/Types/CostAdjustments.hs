module Pawl.Types.CostAdjustments where

import Numeric.Natural (Natural)
import qualified Pawl.Types.ManaCost as ManaCost

-- | CR 601.2f's adjustments to one cost: the increases that apply to it, the
-- reductions that apply to it, and the floor the reducing effects impose on
-- what is left. Gathered by Pawl.Engine.Cost (spellAdjustments,
-- activationAdjustments) and consumed by Pawl.Engine.Cost.applyAdjustments;
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
    minimumMana :: Natural
  }
  deriving (Eq, Ord, Show)
