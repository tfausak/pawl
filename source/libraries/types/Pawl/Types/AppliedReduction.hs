module Pawl.Types.AppliedReduction where

import Numeric.Natural (Natural)
import qualified Pawl.Types.ManaCost as ManaCost

-- | CR 601.2f: ONE cost reduction as it applies to one cost -- the amount of
-- mana it takes off, plus the restrictions its own effect states.
--
-- A record rather than a tuple because the restrictions are per-EFFECT and not
-- per-pool: both fields below quote a sentence printed on one card, so two
-- reductions applying to one cost can disagree about either.
-- Pawl.Engine.Cost.applyAdjustments folds them one at a time for that reason.
--
-- Built only by Pawl.Engine.PlayerEffect's gatherers and by
-- Pawl.Engine.Cost.spellAdjustments; it is the element type of
-- Pawl.Types.CostAdjustments.reductions and has no wire form of its own, the
-- card-facing shapes being Pawl.Types.ReduceSpellCost,
-- Pawl.Types.ReduceActivationCost and Pawl.Types.CostReduction.
data AppliedReduction = MkAppliedReduction
  { -- | What comes off, as an amount of MANA rather than a number: CR 118.7
    -- reduces a cost by mana of a stated type, and Sapphire Medallion's {1} and
    -- Edgewalker's {W}{B} are the same shape of thing.
    amount :: ManaCost.ManaCost,
    -- | The fewest mana this effect may leave in the cost -- Heartstone's "This
    -- effect can't reduce the mana in that cost to less than one mana", which is
    -- card text CR 101.1 lets override the rules rather than a rule of its own.
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
    atLeast :: Natural,
    -- | Whether this effect confines itself to the COLOURED mana paid --
    -- Edgewalker's "This effect reduces only the amount of colored mana you
    -- pay", card text CR 101.1 lets override the rules.
    --
    -- False is the RULE, not the absence of one: CR 118.7b-d spill a coloured or
    -- colourless reduction the cost cannot use onto the cost's generic
    -- component, and True is what stops that spill, leaving the excess to do
    -- nothing at all.
    coloredOnly :: Bool
  }
  deriving (Eq, Ord, Show)
