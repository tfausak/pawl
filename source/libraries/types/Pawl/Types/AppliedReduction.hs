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
-- The card-facing payload of Pawl.Types.CostChange's Reduce arm too, which is
-- why it has a wire form; the element type of
-- Pawl.Types.CostAdjustments.reductions, where Pawl.Engine.Cost.spellAdjustments
-- and Pawl.Engine.Cost.plusReductions (CR 702.119a's amount) also build one.
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
    -- reducer without the sentence carries (Blossoming Tortoise); Scryfall
    -- o:"to less than one mana", 2026-10-10, finds the sentence only on
    -- activation-cost reducers, though a spell's would be read the same way.
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
