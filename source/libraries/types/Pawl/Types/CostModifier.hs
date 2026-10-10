module Pawl.Types.CostModifier where

import qualified Pawl.Types.CostChange as CostChange
import qualified Pawl.Types.CostSubject as CostSubject
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.TurnScope as TurnScope

-- | The payload of Pawl.Types.PlayerEffect's ModifyCost arm: CR 601.2f's
-- increase, reduction or additional cost, applied by a CR 613.11 effect to the
-- spells or the activations its subject names (Thalia, Sapphire Medallion,
-- Drought, Oppressive Rays, Heartstone, Brutal Suppression).
--
-- ONE record for both subjects because CR 602.2b applies CR 601.2b-i to an
-- activation "just as they apply to casting a spell": every criterion below
-- reads a step of that shared procedure, so it reads the same for either.
-- What only an activation has -- an ability among several on one object --
-- rides the subject (Pawl.Types.ActivationCriteria).
data CostModifier = MkCostModifier
  { -- | Spells, or activated abilities and what narrows them.
    subject :: CostSubject.CostSubject,
    -- | The spell, or the activated ability's SOURCE object, matched against
    -- its projection: Thalia's "noncreature spells", Heartstone's "activated
    -- abilities of creatures".
    matching :: Filter.Filter Keyword.Keyword,
    -- | CR 601.2c: when Just, applies only where SOME announced target matches
    -- -- Kopala, Warden of Waves' "that target a Merfolk you control", Dwarven
    -- Mauler's "that target this creature". Once, however many match.
    whichTargets :: Maybe (Filter.Filter Keyword.Keyword),
    -- | CR 601.2c: when Just, the change applies once per DISTINCT announced
    -- target this matches -- Hinata, Dawn-Crowned's "{1} more to cast for each
    -- target", Battlefield Thaumaturge's "for each creature it targets".
    perTarget :: Maybe (Filter.Filter Keyword.Keyword),
    -- | Professor Hojo's "the FIRST activated ability you activate during your
    -- turn": Just a scope applies the change only in a turn the scope admits,
    -- and only to the first matching activation the player begins that turn --
    -- whether or not that one was changed, and whether or not this effect
    -- existed yet (Tezzeret, Betrayer of Flesh's ruling).
    --
    -- Not implemented for spells: Shadow in the Warp's "the first creature
    -- spell you cast each turn" (#4924); Pawl.Codec.CostModifier refuses it.
    onlyFirst :: Maybe TurnScope.TurnScope,
    -- | What it does to the total cost.
    change :: CostChange.CostChange
  }
  deriving (Eq, Ord, Show)
