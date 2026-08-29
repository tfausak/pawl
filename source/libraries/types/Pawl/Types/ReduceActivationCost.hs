module Pawl.Types.ReduceActivationCost where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.AbilityKind as AbilityKind
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.KeywordFamily as KeywordFamily
import qualified Pawl.Types.ManaCost as ManaCost

-- | The payload of Pawl.Types.PlayerEffect's ReduceActivationCost arm (#1305).
--
-- The FLOOR is carried rather than assumed, because it is card text (CR 101.1)
-- and not a rule: Heartstone says "This effect can't reduce the mana in that
-- cost to less than one mana" and so carries 1, while Blossoming Tortoise's
-- "Activated abilities of lands you control cost {1} less to activate" does not
-- say it and carries 0. See Pawl.Types.CostAdjustments.reductions for what zero
-- means, why a floor never raises a cost, and why the two kinds cannot share one
-- floor over the pool.
--
-- FOUR criteria, and they ask about different things. `whichAbilities` names the
-- ability's SOURCE OBJECT -- Heartstone's "activated abilities of creatures",
-- Blossoming Tortoise's "of lands you control" -- and is matched by
-- Pawl.Engine.PlayerEffect.matchesObject against that object's projection.
-- `grantedBy` names the KIND of ability instead: Nothing is every activated
-- ability of a matching source, and Just a family is only the ability rule 702
-- mints for a keyword of that family, which is Fluctuator's "CYCLING abilities
-- you activate", and Bureau Headmaster's "EQUIP abilities you activate". A
-- source filter cannot say that -- a Barkhide Mauler in
-- a hand is not distinguished from itself by anything about the object -- and an
-- ability-shaped Filter atom put in `whichAbilities` could never be true there,
-- since that field is matched against the object and not the ability.
--
-- A rule-702 FAMILY designator and not an effect: what the closed half compares
-- here is which rule minted the ability (Pawl.Engine.Keyword.familyGranting),
-- never what the ability does.
--
-- `whichKind` is the THIRD, and neither of the two above can say it: CR 605.1a's
-- classification of the ability BEING ACTIVATED, which is Zirda, the
-- Dawnwaker's "abilities you activate THAT AREN'T MANA ABILITIES". Nothing is
-- every activated ability of a matching source, which is what Heartstone and
-- Blossoming Tortoise print; Just a kind is only the abilities on that side of
-- the rule. `grantedBy`'s Nothing could never have stood in for it -- no
-- rule-702 provenance is equally true of every ordinary non-keyword activated
-- ability -- and a Filter atom could never either, since `whichAbilities` is
-- asked of the Mountain and whether the Mountain's `{T}: Add {R}` is a mana
-- ability is a fact about the ABILITY. The same field
-- Pawl.Types.IncreaseActivationCost carries, for the same reason.
--
-- `whichTargets` is the FOURTH question, and it is none of the other three: an
-- object again, but the ability's CHOSEN TARGET rather than its source -- Dwarven
-- Mauler's "equip abilities you activate THAT TARGET THIS CREATURE". Nothing is
-- the sentence that names no target, which is every other reducer in the pool;
-- Just a filter holds when SOME target the activation announced matches it, which
-- is what "that target this creature" says of an ability with more than one
-- target slot. A filter here can never be spelled as a conjunct of
-- `whichAbilities`: that field is asked of the Equipment, and the creature the
-- equip aims at is a different object entirely.
--
-- Asked LATER than the other three, and that is the field's whole difficulty: CR
-- 601.2c announces targets and CR 601.2f applies reductions, so a target-aware
-- reduction cannot be gathered at CR 601.2b's position.
-- Pawl.Engine.Activate.activateAbility gathers twice for exactly that reason.
data ReduceActivationCost = MkReduceActivationCost
  { whichAbilities :: Filter.Filter Keyword.Keyword,
    grantedBy :: Maybe KeywordFamily.KeywordFamily,
    whichKind :: Maybe AbilityKind.AbilityKind,
    whichTargets :: Maybe (Filter.Filter Keyword.Keyword),
    reduction :: ManaCost.ManaCost,
    floor :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
