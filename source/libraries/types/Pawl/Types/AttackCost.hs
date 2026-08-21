module Pawl.Types.AttackCost where

import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.AttackCostScope as AttackCostScope
import qualified Pawl.Types.PerCreature as PerCreature

-- | CR 508.1c / CR 508.1h: one printed COST TO ATTACK (Ghostly Prison). CR 508.1c
-- classifies it as a RESTRICTION -- the second arm of its parenthetical, whose
-- condition CR 508.1h-508.1j then determine and pay -- and CR 508.1d's third
-- sentence makes paying it optional.
--
-- The SIXTH carrier of a printed static ability, alongside
-- Pawl.Types.StaticAbility, Pawl.Types.PlayerStaticAbility,
-- Pawl.Types.BlockRequirement, Pawl.Types.AttackRequirement and
-- Pawl.Types.CombatRestriction. Pawl.Types.BlockRequirement's header argues why
-- neither of the first two can hold one of these, and that argument holds here
-- unchanged.
--
-- What is different is that this is a SECOND carrier for one rule.
-- Pawl.Types.CombatRestriction holds CR 508.1c's arms alongside CR 509.1b's;
-- this type is that same second clause narrowed to the one condition that is not
-- a Condition at all, because it is a cost to be PAID rather than a fact to be
-- read. The split
-- is pawl's and not the rules': at the CR's own level Ghostly Prison is a 508.1c
-- restriction like Pacifism.
--
-- The split is forced by what the other carrier's ANSWER is.
-- Pawl.Engine.CombatRestriction.cantAttack returns which creatures may not
-- attack AT ALL, and Pawl.Engine.Combat.canAttackGiven drops every one of them
-- from CR 508.1a's candidate list. A creature under a Ghostly Prison is not one
-- of them: it CAN attack, and does so the moment CR 508.1j's payment is made.
-- Folding this into that type would either strike a payable attacker off the
-- candidate list or teach a set of ids a cost it has nowhere to put.
--
-- The "YOU" is IMPLICIT and is the source's controller (CR 109.5). WHAT that
-- player's "you" covers is the `scope` field below, and Pawl.Engine.AttackCost
-- charges an attack exactly when its CR 508.1b announcement falls inside it.
--
-- Gathered LIVE from the battlefield on every read and never captured, the
-- posture all five siblings take -- so a Ghostly Prison leaving the battlefield
-- lifts its cost with nothing to unwind. CR 508.1h's "locked in" is the one
-- moment that is deliberately NOT live, and it is Pawl.Engine.Combat's to
-- enforce: the total is computed once from the finished declaration and never
-- recomputed.
data AttackCost = MkAttackCost
  { -- | Which creatures the cost is on -- Ghostly Prison's "creatures". An
    -- Affected, and not a bare Filter, for the reason
    -- Pawl.Types.CombatRestriction's field is one: the set is re-derived every
    -- time it is asked, so a creature that stops matching stops being taxed.
    --
    -- Vacuous for every printing in the pool -- every creature matches, since
    -- only a creature can be declared as an attacker (CR 508.1a) -- and carried
    -- anyway, so that the sentence stays card DATA rather than a fact the engine
    -- knows about Ghostly Prison. No printing of this family narrows it, so the
    -- field is where a narrowing one would go.
    subject :: Affected.Affected,
    -- | What ONE taxed attacker costs -- Ghostly Prison's "{2} for each creature
    -- they control that's attacking you". CR 508.1h's total cost to attack is
    -- this repeated once per taxed attacker, so a declaration of three creatures
    -- owes {6}; that rule only TOTALS, and the multiplying is the card's own "for
    -- each".
    --
    -- Mana only, and not a Pawl.Types.Cost, so it carries no components. CR
    -- 508.1h's list is wider than mana, but a cost to attack that is not mana has
    -- no printing here (gap #599). Mana alone is also what makes CR 508.1i's
    -- window plus CR 508.1j the whole payment, which Pawl.Engine.Cost.payMana is.
    perAttacker :: PerCreature.PerCreature,
    -- | Which attacks the cost is on -- CR 508.1b's announcement judged against
    -- the source's controller. Ghostly Prison's family protects that player
    -- alone; Sphere of Safety's protects their planeswalkers too. See
    -- Pawl.Types.AttackCostScope.
    scope :: AttackCostScope.AttackCostScope
  }
  deriving (Eq, Ord, Show)
