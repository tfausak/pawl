module Pawl.Types.AttackCost where

import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.ManaCost as ManaCost

-- | CR 508.1c / CR 508.1h: one printed COST TO ATTACK. Ghostly Prison's "creatures
-- can't attack you unless their controller pays {2} for each creature they
-- control that's attacking you" -- which CR 508.1c classifies as a RESTRICTION,
-- the second arm of its parenthetical ("effects that say a creature can't attack,
-- OR THAT IT CAN'T ATTACK UNLESS SOME CONDITION IS MET"), whose condition CR
-- 508.1h-508.1j then determine and pay. It is also the thing CR 508.1d's cost
-- clause is about: "if a creature can't attack unless a player pays a cost, that
-- player is not required to pay that cost, even if attacking with that creature
-- would increase the number of requirements being obeyed".
--
-- The SIXTH carrier of a printed static ability, alongside
-- Pawl.Types.StaticAbility, Pawl.Types.PlayerStaticAbility,
-- Pawl.Types.BlockRequirement, Pawl.Types.AttackRequirement and
-- Pawl.Types.CombatRestriction. Pawl.Types.BlockRequirement's header argues at
-- length why neither of the first two can hold one of these -- CR 613.1 makes the
-- seven layers a machine for computing an OBJECT's characteristics, and "can't
-- attack" is not one of them, so no Modification could express it; and
-- PlayerScope names PLAYERS, where this names a creature. That argument is not
-- repeated; only what is different is.
--
-- What is different is that this is a SECOND carrier for one rule.
-- Pawl.Types.CombatRestriction holds both of CR 508.1c's arms -- the
-- unconditional one and, since Blind-Spot Giant, the one gated on a
-- Pawl.Types.Condition; this type is that same second clause narrowed to the one
-- condition that is not a Condition at all, because it is a cost to be PAID
-- rather than a fact to be read, and CR 508.1d's third sentence makes paying it
-- optional. The split is pawl's and not the rules': at the CR's own level Ghostly
-- Prison is a 508.1c restriction like Pacifism, and 508.1g-508.1j is the
-- machinery for meeting its condition rather than a category beside it.
--
-- The split is forced by what the other carrier's ANSWER is.
-- Pawl.Engine.CombatRestriction.cantAttack returns "which of these creatures may
-- not attack AT ALL", and Pawl.Engine.Combat.canAttackGiven drops every such
-- creature from CR 508.1a's candidate list. A creature under a Ghostly Prison is
-- not one of them: it CAN attack, and does so the moment CR 508.1j's payment is
-- made. Folding this into that type would either strike a payable attacker off
-- the candidate list or teach a set of ids a cost it has nowhere to put.
--
-- The "YOU" is IMPLICIT and is the source's controller, CR 109.5 ("the words
-- 'you' and 'your' on an object refer to the object's controller ... For a static
-- ability, this is the current controller of the object it's on"), and the thing
-- it names is the PLAYER. Pawl.Engine.AttackCost therefore charges an attack
-- exactly when its CR 508.1b announcement names that player, which is Ghostly
-- Prison's own ruling: "a creature that can't attack you can still attack a
-- planeswalker you control."
--
-- That is the whole object axis this type has, and the wider one is not
-- representable: Baird, Steward of Argive, Norn's Annex, Sphere of Safety and
-- Archangel of Tithes all print "creatures can't attack you OR PLANESWALKERS YOU
-- CONTROL", which charges an attack Ghostly Prison lets through (#598). Ghostly
-- Prison's own family -- Propaganda, Windborn Muse, Collective Restraint -- says
-- "you" and stops there. Checked against Scryfall 2026-08-02.
--
-- Gathered LIVE from the battlefield on every read and never captured, the
-- posture all five siblings take -- so a Ghostly Prison leaving the battlefield
-- lifts its cost with nothing to unwind. CR 508.1h's "locked in" is the one
-- moment that is deliberately NOT live, and it is Pawl.Engine.Combat's to enforce:
-- the total is computed once from the finished declaration and never recomputed.
data AttackCost = MkAttackCost
  { -- | Which creatures the cost is on -- Ghostly Prison's "creatures". An
    -- Affected, and not a bare Filter, for the reason
    -- Pawl.Types.CombatRestriction's field is one: the set is re-derived every
    -- time it is asked, so a creature that stops matching stops being taxed.
    --
    -- Vacuous for the one printing that has it -- every creature matches, since
    -- only a creature can be declared as an attacker (CR 508.1a) -- and carried
    -- anyway, so that the sentence stays card DATA rather than a fact the engine
    -- knows about Ghostly Prison. No printing of this family narrows it -- every
    -- one of them says "creatures" -- so the field is where a narrowing one would
    -- go rather than a distinction any card draws today.
    subject :: Affected.Affected,
    -- | What ONE taxed attacker costs -- Ghostly Prison's "{2} for each creature
    -- they control that's attacking you". The rule's own multiplication: CR
    -- 508.1h's total cost to attack is this repeated once per taxed attacker, so
    -- a declaration of three creatures owes {6}. CR 508.1h itself only TOTALS
    -- ("the active player determines the total cost to attack"); the multiplying
    -- is the card's own "for each", and this field is the thing being multiplied.
    --
    -- FIXED, so a share that counts the board is unrepresentable: Collective
    -- Restraint's "{X} ... where X is the number of basic land types among lands
    -- you control" and Sphere of Safety's enchantment count are the same sentence
    -- with a Pawl.Types.Quantity where this has a constant (#601).
    --
    -- A ManaCost and not a Pawl.Types.Cost, so it carries no components. CR
    -- 508.1h's list is wider than mana -- "costs may include paying mana, tapping
    -- permanents, sacrificing permanents, discarding cards, and so on" -- and a
    -- cost to attack that is not mana has no printing here (#599). Mana alone is
    -- also what makes CR 508.1i's window the whole of the payment:
    -- Pawl.Engine.Mana.payCost is that rule and CR 508.1j together.
    perAttacker :: ManaCost.ManaCost
  }
  deriving (Eq, Ord, Show)
