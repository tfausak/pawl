module Pawl.Types.AttackCost where

import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.ManaCost as ManaCost

-- | CR 508.1d's cost clause: one printed COST TO ATTACK -- the thing that rule
-- means by "if a creature can't attack unless a player pays a cost, that player
-- is not required to pay that cost, even if attacking with that creature would
-- increase the number of requirements being obeyed". Ghostly Prison's "creatures
-- can't attack you unless their controller pays {2} for each creature they
-- control that's attacking you".
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
-- What is different is that this is NOT an arm of Pawl.Types.CombatRestriction,
-- which is the carrier it reads most like. Pawl.Engine.CombatRestriction.cantAttack
-- answers "which of these creatures may not attack AT ALL", and
-- Pawl.Engine.Combat.canAttackGiven drops every such creature from CR 508.1a's
-- candidate list. A creature under a Ghostly Prison is not one of them: it CAN
-- attack, and does so the moment CR 508.1j's payment is made. Folding this into
-- that type would either strike a payable attacker off the candidate list or
-- teach that type's set-shaped answer a cost it has nowhere to put.
--
-- The rules keep them apart in the same place: CR 508.1c's restrictions are
-- checked, and then CR 508.1h-508.1j determine and pay "the total cost to attack"
-- as steps of their own.
--
-- The "YOU" is IMPLICIT and is the source's controller, CR 109.5 ("the text of an
-- ability of an object that isn't a spell ... uses 'you' to refer to the object's
-- controller"). It is not a field because the alternative shape is not in the
-- pool: Archangel of Tithes prints the same sentence with no object at all
-- ("creatures can't attack unless their controller pays {1} for each of those
-- creatures"), which would tax every attack rather than only the ones aimed at
-- one player, and nothing here can say that (#598). Every printing of Ghostly
-- Prison's own family -- Propaganda, Windborn Muse, Baird, Collective Restraint
-- -- says "you".
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
    -- knows about Ghostly Prison. A narrowing printing (Norn's Annex taxes with
    -- Phyrexian mana; Sphere of Safety scales instead) would fill it in.
    subject :: Affected.Affected,
    -- | What ONE taxed attacker costs -- Ghostly Prison's "{2} for each creature
    -- they control that's attacking you". The rule's own multiplication: CR
    -- 508.1h's total cost to attack is this repeated once per taxed attacker, so
    -- a declaration of three creatures owes {6}. Ghostly Prison's Two-Headed
    -- Giant ruling states the same arithmetic from the other end ("you still only
    -- have to pay once per creature").
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
