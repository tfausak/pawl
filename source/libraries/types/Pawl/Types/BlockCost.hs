module Pawl.Types.BlockCost where

import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.PerCreature as PerCreature

-- | CR 509.1b / CR 509.1d: one printed COST TO BLOCK (Oppressive Rays). CR 509.1b
-- classifies it as a RESTRICTION -- the second arm of its parenthetical, whose
-- condition CR 509.1d-509.1f then determine and pay -- and CR 509.1c's second
-- sentence makes paying it optional.
--
-- The SEVENTH carrier of a printed static ability, alongside
-- Pawl.Types.StaticAbility, Pawl.Types.PlayerStaticAbility,
-- Pawl.Types.BlockRequirement, Pawl.Types.AttackRequirement,
-- Pawl.Types.CombatRestriction and Pawl.Types.AttackCost.
--
-- Pawl.Types.AttackCost's blocking twin, and everything that type's header argues
-- about carrier choice holds here with CR 509 substituted for CR 508: a creature
-- under an Oppressive Rays is not one Pawl.Engine.CombatRestriction.cantBlock may
-- name, because it CAN block, and does so the moment CR 509.1f's payment is made.
--
-- The "YOU" is the BLOCKER's controller and not the source's, which is the one
-- axis on which this type is simpler than its twin. Oppressive Rays says "its
-- controller pays", and CR 509.1a makes every chosen creature one the defending
-- player controls, so the payer is that player under any phrasing.
--
-- No `scope` field either, for the same reason stated as a rule: CR 508.1b makes
-- an attack an announcement ABOUT something, which is what Ghostly Prison's "you"
-- narrows, while CR 509.1d totals over the CHOSEN CREATURES and nothing in CR 509
-- gives the cost a second object to be judged against. A printing that taxed only
-- blocks of a particular attacker would add a field here.
--
-- Gathered LIVE from the battlefield on every read and never captured, the posture
-- every carrier on this axis takes -- so an Oppressive Rays leaving the
-- battlefield lifts its cost with nothing to unwind. CR 509.1d's "locked in" is
-- the one moment that is deliberately NOT live, and it is Pawl.Engine.Combat's to
-- enforce: the total is computed once from the finished declaration and never
-- recomputed.
data BlockCost = MkBlockCost
  { -- | Which creatures the cost is on -- Oppressive Rays' "enchanted creature".
    -- An Affected, and not a bare Filter, for the reason
    -- Pawl.Types.AttackCost's field is one: the set is re-derived every time it
    -- is asked, so a creature the Aura stops enchanting stops being taxed.
    subject :: Affected.Affected,
    -- | What ONE taxed blocker costs -- Oppressive Rays' {3}. CR 509.1d's total
    -- cost to block is this repeated once per taxed creature CHOSEN as a blocker,
    -- so it is charged once however many attackers that creature was assigned to
    -- (CR 509.1a plus Pawl.Engine.BlockPermission): the rule totals over the
    -- creatures, not over the pairs.
    --
    -- A whole cost and not only mana, which is CR 509.1d's own width, and CR
    -- 509.1e's window plus CR 509.1f's payment are together
    -- Pawl.Engine.Cost.payToll -- Pawl.Types.AttackCost's field for both, its
    -- reasons unchanged.
    perBlocker :: PerCreature.PerCreature
  }
  deriving (Eq, Ord, Show)
