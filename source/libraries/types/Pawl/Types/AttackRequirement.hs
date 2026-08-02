module Pawl.Types.AttackRequirement where

import qualified Pawl.Types.Affected as Affected

-- | CR 508.1d: one printed ATTACKING REQUIREMENT -- an "effect that says a creature
-- attacks if able, or that it attacks if some condition is met". Curse of the
-- Nightly Hunt's "creatures enchanted player controls attack each combat if
-- able".
--
-- The FOURTH carrier of a printed static ability, and the twin of
-- Pawl.Types.BlockRequirement, whose comment argues at length why neither
-- Pawl.Types.StaticAbility (a CR 613.1 layer computes CHARACTERISTICS, and this is
-- not one) nor Pawl.Types.PlayerStaticAbility (whose scope names PLAYERS) can hold
-- a requirement. That argument is not repeated here; only the difference is.
--
-- The difference is the AXIS. CR 508.1d and CR 509.1c each imply two -- WHICH
-- creatures are required to act, and WHAT they must act on -- and the two carriers
-- collapse OPPOSITE ones, because the printings do:
--
--   * A blocking requirement's subject is always "all creatures able to block",
--     so BlockRequirement carries only the attacker to be blocked (#341).
--   * An attacking requirement has no object to carry at all. CR 508.1b's
--     announcement of WHAT each attacker is attacking is a separate choice the
--     active player makes, and no printing in the pool narrows it -- "attack each
--     combat if able" says nothing about whom. So this carries only the SUBJECT,
--     and a requirement that named its object ("attacks a player other than you
--     if able") is unrepresentable (#461).
--
-- Gathered LIVE from the battlefield on every read and never captured, the posture
-- all three siblings take -- so a Curse leaving the battlefield lifts its
-- requirement with nothing to unwind.
newtype AttackRequirement = MkAttackRequirement
  { -- | Which creatures are required to attack. An Affected, and not a bare
    -- ObjectId, for the reason BlockRequirement's field is one: the set is
    -- re-derived every time it is asked. Curse of the Nightly Hunt's is
    -- Affected.AttachedPlayerControls (CR 303.4m read through the enchanted
    -- player), and the shape a creature's own "attacks each combat if able" would
    -- take is Affected.Matching Filter.IsSource -- the same two arms
    -- BlockRequirement's field already sees.
    --
    -- CR 611.2c is why Affected.TheseObjects is the arm this field has no use
    -- for: a continuous effect that "doesn't modify the characteristics or change
    -- the controller of any objects modifies the rules of the game, so it can
    -- affect objects that weren't affected when that continuous effect began",
    -- and CR 613.11 classifies a requirement as exactly that kind of effect.
    subject :: Affected.Affected
  }
  deriving (Eq, Ord, Show)
