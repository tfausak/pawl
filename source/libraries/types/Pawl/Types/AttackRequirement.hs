module Pawl.Types.AttackRequirement where

import qualified Pawl.Types.Affected as Affected

-- | CR 508.1d: one printed ATTACKING REQUIREMENT. Curse of the Nightly Hunt's
-- "creatures enchanted player controls attack each combat if able".
--
-- The twin of Pawl.Types.BlockRequirement, which argues why neither
-- Pawl.Types.StaticAbility nor Pawl.Types.PlayerStaticAbility can hold a
-- requirement. The two collapse opposite axes, because the printings do:
-- BlockRequirement carries only the attacker to be blocked, its subject always
-- being "all creatures able to block" (#341), while this carries only the
-- subject, since no printing in the pool narrows CR 508.1b's announcement of
-- whom each attacker is attacking. A requirement naming its object ("attacks a
-- player other than you if able") is unrepresentable (#461).
--
-- Gathered LIVE from the battlefield on every read and never captured, so a
-- Curse leaving the battlefield lifts its requirement with nothing to unwind.
newtype AttackRequirement = MkAttackRequirement
  { -- | Which creatures are required to attack. An Affected, not a bare
    -- ObjectId, so the set is re-derived every time it is asked. CR 611.2c and
    -- CR 613.11 are why Affected.TheseObjects is the arm this field has no use
    -- for: a rule-modifying continuous effect can affect objects that were not
    -- affected when it began.
    subject :: Affected.Affected
  }
  deriving (Eq, Ord, Show)
