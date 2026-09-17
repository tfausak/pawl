module Pawl.Types.LimitUnless where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Condition as Condition

-- | A SIZE BOUND on how many creatures may be declared as blockers, and CR
-- 509.1b's "unless" gate. The bound names no creature, which is why the key is
-- @limit@ rather than @affected@.

-- CombatRestriction's CantBlockMoreThan alone. It was shared with
-- CantAttackMoreThan until that one grew Pawl.Types.AttackLimitUnless's
-- `defenders`, which is the field this record's own note said would split them.
data LimitUnless = MkLimitUnless
  { limit :: Natural.Natural,
    -- | Nothing is the unconditional restriction. Elided rather than written
    -- null.
    unless :: Maybe Condition.Condition
  }
  deriving (Eq, Ord, Show)
