module Pawl.Types.LimitUnless where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Condition as Condition

-- | A SIZE BOUND on how many creatures may be declared, and CR 508.1c's "unless"
-- gate. The bound names no creature, which is why the key is @limit@ rather than
-- @affected@.

-- Shared by CombatRestriction's CantAttackMoreThan and CantBlockMoreThan as
-- expediency, on [[AffectedUnless]]'s terms: the two bound opposite declarations
-- and would each take their own record the moment one of them grew a field.
data LimitUnless = MkLimitUnless
  { limit :: Natural.Natural,
    -- | Nothing is the unconditional restriction. Elided rather than written
    -- null.
    unless :: Maybe Condition.Condition
  }
  deriving (Eq, Ord, Show)
