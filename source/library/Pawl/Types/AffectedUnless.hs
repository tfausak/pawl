module Pawl.Types.AffectedUnless where

import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.Condition as Condition

-- | Which creatures a combat restriction names, and CR 508.1c's "unless" gate.

-- Shared by CombatRestriction's CantAttack, CantBlock and CantAttackAlone as
-- expediency, not because those three declare the same thing: they forbid
-- different acts and Pawl.Engine.Combat reads each separately. If one of them
-- comes to need a field the others do not, it gets its own record rather than an
-- optional field here.
data AffectedUnless = MkAffectedUnless
  { affected :: Affected.Affected,
    -- | Nothing is the unconditional restriction, which is most of them, so the
    -- key is elided rather than written null.
    unless :: Maybe Condition.Condition
  }
  deriving (Eq, Ord, Show)
