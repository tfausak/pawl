module Pawl.Types.AffectedUnless where

import qualified Pawl.Types.AbilityName as AbilityName
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
    unless :: Maybe Condition.Condition,
    -- | CR 116.2d: the name this face gives the ability stating the restriction,
    -- so that face's own SpecialAction.IgnoreThisUntilEndOfTurn can say WHICH
    -- effect a payment ignores -- "the effect from that ability", singular.
    -- Pawl.Types.PlayerStaticAbility.name on the player axis, with the same
    -- meaning and joined to the grant by the same dataflow lint in
    -- Pawl.AbilitySlotLintSpec.
    --
    -- Nothing for the overwhelming majority, which grant no such permission and
    -- so have nothing to be referred to by. Volrath's Curse's one sentence
    -- declares BOTH halves of "can't attack or block" under one name, which is
    -- what makes one payment cover both.
    name :: Maybe AbilityName.AbilityName
  }
  deriving (Eq, Ord, Show)
