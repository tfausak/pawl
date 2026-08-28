module Pawl.Types.AbilityKind where

-- | CR 605.1a's classification of one ACTIVATED ability, as a cost adjustment
-- has to ask it: Suppression Field's "Activated abilities cost {2} more to
-- activate unless they're mana abilities".
--
-- The rulebook's own division and not an effect: what the closed half compares
-- here is which side of CR 605.1a the ability being activated falls on
-- (Pawl.Engine.ManaAbility.isManaAbility), never what that ability does. The
-- same posture Pawl.Types.KeywordFamily takes for rule 702.
--
-- A TYPE and not a Bool, and read at two different scales, which is why one type
-- serves both: Pawl.Engine.PlayerEffect.activationCostAdjustmentsGiven is told
-- which kind the ability being adjusted IS, and
-- Pawl.Types.IncreaseActivationCost.whichKind says which kind an adjustment
-- APPLIES to.
data AbilityKind
  = -- | CR 605.1a's four criteria all met.
    ManaAbility
  | -- | Every other activated ability, which is every one CR 602.2a puts on the
    -- stack -- CR 605.3b being the exception the constructor above names.
    NonManaAbility
  deriving (Bounded, Enum, Eq, Ord, Show)
