module Pawl.Types.CantBeBlockedBy where

import qualified Pawl.Types.AbilityName as AbilityName
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.Condition as Condition
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword

-- | CR 509.1b's PAIRWISE restriction: which attackers are restricted, which
-- blockers are barred from them, and CR 508.1c's "unless" gate.

-- Its own record rather than [[AffectedUnless]] plus a field, because the two
-- creature-naming halves are not interchangeable: 'affected' is the set of
-- attackers restricted and 'blockers' describes what may not block them. That
-- asymmetry is also why the second key is spelled @blockers@ and not a second
-- @affected@.
data CantBeBlockedBy = MkCantBeBlockedBy
  { affected :: Affected.Affected,
    blockers :: Filter.Filter Keyword.Keyword,
    -- | Nothing is the unconditional restriction. Elided rather than written
    -- null.
    unless :: Maybe Condition.Condition,
    -- | CR 116.2d: the name this face gives the ability stating the restriction,
    -- so that face's own SpecialAction.IgnoreThisUntilEndOfTurn can say WHICH
    -- effect a payment ignores -- "the effect from that ability", singular.
    -- Pawl.Types.PlayerStaticAbility.name on the player axis, with the same
    -- meaning and joined to the grant by the same dataflow lint in
    -- Pawl.AbilitySlotLintSpec.
    --
    -- Nothing for every printing, this arm included: CR 116.2d's producers that
    -- aim at an object state CantAttack and CantBlock instead. The field is here
    -- because whether a restriction can be NAMED is independent of its shape,
    -- which is why the "unless" gate beside it is on every arm too.
    name :: Maybe AbilityName.AbilityName
  }
  deriving (Eq, Ord, Show)
