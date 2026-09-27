module Pawl.Types.CantBlockCreatures where

import qualified Pawl.Types.AbilityName as AbilityName
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.Condition as Condition
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword

-- | CR 509.1b's pairwise restriction written from the BLOCKER's side: which
-- blockers are restricted, which attackers they may not block, and CR 508.1c's
-- "unless" gate. Pawl.Types.CantBeBlockedBy's mirror.
data CantBlockCreatures = MkCantBlockCreatures
  { affected :: Affected.Affected,
    -- | Read in the BLOCKER's context, so a source-relative atom compares
    -- against the blocker (Spitfire Handler).
    attackers :: Filter.Filter Keyword.Keyword,
    -- | Nothing is the unconditional restriction. Elided rather than written
    -- null.
    unless :: Maybe Condition.Condition,
    -- | CR 116.2d: Pawl.Types.CantBeBlockedBy.name's field on this arm.
    name :: Maybe AbilityName.AbilityName
  }
  deriving (Eq, Ord, Show)
