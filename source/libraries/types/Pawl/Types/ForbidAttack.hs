module Pawl.Types.ForbidAttack where

import qualified Pawl.Types.AimedAt as AimedAt
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.RestrictedCreatures as RestrictedCreatures

-- | The payload of Pawl.Types.Effect's ForbidAttack arm: CR 508.1c's
-- restriction, standing over the creatures 'affected' describes for this
-- duration, and -- when 'aimedAt' is stated -- barring only the announcements it
-- names (CR 802.3a).
--
-- Netter en-Dal's is @ForbidAttack UntilEndOfTurn (Named (InSlot target))
-- Nothing@; Chronomantic Escape's is @ForbidAttack UntilYourNextTurn (Matching
-- creature) (Just (AimedAt You {OfPlayer}))@.
--
-- Two axes where Pawl.Types.ForbidBlock has one, and Pawl.Types.RestrictedCreatures
-- where it has an ObjectRef: CR 611.2c makes "creatures can't attack you" a class
-- rather than a set, and a class cannot be enumerated once at resolution.
--
-- The RESTRICTION side of the axis Pawl.Types.RequireAttack states the
-- requirement side of (CR 508.1c against CR 508.1d). Not a
-- Pawl.Types.CombatRestriction: that type is printed card text gathered live off
-- a SOURCE on the battlefield, where this outlives its source (CR 611.2a).
data ForbidAttack = MkForbidAttack
  { duration :: Duration.Duration,
    affected :: RestrictedCreatures.RestrictedCreatures ObjectRef.ObjectRef,
    -- | Nothing is a restriction on attacking at all. Elided rather than written
    -- null.
    aimedAt :: Maybe AimedAt.AimedAt
  }
  deriving (Eq, Ord, Show)
