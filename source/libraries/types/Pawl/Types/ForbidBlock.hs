module Pawl.Types.ForbidBlock where

import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.ObjectRef as ObjectRef

-- | The payload of Pawl.Types.Effect's ForbidBlock arm (#2588): CR 509.1b's
-- restriction, standing over the named permanents for this duration.
--
-- Zirda, the Dawnwaker's is @ForbidBlock UntilEndOfTurn (InSlot target)@.
--
-- Pawl.Types.CantBeRegenerated's shape exactly, and for its reason: rule 509.1b's
-- subject is an OBJECT and there is only one axis, so an ObjectRef rather than
-- Pawl.Types.RequireBlock's pair or Pawl.Types.AffectPlayers' scope.
--
-- The RESTRICTION side of the axis Pawl.Types.RequireBlock states the
-- requirement side of (CR 509.1b against CR 509.1c). Not a
-- Pawl.Types.CombatRestriction: that type is printed card text gathered live off
-- a SOURCE on the battlefield, where this outlives its source (CR 611.2a) and
-- names the permanents it covers once, at resolution.
--
-- Only the "can't block" arm: Pawl.Types.ForbidAttack states CR 508.1c's side of
-- the same shape.
data ForbidBlock = MkForbidBlock
  { duration :: Duration.Duration,
    ref :: ObjectRef.ObjectRef
  }
  deriving (Eq, Ord, Show)
