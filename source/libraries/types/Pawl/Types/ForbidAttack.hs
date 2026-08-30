module Pawl.Types.ForbidAttack where

import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.ObjectRef as ObjectRef

-- | The payload of Pawl.Types.Effect's ForbidAttack arm: CR 508.1c's
-- restriction, standing over the named permanents for this duration.
--
-- Pawl.Types.ForbidBlock's shape exactly and for its reason: rule 508.1c's
-- subject is an OBJECT and there is only one axis, so an ObjectRef rather than
-- Pawl.Types.RequireAttack's object-and-player pair.
--
-- The RESTRICTION side of the axis Pawl.Types.RequireAttack states the
-- requirement side of (CR 508.1c against CR 508.1d). Not a
-- Pawl.Types.CombatRestriction: that type is printed card text gathered live off
-- a SOURCE on the battlefield, where this outlives its source (CR 611.2a) and
-- names the permanents it covers once, at resolution.
--
-- Not implemented: the PAIRWISE shape, a restriction naming what the attack is
-- aimed at, "can't attack you or planeswalkers you control" (#1686).
data ForbidAttack = MkForbidAttack
  { duration :: Duration.Duration,
    ref :: ObjectRef.ObjectRef
  }
  deriving (Eq, Ord, Show)
