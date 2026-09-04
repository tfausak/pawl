module Pawl.Types.Transformed where

import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.ProjectedCharacteristics as ProjectedCharacteristics

-- | CR 701.27a: a permanent turned over -- which permanent, and what it turned
-- into.

-- The characteristics are the object's PROJECTED ones (CR 613's layer fold), sampled
-- the instant the turn finished, which is what CR 701.27e's "has the specified
-- characteristic immediately after it does so" asks for. The whole record and
-- not the names alone, because that rule's "specified characteristic" is any of
-- them: Cult of the Waxing Moon asks for a creature that is not a Human.
--
-- Carried rather than re-derived when a trigger is matched, for the reason
-- Pawl.Types.HalfUnlocked carries its flag: the CR 117.5 scan runs after the
-- board has moved on, and a permanent that turned twice in one resolution would
-- answer about the second turn on both events.
--
-- CHARACTERISTICS only, and only because that is what a
-- ProjectedCharacteristics holds: CR 603.10 pins the whole appearance of the
-- objects to immediately after the event, so the axes CR 109.3 excludes want
-- sampling too. Pawl.Engine.Event's TriggerCondition.PermanentTransforms arm
-- carries the elision that says they are read live instead.
--
-- Pawl.Types.Moved is the same posture over CR 608.2h's zone change, and both
-- samples reach a Filter through Pawl.Engine.Count.viewOfSnapshot -- directly
-- for a move, and through overlaySnapshot beside it for a turn.
data Transformed = MkTransformed
  { object :: ObjectId.ObjectId,
    characteristics :: ProjectedCharacteristics.ProjectedCharacteristics
  }
  deriving (Eq, Ord, Show)
