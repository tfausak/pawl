module Pawl.Types.Transformed where

import qualified Data.Set as Set
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId
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
-- The CONTROLLER and the ATTACHMENTS ride beside the characteristics rather than
-- being read live, and for the rule's reason rather than the record's: CR 109.3
-- keeps both out of an object's characteristics, so no ProjectedCharacteristics
-- can hold them, while CR 603.10 pins the whole appearance of the objects to
-- immediately after the event all the same. The board the CR 117.5 scan reads has
-- had state-based actions run over it, and CR 704.5n is the one that moves --
-- Neglected Heirloom's "when equipped creature transforms" is checked after the
-- Equipment has already fallen off a creature that turned into a Vehicle.
--
-- The attachments are the attachers' IDS and not their views: CR 603.10's
-- "objects involved in the event" is what was attached, and an attacher's own
-- appearance is rebuilt from the board at the scan. Pawl.Engine.Filter's View
-- holds them as a lazy list of recursive Views, which is not a thing an event can
-- carry.
--
-- Pawl.Types.Moved is the same posture over CR 608.2h's zone change, and both
-- samples reach a Filter through Pawl.Engine.Count.viewOfSnapshot -- directly
-- for a move, and through overlaySnapshot beside it for a turn.
data Transformed = MkTransformed
  { object :: ObjectId.ObjectId,
    characteristics :: ProjectedCharacteristics.ProjectedCharacteristics,
    controller :: Maybe PlayerId.PlayerId,
    attachments :: Set.Set ObjectId.ObjectId
  }
  deriving (Eq, Ord, Show)
