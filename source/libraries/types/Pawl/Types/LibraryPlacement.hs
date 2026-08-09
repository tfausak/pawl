module Pawl.Types.LibraryPlacement where

import qualified Pawl.Types.LibraryPosition as LibraryPosition

-- | How a move settles CR 401.2's end: the card states it, or the object's OWNER
-- picks. Griptide's "on top of its owner's library" is 'Stated'; Aetherspouts'
-- "its owner puts it on their choice of the top or bottom of their library" is
-- 'OwnerChooses'.
--
-- A SIBLING of LibraryPosition rather than a third inhabitant of it, because
-- Pawl.Engine.Game.insertIntoZone turns an end into an index and sits below the
-- prompt channel: a third inhabitant would make its two-way case answer a
-- question it cannot ask, forcing either a partial function or a silent
-- fallback. The choice is made in Pawl.Engine.Resolve BEFORE the CR 400.7 funnel
-- is entered, the posture the tap state and the stated position already take --
-- settled by the move rather than by a second write afterward.
--
-- The OWNER and not the resolving spell's controller, which is CR 401.2 read
-- through the card ("its owner puts it") and is the same decider CR 401.4 names
-- for the arrangement. A reader who knows CR 608.2f's secondary sentence would
-- otherwise hand this to the controller.
data LibraryPlacement
  = Stated LibraryPosition.LibraryPosition
  | OwnerChooses
  deriving (Eq, Ord, Show)

-- | What a move that says nothing about an end uses, which is
-- 'LibraryPosition.defaultValue' stated -- see that value for the two specs that
-- observe it.
defaultValue :: LibraryPlacement
defaultValue = Stated LibraryPosition.defaultValue
