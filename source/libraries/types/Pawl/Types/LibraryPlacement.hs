module Pawl.Types.LibraryPlacement where

import qualified Pawl.Types.LibraryPosition as LibraryPosition

-- | How a move settles CR 401.2's end, and who settles CR 401.4's order: the
-- card states the end, or the object's OWNER picks it. Griptide's "on top of its
-- owner's library" is 'Stated'; Aetherspouts' "its owner puts it on their choice
-- of the top or bottom of their library" is 'OwnerChooses'; Endurance's "on the
-- bottom of their library in a random order" is 'RandomOrder'.
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
  | -- | The end is stated and the ORDER is the effect's rather than the owner's.
    -- CR 401.4 lets the owner arrange two or more cards an effect puts in one
    -- position at once; text that states a random order takes that back, so
    -- nobody arranges and the batch is randomised instead, to CR 701.24a's
    -- standard of no player knowing the order.
    --
    -- NOT a shuffle (CR 701.24a randomises a whole library or pile): only the
    -- arriving cards are randomised, the library they join keeps its order, and
    -- nothing that triggers on a library being shuffled sees this.
    RandomOrder LibraryPosition.LibraryPosition
  deriving (Eq, Ord, Show)

-- | What a move that says nothing about an end uses, which is
-- 'LibraryPosition.defaultValue' stated -- see that value for the two specs that
-- observe it.
defaultValue :: LibraryPlacement
defaultValue = Stated LibraryPosition.defaultValue
