module Pawl.Types.MoveToZone where

import qualified Pawl.Types.EntryRiders as EntryRiders
import qualified Pawl.Types.LibraryPlacement as LibraryPlacement
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Zone as Zone

-- | The payload of Pawl.Types.Effect's MoveToZone arm (#1305).
--
-- Four of the six fields are optional in the sense that a card stating nothing
-- about them writes nothing: the riders when they are CR 110.5b's default, the
-- bound slot when nothing looks back at the move, the origin zone when the
-- effect names none (CR 113.6m), and the placement when it is the default end
-- (CR 401.2). On the wire those are absent KEYS. Before #1305 they were a
-- variable-length array tail, and which element was which had to be recovered
-- from each one's JSON type.
--
-- The origin zone is what CR 113.6m's "out of a particular zone" reads, and it
-- is a CLASSIFICATION of the effect (Pawl.Engine.EffectZone) rather than
-- something the resolver consults.
--
-- The LibraryPlacement is the END a LIBRARY destination arrives at (CR 401.2's
-- order), either stated -- Griptide's "on top of its owner's library", against
-- Unsummon's silence -- or left to each moved object's OWNER, which is
-- Aetherspouts. Its third reading states the end and takes CR 401.4's
-- arrangement away from the owner, making the batch random: Endurance's "on the
-- bottom of their library in a random order". Inert for every other destination,
-- so a card that states one on a non-library move states something nothing
-- reads; a CardSpec lint additionally forbids OwnerChooses there, since that one
-- would ask a player a question with no board behind it.
data MoveToZone = MkMoveToZone
  { ref :: ObjectRef.ObjectRef,
    zone :: Zone.Zone,
    riders :: EntryRiders.EntryRiders,
    slot :: Maybe SlotName.SlotName,
    origin :: Maybe Zone.Zone,
    placement :: LibraryPlacement.LibraryPlacement
  }
  deriving (Eq, Ord, Show)
