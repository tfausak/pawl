module Pawl.Types.MoveToZone where

import qualified Pawl.Types.EntryRiders as EntryRiders
import qualified Pawl.Types.LibraryPlacement as LibraryPlacement
import qualified Pawl.Types.MoveDuration as MoveDuration
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Zone as Zone

-- | The payload of Pawl.Types.Effect's MoveToZone arm (#1305).
--
-- Five of the seven fields are optional in the sense that a card stating nothing
-- about them writes nothing: the riders when they are CR 110.5b's default, the
-- bound slot when nothing looks back at the move, the origin zone when the
-- effect names none (CR 113.6m), the placement when it is the default end (CR
-- 401.2), and the duration when the move is CR 610.1's plain one, which is every
-- move but the handful rule 610.3 covers. On the wire those are absent KEYS. Before #1305 they were a
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
--
-- The MoveDuration is CR 610.3's "until", which makes the move one half of a
-- pair: the resolver declines it outright when the event has already happened
-- (CR 610.3a, CR 610.3b) and registers what did move in
-- GameState.movedUntilSourceLeaves, whose sweep performs rule 610.3's second
-- one-shot effect. Nothing on a card returns the object, and nothing may:
-- writing the return as a second triggered ability puts it on the stack, where
-- rule 610.3 gives nobody a window to respond -- see #2626. The event it names
-- is the SOURCE leaving the battlefield, so the arm belongs on a permanent's
-- ability; a card stating it on an instant's or sorcery's effect states an event
-- that has already happened when the spell resolves (CR 608.1 puts the source on
-- the stack), and every such move is declined. Nothing lints that, as nothing
-- lints the placement one paragraph up.
data MoveToZone = MkMoveToZone
  { ref :: ObjectRef.ObjectRef,
    zone :: Zone.Zone,
    riders :: EntryRiders.EntryRiders Quantity.Quantity,
    slot :: Maybe SlotName.SlotName,
    origin :: Maybe Zone.Zone,
    placement :: LibraryPlacement.LibraryPlacement,
    duration :: Maybe MoveDuration.MoveDuration
  }
  deriving (Eq, Ord, Show)
