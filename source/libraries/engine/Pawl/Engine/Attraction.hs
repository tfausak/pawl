-- | CR 717 and CR 701.51: a player's Attraction deck and opening an Attraction
-- from it. Rolling to visit (CR 701.52) is a die roll and lives with the CR 706
-- pipeline in Pawl.Engine.Resolve.Effect; Pawl.Engine.Engine runs it as CR
-- 703.4g's turn-based action.
module Pawl.Engine.Attraction where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Types.EntryRiders as EntryRiders
import Pawl.Types.Game (Game)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.LibraryPosition as LibraryPosition
import Pawl.Types.ObjectId (ObjectId)
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Zone as Zone

-- | CR 717.2: this player's Attraction deck, top card first.
deckOf :: PlayerId -> GameState.GameState -> [ObjectId]
deckOf pid gs = foldMap Foldable.toList (Map.lookup pid (GameState.attractionDecks gs))

-- | CR 717.4: does this player control an Attraction? Read off the projection,
-- the subtype being a characteristic.
controlsAttraction :: PlayerId -> GameState.GameState -> Bool
controlsAttraction pid gs = any (\oid -> Set.member Subtype.Attraction (Projection.subtypesOf oid gs)) (Projection.controls pid gs)

-- | CR 701.51b: move the top card of this player's Attraction deck onto the
-- battlefield under their control. Through Event.changeZoneEntering, so an
-- entry replacement or prohibition applies as to any other entry (CR 701.51c).
-- An empty deck opens nothing (CR 609.3).
open :: PlayerId -> Game ()
open pid = do
  gs <- State.get
  case deckOf pid gs of
    [] -> pure ()
    top : _ -> Monad.void (Event.changeZoneEntering top Zone.Battlefield LibraryPosition.defaultValue riders (Just pid))

-- CR 701.51b states no riders: the Attraction enters untapped, face up and
-- with no counters.
riders :: EntryRiders.EntryRiders Natural ability
riders =
  EntryRiders.MkEntryRiders
    { EntryRiders.tapped = TapState.Untapped,
      EntryRiders.attacking = Nothing,
      EntryRiders.blocking = Nothing,
      EntryRiders.transformed = False,
      EntryRiders.counters = Map.empty,
      EntryRiders.underOwner = False,
      EntryRiders.exiledFaceDown = False,
      EntryRiders.attachedTo = Nothing,
      EntryRiders.noted = False,
      EntryRiders.faceDown = Nothing
    }
