-- | CR 701.62, manifest dread: "Look at the top two cards of your library.
-- Manifest one of them, then put the cards you looked at that were not
-- manifested this way into your graveyard", and the whole of the keyword
-- action.
--
-- Pawl.Engine.Cloak's sibling, on the same ground: rule 701 is a keyword-action
-- rule, so the procedure lives in the engine, and Pawl.Engine.Resolve.Effect's
-- Effect.ManifestDread arm calls in without saying which effect it is.
--
-- An opcode rather than three instructions because CR 701.62b's trigger watches
-- the PROCESS: it fires once, after the whole of it, even when some or all of it
-- was impossible -- which no reader of the moves inside it can see -- and
-- Paranormal Analyst's "a card you put into your graveyard this way" reads that
-- same process back.
module Pawl.Engine.ManifestDread where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Decide as Decide
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Types.EntryRiders as EntryRiders
import qualified Pawl.Types.FaceDownCharacteristics as FaceDownCharacteristics
import qualified Pawl.Types.FaceDownReason as FaceDownReason
import qualified Pawl.Types.FaceDownState as FaceDownState
import Pawl.Types.Game (Game)
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.LibraryPosition as LibraryPosition
import qualified Pawl.Types.ManifestedDread as ManifestedDread
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Zone as Zone

-- | CR 701.40a's face-down 2\/2, the listing being the rule's and never a
-- card's. A settled count rather than a Quantity, Pawl.Engine.Cloak.riders'
-- reason: the rule states no counters.
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
      EntryRiders.characteristics = Seq.empty,
      EntryRiders.faceDown =
        Just
          FaceDownState.MkFaceDownState
            { FaceDownState.reason = FaceDownReason.Manifested,
              FaceDownState.listed = FaceDownCharacteristics.defaultValue
            }
    }

-- | CR 701.62a performed by this player on their own library, on behalf of the
-- object `source`.
--
-- Which card is manifested is the player's choice (CR 608.2d), asked as
-- Prompt.ChooseCardFromAmong and filtered rather than trusted; one card is no
-- choice (the Paranormal Analyst ruling: a one-card library manifests that
-- card) and none is nothing to do.
--
-- "Not manifested this way" is read off the board after the manifest: a looked-at
-- card still in the library is one the manifest did not take, which is CR
-- 701.40f's refusal (Grafdigger's Cage) as well as the card that was never
-- chosen. A card a CR 614 replacement sent elsewhere has left the library and is
-- put nowhere.
--
-- The event is recorded unconditionally, CR 701.62b, carrying the cards the
-- graveyard move left IN a graveyard -- a replacement that sends one elsewhere
-- (Leyline of the Void) leaves it out, since it was not put there.
manifestDread :: ObjectId -> PlayerId -> Game ()
manifestDread source pid = do
  gs <- State.get
  let looked = take 2 (Game.zoneMembers Zone.Library pid gs)
  chosen <- case looked of
    [] -> pure Nothing
    [only] -> pure (Just only)
    first : second : more -> do
      let offered = first NonEmpty.:| (second : more)
      answer <- Game.choose (Prompt.ChooseCardFromAmong (Decide.deciderFor pid gs) pid source offered)
      pure (Just (if List.elem answer (NonEmpty.toList offered) then answer else first))
  Monad.forM_ chosen $ \card ->
    Event.simultaneously (Monad.void (Event.changeZoneEntering card Zone.Battlefield LibraryPosition.defaultValue riders (Just pid)))
  after <- State.get
  let stillInLibrary oid = fmap Object.zone (Game.lookupObject oid after) == Just Zone.Library
      rest = filter stillInLibrary looked
  arrived <- if null rest then pure [] else Event.changeZonesTogether (fmap (\oid -> (oid, Zone.Graveyard)) rest)
  landed <- State.get
  let inGraveyard oid = fmap Object.zone (Game.lookupObject oid landed) == Just Zone.Graveyard
      binned = Seq.fromList (concatMap (filter inGraveyard . Foldable.toList) arrived)
  State.modify' (Event.recordEvent (GameEvent.ManifestedDread (ManifestedDread.MkManifestedDread pid binned)))
