-- | CR 309 and CR 701.49: dungeon cards, the venture marker, and venturing into
-- the dungeon.
--
-- The sibling of Pawl.Engine.Ring and Pawl.Engine.Monarch: a rule-701 keyword
-- action whose whole text is in the rulebook, so casing on it here is casing on
-- the RULEBOOK rather than on an effect's identity. Pawl.Engine.Resolve's
-- Effect.Venture arm calls `venture` and asks nothing about which effect it came
-- from.
--
-- Where it DIVERGES from those two: a dungeon is a CARD (CR 309.1), not rulebook
-- text a module mints. Its rooms, their arrows and their effects are card data
-- (Pawl.Types.Face.rooms); what this module supplies is the one thing CR 309.4c
-- says is not printed -- every room ability's trigger condition.
module Pawl.Engine.Dungeon where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Pawl.Engine.Decide as Decide
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.DungeonRoom as DungeonRoom
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Facing as Facing
import Pawl.Types.Game (Game)
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.LibraryPosition as LibraryPosition
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.PendingTrigger as PendingTrigger
import qualified Pawl.Types.Player as Player
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.RoomIndex as RoomIndex
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.TriggerLimit as TriggerLimit
import qualified Pawl.Types.TriggerSource as TriggerSource
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.TypeLine as TypeLine
import qualified Pawl.Types.VentureMarkerEntered as VentureMarkerEntered
import qualified Pawl.Types.Zone as Zone

-- | CR 309.1 \/ 205.2a: is this a dungeon card? Read off the PRINTED type line
-- rather than the projection, because CR 309.2c keeps a dungeon in the command
-- zone and this reader takes the printed card (#1859) -- and because nothing
-- in the rules changes a dungeon's card type: CR 613 layer 4 reaches permanents,
-- and CR 309.2c makes a dungeon card never one.
isDungeonFace :: Face.Face Card.Card -> Bool
isDungeonFace face = Set.member CardType.Dungeon (TypeLine.types (Face.typeLine face))

-- | CR 309.3: the dungeon card this player owns in the command zone, if any. "A
-- player can own only one dungeon card in the command zone at a time", so the
-- first hit is the answer; `venture` is the only writer and honours the rule by
-- entering a dungeon only when this is Nothing.
--
-- Sliced by OWNER, which is CR 309.2a's own word -- a dungeon is put into the
-- command zone by the player who owns it, and nothing moves it afterwards.
inDungeon :: PlayerId -> GameState.GameState -> Maybe ObjectId
inDungeon pid gs =
  let dungeon oid = fmap isDungeonFace (Game.faceOf oid gs) == Just True
   in List.find dungeon (Game.zoneMembers Zone.Command pid gs)

-- | CR 309.4: the rooms printed on a dungeon card, topmost first. Empty for
-- anything that is not a dungeon, which is how every other command-zone resident
-- answers.
roomsOf :: ObjectId -> GameState.GameState -> Seq.Seq (DungeonRoom.DungeonRoom Card.Card)
roomsOf oid gs = maybe Seq.empty Face.rooms (Game.faceOf oid gs)

-- | CR 309.4: one room by index, or Nothing for an index the card has no room
-- for.
roomAt :: RoomIndex.RoomIndex -> Seq.Seq (DungeonRoom.DungeonRoom Card.Card) -> Maybe (DungeonRoom.DungeonRoom Card.Card)
roomAt room = Seq.lookup (Natural.toIntSaturating (RoomIndex.unwrap room))

-- | CR 309.6: is this the bottommost room of a card with this many rooms? The
-- LAST one, which is what "bottommost" names -- Pawl.Types.RoomIndex argues why
-- position rather than the absence of arrows is the handle, and Pawl.CardSpec's
-- lint holds the two together by requiring the last room to be the only exitless
-- one.
isBottommost :: Seq.Seq a -> RoomIndex.RoomIndex -> Bool
isBottommost rooms room = RoomIndex.unwrap room + 1 == Natural.length rooms

-- | CR 309.4c: the room ability of one room -- "When you move your venture marker
-- into this room, [effect]", where the condition is the rule's and the effect is
-- the card's.
--
-- intervening = Nothing because CR 309.4c states no "if" clause; the full text of
-- a room ability is the sentence above and nothing else.
roomAbility :: RoomIndex.RoomIndex -> DungeonRoom.DungeonRoom Card.Card -> TriggeredAbility.TriggeredAbility Card.Card
roomAbility room dungeonRoom =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition = TriggerCondition.RoomEntered room,
      TriggeredAbility.modal = DungeonRoom.ability dungeonRoom,
      TriggeredAbility.intervening = Nothing,
      TriggeredAbility.limit = TriggerLimit.Unlimited
    }

-- | CR 309.4c: the room abilities that fired on this batch of events, as ordinary
-- PendingTriggers borne by the dungeon card ("each room ability is controlled by
-- the player who owns the dungeon card that is that ability's source").
--
-- Gathered here rather than by Event.gatherTriggers for the reason
-- Monarch.inherentMonarchPending is: that scan asks each BATTLEFIELD permanent
-- what it triggers, plus the graveyards, the just-cast spell and -- under CR
-- 114.4 -- the EMBLEMS in the command zone, which a dungeon card is not (#1411).
-- A room ability is minted rather than printed besides, so there is nothing on
-- the card's face for that scan to read. Unlike the
-- monarch's, these abilities do have a source, so they carry
-- TriggerSource.OfObject and Engine.placeBorne puts them on the stack with no
-- special case -- which is what lets a room ability choose targets (CR 603.3d).
--
-- The bindings are empty and the reserved source slot is stamped at placement, so
-- a room's "target creature" resolves against the dungeon exactly as a permanent's
-- trigger resolves against itself.
roomPending :: [GameEvent.GameEvent] -> GameState.GameState -> [PendingTrigger.PendingTrigger]
roomPending events gs = Maybe.mapMaybe pendingFor events
  where
    pendingFor event = case event of
      GameEvent.VentureMarkerEntered (VentureMarkerEntered.MkVentureMarkerEntered pid oid room) -> do
        entered <- roomAt room (roomsOf oid gs)
        Monad.guard (fmap Object.owner (Game.lookupObject oid gs) == Just pid)
        Just (PendingTrigger.MkPendingTrigger (TriggerSource.OfObject oid) pid (roomAbility room entered) Map.empty)
      _ -> Nothing

-- | CR 704.5t \/ 309.6: the dungeon cards whose owner must remove them from the
-- game -- marker on the bottommost room, with no room ability of theirs still
-- pending or on the stack.
--
-- "Has triggered but not yet left the stack" is asked in BOTH halves, because a
-- triggered ability spends a window in neither place: Engine.performSettle runs
-- this pass before placePendingTriggers, so a room ability that has triggered on
-- an event this settle has not yet scanned is on no stack and in no batch. The
-- unscanned VentureMarkerEntered naming this dungeon is what stands for it, and
-- it is exactly the event the ability will be gathered from.
finished :: GameState.GameState -> [ObjectId]
finished gs = filter isFinished (Set.toList (GameState.command gs))
  where
    onStack oid = any (fromDungeon oid) (GameState.stack gs)
    fromDungeon oid stacked = case fmap Object.source (Game.lookupObject stacked gs) of
      Just (Source.OfTrigger srcId _) -> srcId == oid
      _ -> False
    pending oid = any (aboutDungeon oid) (Event.unscannedEvents gs)
    aboutDungeon oid event = case event of
      GameEvent.VentureMarkerEntered (VentureMarkerEntered.MkVentureMarkerEntered _ entered _) -> entered == oid
      _ -> False
    isFinished oid = case Game.lookupObject oid gs of
      Nothing -> False
      Just obj -> case Object.ventureRoom obj of
        Nothing -> False
        Just room -> isBottommost (roomsOf oid gs) room && not (onStack oid) && not (pending oid)

-- CR 701.49a: put a dungeon this player owns from outside the game into the
-- command zone with their venture marker on the topmost room.
--
-- The card is minted here rather than at setup, because CR 309.2 keeps dungeon
-- cards outside the game until a venture brings one in -- and outside the game is
-- not a zone (CR 400.11), so there was nowhere to mint it into. Player.dungeon is
-- the supply, and it is NOT consumed: CR 309.5b lets the same card be brought back
-- in after it is finished.
--
-- A player who owns no dungeon card enters none. CR 309.2a assumes they own one
-- and says nothing about a player who does not; a card that says to venture is
-- still resolved, and this is the "even if impossible" reading Ring.tempt takes.
enter :: PlayerId -> Game ()
enter pid = do
  gs <- State.get
  case Map.lookup pid (GameState.players gs) >>= Player.dungeon of
    Nothing -> pure ()
    Just printing -> do
      let (oid, gs1) = Game.freshObjectId gs
          (ts, gs2) = Game.freshTimestamp gs1
          obj =
            Object.MkObject
              { Object.owner = pid,
                Object.enteredUnder = Nothing,
                Object.source = Source.OfCard printing,
                Object.zone = Zone.Command,
                Object.tapped = TapState.Untapped,
                Object.facing = Facing.FaceUp,
                Object.exiledFaceDown = False,
                Object.damage = 0,
                Object.sickness = Sickness.Settled pid,
                Object.bindings = Map.empty,
                Object.counters = Map.empty,
                Object.counterTimestamps = Map.empty,
                Object.attachedTo = Nothing,
                Object.chosenColor = Nothing,
                Object.chosenSubtype = Nothing,
                Object.chosenNames = Set.empty,
                Object.chosenPlayer = Nothing,
                Object.timestamp = ts,
                Object.face = Nothing,
                Object.turnedOverAt = Nothing,
                Object.worldSince = Nothing,
                Object.playableFromExile = Nothing,
                Object.plotted = Nothing,
                Object.foretold = Nothing,
                Object.ringBearerFor = Nothing,
                Object.protector = Nothing,
                Object.classLevel = Nothing,
                -- CR 309.4a: "as a player puts a dungeon they own into the command
                -- zone, they put their venture marker on the topmost room".
                Object.ventureRoom = Just RoomIndex.topmost,
                Object.unlockedHalves = Set.empty,
                Object.designations = Set.empty,
                Object.kicked = False,
                Object.phyrexianLifePaid = 0,
                Object.announcedX = Nothing,
                Object.detainedUntil = Set.empty,
                Object.doesNotUntapNext = False,
                Object.exertedBy = Set.empty
              }
          gs3 =
            Game.insertIntoZone
              Zone.Command
              LibraryPosition.defaultValue
              pid
              oid
              gs2 {GameState.objects = Map.insert oid obj (GameState.objects gs2)}
      State.put (Event.recordEvent (GameEvent.VentureMarkerEntered (VentureMarkerEntered.MkVentureMarkerEntered pid oid RoomIndex.topmost)) gs3)

-- CR 309.5b \/ 701.49c: remove a finished dungeon card from the game. Also the
-- CR 704.5t state-based action's action, which is why it takes an id rather than a
-- player.
--
-- Removed from the GAME and not to a zone: CR 309.2c says a dungeon card can't
-- leave the command zone except as it leaves the game, and outside the game is not
-- a zone (CR 400.11) -- so the object ceases to be, the shape CR 704.5d's
-- vanishing token takes in Pawl.Engine.Sba.
remove :: ObjectId -> GameState.GameState -> GameState.GameState
remove oid gs = case Game.lookupObject oid gs of
  Nothing -> gs
  Just obj ->
    let stripped = Game.removeFromZones (Object.owner obj) oid gs
     in stripped {GameState.objects = Map.delete oid (GameState.objects stripped)}

-- CR 701.49b: move the marker along one arrow out of the room it is on.
--
-- The arrow is the player's choice where there are several (CR 309.5a) and no
-- choice at all where there is one. FILTERED, NOT TRUSTED, Ring.tempt's posture:
-- an answer naming a room no arrow leads to falls back to the first offered, since
-- the move is mandatory.
advance :: PlayerId -> ObjectId -> RoomIndex.RoomIndex -> Game ()
advance pid oid room = do
  gs <- State.get
  case roomAt room (roomsOf oid gs) >>= (nonEmptyExits . DungeonRoom.exits) of
    Nothing -> pure ()
    Just offered -> do
      chosen <- case offered of
        only NonEmpty.:| [] -> pure only
        first NonEmpty.:| _ -> do
          answer <- Game.choose (Prompt.ChooseRoom (Decide.deciderFor pid gs) pid oid offered)
          pure (if List.elem answer (NonEmpty.toList offered) then answer else first)
      State.modify' $ \g ->
        Event.recordEvent
          (GameEvent.VentureMarkerEntered (VentureMarkerEntered.MkVentureMarkerEntered pid oid chosen))
          g {GameState.objects = Map.adjust (\o -> o {Object.ventureRoom = Just chosen}) oid (GameState.objects g)}
  where
    -- Ascending, so both the single-arrow shortcut and a transcript are
    -- deterministic -- Ring.tempt's posture.
    nonEmptyExits = NonEmpty.nonEmpty . Set.toAscList

-- | CR 701.49: venture into the dungeon.
--
-- The rule's three cases in its own order: no dungeon in the command zone, so
-- enter one (CR 701.49a); marker anywhere but the bottommost room, so follow an
-- arrow (CR 701.49b); marker on the bottommost room, so finish this dungeon and
-- enter another (CR 701.49c).
--
-- The third case is REACHABLE but rare, and that is CR 704.5t's doing rather than
-- this module's: the state-based action removes a dungeon whose marker sits on the
-- bottommost room as soon as its last room ability has left the stack, so a player
-- is normally out of the dungeon before they next have priority. It is reached
-- when two ventures happen inside one resolution.
--
-- CR 309.7's "a player completes a dungeon as that dungeon card is removed from
-- the game" is not recorded anywhere, so no card can ask whether a dungeon has
-- been completed (#1336).
venture :: PlayerId -> Game ()
venture pid = do
  gs <- State.get
  case inDungeon pid gs of
    Nothing -> enter pid
    Just oid -> case Object.ventureRoom =<< Game.lookupObject oid gs of
      -- A dungeon in the command zone with no marker on it is a state no rule
      -- describes and nothing here writes: `enter` places the marker as the card
      -- arrives. Treated as CR 701.49a's case rather than silently doing nothing,
      -- so the marker is repaired rather than left absent.
      Nothing -> enter pid
      Just room
        | isBottommost (roomsOf oid gs) room -> do
            State.modify' (remove oid)
            enter pid
        | otherwise -> advance pid oid room
