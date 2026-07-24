module Pawl.Setup where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Combat as Combat
import qualified Pawl.Event as Event
import qualified Pawl.Game as Game
import qualified Pawl.Turn as Turn
import qualified Pawl.Type.Deck as Deck
import Pawl.Type.Game (Game)
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Object as Object
import Pawl.Type.ObjectId (ObjectId)
import qualified Pawl.Type.ObjectId as ObjectId
import qualified Pawl.Type.Player as Player
import Pawl.Type.PlayerId (PlayerId)
import Pawl.Type.Printing (Printing)
import qualified Pawl.Type.Program as Program
import qualified Pawl.Type.Prompt as Prompt
import qualified Pawl.Type.RestartSignal as RestartSignal
import qualified Pawl.Type.Sickness as Sickness
import qualified Pawl.Type.Source as Source
import qualified Pawl.Type.Status as Status
import qualified Pawl.Type.TapState as TapState
import qualified Pawl.Type.Timestamp as Timestamp
import qualified Pawl.Type.Zone as Zone

startingLife :: Integer
startingLife = 20

deckSize :: Deck.Deck -> Natural
deckSize (Deck.MkDeck m) = sum (Map.elems m)

-- Pair every player with one deck, for a symmetric (mirror) matchup.
mirror :: Deck.Deck -> NonEmpty.NonEmpty PlayerId -> NonEmpty.NonEmpty (PlayerId, Deck.Deck)
mirror deck = NonEmpty.map (\pid -> (pid, deck))

openingHand :: Int
openingHand = 7

-- Takes a NonEmpty so the active player is total (no partial head).
emptyGame :: NonEmpty.NonEmpty PlayerId -> GameState
emptyGame order =
  let order_ = NonEmpty.toList order
      newPlayer pid =
        ( pid,
          Player.MkPlayer
            { Player.life = startingLife,
              Player.status = Status.Playing,
              Player.counters = Map.empty
            }
        )
   in GameState.MkGameState
        { GameState.objects = Map.empty,
          GameState.library = Map.empty,
          GameState.hand = Map.empty,
          GameState.graveyard = Map.empty,
          GameState.battlefield = mempty,
          GameState.exile = mempty,
          GameState.command = mempty,
          GameState.stack = [],
          GameState.players = Map.fromList (map newPlayer order_),
          GameState.manaPool = Map.empty,
          GameState.combat = Combat.emptyCombat,
          GameState.events = Seq.empty,
          GameState.scannedThrough = 0,
          GameState.damageScannedThrough = 0,
          GameState.delayedTriggers = Seq.empty,
          GameState.continuousEffects = [],
          GameState.replacements = [],
          GameState.playerEffects = [],
          GameState.turnOrder = order_,
          GameState.activePlayer = NonEmpty.head order,
          GameState.phase = Turn.firstPhase,
          GameState.remaining = Turn.laterPhases,
          GameState.priority = Nothing,
          GameState.passes = 0,
          GameState.turnNumber = 1,
          GameState.result = Nothing,
          GameState.restartSignal = RestartSignal.Playing,
          GameState.nextObjectId = ObjectId.MkObjectId 0,
          GameState.nextTimestamp = Timestamp.MkTimestamp 0,
          GameState.drewFromEmpty = mempty,
          GameState.landPlayed = mempty,
          GameState.pendingControl = Map.empty,
          GameState.activeControl = Nothing,
          GameState.monarch = Nothing,
          GameState.exiledUntilMonarch = Map.empty
        }

createCard :: PlayerId -> Printing -> Game ObjectId
createCard pid printing = do
  gs <- State.get
  let (oid, gs1) = Game.freshObjectId gs
      (ts, gs2) = Game.freshTimestamp gs1
      obj =
        Object.MkObject
          { Object.owner = pid,
            Object.source = Source.OfCard printing,
            Object.zone = Zone.Library,
            Object.tapped = TapState.Untapped,
            Object.damage = 0,
            Object.sickness = Sickness.Sick,
            Object.bindings = Map.empty,
            Object.counters = Map.empty,
            Object.timestamp = ts
          }
      gs3 =
        gs2
          { GameState.objects = Map.insert oid obj (GameState.objects gs2),
            GameState.library = Map.insertWith (flip (Seq.><)) pid (Seq.singleton oid) (GameState.library gs2)
          }
  State.put gs3
  pure oid

shuffleLibrary :: PlayerId -> Game ()
shuffleLibrary pid = do
  gs <- State.get
  let ids = Game.zoneMembers Zone.Library pid gs
  shuffled <- Trans.lift (Program.prompt (Prompt.Shuffle ids))
  State.put gs {GameState.library = Map.insert pid (Seq.fromList shuffled) (GameState.library gs)}

-- Build each player's library from their deck's multiset, shuffle, draw.
createDeck :: PlayerId -> Deck.Deck -> Game ()
createDeck pid (Deck.MkDeck m) =
  Monad.forM_ (Map.toList m) $ \(printing, n) ->
    Monad.replicateM_ (fromIntegral n) (createCard pid printing)

newGame :: NonEmpty.NonEmpty (PlayerId, Deck.Deck) -> Game ()
newGame matchup = Monad.forM_ (NonEmpty.toList matchup) $ \(pid, deck) -> do
  createDeck pid deck
  shuffleLibrary pid
  -- CR 103.4 mulligans are not implemented; the opening hand is always exactly
  -- `openingHand` cards, unconditionally (#141).
  Monad.replicateM_ openingHand (Event.drawCard pid)

-- CR 727.2 / 729.2: build every player's library from an EXISTING object pool --
-- each player's owned CARDS, wherever they currently sit -- then shuffle and draw
-- opening hands (CR 103.5). This is deliberately NOT newGame: it reuses the real
-- objects (ownership preserved, CR 727.2) instead of minting fresh ones from Deck
-- definitions. Only Magic cards survive: an ability on the stack, a token, an
-- emblem, or a trigger is not a card (CR 727.2 / 111.7) and ceases to exist.
-- Shared by restart (CR 727) and, later, subgames (CR 729).
startGameFromCards :: Game ()
startGameFromCards = do
  gs <- State.get
  let owners = GameState.turnOrder gs
      isCard obj = case Object.source obj of
        Source.OfCard _ -> True
        _ -> False
      toLibraryCard obj =
        obj
          { Object.zone = Zone.Library,
            Object.tapped = TapState.Untapped,
            Object.damage = 0,
            Object.sickness = Sickness.Sick,
            Object.bindings = Map.empty,
            Object.counters = Map.empty
          }
      cards = Map.map toLibraryCard (Map.filter isCard (GameState.objects gs))
      libraryOf pid = Seq.fromList (Map.keys (Map.filter (\obj -> Object.owner obj == pid) cards))
  State.put
    gs
      { GameState.objects = cards,
        GameState.library = Map.fromList (map (\pid -> (pid, libraryOf pid)) owners),
        GameState.hand = Map.empty,
        GameState.graveyard = Map.empty,
        GameState.battlefield = mempty,
        GameState.exile = mempty,
        GameState.command = mempty,
        GameState.stack = []
      }
  Monad.forM_ owners $ \pid -> do
    shuffleLibrary pid
    -- CR 103.4 mulligans are not implemented here either (#141).
    Monad.replicateM_ openingHand (Event.drawCard pid)

-- CR 103 / 727.1a: put `starter` at the head of the turn order, preserving the
-- cyclic order ("the game's default turn order begins with the starting player
-- and proceeds clockwise"). Total: a `starter` not in the order leaves it as-is.
rotateTo :: PlayerId -> [PlayerId] -> [PlayerId]
rotateTo starter order = case break (== starter) order of
  (before, after) -> after ++ before

-- CR 727: restart the game in place. CR 727.1: the current game immediately ends
-- and a new game begins per CR 103, with the CR 727.1a exception -- the starting
-- player is `starter` (the controller of the restarting ability), so the turn
-- order is rotated to begin with them. CR 727.2: every card returns to its
-- owner's new library via startGameFromCards, built from the ACTUAL object pool
-- (never emptyGame+newGame, which would lose the real cards and pick the wrong
-- starting player). CR 727.4: the effect finishes resolving just before the first
-- turn's untap step, with no player holding priority -- phase = firstPhase,
-- priority = Nothing, turn 1. The object and timestamp id supplies are preserved
-- so reused cards keep unique ids; startGameFromCards rebuilds objects and zones.
restartGame :: PlayerId -> Game ()
restartGame starter = do
  State.modify' $ \gs ->
    let resetPlayer player =
          player
            { Player.life = startingLife,
              Player.status = Status.Playing,
              Player.counters = Map.empty
            }
     in gs
          { GameState.players = Map.map resetPlayer (GameState.players gs),
            GameState.manaPool = Map.empty,
            GameState.combat = Combat.emptyCombat,
            GameState.events = Seq.empty,
            GameState.scannedThrough = 0,
            GameState.damageScannedThrough = 0,
            GameState.delayedTriggers = Seq.empty,
            GameState.continuousEffects = [],
            GameState.replacements = [],
            GameState.playerEffects = [],
            GameState.turnOrder = rotateTo starter (GameState.turnOrder gs),
            GameState.activePlayer = starter,
            GameState.phase = Turn.firstPhase,
            GameState.remaining = Turn.laterPhases,
            GameState.priority = Nothing,
            GameState.passes = 0,
            GameState.turnNumber = 1,
            GameState.result = Nothing,
            -- CR 727.4: the game the caller was running has been replaced.
            -- Engine.priorityLoop and Engine.runStep read this and unwind to the
            -- rebuilt turn 1 rather than granting priority or advancing past it.
            GameState.restartSignal = RestartSignal.Restarted,
            GameState.drewFromEmpty = mempty,
            GameState.landPlayed = mempty,
            GameState.pendingControl = Map.empty,
            GameState.activeControl = Nothing,
            GameState.monarch = Nothing,
            GameState.exiledUntilMonarch = Map.empty
          }
  startGameFromCards

-- CR 729.2: build a fresh subgame state from the parent's LIBRARY cards ONLY --
-- each player takes all the cards in their main-game library into the subgame
-- library; no other main-game zone enters (rule 729.2). The object pool is
-- restricted to those library objects; startGameFromCards (called by playSubgame)
-- then rebuilds each subgame library from that pool, shuffles, and draws opening
-- hands (CR 103). Players reset to a new game (CR 103); every transient field is
-- cleared, exactly as restartGame does, EXCEPT the object/timestamp id supplies,
-- which are INHERITED from the parent so every object the subgame mints (CR 400.7)
-- gets an id above every parent id -- funnelBack relies on that for non-collision.
-- CR 729.2's "randomly determine which player goes first" is elided to the
-- head of the turn order (pawl has no first-player randomness prompt) (#136).
subgameStateFrom :: GameState -> GameState
subgameStateFrom parent =
  let libIds =
        Set.fromList
          (concatMap (\pid -> Foldable.toList (Map.findWithDefault Seq.empty pid (GameState.library parent))) (GameState.turnOrder parent))
      libObjects = Map.restrictKeys (GameState.objects parent) libIds
      resetPlayer player =
        player
          { Player.life = startingLife,
            Player.status = Status.Playing,
            Player.counters = Map.empty
          }
      firstPlayer = Maybe.fromMaybe (GameState.activePlayer parent) (Maybe.listToMaybe (GameState.turnOrder parent))
   in parent
        { GameState.objects = libObjects,
          GameState.players = Map.map resetPlayer (GameState.players parent),
          GameState.library = Map.empty,
          GameState.hand = Map.empty,
          GameState.graveyard = Map.empty,
          GameState.battlefield = mempty,
          GameState.exile = mempty,
          GameState.command = mempty,
          GameState.stack = [],
          GameState.manaPool = Map.empty,
          GameState.combat = Combat.emptyCombat,
          GameState.events = Seq.empty,
          GameState.scannedThrough = 0,
          GameState.damageScannedThrough = 0,
          GameState.delayedTriggers = Seq.empty,
          GameState.continuousEffects = [],
          GameState.replacements = [],
          GameState.playerEffects = [],
          GameState.activePlayer = firstPlayer,
          GameState.phase = Turn.firstPhase,
          GameState.remaining = Turn.laterPhases,
          GameState.priority = Nothing,
          GameState.passes = 0,
          GameState.turnNumber = 1,
          GameState.result = Nothing,
          GameState.restartSignal = RestartSignal.Playing,
          GameState.drewFromEmpty = mempty,
          GameState.landPlayed = mempty,
          GameState.pendingControl = Map.empty,
          GameState.activeControl = Nothing,
          GameState.monarch = Nothing,
          GameState.exiledUntilMonarch = Map.empty
        }

-- CR 729.5: at the end of a subgame, each player takes all traditional cards
-- (Source.OfCard) they own ANYWHERE in the subgame into their main-game library
-- and reshuffles (the reshuffle is playSubgame's Prompt.Shuffle step). All other
-- subgame objects and the subgame's zones cease to exist -- they are simply not
-- carried over. The parent's non-library objects (hand, battlefield, graveyard,
-- ...) are untouched: the main game continues from where it was discontinued. The
-- old parent library objects are dropped (they moved into the subgame). Returned
-- cards keep their subgame ids, which are all above the parent supply
-- (subgameStateFrom inherited it), so Map.union cannot collide; the id/timestamp
-- supplies advance to the subgame high-water mark.
funnelBack :: GameState -> GameState -> GameState
funnelBack finalSub parent =
  let isCard obj = case Object.source obj of
        Source.OfCard _ -> True
        _ -> False
      toLibraryCard obj =
        obj
          { Object.zone = Zone.Library,
            Object.tapped = TapState.Untapped,
            Object.damage = 0,
            Object.sickness = Sickness.Sick,
            Object.bindings = Map.empty,
            Object.counters = Map.empty
          }
      returned = Map.map toLibraryCard (Map.filter isCard (GameState.objects finalSub))
      libraryOf pid = Seq.fromList (Map.keys (Map.filter (\obj -> Object.owner obj == pid) returned))
      oldLibIds =
        Set.fromList
          (concatMap (\pid -> Foldable.toList (Map.findWithDefault Seq.empty pid (GameState.library parent))) (GameState.turnOrder parent))
      keptParentObjects = Map.withoutKeys (GameState.objects parent) oldLibIds
   in parent
        { GameState.objects = Map.union returned keptParentObjects,
          GameState.library = Map.fromList (map (\pid -> (pid, libraryOf pid)) (GameState.turnOrder parent)),
          GameState.nextObjectId = max (GameState.nextObjectId parent) (GameState.nextObjectId finalSub),
          GameState.nextTimestamp = max (GameState.nextTimestamp parent) (GameState.nextTimestamp finalSub)
        }
