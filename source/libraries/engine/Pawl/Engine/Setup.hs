module Pawl.Engine.Setup where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Mulligan as Mulligan
import qualified Pawl.Engine.Turn as Turn
import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.Types.Deck as Deck
import Pawl.Types.Game (Game)
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import Pawl.Types.HandActionPerformer (HandActionPerformer)
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Player as Player
import Pawl.Types.PlayerId (PlayerId)
import Pawl.Types.Printing (Printing)
import qualified Pawl.Types.RestartSignal as RestartSignal
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.Status as Status
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Timestamp as Timestamp
import qualified Pawl.Types.Zone as Zone

startingLife :: Integer
startingLife = 20

deckSize :: Deck.Deck -> Natural
deckSize (Deck.MkDeck m) = sum (Map.elems m)

-- Pair every player with one deck, for a symmetric (mirror) matchup.
mirror :: Deck.Deck -> NonEmpty.NonEmpty PlayerId -> NonEmpty.NonEmpty (PlayerId, Deck.Deck)
mirror deck = fmap (\pid -> (pid, deck))

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
          GameState.players = Map.fromList (fmap newPlayer order_),
          GameState.manaPool = Map.empty,
          GameState.combat = Combat.emptyCombat,
          GameState.events = Seq.empty,
          GameState.lastKnown = Map.empty,
          GameState.scannedThrough = 0,
          GameState.controlWhenTriggered = Map.empty,
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
          GameState.exiledUntilMonarch = Map.empty,
          GameState.extraTurns = [],
          GameState.turnAnchor = Nothing
        }

createCard :: PlayerId -> Printing -> Game ObjectId
createCard pid printing = do
  gs <- State.get
  let (oid, gs1) = Game.freshObjectId gs
      (ts, gs2) = Game.freshTimestamp gs1
      obj =
        Object.MkObject
          { Object.owner = pid,
            Object.enteredUnder = Nothing,
            Object.source = Source.OfCard printing,
            Object.zone = Zone.Library,
            Object.tapped = TapState.Untapped,
            Object.damage = 0,
            Object.sickness = Sickness.Sick,
            Object.bindings = Map.empty,
            Object.counters = Map.empty,
            Object.attachedTo = Nothing,
            Object.chosenColor = Nothing,
            Object.timestamp = ts
          }
      gs3 =
        gs2
          { GameState.objects = Map.insert oid obj (GameState.objects gs2),
            GameState.library = Map.insertWith (flip (Seq.><)) pid (Seq.singleton oid) (GameState.library gs2)
          }
  State.put gs3
  pure oid

-- Build each player's library from their deck's multiset, shuffle, draw.
createDeck :: PlayerId -> Deck.Deck -> Game ()
createDeck pid (Deck.MkDeck m) =
  Monad.forM_ (Map.toList m) $ \(printing, n) ->
    Monad.replicateM_ (Natural.toIntSaturating n) (createCard pid printing)

newGame :: HandActionPerformer -> NonEmpty.NonEmpty (PlayerId, Deck.Deck) -> Game ()
newGame perform matchup = do
  -- CR 103.3: build and shuffle every library BEFORE any opening hand is drawn,
  -- so CR 103.5's declaration round (Mulligan.openingHands) sees settled libraries.
  Monad.forM_ (NonEmpty.toList matchup) $ \(pid, deck) -> do
    createDeck pid deck
    Mulligan.shuffleLibrary pid
  Mulligan.openingHands perform (fmap fst (NonEmpty.toList matchup))

-- CR 727.2 / 729.2: build every player's library from an EXISTING object pool --
-- each player's owned CARDS, wherever they currently sit -- then shuffle and draw
-- opening hands (CR 103.5). This is deliberately NOT newGame: it reuses the real
-- objects (ownership preserved, CR 727.2) instead of minting fresh ones from Deck
-- definitions. Only Magic cards survive: an ability on the stack, a token, an
-- emblem, or a trigger is not a card (CR 727.2 / 111.7) and ceases to exist.
-- Shared by restart (CR 727) and, later, subgames (CR 729).
--
-- CR 727.1 / CR 729.2 rebuild the game for the players who are IN it, so a player
-- who has left gets no library, no shuffle and no opening hand (fixed by #147); `owners` is
-- the still-playing seats, in seating order, because CR 103.5's declaration round
-- goes around the table in turn order. Their cards are not here to skip: CR 800.4a
-- takes every object a departing player owns out of the game with them
-- (Departure.objectsLeaveWith), so a rebuild has nothing of theirs left in the
-- object pool to orphan.
startGameFromCards :: HandActionPerformer -> Game ()
startGameFromCards perform = do
  gs <- State.get
  let owners = Game.stillPlayingInOrder gs
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
      cards = fmap toLibraryCard (Map.filter isCard (GameState.objects gs))
      libraryOf pid = Seq.fromList (Map.keys (Map.filter (\obj -> Object.owner obj == pid) cards))
  State.put
    gs
      { GameState.objects = cards,
        GameState.library = Map.fromList (fmap (\pid -> (pid, libraryOf pid)) owners),
        GameState.hand = Map.empty,
        GameState.graveyard = Map.empty,
        GameState.battlefield = mempty,
        GameState.exile = mempty,
        GameState.command = mempty,
        GameState.stack = []
      }
  Monad.forM_ owners Mulligan.shuffleLibrary
  Mulligan.openingHands perform owners

-- CR 103 / 727.1a: put `starter` at the head of the turn order, preserving the
-- cyclic order ("the game's default turn order begins with the starting player
-- and proceeds clockwise"). Total: a `starter` not in the order leaves it as-is.
rotateTo :: PlayerId -> [PlayerId] -> [PlayerId]
rotateTo starter order = case break (== starter) order of
  (before, after) -> after <> before

-- CR 103.4 / CR 727.1 / CR 729.2: put every player back to a new game's starting
-- state -- the starting life total, no counters -- for the two paths that rebuild
-- a game in place (restart and subgames). One helper, because the two rules want
-- the same filter for different reasons.
--
-- A player who has already LEFT is not reset. CR 727.1: "All players in that game
-- when it ended then start a new game following the procedures set forth in rule
-- 103" -- a player who left before it ended is not one of them. CR 729.4: "All
-- players not currently in the subgame are considered outside the subgame." So
-- their Status.Departed survives the rebuild, and nothing else about them is
-- touched either: they are not starting a game (fixed by #147).
--
-- They keep their entry in the map rather than being deleted, so every Map.lookup
-- on a PlayerId that still names them stays total. Which players are IN the
-- rebuilt game is the rebuilt GameState.turnOrder.
resetPlayers :: Map.Map PlayerId Player.Player -> Map.Map PlayerId Player.Player
resetPlayers players =
  let reset player = case Player.status player of
        -- Already Playing, so the status is not rewritten -- only the fields a
        -- new game resets.
        Status.Playing ->
          player
            { Player.life = startingLife,
              Player.counters = Map.empty
            }
        Status.Departed _ -> player
   in fmap reset players

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
restartGame :: HandActionPerformer -> PlayerId -> Game ()
restartGame perform starter = do
  State.modify' $ \gs ->
    -- CR 727.1: "All players in that game when it ended then start a new game
    -- ..." -- so the rebuilt seating order is the players who were still in the
    -- game, in their seats (fixed by #147), rotated to begin with `starter` (CR 727.1a).
    let order = rotateTo starter (Game.stillPlayingInOrder gs)
     in gs
          { GameState.players = resetPlayers (GameState.players gs),
            GameState.manaPool = Map.empty,
            GameState.combat = Combat.emptyCombat,
            GameState.events = Seq.empty,
            GameState.lastKnown = Map.empty,
            GameState.scannedThrough = 0,
            GameState.controlWhenTriggered = Map.empty,
            GameState.damageScannedThrough = 0,
            GameState.delayedTriggers = Seq.empty,
            GameState.continuousEffects = [],
            GameState.replacements = [],
            GameState.playerEffects = [],
            GameState.turnOrder = order,
            -- CR 727.1a: "The starting player in the new game is the player who
            -- controlled the spell or ability that restarted the game." Read back
            -- off the rebuilt order, exactly as subgameStateFrom does, so the two
            -- can never disagree and this always names a seat: rotateTo leaves an
            -- order alone when `starter` is not in it, and the head is then the
            -- first still-playing seat.
            GameState.activePlayer = Maybe.fromMaybe starter (Maybe.listToMaybe order),
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
            GameState.exiledUntilMonarch = Map.empty,
            -- CR 727.1: the game that scheduled them has ended, so no extra turn
            -- survives into the new one -- cleared exactly as every other
            -- transient field is.
            GameState.extraTurns = [],
            GameState.turnAnchor = Nothing
          }
  startGameFromCards perform

-- CR 729.2: build a fresh subgame state from the parent's LIBRARY cards ONLY --
-- each player takes all the cards in their main-game library into the subgame
-- library; no other main-game zone enters (rule 729.2). The object pool is
-- restricted to those library objects; startGameFromCards (called by playSubgame)
-- then rebuilds each subgame library from that pool, shuffles, and draws opening
-- hands (CR 103). Players reset to a new game (CR 103); every transient field is
-- cleared, exactly as restartGame does, EXCEPT the object/timestamp id supplies,
-- which are INHERITED from the parent so every object the subgame mints (CR 400.7)
-- gets an id above every parent id -- funnelBack relies on that for non-collision.
-- CR 729.2's "randomly determine which player goes first" happens in the caller
-- (Engine.playSubgame asks Prompt.RandomFirstPlayer); `starter` is what it rolled.
-- CR 103.1: the turn order is rotated to begin with them, exactly as CR 727.1a's
-- restart does -- rotating rather than only setting activePlayer is load-bearing,
-- because Engine.skipsDraw (CR 103.8a) tests the HEAD of the turn order. Total: a
-- `starter` outside the order leaves it alone (rotateTo), and activePlayer is read
-- back off the rotated order, so the two can never disagree.
subgameStateFrom :: PlayerId -> GameState -> GameState
subgameStateFrom starter parent =
  let libIds =
        Set.fromList
          (concatMap (\pid -> Foldable.toList (Map.findWithDefault Seq.empty pid (GameState.library parent))) (GameState.turnOrder parent))
      libObjects = Map.restrictKeys (GameState.objects parent) libIds
      -- The pool is drawn from EVERY seat in the parent's FULL roster
      -- (GameState.turnOrder), never narrowed to Game.stillPlayingInOrder
      -- -- `order` below DOES narrow to the seated players (CR 729.4, fixed by #147), but
      -- this pool must not, because funnelBack's oldLibIds is built the SAME way
      -- over the SAME full roster, and the two have to agree: funnelBack drops
      -- every id in that set from the parent's kept objects (Map.withoutKeys)
      -- and rebuilds the returned set only from what actually survived the
      -- subgame (`returned`, plus `recovered` for CR 800.4a departures -- see
      -- funnelBack). Narrowing THIS pool to the seated players while funnelBack
      -- keeps the full roster would let funnelBack drop a STILL-PLAYING player's
      -- real library object that this pool never captured to fund a return for
      -- -- destroying it, silently.
      --
      -- A departed seat's own share of this pool is, in practice, always empty
      -- now, which is a change from before CR 800.4a landed (this same
      -- milestone): Departure.objectsLeaveWith deletes a departing player's
      -- library outright the instant they leave a CR 800.1 multiplayer game,
      -- and a departure in a two-player game ends the whole game (CR 104.2a)
      -- before any subgame could be built from it in the first place -- see
      -- Pawl.Engine.Departure. (A restart INSIDE a subgame, Setup.restartGame, can
      -- later shrink turnOrder below what it was at departure time, but it
      -- never resurrects objects Departure.objectsLeaveWith already deleted, so
      -- this stays true even then; funnelBack's own recovery guard relies on
      -- the same fact.) So this is no longer about ferrying a departed player's
      -- cards through the subgame inert so funnelBack can return them later --
      -- there is nothing left of theirs to ferry -- it is purely the
      -- bookkeeping symmetry with funnelBack described above, which has to hold
      -- regardless of who is or isn't currently seated.
      --
      -- Named plainly: `libIds` here and funnelBack's `oldLibIds` MUST compute
      -- the identical id set, and today they do, by construction -- both are the
      -- same expression (`concatMap` over each `GameState.library parent` entry,
      -- keyed by `GameState.turnOrder parent`) applied to the same `parent`
      -- value, because playSubgame's outer state is untouched while the subgame
      -- runs (CR 729.1a), so the `parent` funnelBack later reads back is this
      -- `parent` argument, unchanged. The match is a maintenance invariant, not a
      -- live gap: nothing today lets the two expressions drift apart, but an edit
      -- that changed one side's roster (e.g. narrowing it to seated players)
      -- without the other would reintroduce exactly the silent-destruction risk
      -- described above. This is a SEPARATE concern from a player who departs
      -- INSIDE the subgame, after this pool is already fixed -- that is
      -- funnelBack's `recovered` pass, which is driven by the owner's absence
      -- from the FINISHED subgame's own objects, not by anything this pool
      -- captured for them at the start; see funnelBack's haddock.
      order = rotateTo starter (Game.stillPlayingInOrder parent)
      firstPlayer = Maybe.fromMaybe (GameState.activePlayer parent) (Maybe.listToMaybe order)
   in parent
        { GameState.objects = libObjects,
          GameState.turnOrder = order,
          GameState.players = resetPlayers (GameState.players parent),
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
          GameState.lastKnown = Map.empty,
          GameState.scannedThrough = 0,
          GameState.controlWhenTriggered = Map.empty,
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
          GameState.exiledUntilMonarch = Map.empty,
          -- CR 729.1a: the subgame is its own game and starts from turn 1, so
          -- the main game's pending extra turns are not in it. The main game's
          -- own copy is untouched -- the parent state sits in the outer frame --
          -- so they are still waiting when the subgame ends.
          GameState.extraTurns = [],
          GameState.turnAnchor = Nothing
        }

-- CR 729.5: at the end of a subgame, each player takes all traditional cards
-- (Source.OfCard) they own ANYWHERE in the subgame into their main-game library
-- and reshuffles (the reshuffle is playSubgame's Prompt.Shuffle step). All other
-- subgame objects and the subgame's zones cease to exist -- they are simply not
-- carried over. The parent's non-library objects (hand, battlefield, graveyard,
-- ...) are untouched: the main game continues from where it was discontinued. The
-- old parent library objects are dropped (they moved into the subgame).
-- `oldLibIds` spans the parent's FULL seating roster, matching
-- subgameStateFrom's `libIds` -- see the comment there for why the two
-- expressions must stay identical. Returned cards keep their subgame ids, which
-- are all above the parent supply (subgameStateFrom inherited it), so Map.union
-- cannot collide; the id/timestamp supplies advance to the subgame high-water
-- mark.
--
-- CR 729.4's second sentence keeps the subgame and the main game as separate
-- populations: a player who leaves the SUBGAME has not left the main game, and
-- nothing in the CR removes a card from a player's deck for losing a subgame --
-- CR 729.5 says the opposite. But a subgame that itself began with more than two
-- players is CR 800.1 multiplayer (a subgame can be multiplayer even when its
-- departing player has only two OPPONENTS in the PARENT), so a departure inside
-- it really does reach Departure.objectsLeaveWith (CR 800.4a) and delete every
-- object that player owned in the subgame outright -- and `returned`, built only
-- from `finalSub`'s surviving objects, has nothing left to funnel back for them.
-- `recovered` restores exactly that set from the PARENT's pre-subgame copies.
--
-- The guard is on the CARD'S OWNER, not on whether its id merely fails to
-- appear in `finalSub`'s objects: CR 400.7 mints a fresh id on every zone
-- change (a draw, a cast, a death), including the opening-hand draws every
-- subgame runs through startGameFromCards, so an id from `oldLibIds` going
-- missing is the ORDINARY case for a card that is alive and well under a NEW
-- id -- one `returned` already has. A real card's object is never deleted
-- outright by anything other than Departure.objectsLeaveWith (CR 704.5d's
-- `ceaseToExist` in Pawl.Engine.Sba guards on Source.OfToken and never fires for
-- Source.OfCard), so the only players whose oldLibIds objects can legitimately
-- need recovering are ones for whom objectsLeaveWith actually fired.
--
-- The test for THAT is "this owner has no object of any kind left anywhere in
-- `finalSub`" -- not "Departure.continuesAfterDeparture finalSub", which was
-- tried and rejected: that reads `finalSub`'s turnOrder at the END of the
-- subgame, but objectsLeaveWith's own gate was decided at DEPARTURE time
-- (Departure.hs), and the two can disagree. Setup.restartGame rewrites
-- turnOrder to `Game.stillPlayingInOrder`, DROPPING departed seats, and a
-- restart can resolve inside a subgame (Effect.RestartGame, Resolve.hs;
-- playSubgame's playGame honours restartSignal) -- so a three-seat subgame
-- where bob departs (wiping him) followed by an in-subgame restart leaves
-- `finalSub`'s own turnOrder at length 2, and continuesAfterDeparture on
-- `finalSub` reads False even though bob's objects are gone. The owner-absence
-- test doesn't have this problem: restartGame's startGameFromCards rebuilds
-- `objects` from the pool that ALREADY EXISTS (Map.filter isCard, no owner
-- restriction) -- it can carry a survivor's objects forward, but it can never
-- resurrect a departed player's, because objectsLeaveWith already deleted
-- every one of them before the restart ran. A departed owner therefore stays
-- absent from `finalSub`'s objects through any number of intervening
-- restarts, which is exactly the robustness this predicate needs.
--
-- It is also still correctly False for a player who merely decks out in a
-- subgame that never reaches multiplayer (CR 800.1): there, objectsLeaveWith
-- never fires at all (Departure.depart's own gate), so their drawn cards are
-- untouched and still sit in `finalSub`'s objects under their post-draw ids --
-- `returned` already has them, and the owner is not absent.
--
-- The explicit "id itself is still missing from finalSub" check used to be a
-- separate third conjunct; it is now REDUNDANT and has been dropped: owner is
-- invariant across a card's whole life (Event.changeZoneReturning carries it
-- forward on every zone change), so if the owner has no object anywhere in
-- `finalSub`, this specific `oid` -- which belongs to that same owner -- cannot
-- be one of finalSub's objects either.
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
      returned = fmap toLibraryCard (Map.filter isCard (GameState.objects finalSub))
      oldLibIds =
        Set.fromList
          (concatMap (\pid -> Foldable.toList (Map.findWithDefault Seq.empty pid (GameState.library parent))) (GameState.turnOrder parent))
      ownersPresentInSub = Set.fromList (fmap Object.owner (Map.elems (GameState.objects finalSub)))
      removedByDeparture oid = case Map.lookup oid (GameState.objects parent) of
        Nothing -> False
        Just obj -> Set.notMember (Object.owner obj) ownersPresentInSub
      recovered = fmap toLibraryCard (Map.restrictKeys (GameState.objects parent) (Set.filter removedByDeparture oldLibIds))
      allReturned = Map.union returned recovered
      libraryOf pid = Seq.fromList (Map.keys (Map.filter (\obj -> Object.owner obj == pid) allReturned))
      keptParentObjects = Map.withoutKeys (GameState.objects parent) oldLibIds
   in parent
        { GameState.objects = Map.union allReturned keptParentObjects,
          GameState.library = Map.fromList (fmap (\pid -> (pid, libraryOf pid)) (GameState.turnOrder parent)),
          GameState.nextObjectId = max (GameState.nextObjectId parent) (GameState.nextObjectId finalSub),
          GameState.nextTimestamp = max (GameState.nextTimestamp parent) (GameState.nextTimestamp finalSub)
        }
