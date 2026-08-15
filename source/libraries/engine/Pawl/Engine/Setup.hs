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
import qualified Pawl.Engine.Commander as Commander
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Mulligan as Mulligan
import qualified Pawl.Engine.Turn as Turn
import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.Types.Deck as Deck
import qualified Pawl.Types.EventGroup as EventGroup
import qualified Pawl.Types.Facing as Facing
import Pawl.Types.Game (Game)
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import Pawl.Types.HandActionPerformer (HandActionPerformer)
import qualified Pawl.Types.LibraryPosition as LibraryPosition
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Player as Player
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.PrintingId as PrintingId
import qualified Pawl.Types.RestartSignal as RestartSignal
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.Status as Status
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Timestamp as Timestamp
import qualified Pawl.Types.Zone as Zone

-- CR 103.4's twenty, and CR 903.7 / CR 103.4c's forty for a Commander game,
-- taken off the deck's CR 903.3 designation.
--
-- "Has a commander designated" standing in for "is a Commander game" is exact
-- today rather than an approximation, and it rests on a capability pawl lacks
-- rather than on a claim about Magic: Deck.commander is set by nothing but a
-- Commander deck, and pawl has no game options at all (#175), so there is no
-- way for the two to come apart.
--
-- Parametric because the designation reaches it two ways -- as the Deck's
-- Printing when a game is built, and as the Player's PrintingId when one is
-- restarted (CR 727) -- and rule 903.7 turns on WHETHER a commander was
-- designated, never on which card it is.
startingLife :: Maybe a -> Integer
startingLife commander = if Maybe.isJust commander then 40 else 20

-- How many cards this deck holds, CR 903.5a's commander included: rule 903.5a
-- counts the deck at exactly 100 cards "including its commander", so the card
-- that starts in the command zone is still one of the deck's cards. Every
-- non-Commander deck has no commander and so is unaffected.
deckSize :: Deck.Deck -> Natural
deckSize deck = sum (Map.elems (Deck.cards deck)) + maybe 0 (const 1) (Deck.commander deck)

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
            { -- CR 903.7 sets the total once the decks are known, which is
              -- createDeck below; no deck has been read yet here.
              Player.life = startingLife Nothing,
              Player.status = Status.Playing,
              Player.counters = Map.empty,
              -- CR 701.54c: the Ring has tempted nobody in a game that has not
              -- started.
              Player.ringTemptations = 0,
              -- CR 702.179b: "players do not have speed until a rule or effect
              -- sets their speed to a specific value", and no rule has. CR
              -- 704.5aa gives one to a player who controls a permanent with start
              -- your engines!, which is a state-based action and so cannot have
              -- happened before the game began.
              Player.speed = Nothing,
              Player.commander = Nothing,
              Player.commanderCasts = 0,
              -- CR 903.10a counts "over the course of the game", and no
              -- commander has dealt anybody anything in one that has not
              -- started.
              Player.commanderDamage = Map.empty,
              -- CR 309.2: dungeon cards begin outside the game, and which one a
              -- player brought is their deck's business -- createDeck below.
              Player.dungeon = Nothing
            }
        )
   in GameState.MkGameState
        { GameState.objects = Map.empty,
          GameState.library = Map.empty,
          GameState.hand = Map.empty,
          GameState.graveyard = Map.empty,
          GameState.battlefield = mempty,
          GameState.phasedOut = mempty,
          GameState.exile = mempty,
          GameState.command = mempty,
          GameState.stack = [],
          GameState.players = Map.fromList (fmap newPlayer order_),
          GameState.manaPool = Map.empty,
          GameState.combat = Combat.emptyCombat,
          GameState.events = Seq.empty,
          GameState.nextEventGroup = EventGroup.first,
          GameState.eventGroupDepth = 0,
          GameState.lastKnown = Map.empty,
          GameState.scannedThrough = 0,
          GameState.battlefieldWhenTriggered = Map.empty,
          GameState.controlSample = Map.empty,
          GameState.damageScannedThrough = 0,
          GameState.delayedTriggers = Seq.empty,
          GameState.continuousEffects = [],
          GameState.replacements = [],
          GameState.pendingPreventionRiders = Seq.empty,
          GameState.playerEffects = [],
          GameState.blockRequirements = [],
          GameState.ignoredAbilities = [],
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
          GameState.printings = Map.empty,
          GameState.printingIds = Map.empty,
          GameState.nextPrintingId = PrintingId.MkPrintingId 0,
          GameState.nextTimestamp = Timestamp.MkTimestamp 0,
          GameState.lastChoice = Timestamp.MkTimestamp 0,
          GameState.drewFromEmpty = mempty,
          GameState.landsPlayed = mempty,
          GameState.drawsThisTurn = mempty,
          GameState.speedIncreasedThisTurn = mempty,
          GameState.pendingControl = Map.empty,
          GameState.activeControl = Nothing,
          GameState.monarch = Nothing,
          -- CR 731.1: "the game starts with neither designation".
          GameState.daytime = Nothing,
          GameState.spellsCastLastTurn = 0,
          GameState.exiledUntilMonarch = Map.empty,
          GameState.haunting = Map.empty,
          GameState.exiledWith = Map.empty,
          GameState.extraTurns = [],
          GameState.turnAnchor = Nothing
        }

createCard :: PlayerId -> PrintingId.PrintingId -> Game ObjectId
createCard pid printingId = do
  gs <- State.get
  let (oid, gs1) = Game.freshObjectId gs
      (ts, gs2) = Game.freshTimestamp gs1
      obj =
        Object.MkObject
          { Object.owner = pid,
            Object.enteredUnder = Nothing,
            Object.source = Source.OfCard printingId,
            Object.zone = Zone.Library,
            Object.tapped = TapState.Untapped,
            Object.facing = Facing.FaceUp,
            Object.exiledFaceDown = False,
            Object.damage = 0,
            Object.sickness = Sickness.Sick,
            Object.bindings = Map.empty,
            Object.counters = Map.empty,
            Object.attachedTo = Nothing,
            Object.chosenColor = Nothing,
            Object.chosenSubtype = Nothing,
            Object.chosenNames = Set.empty,
            Object.timestamp = ts,
            Object.face = Nothing,
            Object.turnedOverAt = Nothing,
            Object.worldSince = Nothing,
            Object.playableFromExile = Nothing,
            Object.plotted = Nothing,
            Object.foretold = Nothing,
            Object.ringBearerFor = Nothing,
            Object.protector = Nothing,
            Object.ventureRoom = Nothing,
            Object.unlockedHalves = Set.empty,
            Object.designations = Set.empty,
            Object.kicked = False
          }
      gs3 =
        gs2
          { GameState.objects = Map.insert oid obj (GameState.objects gs2),
            GameState.library = Map.insertWith (flip (Seq.><)) pid (Seq.singleton oid) (GameState.library gs2)
          }
  State.put gs3
  pure oid

-- Build each player's library from their deck's multiset, shuffle, draw.
-- CR 103.1: build this player's library from their deck -- and, for a Commander
-- deck, CR 903.6's "the commander begins the game in the command zone" and CR
-- 903.7's forty life.
--
-- The commander is created exactly like any other card and then placed in a
-- different zone, so it is an ordinary Source.OfCard object throughout: CR 903.3
-- designates a CARD, not a special kind of object, and everything that reads a
-- card -- the projection, casting, the CR 400.7 conservation count -- must find it
-- unchanged. The designation itself lives on the player (Player.commander),
-- because CR 400.7 mints a fresh id on every zone change and a commander crosses
-- zones constantly; an id or an object field could not survive that.
createDeck :: PlayerId -> Deck.Deck -> Game ()
createDeck pid deck = do
  dungeonId <- Monad.mapM (State.state . Game.intern) (Deck.dungeon deck)
  -- CR 903.7 / CR 103.4: the starting life total, which is the deck's business
  -- and so cannot be settled by emptyGame above.
  State.modify' $ \gs ->
    gs
      { GameState.players =
          -- CR 309.2: the dungeon card is recorded on the player and no object is
          -- minted for it, because dungeon cards begin OUTSIDE the game and
          -- outside the game is not a zone (CR 400.11). CR 701.49a is what brings
          -- it in; Pawl.Engine.Dungeon.enter is that rule.
          Map.adjust (\p -> p {Player.life = startingLife (Deck.commander deck), Player.dungeon = dungeonId}) pid (GameState.players gs)
      }
  Monad.forM_ (Map.toList (Deck.cards deck)) $ \(printing, n) -> do
    printingId <- State.state (Game.intern printing)
    Monad.replicateM_ (Natural.toIntSaturating n) (createCard pid printingId)
  -- One intern, and the id goes to BOTH the object and the designation below --
  -- which is why Commander.isCommander's comparison holds without leaning on
  -- Game.intern's idempotence. That idempotence is what keeps a malformed deck
  -- listing its commander among its cards too (CR 903.5b forbids it; #940 means
  -- pawl does not enforce it) down to one entry.
  Monad.forM_ (Deck.commander deck) $ \printing -> do
    printingId <- State.state (Game.intern printing)
    oid <- createCard pid printingId
    State.modify' $ \gs ->
      let moved =
            Game.insertIntoZone Zone.Command LibraryPosition.defaultValue pid oid (Game.removeFromZones pid oid gs)
          -- Object.zone tracks the zone sets, so it moves with them. A hand-written
          -- move outside Event.changeZone for createCard's own reason: this is the
          -- game being built, so there is no CR 400.7 event to emit and nothing to
          -- trigger.
          rezoned =
            moved
              { GameState.objects =
                  Map.adjust (\o -> o {Object.zone = Zone.Command}) oid (GameState.objects moved)
              }
       in Commander.designate pid printingId rezoned

newGame :: HandActionPerformer -> NonEmpty.NonEmpty (PlayerId, Deck.Deck) -> Game ()
newGame perform matchup = do
  -- CR 103.3: build and shuffle every library before any opening hand is drawn,
  -- so CR 103.5's declaration round sees settled libraries.
  Monad.forM_ (NonEmpty.toList matchup) $ \(pid, deck) -> do
    createDeck pid deck
    Mulligan.shuffleLibrary pid
  Mulligan.openingHands perform (fmap fst (NonEmpty.toList matchup))

-- CR 727.2 / 729.2: build every player's library from the EXISTING object pool
-- -- each player's owned cards, wherever they currently sit -- then shuffle and
-- draw opening hands (CR 103.5). Deliberately not newGame: it reuses the real
-- objects (ownership preserved) instead of minting fresh ones from Deck
-- definitions. Only Magic cards survive; an ability on the stack, a token, an
-- emblem or a trigger is not a card (CR 727.2 / 111.7). Shared by restart (CR
-- 727) and subgames (CR 729).
--
-- `owners` is the still-playing seats in seating order: CR 727.1 / 729.2
-- rebuild the game for the players who are in it, and CR 103.5's declaration
-- round goes around the table in turn order. A departed player's cards are not
-- here to skip -- CR 800.4a took them out of the game with them.
startGameFromCards :: HandActionPerformer -> Game ()
startGameFromCards perform = do
  gs <- State.get
  let owners = Game.stillPlayingInOrder gs
      isCard obj = case Object.source obj of
        Source.OfCard _ -> True
        _ -> False
      -- CR 400.7: a hand-written zone move outside Event.changeZone, so the
      -- per-incarnation reset that funnel performs has to happen here too --
      -- through the same Object.newIncarnation, so that a field added later
      -- cannot be forgotten on one path and reset on the other.
      toLibraryCard obj = (Object.newIncarnation obj) {Object.zone = Zone.Library}
      cards = fmap toLibraryCard (Map.filter isCard (GameState.objects gs))
      libraryOf pid = Seq.fromList (Map.keys (Map.filter (\obj -> Object.owner obj == pid) cards))
  State.put
    gs
      { GameState.objects = cards,
        GameState.library = Map.fromList (fmap (\pid -> (pid, libraryOf pid)) owners),
        GameState.hand = Map.empty,
        GameState.graveyard = Map.empty,
        GameState.battlefield = mempty,
        GameState.phasedOut = mempty,
        GameState.exile = mempty,
        GameState.command = mempty,
        GameState.stack = []
      }
  Monad.forM_ owners Mulligan.shuffleLibrary
  Mulligan.openingHands perform owners

-- CR 103 / 727.1a: put `starter` at the head of the turn order, preserving the
-- cyclic order. Total: a `starter` not in the order leaves it as-is.
rotateTo :: PlayerId -> [PlayerId] -> [PlayerId]
rotateTo starter order = case break (== starter) order of
  (before, after) -> after <> before

-- CR 103.4 / CR 727.1 / CR 729.2: put every player back to a new game's
-- starting state for the two paths that rebuild a game in place (restart and
-- subgames).
--
-- A player who has already left is not reset: CR 727.1 restarts for the players
-- in the game when it ended, and CR 729.4 puts everyone else outside the
-- subgame. Their Status.Departed survives the rebuild, and they keep their
-- entry in the map rather than being deleted, so every Map.lookup naming them
-- stays total. Which players are in the rebuilt game is the rebuilt
-- GameState.turnOrder.
resetPlayers :: Map.Map PlayerId Player.Player -> Map.Map PlayerId Player.Player
resetPlayers players =
  let reset player = case Player.status player of
        Status.Playing ->
          player
            { -- CR 903.7 again: Player.commander survives the rebuild (see
              -- Player.commanderCasts below), so a restarted Commander game
              -- starts back at forty rather than at twenty.
              Player.life = startingLife (Player.commander player),
              Player.counters = Map.empty,
              -- CR 727.1 / 729.2: a NEW game, so the Ring has tempted nobody in
              -- it. The command zone this line's callers empty is where the
              -- emblem the count belongs to went.
              Player.ringTemptations = 0,
              -- CR 727.1 again: a new game starts with nobody having speed (CR
              -- 702.179b), whatever the restarted one reached.
              Player.speed = Nothing,
              -- CR 903.8 counts casts "this game", and CR 727.1 makes the
              -- restarted one a new game, so the tax starts over. Player.commander
              -- is deliberately NOT reset beside it: rule 903.3's designation is
              -- made from the deck before the game begins and the restart reuses
              -- the same decks, so the same card is still the commander.
              Player.commanderCasts = 0,
              -- CR 903.10a counts "over the course of the game", and CR 727.1
              -- makes the restarted one a new game, so the tally starts over
              -- with the tax.
              Player.commanderDamage = Map.empty
            }
        Status.Departed _ -> player
   in fmap reset players

-- CR 727: restart the game in place. CR 727.1a's starting player is `starter`
-- (the controller of the restarting ability), so the turn order is rotated to
-- begin with them. CR 727.2: every card returns to its owner's new library via
-- startGameFromCards, built from the actual object pool -- never emptyGame plus
-- newGame, which would lose the real cards and pick the wrong starting player.
-- CR 727.4: the effect finishes resolving just before the first turn's untap
-- step with no player holding priority. The object and timestamp id supplies
-- are preserved so reused cards keep unique ids.
restartGame :: HandActionPerformer -> PlayerId -> Game ()
restartGame perform starter = do
  State.modify' $ \gs ->
    -- CR 727.1: the rebuilt seating order is the players who were still in the
    -- game, in their seats, rotated to begin with `starter` (CR 727.1a).
    let order = rotateTo starter (Game.stillPlayingInOrder gs)
     in gs
          { GameState.players = resetPlayers (GameState.players gs),
            GameState.manaPool = Map.empty,
            GameState.combat = Combat.emptyCombat,
            GameState.events = Seq.empty,
            GameState.nextEventGroup = EventGroup.first,
            GameState.eventGroupDepth = 0,
            GameState.lastKnown = Map.empty,
            GameState.scannedThrough = 0,
            GameState.battlefieldWhenTriggered = Map.empty,
            GameState.controlSample = Map.empty,
            GameState.damageScannedThrough = 0,
            GameState.delayedTriggers = Seq.empty,
            GameState.continuousEffects = [],
            GameState.replacements = [],
            GameState.pendingPreventionRiders = Seq.empty,
            GameState.playerEffects = [],
            GameState.blockRequirements = [],
            GameState.ignoredAbilities = [],
            GameState.turnOrder = order,
            -- CR 727.1a. Read back off the rebuilt order, exactly as
            -- subgameStateFrom does, so the two can never disagree and this
            -- always names a seat: rotateTo leaves an order alone when
            -- `starter` is not in it, and the head is then the first
            -- still-playing seat.
            GameState.activePlayer = Maybe.fromMaybe starter (Maybe.listToMaybe order),
            GameState.phase = Turn.firstPhase,
            GameState.remaining = Turn.laterPhases,
            GameState.priority = Nothing,
            GameState.passes = 0,
            GameState.turnNumber = 1,
            GameState.result = Nothing,
            -- CR 727.4: the game the caller was running has been replaced.
            -- Engine.priorityLoop and Engine.runStep read this and unwind to
            -- the rebuilt turn 1 rather than granting priority.
            GameState.restartSignal = RestartSignal.Restarted,
            -- CR 104.4b: CR 727.1 ends the game that was being played, so the
            -- rebuilt one starts owing nobody a choice. The timestamp supply is
            -- preserved across the restart, so this is the supply's current
            -- value and not zero.
            GameState.lastChoice = GameState.nextTimestamp gs,
            GameState.drewFromEmpty = mempty,
            GameState.landsPlayed = mempty,
            GameState.drawsThisTurn = mempty,
            GameState.speedIncreasedThisTurn = mempty,
            GameState.pendingControl = Map.empty,
            GameState.activeControl = Nothing,
            GameState.monarch = Nothing,
            -- CR 727.1 / 731.1: the restarted game is a new game, which starts
            -- with neither designation however the ended one finished.
            GameState.daytime = Nothing,
            GameState.spellsCastLastTurn = 0,
            GameState.exiledUntilMonarch = Map.empty,
            GameState.haunting = Map.empty,
            GameState.exiledWith = Map.empty,
            -- CR 727.1: the game that scheduled them has ended, so no extra
            -- turn survives into the new one.
            GameState.extraTurns = [],
            GameState.turnAnchor = Nothing
          }
  startGameFromCards perform

-- CR 729.2: build a fresh subgame state from the parent's LIBRARY cards only;
-- no other main-game zone enters. The object pool is restricted to those
-- library objects; startGameFromCards (called by playSubgame) then rebuilds
-- each subgame library from that pool, shuffles, and draws opening hands (CR
-- 103). Every transient field is cleared as restartGame does, EXCEPT the
-- object/timestamp id supplies, which are inherited from the parent so every
-- object the subgame mints (CR 400.7) gets an id above every parent id --
-- funnelBack relies on that for non-collision. `starter` is what the caller's
-- CR 729.2 random roll produced. Rotating the turn order to begin with them (CR
-- 103.1), rather than only setting activePlayer, is load-bearing:
-- Engine.skipsDraw (CR 103.8a) tests the HEAD of the turn order. Total: a
-- `starter` outside the order leaves it alone, and activePlayer is read back
-- off the rotated order, so the two cannot disagree.
subgameStateFrom :: PlayerId -> GameState -> GameState
subgameStateFrom starter parent =
  let libIds =
        Set.fromList
          (concatMap (\pid -> Foldable.toList (Map.findWithDefault Seq.empty pid (GameState.library parent))) (GameState.turnOrder parent))
      libObjects = Map.restrictKeys (GameState.objects parent) libIds
      -- Invariant: `libIds` here and funnelBack's `oldLibIds` MUST compute the
      -- identical id set. Both draw from the parent's FULL roster
      -- (GameState.turnOrder), never narrowed to Game.stillPlayingInOrder --
      -- `order` below does narrow to the seated players (CR 729.4), but this
      -- pool must not. funnelBack drops every id in that set from the parent's
      -- kept objects and refunds it only from what survived the subgame, so
      -- narrowing one side and not the other would silently destroy a
      -- still-playing player's library object that this pool never captured.
      --
      -- They agree today by construction: the same expression applied to the
      -- same `parent`, which is unchanged while the subgame runs (CR 729.1a).
      -- That is a maintenance invariant, not a live gap. A player who departs
      -- INSIDE the subgame is a separate concern, handled by funnelBack's
      -- `recovered` pass.
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
          GameState.phasedOut = mempty,
          GameState.exile = mempty,
          GameState.command = mempty,
          GameState.stack = [],
          GameState.manaPool = Map.empty,
          GameState.combat = Combat.emptyCombat,
          GameState.events = Seq.empty,
          GameState.nextEventGroup = EventGroup.first,
          GameState.eventGroupDepth = 0,
          GameState.lastKnown = Map.empty,
          GameState.scannedThrough = 0,
          GameState.battlefieldWhenTriggered = Map.empty,
          GameState.controlSample = Map.empty,
          GameState.damageScannedThrough = 0,
          GameState.delayedTriggers = Seq.empty,
          GameState.continuousEffects = [],
          GameState.replacements = [],
          GameState.pendingPreventionRiders = Seq.empty,
          GameState.playerEffects = [],
          GameState.blockRequirements = [],
          GameState.ignoredAbilities = [],
          GameState.activePlayer = firstPlayer,
          GameState.phase = Turn.firstPhase,
          GameState.remaining = Turn.laterPhases,
          GameState.priority = Nothing,
          GameState.passes = 0,
          GameState.turnNumber = 1,
          GameState.result = Nothing,
          GameState.restartSignal = RestartSignal.Playing,
          -- CR 104.4b: the subgame is its own game, so it starts owing nobody a
          -- choice. Set to the INHERITED nextTimestamp rather than to zero,
          -- which the timestamp supply is not: a copied parent marker would
          -- have the subgame draw itself on entry for events at another level.
          GameState.lastChoice = GameState.nextTimestamp parent,
          GameState.drewFromEmpty = mempty,
          GameState.landsPlayed = mempty,
          GameState.drawsThisTurn = mempty,
          GameState.speedIncreasedThisTurn = mempty,
          GameState.pendingControl = Map.empty,
          GameState.activeControl = Nothing,
          GameState.monarch = Nothing,
          -- CR 729.1a / 731.1: the subgame is its own game, so it starts with
          -- neither designation and the main game's is untouched by it.
          GameState.daytime = Nothing,
          GameState.spellsCastLastTurn = 0,
          GameState.exiledUntilMonarch = Map.empty,
          GameState.haunting = Map.empty,
          GameState.exiledWith = Map.empty,
          -- CR 729.1a: the subgame is its own game and starts from turn 1, so
          -- the main game's pending extra turns are not in it. Its own copy
          -- sits untouched in the outer frame, still waiting when the subgame
          -- ends.
          GameState.extraTurns = [],
          GameState.turnAnchor = Nothing
        }

-- CR 729.5: at the end of a subgame, each player takes all traditional cards
-- (Source.OfCard) they own anywhere in the subgame into their main-game library
-- and reshuffles (the reshuffle is playSubgame's Prompt.Shuffle step).
-- "Anywhere" is literal, and covers rule 729.5's second sentence -- "including
-- phased-out permanents" -- for free: `returned` filters GameState.objects, and
-- CR 702.26d leaves a phased-out permanent in that map with its zone unchanged,
-- so nothing here has to know phasing exists. All
-- other subgame objects and zones simply are not carried over. The parent's
-- non-library objects are untouched -- the main game continues from where it
-- was discontinued -- and the old parent library objects are dropped, having
-- moved into the subgame. `oldLibIds` spans the parent's full seating roster,
-- matching subgameStateFrom's `libIds`; see there for why the two must stay
-- identical. Returned cards keep their subgame ids, all above the parent
-- supply, so Map.union cannot collide; the supplies advance to the subgame
-- high-water mark.
--
-- A subgame that began with more than two players is CR 800.1 multiplayer even
-- when its departing player has only two opponents in the PARENT, so a
-- departure inside it reaches CR 800.4a's Departure.objectsLeaveWith and
-- deletes every object that player owned in the subgame -- leaving `returned`
-- nothing to funnel back for them. `recovered` restores exactly that set from
-- the parent's pre-subgame copies.
--
-- The guard is on the card's OWNER, not on the id merely being missing from
-- `finalSub`: CR 400.7 mints a fresh id on every zone change, including the
-- opening-hand draws, so a missing `oldLibIds` id is the ordinary case for a
-- card that is alive under a new id. Nothing but objectsLeaveWith deletes a
-- real card's object outright (Sba's `ceaseToExist` guards on Source.OfToken),
-- so its firing is the only thing that can need recovering.
--
-- Owner-absence rather than `Departure.continuesAfterDeparture finalSub`, which
-- was tried and rejected: that reads turnOrder at the END of the subgame while
-- objectsLeaveWith's gate was decided at departure time, and an in-subgame
-- restart rewrites turnOrder to the still-playing seats, so the two disagree. A
-- departed owner stays absent from `finalSub`'s objects through any number of
-- restarts, since a restart rebuilds from the pool that already exists and can
-- never resurrect what objectsLeaveWith deleted. It stays correctly False for a
-- player who merely decks out in a subgame that never reaches multiplayer:
-- objectsLeaveWith never fires there, so their cards are still in `finalSub`
-- and `returned` has them. Owner is invariant across a card's life, so an
-- absent owner also implies this `oid` is missing -- no separate id check is
-- needed.
funnelBack :: GameState -> GameState -> GameState
funnelBack finalSub parent =
  let isCard obj = case Object.source obj of
        Source.OfCard _ -> True
        _ -> False
      -- CR 400.7, exactly as startGameFromCards' own toLibraryCard: the second
      -- hand-written zone move outside Event.changeZone, performing that
      -- funnel's per-incarnation reset through the one shared function.
      toLibraryCard obj = (Object.newIncarnation obj) {Object.zone = Zone.Library}
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
          -- The subgame's cards come back as library objects above, and each
          -- names its printing by id -- so the entries those ids name have to
          -- come back too, or the returned cards would resolve to nothing.
          --
          -- Union is unambiguous rather than merely convenient:
          -- subgameStateFrom builds the subgame as a record update on `parent`,
          -- so it INHERITS the table and the counter whole. Anything the
          -- subgame interned was minted above every id the parent held, so the
          -- two tables never disagree about an id.
          GameState.printings = Map.union (GameState.printings finalSub) (GameState.printings parent),
          GameState.printingIds = Map.union (GameState.printingIds finalSub) (GameState.printingIds parent),
          GameState.nextPrintingId = max (GameState.nextPrintingId parent) (GameState.nextPrintingId finalSub),
          GameState.nextTimestamp = max (GameState.nextTimestamp parent) (GameState.nextTimestamp finalSub),
          -- CR 104.4b: the subgame's events are not a stretch during which the
          -- parent's players could not act -- they were playing the subgame.
          -- Cleared to the merged supply so the main game resumes owing nobody a
          -- choice, rather than inheriting a gap the subgame ran up.
          GameState.lastChoice = max (GameState.nextTimestamp parent) (GameState.nextTimestamp finalSub)
        }
