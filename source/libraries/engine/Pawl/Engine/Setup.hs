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
import Pawl.Types.Printing (Printing)
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
startingLife :: Maybe Printing -> Integer
startingLife commander = if Maybe.isJust commander then 40 else 20

-- How many cards this deck holds, CR 903.5's commander included: rule 903.5
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
          GameState.ambientAmounts = Map.empty,
          GameState.pendingEntryEffects = Seq.empty,
          GameState.enteringBeside = Set.empty,
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
            Object.facing = Facing.FaceUp,
            Object.exiledFaceDown = False,
            Object.damage = 0,
            Object.sickness = Sickness.Sick,
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
            Object.ventureRoom = Nothing,
            Object.classLevel = Nothing,
            Object.unlockedHalves = Set.empty,
            Object.designations = Set.empty,
            Object.kicked = False,
            Object.announcedX = Nothing,
            Object.detainedUntil = Set.empty,
            Object.doesNotUntapNext = False,
            Object.exertedBy = Set.empty
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
  -- CR 903.7 / CR 103.4: the starting life total, which is the deck's business
  -- and so cannot be settled by emptyGame above.
  State.modify' $ \gs ->
    gs
      { GameState.players =
          -- CR 309.2: the dungeon card is recorded on the player and no object is
          -- minted for it, because dungeon cards begin OUTSIDE the game and
          -- outside the game is not a zone (CR 400.11). CR 701.49a is what brings
          -- it in; Pawl.Engine.Dungeon.enter is that rule.
          Map.adjust (\p -> p {Player.life = startingLife (Deck.commander deck), Player.dungeon = Deck.dungeon deck}) pid (GameState.players gs)
      }
  Monad.forM_ (Map.toList (Deck.cards deck)) $ \(printing, n) ->
    Monad.replicateM_ (Natural.toIntSaturating n) (createCard pid printing)
  Monad.forM_ (Deck.commander deck) $ \printing -> do
    oid <- createCard pid printing
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
       in Commander.designate pid printing rezoned

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
--
-- `exempt` is CR 727.5's set: "effects may exempt certain cards from the
-- procedure that restarts the game. These cards are not in their owner's deck as
-- the new game begins." An exempted card is left exactly as it is -- same object,
-- same incarnation, still in exile -- rather than rebuilt into a library, so it
-- is untouched by CR 400.7 for the reason nothing moved it. Restricted to EXILE
-- because that is where every card CR 727.5 reaches sits: the rule's only
-- producer leaves cards in exile, and an exemption naming a permanent would have
-- to say which of the zones this function empties it survives in. A subgame
-- exempts nothing (CR 729.2 moves every library card in, and CR 729.2c each
-- commander).
startGameFromCards :: HandActionPerformer -> Set.Set ObjectId -> Game ()
startGameFromCards perform exemptions = do
  gs <- State.get
  let owners = Game.stillPlayingInOrder gs
      -- Cards in exile, and nothing else: CR 727.2's "all Magic cards" is what
      -- survives a rebuild at all, so an exemption is filtered by the same
      -- `isCard` test the funnel below applies rather than being able to smuggle
      -- a token or an emblem through it (CR 111.7).
      exempt =
        Map.keysSet
          (Map.filter isCard (Map.restrictKeys (GameState.objects gs) (Set.intersection exemptions (GameState.exile gs))))
      isCard obj = case Object.source obj of
        Source.OfCard _ -> True
        _ -> False
      -- CR 400.7: a hand-written zone move outside Event.changeZone, so the
      -- per-incarnation reset that funnel performs has to happen here too --
      -- through the same Object.newIncarnation, so that a field added later
      -- cannot be forgotten on one path and reset on the other.
      toLibraryCard obj = (Object.newIncarnation obj) {Object.zone = Zone.Library}
      toCommandCard obj = (Object.newIncarnation obj) {Object.zone = Zone.Command}
      rebuilt = Map.filter isCard (Map.withoutKeys (GameState.objects gs) exempt)
      -- CR 903.6: "each player puts their commander from their deck face up into
      -- the command zone". Both callers start a new game following rule 103 (CR
      -- 727.1, CR 729.2), so rule 903.6 applies to each of them, and CR 729.2c
      -- names the subgame case outright. Held back BEFORE the libraries are built,
      -- because rule 903.6 shuffles "the remaining cards of their deck".
      --
      -- CR 727.5a -- an exempted commander does not begin the new game in the
      -- command zone -- is satisfied by the ordering rather than by a second test:
      -- `rebuilt` already drops `exempt`, so an exempted commander stays in exile.
      -- Pawl.CommanderSpec's Restart group is what proves that, against a control
      -- leg whose only difference is an empty exempt set.
      --
      -- One object per player, by Commander.isCommander's CR 903.5 argument: the
      -- designation is a printing and a legal deck holds one copy of it, so at
      -- most one object can answer here. Pawl enforces no deck legality (#940), so
      -- a deck with two copies would have this take the lower id.
      commanderOf pid =
        Maybe.listToMaybe (Map.keys (Map.filterWithKey (\oid obj -> Object.owner obj == pid && Commander.isCommander oid gs) rebuilt))
      commanderIds = Set.fromList (Maybe.mapMaybe commanderOf owners)
      commanders = fmap toCommandCard (Map.restrictKeys rebuilt commanderIds)
      cards = fmap toLibraryCard (Map.withoutKeys rebuilt commanderIds)
      libraryOf pid = Seq.fromList (Map.keys (Map.filter (\obj -> Object.owner obj == pid) cards))
  State.put
    gs
      { GameState.objects = Map.unions [Map.restrictKeys (GameState.objects gs) exempt, cards, commanders],
        GameState.library = Map.fromList (fmap (\pid -> (pid, libraryOf pid)) owners),
        GameState.hand = Map.empty,
        GameState.graveyard = Map.empty,
        GameState.battlefield = mempty,
        GameState.phasedOut = mempty,
        GameState.exile = exempt,
        GameState.command = commanderIds,
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
--
-- CR 727.5's `exempt` cards are the exception to CR 727.2's funnel: they stay in
-- exile instead of going into a library. See startGameFromCards, which is where
-- they are held back.
restartGame :: HandActionPerformer -> Set.Set ObjectId -> PlayerId -> Game ()
restartGame perform exempt starter = do
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
            GameState.ambientAmounts = Map.empty,
            GameState.pendingEntryEffects = Seq.empty,
            GameState.enteringBeside = Set.empty,
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
            -- Kept for the CR 727.5 exemptions alone, and cleared for every
            -- other card: an exempted card never left exile, so what put it
            -- there is still true of it, and CR 727.4's additional instructions
            -- -- Karn's "then put THOSE CARDS onto the battlefield" -- are read
            -- after the rebuild and have nothing else left to read them off.
            -- Every other entry names a card the rebuild shuffled into a
            -- library, where CR 400.7 has already made it a different object.
            GameState.exiledWith = Map.restrictKeys (GameState.exiledWith gs) exempt,
            -- CR 727.1: the game that scheduled them has ended, so no extra
            -- turn survives into the new one.
            GameState.extraTurns = [],
            GameState.turnAnchor = Nothing
          }
  startGameFromCards perform exempt

-- CR 729.2: build a fresh subgame state from the parent's LIBRARY cards, plus
-- CR 729.2c's commanders; no other main-game zone enters. The object pool is
-- restricted to those objects; startGameFromCards (called by playSubgame) then
-- rebuilds each subgame library from that pool, puts each commander back in the
-- subgame's command zone (CR 903.6), shuffles, and draws opening hands (CR
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
      -- CR 729.2c: "as a subgame of a Commander game starts, each player moves
      -- their commander from the main-game command zone (if it's there) to the
      -- subgame command zone". Nothing ELSE in the main-game command zone moves --
      -- CR 729.2's "no other cards in a main-game zone are moved" -- and its two
      -- siblings have no format here: supplementary decks (CR 729.2a) and
      -- vanguards (CR 729.2b) are not implemented, nor is any other command-zone
      -- resident (#933, #934, #935, #936, #937).
      --
      -- The parent's own copy is deliberately left where it is: the parent is
      -- untouched while the subgame runs (CR 729.1a), and funnelBack drops these
      -- ids from it and refunds them from the subgame, exactly as it does for
      -- `libIds`.
      cmdIds = Set.filter (\oid -> Commander.isCommander oid parent) (GameState.command parent)
      movedObjects = Map.restrictKeys (GameState.objects parent) (Set.union libIds cmdIds)
      -- Invariant: `libIds` here and funnelBack's `oldLibIds` MUST compute the
      -- identical id set, and so must `cmdIds` and funnelBack's `oldCmdIds`.
      -- Both draw from the parent's FULL roster
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
        { GameState.objects = movedObjects,
          GameState.turnOrder = order,
          GameState.players = resetPlayers (GameState.players parent),
          GameState.library = Map.empty,
          GameState.hand = Map.empty,
          GameState.graveyard = Map.empty,
          GameState.battlefield = mempty,
          GameState.phasedOut = mempty,
          GameState.exile = mempty,
          -- CR 729.2c, above. startGameFromCards keeps them here rather than
          -- funnelling them into a library, which is CR 903.6 for the subgame.
          GameState.command = cmdIds,
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
          GameState.ambientAmounts = Map.empty,
          GameState.pendingEntryEffects = Seq.empty,
          GameState.enteringBeside = Set.empty,
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
-- (Source.OfCard) they own in the subgame other than those in the subgame
-- command zone into their main-game library and reshuffles (the reshuffle is
-- playSubgame's Prompt.Shuffle step). Every other zone is in scope, which covers
-- rule 729.5's second sentence -- "including phased-out permanents" -- for free:
-- `returned` filters GameState.objects, and CR 702.26d leaves a phased-out
-- permanent in that map with its zone unchanged, so nothing here has to know
-- phasing exists. The command zone is the rule's own exclusion, and CR 729.5c
-- moves the commanders sitting in it back to the main-game command zone; all
-- other subgame objects and zones simply are not carried over. The parent's
-- objects are untouched except for the ones CR 729.2 / 729.2c moved into the
-- subgame -- the main game continues from where it was discontinued -- and those
-- are dropped and refunded from what the subgame ended with. `oldLibIds` and
-- `oldCmdIds` span the parent's full seating roster, matching
-- subgameStateFrom's `libIds` and `cmdIds`; see there for why the two sides must
-- stay identical. Returned cards keep their subgame ids, all above the parent
-- supply, so Map.union cannot collide; the supplies advance to the subgame
-- high-water mark.
--
-- A subgame that began with more than two players is CR 800.1 multiplayer even
-- when its departing player has only two opponents in the PARENT, so a
-- departure inside it reaches CR 800.4a's Departure.objectsLeaveWith and
-- deletes every object that player owned in the subgame -- leaving `returned`
-- nothing to funnel back for them. `recovered` and `recoveredCmd` restore
-- exactly that set from the parent's pre-subgame copies.
--
-- The guard is on the card's OWNER, not on the id merely being missing from
-- `finalSub`: CR 400.7 mints a fresh id on every zone change, including the
-- opening-hand draws, so a missing `movedIds` id is the ordinary case for a
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
      toCommandCard obj = (Object.newIncarnation obj) {Object.zone = Zone.Command}
      -- CR 729.5's own exclusion: "all traditional cards they own that are in the
      -- subgame OTHER THAN those in the subgame command zone". So the library
      -- funnel skips the subgame's command zone wholesale, and CR 729.5c takes
      -- back out of it exactly the commanders. Any other CARD that ended there is
      -- covered by rule 729.5's "except as specified in rules 729.5a-c, all other
      -- objects in the subgame cease to exist", which is the literal reading; a
      -- commander is the only card a subgame can put in that zone anyway
      -- (CR 903.9a), and an emblem there is not a card and never was in scope.
      subCmdIds = GameState.command finalSub
      returned = fmap toLibraryCard (Map.filter isCard (Map.withoutKeys (GameState.objects finalSub) subCmdIds))
      backFromSub =
        fmap
          toCommandCard
          (Map.filterWithKey (\oid obj -> isCard obj && Commander.isCommander oid finalSub) (Map.restrictKeys (GameState.objects finalSub) subCmdIds))
      oldLibIds =
        Set.fromList
          (concatMap (\pid -> Foldable.toList (Map.findWithDefault Seq.empty pid (GameState.library parent))) (GameState.turnOrder parent))
      -- The same expression as subgameStateFrom's `cmdIds`; see the invariant note
      -- there. These are the ids CR 729.2c moved out of the parent's command zone,
      -- so they are dropped from the parent and refunded from the subgame -- into
      -- the command zone by `backFromSub` if the commander is still there (CR
      -- 729.5c's "if it's there"), and into the library by `returned` if it ended
      -- the subgame anywhere else (CR 729.5's first sentence).
      oldCmdIds = Set.filter (\oid -> Commander.isCommander oid parent) (GameState.command parent)
      movedIds = Set.union oldLibIds oldCmdIds
      ownersPresentInSub = Set.fromList (fmap Object.owner (Map.elems (GameState.objects finalSub)))
      removedByDeparture oid = case Map.lookup oid (GameState.objects parent) of
        Nothing -> False
        Just obj -> Set.notMember (Object.owner obj) ownersPresentInSub
      recoveredIds = Set.filter removedByDeparture movedIds
      recovered = fmap toLibraryCard (Map.restrictKeys (GameState.objects parent) (Set.difference recoveredIds oldCmdIds))
      -- A commander whose owner departed INSIDE the subgame is recovered to the
      -- zone it left the parent from, not to a library: CR 729.1b keeps the
      -- subgame's departure from meaning anything in the main game, where that
      -- player is still playing and their commander is still in the command zone.
      recoveredCmd = fmap toCommandCard (Map.restrictKeys (GameState.objects parent) (Set.intersection recoveredIds oldCmdIds))
      allReturned = Map.union returned recovered
      toCommand = Map.union backFromSub recoveredCmd
      libraryOf pid = Seq.fromList (Map.keys (Map.filter (\obj -> Object.owner obj == pid) allReturned))
      keptParentObjects = Map.withoutKeys (GameState.objects parent) movedIds
   in parent
        { GameState.objects = Map.unions [allReturned, toCommand, keptParentObjects],
          GameState.library = Map.fromList (fmap (\pid -> (pid, libraryOf pid)) (GameState.turnOrder parent)),
          -- CR 729.5c. The parent's other command-zone residents are untouched:
          -- CR 729.2c moved only the commanders, so only they can come back.
          GameState.command = Set.union (Set.difference (GameState.command parent) oldCmdIds) (Map.keysSet toCommand),
          GameState.nextObjectId = max (GameState.nextObjectId parent) (GameState.nextObjectId finalSub),
          GameState.nextTimestamp = max (GameState.nextTimestamp parent) (GameState.nextTimestamp finalSub),
          -- CR 104.4b: the subgame's events are not a stretch during which the
          -- parent's players could not act -- they were playing the subgame.
          -- Cleared to the merged supply so the main game resumes owing nobody a
          -- choice, rather than inheriting a gap the subgame ran up.
          GameState.lastChoice = max (GameState.nextTimestamp parent) (GameState.nextTimestamp finalSub)
        }
