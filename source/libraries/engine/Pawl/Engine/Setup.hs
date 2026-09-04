module Pawl.Engine.Setup where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Commander as Commander
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Mulligan as Mulligan
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Turn as Turn
import qualified Pawl.Engine.Vanguard as Vanguard
import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.Types.AttackOption as AttackOption
import qualified Pawl.Types.Combat as Combat.Type
import qualified Pawl.Types.Deck as Deck
import qualified Pawl.Types.EndTurnSignal as EndTurnSignal
import qualified Pawl.Types.EventGroup as EventGroup
import qualified Pawl.Types.Facing as Facing
import Pawl.Types.Game (Game)
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameSettings as GameSettings
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import Pawl.Types.HandActionPerformer (HandActionPerformer)
import qualified Pawl.Types.LastKnown as LastKnown
import qualified Pawl.Types.LibraryPosition as LibraryPosition
import qualified Pawl.Types.Mana as Mana
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OutsideObject as OutsideObject
import qualified Pawl.Types.Player as Player
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.PrintingId as PrintingId
import qualified Pawl.Types.RestartSignal as RestartSignal
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.Status as Status
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Teams as Teams
import qualified Pawl.Types.Timestamp as Timestamp
import qualified Pawl.Types.Zone as Zone

-- CR 103.4's twenty, CR 903.7 / CR 103.4c's forty for a Commander game taken off
-- the deck's CR 903.3 designation, and CR 903.12f / CR 103.4d's twenty-five or
-- thirty when the Brawl option is in use.
--
-- CR 903.12f reads the SEAT COUNT and not the designation: "in a two-player
-- Brawl game, each player's starting life total is 25. In a multiplayer Brawl
-- game, each player's starting life total is 30." Multiplayer is CR 800.1's
-- "begins with more than two players", which is what `seats` counts, so a
-- one-seat game -- which the CR does not describe -- takes the two-player
-- number rather than the multiplayer one.
--
-- Brawl is tested BEFORE the designation, not beside it: CR 903.12a makes a
-- Brawl game a Commander game, so both branches hold at once and rule 903.12f
-- is the modification rule 903.12a announces.
--
-- "Has a commander designated" standing in for "is a Commander game" outside
-- Brawl is exact today rather than an approximation, and it rests on a
-- capability pawl lacks rather than on a claim about Magic: Deck.commander is
-- set by nothing but a Commander deck, and GameSettings carries no Commander
-- variant selection for the two to disagree with (#175).
--
-- Parametric in the designation because it reaches this two ways -- as the
-- Deck's Printing when a game is built, and as the Player's PrintingId when one
-- is restarted (CR 727) -- and rule 903.7 turns on WHETHER a commander was
-- designated, never on which card it is.
--
-- CR 902.4's life modifier is the `modifier` addend rather than a fourth branch:
-- rule 902.4 modifies rule 103.4's twenty rather than replacing it, and the
-- Vanguard variant combines with none of the three above -- no Brawl or Commander
-- deck brings a vanguard card, so a nonzero modifier only ever meets the
-- `otherwise` branch. Written as an addend anyway, because that is what rule
-- 902.4 says of whatever the starting total is.
startingLife :: GameSettings.GameSettings -> Int -> Maybe a -> Integer -> Integer
startingLife settings seats commander modifier = modifier + base
  where
    base :: Integer
    base
      | GameSettings.brawl settings = if seats > 2 then 30 else 25
      | Maybe.isJust commander = 40
      | otherwise = 20

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
      -- CR 800.2 / CR 103: a game with no option in use, which is what rule 103
      -- describes on its own.
      --
      -- Not implemented: an options argument. Nothing threads one down to here,
      -- so a game that uses an option is reached by record-updating
      -- GameState.settings afterwards rather than by being started with it
      -- (#2837).
      -- CR 802.1 is the option in use by default: at two seats it coincides
      -- exactly with CR 506.2, and at three or more CR 806.2b requires one of
      -- the three and this is the one that needs no seating to be agreed.
      --
      -- CR 102.4 / CR 808.1: and a game not played between teams, which every
      -- variant but CR 808's, CR 809's, CR 810's and CR 811's is.
      settings = GameSettings.MkGameSettings {GameSettings.brawl = False, GameSettings.attackOption = Just AttackOption.MultiplePlayers, GameSettings.teams = Teams.none}
      newPlayer pid =
        ( pid,
          Player.MkPlayer
            { -- CR 903.7 sets the total once the decks are known, which is
              -- createDeck below; no deck has been read yet here.
              -- CR 902.4's modifier is zero for the same reason: no vanguard card
              -- is in a command zone this function leaves empty.
              Player.life = startingLife settings (length order_) Nothing 0,
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
              -- CR 309.2: dungeon cards begin outside the game, and which ones a
              -- player brought is their deck's business -- createDeck below.
              Player.dungeons = Set.empty,
              -- CR 400.11a: a player's sideboard is outside the game, and which
              -- cards they set aside is their deck's business -- createDeck below.
              Player.outsideTheGame = Map.empty,
              -- CR 309.7: nobody has completed a dungeon in a game that has not
              -- started, there having been no venture to remove one from it.
              Player.completedDungeons = 0,
              Player.completedDungeonNames = Set.empty
            }
        )
   in GameState.MkGameState
        { GameState.settings = settings,
          GameState.objects = Map.empty,
          GameState.library = Map.empty,
          GameState.hand = Map.empty,
          GameState.graveyard = Map.empty,
          GameState.battlefield = mempty,
          GameState.phasedOut = mempty,
          GameState.exile = mempty,
          GameState.command = mempty,
          GameState.stack = [],
          GameState.players = Map.fromList (fmap newPlayer order_),
          -- CR 729.4: nobody is nested inside another game here.
          GameState.outsideObjects = Map.empty,
          GameState.broughtIn = Seq.empty,
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
          GameState.detachedBindings = Map.empty,
          GameState.pendingEntryEffects = Seq.empty,
          GameState.enteringBeside = Set.empty,
          GameState.enteringSubjects = Set.empty,
          GameState.enteringCounters = Map.empty,
          GameState.playerEffects = [],
          GameState.blockRequirements = [],
          GameState.unregeneratables = [],
          GameState.blockProhibitions = [],
          GameState.attackProhibitions = [],
          GameState.attackRequirements = [],
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
          GameState.endTurnSignal = EndTurnSignal.Running,
          GameState.nextObjectId = ObjectId.MkObjectId 0,
          GameState.printings = Map.empty,
          GameState.printingIds = Map.empty,
          GameState.nextPrintingId = PrintingId.MkPrintingId 0,
          GameState.nextTimestamp = Timestamp.MkTimestamp 0,
          GameState.lastChoice = Timestamp.MkTimestamp 0,
          GameState.drewFromEmpty = mempty,
          GameState.landsPlayed = mempty,
          GameState.drawsThisTurn = mempty,
          GameState.pendingControl = Map.empty,
          GameState.activeControl = Nothing,
          GameState.monarch = Nothing,
          GameState.initiative = Nothing,
          -- CR 731.1: "the game starts with neither designation".
          GameState.daytime = Nothing,
          GameState.spellsCastLastTurn = 0,
          GameState.castsLastTurn = mempty,
          GameState.exiledUntilMonarch = Map.empty,
          GameState.movedUntilSourceLeaves = Map.empty,
          GameState.haunting = Map.empty,
          GameState.exiledWith = Map.empty,
          GameState.exilePiles = Map.empty,
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
            Object.kicked = Map.empty,
            Object.bestowed = False,
            Object.phyrexianLifePaid = 0,
            Object.manaSpent = Mana.MkMana [],
            Object.announcedX = Nothing,
            Object.castFrom = Nothing,
            Object.detainedUntil = Set.empty,
            Object.goadedBy = Set.empty,
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
  dungeonIds <- Monad.mapM (State.state . Game.intern) (Set.toAscList (Deck.dungeons deck))
  -- CR 103.2a: the sideboard is set aside before the game, so these are interned
  -- alongside the deck's own printings but no card is created for them.
  sideboardIds <- Monad.mapM (\(printing, n) -> fmap (\i -> (i, n)) (State.state (Game.intern printing))) (Map.toAscList (Deck.sideboard deck))
  -- CR 902.3: "each vanguard card is placed face up next to its owner's library
  -- before the game begins", which is the command zone (CR 313.2). Placed BEFORE
  -- the life total below, and that ordering is load-bearing: rule 902.4's modifier
  -- is read off the card where it sits, so the card has to be sitting there.
  --
  -- No designation on the player beside Commander.designate's: the card type says
  -- which object this is (Pawl.Engine.Vanguard.isVanguard) and rule 313.2 keeps it
  -- from ever changing zones, so nothing has to survive CR 400.7's fresh id.
  Monad.forM_ (Deck.vanguard deck) $ \printing -> do
    printingId <- State.state (Game.intern printing)
    Monad.void (createInCommandZone pid printingId)
  -- CR 903.7 / CR 103.4 / CR 103.4d / CR 902.4: the starting life total, which is
  -- the deck's business -- and the settings' and the seat count's and the
  -- vanguard's -- and so cannot be settled by emptyGame above.
  State.modify' $ \gs ->
    gs
      { GameState.players =
          -- CR 309.2: the dungeon card is recorded on the player and no object is
          -- minted for it, because dungeon cards begin OUTSIDE the game and
          -- outside the game is not a zone (CR 400.11). CR 701.49a is what brings
          -- it in; Pawl.Engine.Dungeon.enter is that rule.
          -- CR 400.11a: the sideboard is recorded on the player for the same
          -- reason and no object is minted for it either. CR 400.11c is what
          -- keeps anything else from reaching these until a card brings one in.
          Map.adjust (\p -> p {Player.life = startingLife (GameState.settings gs) (length (GameState.turnOrder gs)) (Deck.commander deck) (Vanguard.lifeModifierOf pid gs), Player.dungeons = Set.fromList dungeonIds, Player.outsideTheGame = Map.fromList sideboardIds}) pid (GameState.players gs)
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
    Monad.void (createInCommandZone pid printingId)
    State.modify' (Commander.designate pid printingId)

-- CR 903.6 / CR 902.3: mint one of this player's cards straight into the command
-- zone, which is where both of the cards a deck starts outside its library begin.
--
-- Shared by the two rather than written twice, because what it does is CR 400.1's
-- zone bookkeeping and neither rule's own business: Object.zone tracks the zone
-- sets, so it moves with them, and this is a hand-written move outside
-- Event.changeZone for createCard's own reason -- the game is being built, so
-- there is no CR 400.7 event to emit and nothing to trigger.
createInCommandZone :: PlayerId -> PrintingId.PrintingId -> Game ObjectId
createInCommandZone pid printingId = do
  oid <- createCard pid printingId
  State.modify' $ \gs ->
    let moved =
          Game.insertIntoZone Zone.Command LibraryPosition.defaultValue pid oid (Game.removeFromZones pid oid gs)
     in moved
          { GameState.objects =
              Map.adjust (\o -> o {Object.zone = Zone.Command}) oid (GameState.objects moved)
          }
  pure oid

newGame :: HandActionPerformer -> NonEmpty.NonEmpty (PlayerId, Deck.Deck) -> Game ()
newGame perform matchup = do
  -- CR 103.3: build and shuffle every library before any opening hand is drawn,
  -- so CR 103.5's declaration round sees settled libraries.
  Monad.forM_ (NonEmpty.toList matchup) $ \(pid, deck) -> do
    createDeck pid deck
    Event.shuffleLibrary pid
  Mulligan.openingHands perform (fmap fst (NonEmpty.toList matchup))

-- CR 712.21 / CR 108.2: a melded permanent is ONE object represented by TWO
-- cards, so a funnel that carries cards over -- CR 727.2's restart and CR
-- 729.5's subgame teardown -- has to take the two and not the one. Replaces each
-- such object with one card object per component, minting ids off `next`, and
-- answers the advanced supply. Every other object is passed through untouched.
--
-- Read through Game.componentsOf, a classifier over Source and never a case on
-- Source.OfMeld, so CR 730.3's merged permanent (#874) arrives through the same
-- door -- the posture Event.changeZoneAttaching's CR 712.21 split already takes.
--
-- No zone conjunct where that split carries one: this is not a zone change but
-- the rebuild of a game's whole object pool, and both callers empty every zone
-- anyway. Nothing outside the battlefield can hold a Source.OfMeld in any case
-- -- Event.meld is the only writer of one and it places the permanent there.
-- This is the second road off the battlefield beside changeZoneAttaching's, and
-- it keeps that invariant the same way: the melded object is dropped outright
-- rather than moved, so no other zone can come to hold one.
--
-- Object.newIncarnation is CR 400.7's forgetting, the same one `toLibraryCard`
-- performs below; only Object.source differs between the two cards, since each
-- represents itself again and not the permanent they were. CR 712.21a's
-- arrangement is not asked: both callers shuffle what they build (CR 103.5, CR
-- 729.5), so the order the two cards are placed in is not observable.
splitComponents :: ObjectId -> Map.Map ObjectId Object.Object -> (Map.Map ObjectId Object.Object, ObjectId)
splitComponents next objects =
  let bump (ObjectId.MkObjectId n) = ObjectId.MkObjectId (n + 1)
      mint obj (acc, oid) printingId =
        (Map.insert oid ((Object.newIncarnation obj) {Object.source = Source.OfCard printingId}) acc, bump oid)
      step (acc, oid) (key, obj) =
        let components = Game.componentsOf (Object.source obj)
         in if Seq.null components
              then (Map.insert key obj acc, oid)
              else Foldable.foldl' (mint obj) (acc, oid) components
   in List.foldl' step (Map.empty, next) (Map.toAscList objects)

-- CR 727.2 / 729.2: build every player's library from the EXISTING object pool
-- -- each player's owned cards, wherever they currently sit -- then shuffle and
-- draw opening hands (CR 103.5). Deliberately not newGame: it reuses the real
-- objects (ownership preserved) instead of minting fresh ones from Deck
-- definitions. Only Magic cards survive; an ability on the stack, a token, an
-- emblem or a trigger is not a card (CR 727.2 / 111.7), and a melded permanent
-- is two of them, which `splitComponents` makes so before the funnel runs (CR
-- 712.21 / CR 108.2). Shared by restart (CR 727) and subgames (CR 729).
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
  -- CR 727.2 / CR 712.21, BEFORE `gs` is read: the split has to be in the state
  -- rather than in the `let` below, because `commanderOf` and `vanguardIds` ask
  -- Commander.isCommander and Vanguard.isVanguard of `gs` -- so a component
  -- minted outside it could not be recognised as CR 903.6's commander. With it
  -- here, CR 903.9c's answer falls out of the existing funnel: the component
  -- card that is the commander goes to the command zone and its partner to the
  -- library.
  State.modify' $ \g ->
    let (split, next) = splitComponents (GameState.nextObjectId g) (GameState.objects g)
     in g {GameState.objects = split, GameState.nextObjectId = next}
  gs <- State.get
  let owners = Game.stillPlayingInOrder gs
      -- Cards in exile, and nothing else: CR 727.2's "all Magic cards" is what
      -- survives a rebuild at all, so an exemption is filtered by the same
      -- `isCard` test the funnel below applies rather than being able to smuggle
      -- a token or an emblem through it (CR 111.7).
      exempt =
        Map.keysSet
          (Map.filter isCard (Map.restrictKeys (GameState.objects gs) (Set.intersection exemptions (GameState.exile gs))))
      -- A melded permanent never reaches this: `splitComponents` above has
      -- already replaced it with its two component cards, each of which answers
      -- True here (CR 712.21 / CR 108.2).
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
      -- CR 313.2: "if a vanguard card would leave the command zone, it remains in
      -- the command zone", which is the exception CR 727.2's "all cards" and CR
      -- 729.2's own funnel would otherwise sweep into a library. So a vanguard is
      -- held back exactly as a commander is, and for a rule of its own rather than
      -- for rule 903.6's. CR 729.2b is the same holding read forwards.
      --
      -- Every one of them, where `commanderOf` takes one per player: CR 902.1's
      -- one card per player is a deck-construction rule pawl does not enforce
      -- (#940), and rule 313.2 is stated of each vanguard card rather than of the
      -- one the player designated, so there is nothing here to choose between.
      vanguardIds = Map.keysSet (Map.filterWithKey (\oid _ -> Vanguard.isVanguard oid gs) rebuilt)
      inCommandIds = Set.union commanderIds vanguardIds
      commandZoneCards = fmap toCommandCard (Map.restrictKeys rebuilt inCommandIds)
      cards = fmap toLibraryCard (Map.withoutKeys rebuilt inCommandIds)
      libraryOf pid = Seq.fromList (Map.keys (Map.filter (\obj -> Object.owner obj == pid) cards))
  State.put
    gs
      { GameState.objects = Map.unions [Map.restrictKeys (GameState.objects gs) exempt, cards, commandZoneCards],
        GameState.library = Map.fromList (fmap (\pid -> (pid, libraryOf pid)) owners),
        GameState.hand = Map.empty,
        GameState.graveyard = Map.empty,
        GameState.battlefield = mempty,
        GameState.phasedOut = mempty,
        GameState.exile = exempt,
        GameState.command = inCommandIds,
        GameState.stack = []
      }
  Monad.forM_ owners Event.shuffleLibrary
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
--
-- Takes the rebuilt game's settings and seat count rather than reading them off
-- the state it is rebuilding, because CR 903.12f's life total turns on the
-- REBUILT seat count: CR 727.1 restarts for the players who were in the game
-- when it ended, and CR 729.4 leaves a player outside the subgame, so either
-- rebuild can seat fewer players than the game it came from.
--
-- Takes the life modifier per player for the same reason it takes the settings:
-- CR 902.4's number is read off a card in the command zone, and this function is
-- handed a players map rather than the state that holds one. Both callers pass
-- Pawl.Engine.Vanguard.lifeModifierOf against the board the rebuild starts from,
-- where CR 313.2 has kept every vanguard sitting since the game began.
resetPlayers :: GameSettings.GameSettings -> Int -> (PlayerId -> Integer) -> Map.Map PlayerId Player.Player -> Map.Map PlayerId Player.Player
resetPlayers settings seats lifeModifier players =
  let reset pid player = case Player.status player of
        Status.Playing ->
          player
            { -- CR 903.7 again: Player.commander survives the rebuild (see
              -- Player.commanderCasts below), so a restarted Commander game
              -- starts back at forty rather than at twenty -- or at CR
              -- 903.12f's number, the settings carried in having survived too.
              --
              -- CR 902.4 survives it the same way and for a sharper reason: CR
              -- 727.2 and CR 729.2 rebuild the game from the cards, and CR 313.2
              -- forbids the vanguard card to leave the command zone, so the card
              -- the modifier is read off is still there to be read.
              Player.life = startingLife settings seats (Player.commander player) (lifeModifier pid),
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
              Player.commanderDamage = Map.empty,
              -- CR 727.1 / 729.2 again: a NEW game, so no dungeon has been
              -- completed in it. Player.dungeons is deliberately NOT reset beside
              -- it, for Player.commander's reason -- the supply outside the game
              -- comes from the deck, and the restart reuses the same decks.
              --
              -- Player.outsideTheGame is NOT reset either, and for a sharper
              -- reason than either of those: CR 727.2 involves in the new game
              -- every card involved in the restarted one, which is the cards a
              -- wish already brought IN -- and this field no longer holds those,
              -- having been spent as they were brought in. What is left in it is
              -- what stayed outside the game, which the restart does not reach.
              Player.completedDungeons = 0,
              Player.completedDungeonNames = Set.empty
            }
        Status.Departed _ -> player
   in Map.mapWithKey reset players

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
          { -- GameState.outsideObjects and GameState.broughtIn are deliberately
            -- absent from this update: CR 727.1 restarts the game, and a card
            -- outside it is still outside it, so both are carried over unchanged.
            -- GameState.settings is deliberately absent from this update, and
            -- so carries over: CR 727.1 restarts "following the procedures set
            -- forth in rule 103" for the same players with the same decks, and
            -- nothing in rule 727 turns an option off. CR 727.7 confirms the
            -- direction by naming an option the restarted game is still using.
            -- CR 902.4: the modifier is read off `gs`, the board the restart
            -- starts from, where CR 313.2 has kept every vanguard card since the
            -- game began -- and startGameFromCards leaves it there.
            GameState.players = resetPlayers (GameState.settings gs) (length order) (\pid -> Vanguard.lifeModifierOf pid gs) (GameState.players gs),
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
            GameState.detachedBindings = Map.empty,
            GameState.pendingEntryEffects = Seq.empty,
            GameState.enteringBeside = Set.empty,
            GameState.enteringSubjects = Set.empty,
            GameState.enteringCounters = Map.empty,
            GameState.playerEffects = [],
            GameState.blockRequirements = [],
            GameState.unregeneratables = [],
            GameState.blockProhibitions = [],
            GameState.attackProhibitions = [],
            GameState.attackRequirements = [],
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
            -- CR 724.1: the rebuilt game is not the one whose turn was ended.
            GameState.endTurnSignal = EndTurnSignal.Running,
            -- CR 104.4b: CR 727.1 ends the game that was being played, so the
            -- rebuilt one starts owing nobody a choice. The timestamp supply is
            -- preserved across the restart, so this is the supply's current
            -- value and not zero.
            GameState.lastChoice = GameState.nextTimestamp gs,
            GameState.drewFromEmpty = mempty,
            GameState.landsPlayed = mempty,
            GameState.drawsThisTurn = mempty,
            GameState.pendingControl = Map.empty,
            GameState.activeControl = Nothing,
            GameState.monarch = Nothing,
            GameState.initiative = Nothing,
            -- CR 727.1 / 731.1: the restarted game is a new game, which starts
            -- with neither designation however the ended one finished.
            GameState.daytime = Nothing,
            GameState.spellsCastLastTurn = 0,
            GameState.castsLastTurn = mempty,
            GameState.exiledUntilMonarch = Map.empty,
            GameState.movedUntilSourceLeaves = Map.empty,
            GameState.haunting = Map.empty,
            -- Kept for the CR 727.5 exemptions alone, and cleared for every
            -- other card: an exempted card never left exile, so what put it
            -- there is still true of it, and CR 727.4's additional instructions
            -- -- Karn's "then put THOSE CARDS onto the battlefield" -- are read
            -- after the rebuild and have nothing else left to read them off.
            -- Every other entry names a card the rebuild shuffled into a
            -- library, where CR 400.7 has already made it a different object.
            GameState.exiledWith = Map.restrictKeys (GameState.exiledWith gs) exempt,
            -- CR 406.4's pile, kept for exactly the cards the line above keeps
            -- and cleared with the rest: a pile is about a card still in exile.
            GameState.exilePiles = Map.restrictKeys (GameState.exilePiles gs) exempt,
            -- CR 727.1: the game that scheduled them has ended, so no extra
            -- turn survives into the new one.
            GameState.extraTurns = [],
            GameState.turnAnchor = Nothing
          }
  startGameFromCards perform exempt

-- CR 729.2: build a fresh subgame state from the parent's LIBRARY cards, plus
-- CR 729.2b's vanguards and CR 729.2c's commanders; no other main-game zone
-- enters. The object pool is
-- restricted to those objects; startGameFromCards (called by playSubgame) then
-- rebuilds each subgame library from that pool, puts each of those back in the
-- subgame's command zone (CR 903.6, CR 313.2), shuffles, and draws opening hands (CR
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
      -- subgame command zone", and CR 729.2b says the same of a vanguard card.
      -- Nothing ELSE in the main-game command zone moves -- CR 729.2's "no other
      -- cards in a main-game zone are moved" -- and their remaining sibling has no
      -- format here: CR 729.2a's supplementary decks of nontraditional cards are
      -- the attraction (#871), planar (#934) and scheme (#935) decks CR 100.2d
      -- names, neither of them implemented.
      --
      -- The command-zone residents pawl DOES have -- an emblem, and a dungeon a
      -- player has ventured into -- stay in the parent, which is what CR 729.2
      -- asks rather than an elision: `movedObjects` below restricts to the
      -- libraries, CR 729.2b's vanguards and CR 729.2c's commanders, so nothing
      -- else crosses.
      --
      -- The parent's own copy is deliberately left where it is: the parent is
      -- untouched while the subgame runs (CR 729.1a), and funnelBack drops these
      -- ids from it and refunds them from the subgame, exactly as it does for
      -- `libIds`.
      -- CR 729.2b beside CR 729.2c: a vanguard card crosses into the subgame on
      -- the same terms a commander does, and rule 729.2b states it without rule
      -- 729.2c's "if it's there" because CR 313.2 has kept it in the command zone
      -- all along.
      cmdIds = Set.filter (\oid -> Commander.isCommander oid parent || Vanguard.isVanguard oid parent) (GameState.command parent)
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
      -- CR 729.4: "all objects in the main game and all cards outside the main
      -- game are considered outside the subgame (except those specifically
      -- brought into the subgame)". The exception is `movedObjects` -- CR
      -- 729.2's libraries, CR 729.2b's vanguards and CR 729.2c's commanders --
      -- which are IN the
      -- subgame and so are excluded here. Only Source.OfCard objects: CR
      -- 400.11c's "spells and abilities that allow those cards to be brought
      -- into the game" is what a road into a subgame is, and a token or an
      -- emblem is not a card and cannot be brought in.
      --
      -- The parent's OWN outsideObjects ride along, which is CR 729.6's
      -- nesting: "the existing subgame becomes the main game in relation to
      -- the new subgame", so a card already outside the parent (itself
      -- possibly a subgame) is outside this one too. Neither funnelBack nor
      -- anything else touches `parent`'s own copy for the whole life of the game
      -- it spawns (CR 729.1a), and this union is the only place the set can
      -- GROW; applyCrossings shrinks it once that game has ended, dropping the
      -- entries it hands one level further out.
      --
      -- This reach is deliberately NOT scoped away from the parent's STACK: a
      -- wish can name the very spell that is resolving it, which CR 729.4 offers
      -- and CR 729.5 then finishes resolving "even if it was created by a spell
      -- card that's no longer on the stack". Pawl.Engine.Resolve.liveBindings is
      -- what makes that resumption read the slots the resolution had filled,
      -- proved by Pawl.OutsideTheGameSpec's "a wish that takes the resolving
      -- Shahrazad itself still finishes resolving with the winner it bound".
      outside =
        Map.union
          (Map.mapMaybe asOutside (Map.withoutKeys (GameState.objects parent) (Set.union libIds cmdIds)))
          (GameState.outsideObjects parent)
      -- CR 110.5's face-up/face-down status rides along with the printing, and
      -- is the one thing about the parent's object that does. It is not an
      -- effect or a definition, so CR 729.1b does not reach it: CR 708.2 makes
      -- the listed characteristics the object's own COPIABLE VALUES, which
      -- travel with the object the way CR 108.3's ownership does.
      -- Pawl.Engine.OutsideTheGame.eligible is what reads it, and
      -- Pawl.OutsideTheGameSpec's "CR 708.2/729.4 a manifested main-game sorcery
      -- is offered to a subgame's wish as a creature and not as a sorcery" is
      -- the proof.
      --
      -- Not implemented: a melded permanent, which this answers Nothing for. One
      -- OutsideObject names one printing and GameState.outsideObjects is keyed
      -- by the outer object's id, so recording it would have to drop a component
      -- (#2489). The other two funnels that sorted objects into cards split it
      -- instead; this is the site that still does not.
      asOutside obj = case Object.source obj of
        Source.OfCard printingId -> Just (OutsideObject.MkOutsideObject (Object.owner obj) printingId (Object.facing obj))
        _ -> Nothing
   in parent
        { GameState.objects = movedObjects,
          GameState.turnOrder = order,
          -- GameState.settings is deliberately absent from this update too, and
          -- so carries over from the parent: CR 729.2's subgame "proceeds like
          -- a normal game, following all other rules in rule 103", and CR
          -- 729.2c speaks of "a subgame of a Commander game" -- the subgame is
          -- played in the format the main game is. CR 729.1b keeps EFFECTS from
          -- crossing, not the options the game was started with.
          -- CR 902.4 / CR 729.2b: read off the PARENT, whose command zone still
          -- holds every vanguard card at this point -- `cmdIds` below is what
          -- moves them into the subgame, and it is computed from the same board.
          GameState.players = resetPlayers (GameState.settings parent) (length order) (\pid -> Vanguard.lifeModifierOf pid parent) (GameState.players parent),
          GameState.outsideObjects = outside,
          -- CR 729.4a: nothing has crossed into this subgame yet.
          GameState.broughtIn = Seq.empty,
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
          GameState.detachedBindings = Map.empty,
          GameState.pendingEntryEffects = Seq.empty,
          GameState.enteringBeside = Set.empty,
          GameState.enteringSubjects = Set.empty,
          GameState.enteringCounters = Map.empty,
          GameState.playerEffects = [],
          GameState.blockRequirements = [],
          GameState.unregeneratables = [],
          GameState.blockProhibitions = [],
          GameState.attackProhibitions = [],
          GameState.attackRequirements = [],
          GameState.ignoredAbilities = [],
          GameState.activePlayer = firstPlayer,
          GameState.phase = Turn.firstPhase,
          GameState.remaining = Turn.laterPhases,
          GameState.priority = Nothing,
          GameState.passes = 0,
          GameState.turnNumber = 1,
          GameState.result = Nothing,
          GameState.restartSignal = RestartSignal.Playing,
          -- CR 724.1: the subgame is its own game, so it starts running its own turn.
          GameState.endTurnSignal = EndTurnSignal.Running,
          -- CR 104.4b: the subgame is its own game, so it starts owing nobody a
          -- choice. Set to the INHERITED nextTimestamp rather than to zero,
          -- which the timestamp supply is not: a copied parent marker would
          -- have the subgame draw itself on entry for events at another level.
          GameState.lastChoice = GameState.nextTimestamp parent,
          GameState.drewFromEmpty = mempty,
          GameState.landsPlayed = mempty,
          GameState.drawsThisTurn = mempty,
          GameState.pendingControl = Map.empty,
          GameState.activeControl = Nothing,
          GameState.monarch = Nothing,
          GameState.initiative = Nothing,
          -- CR 729.1a / 731.1: the subgame is its own game, so it starts with
          -- neither designation and the main game's is untouched by it.
          GameState.daytime = Nothing,
          GameState.spellsCastLastTurn = 0,
          GameState.castsLastTurn = mempty,
          GameState.exiledUntilMonarch = Map.empty,
          GameState.movedUntilSourceLeaves = Map.empty,
          GameState.haunting = Map.empty,
          GameState.exiledWith = Map.empty,
          GameState.exilePiles = Map.empty,
          -- CR 729.1a: the subgame is its own game and starts from turn 1, so
          -- the main game's pending extra turns are not in it. Its own copy
          -- sits untouched in the outer frame, still waiting when the subgame
          -- ends.
          GameState.extraTurns = [],
          GameState.turnAnchor = Nothing
        }

-- CR 729.4a: the cards the subgame brought in have left the main game, so the
-- main game loses them. Applied HERE, once the subgame has ended, and that is
-- exact rather than late: the main game was discontinued for the whole subgame
-- (CR 729.1a), so no state-based action, no priority and no continuous-effect
-- recomputation could have read the board in between, so replaying
-- GameState.broughtIn in crossing order NOW -- as the running fold below, not as
-- one batch -- produces the same events, the same CR 608.2h last known
-- information and the same trigger order as applying each at the instant it
-- crossed. Rule 729.5's last sentence says the abilities that
-- triggered wait for the main game to resume anyway, so nothing is owed earlier.
--
-- Borrowed from Pawl.Engine.Departure.objectsLeaveWith, CR 800.4a's departure and
-- the tree's other road out of the game that reaches no zone: file CR 608.2h last
-- known information, remove from the zones, delete the object, record a
-- GameEvent.LeftTheGame. What is NOT borrowed is that function's shape. Rule
-- 800.4a's objects leave at ONE INSTANT, which is why it may read one board for
-- the whole batch and wrap the events in Event.simultaneouslyPure; these crossed
-- at different moments of the subgame, so this is a RUNNING fold instead -- one
-- iteration per crossing, in order, each reading the state the crossings before
-- it left behind.
--
-- That running board is load-bearing twice over, and both readings are
-- gameplay-visible:
--
--   * the last known information of the SECOND crossing must be projected on a
--     board the FIRST has already left. Alice's Glorious Anthem crosses, then her
--     Arbor Elf: at the instant the elf left, the anthem was already gone from the
--     main game, so its last known power is 1 and not 2. Projection.controllerOf
--     and Event.copiedSnapshot are read at the same moment for the same reason.
--
--   * each event is recorded AFTER its own crossing's deletion and BEFORE the
--     next one's, so Event.recordEvent's CR 603.10 sample of the battlefield
--     (GameState.battlefieldWhenTriggered) stamps that group with the board as it
--     stood then -- with every later crossing still on it. Sampling after the
--     whole batch would hide a main-game permanent that was there when the first
--     card left, and CR 729.5's last sentence is precisely where the ability it
--     owes would have gone on the stack.
--
-- So no Event.simultaneouslyPure: each crossing takes its own event group and CR
-- 603.10a's look-back triggers read them as a sequence rather than as one event.
--
-- Not implemented: the event is recorded for a permanent on the battlefield and
-- for nothing else, so a card that crossed out of a hand, a graveyard, a library
-- or exile files its last known information and records nothing -- narrower than
-- CR 729.4a's "abilities in the main game that trigger on objects leaving a
-- main-game zone" (#2463).
--
-- An id that is not one of this game's own objects came from further out: CR
-- 729.6 makes a subgame's own parent the main game of a subgame below it, so
-- subgameStateFrom hands a subgame its parent's outsideObjects along with the
-- parent's objects, and a wish two levels down can reach a card two games out.
-- Such an id is dropped from THIS game's outsideObjects and appended to THIS
-- game's broughtIn, so the frame one level out applies it. One level per frame,
-- never a reach past a parent -- rule 729.1a keeps a game from touching the state
-- of the game holding it, and this frame is the only one that holds both.
applyCrossings :: GameState -> GameState -> GameState
applyCrossings finalSub parent =
  let crossed = Foldable.toList (GameState.broughtIn finalSub)
      isOurs oid = Map.member oid (GameState.objects parent)
      (mine, further) = List.partition isOurs crossed
      -- The same deletion Departure.objectsLeaveWith performs, dropping the same
      -- carriers keyed on the departing id: its combat entries (CR 506.4 removes
      -- a permanent from combat as it leaves the battlefield), its
      -- exile-until-monarch entry, CR 610.3's return watch, CR 702.55b's haunt link, CR 607.2a's
      -- exiled-with link and CR 406.4's pile stamp. See there for why each is
      -- keyed on the KEY and not the value.
      leave g oid = case Map.lookup oid (GameState.objects g) of
        -- Unreachable: `mine` holds only ids GameState.objects answered for, and
        -- no id crosses twice -- OutsideTheGame.bringInFrom drops the entry it
        -- spent, so a second wish cannot reach the same card.
        Nothing -> g
        Just obj ->
          let g1 = Game.removeFromZones (Object.owner obj) oid g
              combat = GameState.combat g1
           in g1
                { GameState.objects = Map.delete oid (GameState.objects g1),
                  GameState.combat =
                    combat
                      { Combat.Type.attackers = Map.delete oid (Combat.Type.attackers combat),
                        Combat.Type.struckFirst = fmap (Set.delete oid) (Combat.Type.struckFirst combat)
                      },
                  GameState.exiledUntilMonarch = Map.delete oid (GameState.exiledUntilMonarch g1),
                  GameState.movedUntilSourceLeaves = Map.delete oid (GameState.movedUntilSourceLeaves g1),
                  GameState.haunting = Map.delete oid (GameState.haunting g1),
                  GameState.exiledWith = Map.delete oid (GameState.exiledWith g1),
                  GameState.exilePiles = Map.delete oid (GameState.exilePiles g1)
                }
      -- CR 608.2h, taken against `g` -- the running state, which is this game as
      -- it stands at the moment THIS card crosses. The object itself is still in
      -- it, exactly as Departure.objectsLeaveWith's own `filed` reads a board its
      -- subject has not left yet; what is different is that the cards which
      -- crossed EARLIER have already gone.
      filed g oid = case Map.lookup oid (GameState.objects g) of
        -- Unreachable, for `leave`'s reason.
        Nothing -> Nothing
        Just obj ->
          Just
            ( oid,
              LastKnown.MkLastKnown
                (Projection.project oid g)
                -- CR 613.1b: the projected controller as it left, who need not be
                -- its owner. The fallback is unreachable for the reason
                -- Departure.objectsLeaveWith gives.
                (Maybe.fromMaybe (Object.owner obj) (Projection.controllerOf oid g))
                -- CR 108.3, which no projection moves: read straight off the
                -- object, unlike the controller above.
                (Object.owner obj)
                (Object.source obj)
                (Object.counters obj)
                (Event.copiedSnapshot oid g)
                -- CR 303.4b / 301.5a with the arrow turned round, taken
                -- while the answer still exists (CR 603.10a).
                (Game.attachments oid g)
                (Object.chosenNames obj)
                (Game.isBlocking oid g)
                -- CR 310.9a, read straight off the object like the owner above:
                -- Nothing for everything that is not a battle.
                (Object.protector obj)
            )
      -- One crossing: file, delete, then record. The event LAST, so that
      -- Event.recordEvent's CR 603.10 sample is of the board immediately after
      -- this card left and before the next one does.
      --
      -- The event -- and CR 604.2's handover below it -- only for a permanent on
      -- the battlefield, which is the set Pawl.Types.GameEvent.LeftTheGame
      -- documents at its own constructor.
      -- Battlefield MEMBERSHIP rather than Object.zone, the way
      -- Departure.objectsLeaveWith reads the same question, since
      -- Pawl.Engine.Phasing takes a phased-out permanent out of that set and
      -- leaves its Object.zone alone (CR 702.26d).
      --
      -- So a PHASED-OUT permanent crosses without an event and hands nothing
      -- over, and that is CR 702.26b rather than an omission: excepting rules
      -- that specifically mention phased-out permanents, such a permanent "is
      -- treated as though it does not exist" and "can't affect or be affected by
      -- anything else in the game". CR 729.4a mentions none, so the main-game abilities it speaks of
      -- cannot see one leave -- the answer CR 702.26k spells out for the other
      -- road out of the game. The crossing itself still happens: CR 729.4 puts
      -- every main-game object outside the subgame, and the wish reaching for it
      -- belongs to the subgame, which CR 729.1a makes a different game from the
      -- one rule 702.26b speaks of. Pawl.SetupSpec's "CR 702.26b a phased-out
      -- permanent a subgame takes leaves the main game and triggers nothing" is
      -- the proof, against a phased-in leg differing in the phase-out alone.
      cross g oid =
        let noted = case filed g oid of
              Nothing -> g
              Just (key, value) -> g {GameState.lastKnown = Map.insert key value (GameState.lastKnown g)}
            gone = leave noted oid
         in if Set.member oid (GameState.battlefield g)
              then
                Event.recordEvent
                  (GameEvent.LeftTheGame oid)
                  gone {GameState.continuousEffects = handover g oid <> GameState.continuousEffects gone}
              else gone
      -- CR 604.2's override, the same one Departure.objectsLeaveWith performs on
      -- the other road out of the game: a permanent that leaves the GAME has left
      -- the battlefield, so a card whose text says its effect continues anyway --
      -- Titania's Song -- needs that effect handed to GameState.continuousEffects
      -- as it goes. Event.lingeringHandover is the single writer.
      --
      -- Read from `g`, the running board this crossing is leaving, for `filed`'s
      -- reason: what continues is what was applying at THIS instant, with every
      -- earlier crossing already gone. CR 611.2c then freezes the set.
      --
      -- Gated by the caller on GameState.battlefield membership, exactly as the
      -- event above is and for CR 702.26b's reason: a phased-out permanent was
      -- generating no effect there is anything to continue.
      --
      -- The controller is read the way `filed` reads it (CR 109.5 / CR 613.1b),
      -- and the Object.owner fallback is unreachable for `filed`'s reason.
      handover g oid = case Map.lookup oid (GameState.objects g) of
        -- Unreachable, for `leave`'s reason.
        Nothing -> []
        Just obj -> Event.lingeringHandover oid (Maybe.fromMaybe (Object.owner obj) (Projection.controllerOf oid g)) g
      applied = List.foldl' cross parent mine
   in applied
        { GameState.outsideObjects = Map.withoutKeys (GameState.outsideObjects applied) (Set.fromList further),
          GameState.broughtIn = GameState.broughtIn applied <> Seq.fromList further
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
-- high-water mark, and past it for each card a melded permanent's split minted
-- (CR 712.21).
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
-- real card's object outright (Sba's `ceaseToExist` reaches only a Source the
-- rules say is not a card -- a token under CR 704.5d, a copy of a spell under CR
-- 704.5e),
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
  let -- CR 729.5 / CR 712.21, the same split startGameFromCards performs, in a
      -- PURE function: the fresh ids are threaded off the MERGED supply by hand,
      -- so a component can collide with neither game's objects, and
      -- GameState.nextObjectId below is the advanced supply rather than that
      -- merge. Only the subgame's pool is split; the parent's own battlefield is
      -- untouched by rule 729.5 (CR 729.1a), and `recovered` draws from the
      -- parent's library and command zone, where no melded permanent can sit.
      supply = max (GameState.nextObjectId parent) (GameState.nextObjectId finalSub)
      (subObjects, nextId) = splitComponents supply (GameState.objects finalSub)
      -- A melded permanent never reaches this, for `splitComponents`' reason at
      -- startGameFromCards.
      isCard obj = case Object.source obj of
        Source.OfCard _ -> True
        _ -> False
      -- CR 400.7, exactly as startGameFromCards' own toLibraryCard: the second
      -- hand-written zone move outside Event.changeZone, performing that
      -- funnel's per-incarnation reset through the one shared function.
      toLibraryCard obj = (Object.newIncarnation obj) {Object.zone = Zone.Library}
      toCommandCard obj = (Object.newIncarnation obj) {Object.zone = Zone.Command}
      -- CR 729.5's own exclusion: "all traditional cards they own that are in the
      -- subgame OTHER THAN those in the subgame command zone". So the library
      -- funnel skips the subgame's command zone wholesale, and CR 729.5b and CR
      -- 729.5c take back out of it exactly the vanguards and the commanders. Any
      -- other CARD that ended there is covered by rule 729.5's "except as
      -- specified in rules 729.5a-c, all other objects in the subgame cease to
      -- exist", which is the literal reading; those two are the only cards a
      -- subgame can put in that zone anyway (CR 903.9a, CR 313.2), and an emblem
      -- there is not a card and never was in scope.
      subCmdIds = GameState.command finalSub
      returned = fmap toLibraryCard (Map.filter isCard (Map.withoutKeys subObjects subCmdIds))
      backFromSub =
        fmap
          toCommandCard
          (Map.filterWithKey (\oid obj -> isCard obj && (Commander.isCommander oid finalSub || Vanguard.isVanguard oid finalSub)) (Map.restrictKeys subObjects subCmdIds))
      oldLibIds =
        Set.fromList
          (concatMap (\pid -> Foldable.toList (Map.findWithDefault Seq.empty pid (GameState.library parent))) (GameState.turnOrder parent))
      -- The same expression as subgameStateFrom's `cmdIds`; see the invariant note
      -- there. These are the ids CR 729.2b and CR 729.2c moved out of the parent's
      -- command zone, so they are dropped from the parent and refunded from the
      -- subgame -- into the command zone by `backFromSub` if the card is still
      -- there (CR 729.5c's "if it's there"; a vanguard always is, by CR 313.2),
      -- and into the library by `returned` if it ended the subgame anywhere else
      -- (CR 729.5's first sentence).
      oldCmdIds = Set.filter (\oid -> Commander.isCommander oid parent || Vanguard.isVanguard oid parent) (GameState.command parent)
      movedIds = Set.union oldLibIds oldCmdIds
      ownersPresentInSub = Set.fromList (fmap Object.owner (Map.elems subObjects))
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
      -- CR 400.11b / 729.5: a card a wish inside the subgame took out of a
      -- player's sideboard pool "remains in the game until the game ends", and
      -- rule 729.5 puts it into their main-game library above. So the pool that
      -- comes back is the SUBGAME's, which OutsideTheGame.bringIn already spent
      -- it from; keeping the parent's would leave that card in the library and
      -- still outside the game at once.
      --
      -- Only that field. Every other one stays the parent's, which is CR 729.1b:
      -- nothing that happened in the subgame -- life lost, counters, a commander
      -- tax -- has any meaning in the main game. Nothing but bringIn writes the
      -- pool once a game is under way (createDeck is the only other writer, and
      -- neither startGameFromCards nor restartGame calls it), so what the subgame
      -- hands back differs from the parent's by exactly the cards it spent.
      --
      -- A player who DEPARTED inside the subgame keeps the parent's pool instead.
      -- CR 400.11b's second clause is the rule half: a card brought in remains in
      -- the game "until the game ends, their owner leaves the game, or a rule or
      -- effect removes them", so CR 800.4a's departure took every card they had
      -- wished in back OUT of the game. Where it goes then is pawl's model choice
      -- and not the rule's words -- the CR says only that it is no longer in the
      -- game -- and putting it back in the pool it came from is the choice made
      -- here, because that pool is exactly pawl's "outside the game" for a card
      -- with an owner (CR 400.11a) and the alternative loses the card: the
      -- objects are gone (Departure.objectsLeaveWith deleted them), so `returned`
      -- has nothing to put in their library either.
      --
      -- This arm restores only what the sideboard road spent (CR 400.11a and
      -- CR 400.11c). A card the same player took from the MAIN GAME instead
      -- (OutsideTheGame.bringInFrom)
      -- comes back nowhere: applyCrossings deleted the main game's copy, and
      -- objectsLeaveWith deleted the subgame's, so nothing represents it in
      -- either game. That is the two rules read together rather than an oversight
      -- -- CR 729.4a took it out of the main game and CR 800.4a took it out of
      -- the subgame -- and Pawl.SetupSpec pins it. CR 729.5's funnel offers no
      -- third answer: it takes "cards they own that are in the subgame", and
      -- CR 800.4a removed this one before the subgame ended, so the rule's
      -- Example never reaches it. Nor is the card lost -- CR 400.11 makes
      -- outside the game a non-zone, which is a coherent terminal state.
      --
      -- The two arms of the same road end differently because they must. This
      -- one returns a SIDEBOARD card to where CR 400.11a says it lives; pooling
      -- a maindeck card there would make it fetchable by a later MAIN-GAME wish
      -- and corrupt CR 100.4a's combined deck-and-sideboard accounting.
      --
      -- A player absent from the subgame's roster keeps the parent's pool too.
      -- The lookup cannot miss today -- subgameStateFrom rebuilds the players map
      -- from the parent's, departed seats included -- so that arm is the total
      -- reading of a partial map rather than a case with a rule behind it.
      carriedPools = Map.mapWithKey carryPool (GameState.players parent)
      carryPool pid player = case Map.lookup pid (GameState.players finalSub) of
        Nothing -> player
        Just inSub -> case Player.status inSub of
          Status.Departed _ -> player
          Status.Playing -> player {Player.outsideTheGame = Player.outsideTheGame inSub}
   in parent
        { GameState.objects = Map.unions [allReturned, toCommand, keptParentObjects],
          GameState.players = carriedPools,
          GameState.library = Map.fromList (fmap (\pid -> (pid, libraryOf pid)) (GameState.turnOrder parent)),
          -- CR 729.5c. The parent's other command-zone residents are untouched:
          -- CR 729.2c moved only the commanders, so only they can come back.
          GameState.command = Set.union (Set.difference (GameState.command parent) oldCmdIds) (Map.keysSet toCommand),
          GameState.nextObjectId = nextId,
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
