{-# LANGUAGE GADTs #-}

-- Covers Pawl.Engine.Setup and Pawl.Types.Deck: setup, deck composition, opening
-- hands, and the two funnels that rebuild a game's object pool -- CR 727's
-- restart and CR 729's subgame teardown.
module Pawl.SetupSpec where

import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Cards as Cards
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Departure as Departure
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Mulligan as Mulligan
import qualified Pawl.Engine.OutsideTheGame as OutsideTheGame
import qualified Pawl.Engine.Phasing as Phasing
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Engine.Turn as Turn
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Binding as Binding
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Deck as Deck
import qualified Pawl.Types.Departure as Departure.Type
import qualified Pawl.Types.Designation as Designation
import qualified Pawl.Types.ExilePlayPermission as ExilePlayPermission
import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameSettings as GameSettings
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.LastKnown as LastKnown
import qualified Pawl.Types.LoggedEvent as LoggedEvent
import qualified Pawl.Types.ManaSpending as ManaSpending
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OutsideObject as OutsideObject
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PhasedOut as PhasedOut
import qualified Pawl.Types.PlayPermissionOrigin as PlayPermissionOrigin
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.PrintingId as PrintingId
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Result as Result
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.Status as Status
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Timestamp as Timestamp
import qualified Pawl.Types.Zone as Zone

deckSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
deckSpec s registry = Spec.describe s "Deck" $ do
  Spec.it s "the red deck is 60 cards" $ do
    deck <- Cards.redDeck (S.printingOf s registry)
    Spec.assertEqWith s "size" (Setup.deckSize deck) 60

  Spec.it s "the green deck is 60 cards" $ do
    deck <- Cards.greenDeck (S.printingOf s registry)
    Spec.assertEqWith s "size" (Setup.deckSize deck) 60

  Spec.it s "the black deck is 60 cards" $ do
    deck <- Cards.blackDeck (S.printingOf s registry)
    Spec.assertEqWith s "size" (Setup.deckSize deck) 60

  Spec.it s "red deck composition" $ do
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    birdMaiden <- S.printingOf s registry "Bird Maiden"
    bolt <- S.printingOf s registry "Lightning Bolt"
    blaze <- S.printingOf s registry "Blaze"
    dragonFodder <- S.printingOf s registry "Dragon Fodder"
    chaosCharm <- S.printingOf s registry "Chaos Charm"
    deck <- Cards.redDeck (S.printingOf s registry)
    let m = Deck.cards deck
    Spec.assertEqWith s "mountains" (Map.lookup mountain m) (Just 36)
    Spec.assertEqWith s "pikers" (Map.lookup piker m) (Just 4)
    Spec.assertEqWith s "maidens" (Map.lookup birdMaiden m) (Just 4)
    Spec.assertEqWith s "bolts" (Map.lookup bolt m) (Just 4)
    Spec.assertEqWith s "blazes" (Map.lookup blaze m) (Just 4)
    Spec.assertEqWith s "dragon fodders" (Map.lookup dragonFodder m) (Just 4)
    Spec.assertEqWith s "chaos charms" (Map.lookup chaosCharm m) (Just 4)

  Spec.it s "green deck composition" $ do
    forest <- S.printingOf s registry "Forest"
    warMammoth <- S.printingOf s registry "War Mammoth"
    fog <- S.printingOf s registry "Fog"
    giantGrowth <- S.printingOf s registry "Giant Growth"
    serpentsGift <- S.printingOf s registry "Serpent's Gift"
    battlegrowth <- S.printingOf s registry "Battlegrowth"
    deck <- Cards.greenDeck (S.printingOf s registry)
    let m = Deck.cards deck
    Spec.assertEqWith s "forests" (Map.lookup forest m) (Just 36)
    Spec.assertEqWith s "mammoths" (Map.lookup warMammoth m) (Just 8)
    Spec.assertEqWith s "fogs" (Map.lookup fog m) (Just 4)
    Spec.assertEqWith s "giant growths" (Map.lookup giantGrowth m) (Just 4)
    Spec.assertEqWith s "serpent's gifts" (Map.lookup serpentsGift m) (Just 4)
    Spec.assertEqWith s "battlegrowths" (Map.lookup battlegrowth m) (Just 4)

  Spec.it s "black deck composition" $ do
    swamp <- S.printingOf s registry "Swamp"
    typhoidRats <- S.printingOf s registry "Typhoid Rats"
    drudgeSkeletons <- S.printingOf s registry "Drudge Skeletons"
    murder <- S.printingOf s registry "Murder"
    mindRot <- S.printingOf s registry "Mind Rot"
    instillInfection <- S.printingOf s registry "Instill Infection"
    deck <- Cards.blackDeck (S.printingOf s registry)
    let m = Deck.cards deck
    Spec.assertEqWith s "swamps" (Map.lookup swamp m) (Just 36)
    Spec.assertEqWith s "rats" (Map.lookup typhoidRats m) (Just 8)
    Spec.assertEqWith s "drudge skeletons" (Map.lookup drudgeSkeletons m) (Just 4)
    Spec.assertEqWith s "murders" (Map.lookup murder m) (Just 4)
    Spec.assertEqWith s "mind rots" (Map.lookup mindRot m) (Just 4)
    Spec.assertEqWith s "instill infections" (Map.lookup instillInfection m) (Just 4)

  Spec.it s "36 Mountains per player after a red-red setup" $ do
    gs <- setupState s registry
    Spec.assertEqWith s "mountains" (S.countByName (CardName.MkCardName $ Text.pack "Mountain") S.alice gs) 36

  Spec.it s "4 Bird Maidens per player after a red-red setup" $ do
    gs <- setupState s registry
    Spec.assertEqWith s "maidens" (S.countByName (CardName.MkCardName $ Text.pack "Bird Maiden") S.alice gs) 4

  Spec.it s "4 Pikers per player after a red-red setup" $ do
    gs <- setupState s registry
    Spec.assertEqWith s "pikers" (S.countByName (CardName.MkCardName $ Text.pack "Goblin Piker") S.bob gs) 4

  Spec.it s "4 Lightning Bolts per player after a red-red setup" $ do
    gs <- setupState s registry
    Spec.assertEqWith s "bolts" (S.countByName (CardName.MkCardName $ Text.pack "Lightning Bolt") S.alice gs) 4

  Spec.it s "4 Blazes per player after a red-red setup" $ do
    gs <- setupState s registry
    Spec.assertEqWith s "blazes" (S.countByName (CardName.MkCardName $ Text.pack "Blaze") S.alice gs) 4

  Spec.it s "4 Dragon Fodders per player after a red-red setup" $ do
    gs <- setupState s registry
    Spec.assertEqWith s "dragon fodders" (S.countByName (CardName.MkCardName $ Text.pack "Dragon Fodder") S.alice gs) 4

  Spec.it s "4 Chaos Charms per player after a red-red setup" $ do
    gs <- setupState s registry
    Spec.assertEqWith s "chaos charms" (S.countByName (CardName.MkCardName $ Text.pack "Chaos Charm") S.alice gs) 4

setupState :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m GameState.GameState
setupState s registry = do
  matchup <- S.redRed (S.printingOf s registry)
  pure (snd (Engine.runGamePure S.identityAnswer (Setup.emptyGame S.bothPlayers) (Setup.newGame S.performer matchup)))

setupSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
setupSpec s registry = Spec.describe s "Setup" $ do
  Spec.it s "120 objects after setup" $ do
    gs <- setupState s registry
    Spec.assertEqWith s "count" (Game.objectCount gs) 120

  Spec.it s "each library has 53 after opening draws" $ do
    gs <- setupState s registry
    Spec.assertEqWith s "library" (length (Game.zoneMembers Zone.Library S.alice gs)) 53

  Spec.it s "each hand has 7" $ do
    gs <- setupState s registry
    Spec.assertEqWith s "hand" (length (Game.zoneMembers Zone.Hand S.bob gs)) 7

  -- CR 103.5's fourteen draws happen before CR 103.8's first turn, so none of
  -- them is a draw made "this turn" -- the tally CR 121.1's per-turn count is
  -- read off starts the first turn empty, though the hands above are full.
  Spec.it s "CR 103.8 the opening hands are not draws made this turn" $ do
    gs <- setupState s registry
    Spec.assertEqWith s "no draws counted" (GameState.drawsThisTurn gs) Map.empty

  Spec.it s "active player is first in turn order" $ do
    gs <- setupState s registry
    Spec.assertEqWith s "active" (GameState.activePlayer gs) S.alice

  Spec.it s "runMatch derives the players from the matchup (#24)" $ do
    matchup <- S.redRed (S.printingOf s registry)
    let (result, final) = Engine.runMatchPure S.identityAnswer matchup
    Spec.assertBool s (Maybe.isJust (GameState.result final)) "has a result"
    Spec.assertEqWith s "both players have a life total" (Map.size (GameState.players final)) 2
    Spec.assertEqWith s "the result is the run's result" (GameState.result final) (Just result)

  Spec.it s "CR 122.1 a new player starts with no counters" $ do
    let gs = Setup.emptyGame S.bothPlayers
    Spec.assertEqWith s "empty" (fmap Player.counters (Map.lookup S.alice (GameState.players gs))) (Just Map.empty)

  Spec.it s "CR 400.1 a new game's command zone is empty" $ do
    let gs = Setup.emptyGame S.bothPlayers
    Spec.assertEqWith s "empty command" (GameState.command gs) mempty

  Spec.it s "CR 725.1 a new game has no monarch" $
    Spec.assertEqWith s "no monarch" (GameState.monarch (Setup.emptyGame S.bothPlayers)) Nothing

  Spec.it s "CR 800.5/806.3/103.1 emptyGame seats every player in the order given, starting player first" $ do
    let gs = Setup.emptyGame S.threePlayers
    Spec.assertEqWith s "seating order" (GameState.turnOrder gs) [S.alice, S.bob, S.carol]
    Spec.assertEqWith s "the starting player is active" (GameState.activePlayer gs) S.alice
    Spec.assertEqWith s "three seats in the players map" (Map.size (GameState.players gs)) 3

  Spec.it s "CR 800.1 a three-player game has three players still in it at the start" $
    Spec.assertEqWith s "all three playing" (Game.stillPlaying S.threePlayerGame) [S.alice, S.bob, S.carol]

  -- CR 903.7 / CR 103.4c against CR 103.4, on two decks identical but for
  -- Deck.commander -- so the designation is the sole cause of the difference.
  -- The positive control for Pawl.CommanderSpec's CR 903.10a group, whose victim
  -- has to be alive at 19 after twenty-one damage.
  Spec.it s "CR 903.7 a Commander deck starts its player at 40 life" $ do
    shimatsu <- S.printingOf s registry "Shimatsu the Bloodcloaked"
    let build commander =
          S.runPure S.identityAnswer (Setup.emptyGame S.bothPlayers) $
            Setup.createDeck S.alice Deck.MkDeck {Deck.cards = Map.empty, Deck.commander = commander, Deck.vanguard = Nothing, Deck.dungeons = Set.empty, Deck.sideboard = Map.empty}
    Spec.assertEqWith s "forty with a commander" (S.lifeOf S.alice (build (Just shimatsu))) (Just 40)
    Spec.assertEqWith s "twenty without" (S.lifeOf S.alice (build Nothing)) (Just 20)
    Spec.assertEqWith s "and bob, whose deck was never built, keeps CR 103.4's twenty" (S.lifeOf S.bob (build (Just shimatsu))) (Just 20)

  -- CR 903.12f against CR 903.7, on ONE deck built four ways. The Brawl option
  -- and the seat count are varied independently, so neither can be mistaken for
  -- the other: outside Brawl the seat count changes nothing, and inside it the
  -- designation changes nothing.
  Spec.it s "CR 903.12f a Brawl game starts its players at 25 at two seats and 30 at three" $ do
    shimatsu <- S.printingOf s registry "Shimatsu the Bloodcloaked"
    let build seats brawl =
          S.runPure S.identityAnswer (settingsOf brawl (Setup.emptyGame seats)) $
            Setup.createDeck S.alice Deck.MkDeck {Deck.cards = Map.empty, Deck.commander = Just shimatsu, Deck.vanguard = Nothing, Deck.dungeons = Set.empty, Deck.sideboard = Map.empty}
    Spec.assertEqWith s "two-player Brawl: 25" (S.lifeOf S.alice (build S.bothPlayers True)) (Just 25)
    Spec.assertEqWith s "multiplayer Brawl: 30" (S.lifeOf S.alice (build S.threePlayers True)) (Just 30)
    Spec.assertEqWith s "the same deck outside Brawl: CR 903.7's forty" (S.lifeOf S.alice (build S.bothPlayers False)) (Just 40)
    Spec.assertEqWith s "and forty at three seats too, so the seat count alone is not the cause" (S.lifeOf S.alice (build S.threePlayers False)) (Just 40)

  -- CR 727.1 restarts "following the procedures set forth in rule 103" for the
  -- same players, and CR 729.2's subgame "proceeds like a normal game": neither
  -- turns an option off, which is why Setup.restartGame and
  -- Setup.subgameStateFrom leave GameState.settings out of their updates. Read
  -- at gameplay level -- CR 903.12f's twenty-five, which only the carried
  -- setting can produce -- rather than off the field.
  Spec.it s "CR 727.1 / 729.2 a rebuilt Brawl game is still a Brawl game" $ do
    shimatsu <- S.printingOf s registry "Shimatsu the Bloodcloaked"
    mountain <- S.printingOf s registry "Mountain"
    let deck = Deck.MkDeck {Deck.cards = Map.singleton mountain 10, Deck.commander = Just shimatsu, Deck.vanguard = Nothing, Deck.dungeons = Set.empty, Deck.sideboard = Map.empty}
        built brawl =
          S.runPure S.identityAnswer (settingsOf brawl (Setup.emptyGame S.bothPlayers)) $
            Setup.createDeck S.alice deck
        restarted brawl = S.runPure S.identityAnswer (built brawl) (Setup.restartGame S.performer Set.empty S.alice)
        subgame brawl = S.runPure S.identityAnswer (Setup.subgameStateFrom S.alice (built brawl)) (Setup.startGameFromCards S.performer Set.empty)
    Spec.assertEqWith s "CR 903.12f survives the restart" (S.lifeOf S.alice (restarted True)) (Just 25)
    Spec.assertEqWith s "and the restart of an ordinary Commander game still gives forty" (S.lifeOf S.alice (restarted False)) (Just 40)
    Spec.assertEqWith s "CR 903.12f survives into the subgame" (S.lifeOf S.alice (subgame True)) (Just 25)
    Spec.assertEqWith s "and the subgame of an ordinary Commander game still gives forty" (S.lifeOf S.alice (subgame False)) (Just 40)

-- CR 800.2: put this game's options where the test wants them. The seat count
-- is untouched, so the Brawl legs above differ from their controls in exactly
-- one thing.
settingsOf :: Bool -> GameState.GameState -> GameState.GameState
settingsOf brawl gs = gs {GameState.settings = (GameState.settings gs) {GameSettings.brawl = brawl}}

greenBlackSetup :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m GameState.GameState
greenBlackSetup s registry = do
  matchup <- S.greenBlack (S.printingOf s registry)
  pure (snd (Engine.runGamePure S.identityAnswer (Setup.emptyGame S.bothPlayers) (Setup.newGame S.performer matchup)))

greenBlackSetupSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
greenBlackSetupSpec s registry = Spec.describe s "GreenBlackSetup" $ do
  Spec.it s "alice's green deck deals 36 Forests" $ do
    gs <- greenBlackSetup s registry
    Spec.assertEqWith s "forests" (S.countByName (CardName.MkCardName $ Text.pack "Forest") S.alice gs) 36

  Spec.it s "bob's black deck deals 36 Swamps" $ do
    gs <- greenBlackSetup s registry
    Spec.assertEqWith s "swamps" (S.countByName (CardName.MkCardName $ Text.pack "Swamp") S.bob gs) 36

  Spec.it s "green-black setup conserves 120 objects" $ do
    gs <- greenBlackSetup s registry
    Spec.assertEqWith s "count" (Game.objectCount gs) 120

-- Add n Mountains to pid's battlefield, discarding the ids (used to bulk up a
-- pool of owned cards). replicate n () avoids a list comprehension (CLAUDE.md).
addMany :: Printing.Printing -> Int -> PlayerId -> GameState.GameState -> GameState.GameState
addMany mountain n pid gs =
  List.foldl' (\g _ -> snd (S.addCreature mountain pid g)) gs (replicate n ())

-- CR 400.7's per-incarnation state, all of it at once and every field set to
-- something Object.newIncarnation would erase, so a path that forgets to erase
-- one leaves a trace. The counterpart to `forgotten` below: dirty every card in
-- the pool, run the path, then assert nothing survived the move.
--
-- Deliberately hand-written rather than derived from Object.newIncarnation --
-- if this listed the same fields the implementation does, a field the
-- implementation missed would be missing here too and the assertion would pass
-- for the wrong reason.
dirtied :: PlayerId -> Object.Object -> Object.Object
dirtied pid object =
  object
    { Object.tapped = TapState.Tapped,
      Object.exiledFaceDown = True,
      Object.damage = 1,
      Object.sickness = Sickness.Settled pid,
      Object.bindings = Map.singleton (SlotName.MkSlotName (Text.pack "target")) Binding.empty,
      Object.counters = Map.singleton CounterKind.PlusOnePlusOne 1,
      Object.attachedTo = Just (Recipient.ToPlayer pid),
      Object.enteredUnder = Just pid,
      Object.chosenColor = Just Color.Blue,
      Object.chosenSubtype = Just Subtype.Forest,
      Object.chosenNames = Set.singleton (CardName.MkCardName (Text.pack "Mountain")),
      Object.chosenPlayer = Just pid,
      Object.face = Just (CardName.MkCardName (Text.pack "Mountain")),
      Object.turnedOverAt = Just (Timestamp.MkTimestamp 1),
      Object.worldSince = Just (Timestamp.MkTimestamp 2),
      -- The origin is inert here: newIncarnation clears playableFromExile
      -- outright, so no value of it makes this fixture more or less honest.
      Object.playableFromExile = Just (ExilePlayPermission.MkExilePlayPermission pid S.noSource Expiry.Never ManaSpending.AnyType PlayPermissionOrigin.Granted),
      Object.ringBearerFor = Just pid,
      Object.protector = Just pid,
      Object.unlockedHalves = Set.singleton (CardName.MkCardName (Text.pack "Steaming Sauna")),
      Object.designations = Set.singleton Designation.Renowned,
      Object.detainedUntil = Set.singleton pid,
      Object.goadedBy = Set.empty,
      Object.doesNotUntapNext = True,
      Object.exertedBy = Set.singleton pid
    }

-- CR 400.7: has this object no memory of a previous existence? Applying the
-- forgetting again changes nothing exactly when the move already applied it in
-- full, so this stays honest as fields are added -- unlike a list of field
-- comparisons, which would have to be extended by hand alongside Object.
--
-- ONE BLIND SPOT, since the predicate is Object.newIncarnation's own fixed
-- point: a field that function never resets is invisible here however dirty
-- `dirtied` above leaves it. Deleting a line from newIncarnation therefore
-- leaves this green.
forgotten :: Object.Object -> Bool
forgotten object = Object.newIncarnation object == object

-- Settle the CR 117.5 scan and then run the priority loop, so a trigger a
-- crossing put on the stack actually resolves. Pawl.DepartureSpec's namesake for
-- the other road out of the game.
resolveTriggers :: GameState.GameState -> GameState.GameState
resolveTriggers gs = S.runPure S.identityAnswer (S.runPure S.identityAnswer gs Engine.settleForPriority) Engine.priorityLoop

-- The token Thragtusk's leaves-the-battlefield ability makes, which is how the
-- two crossing legs below say whether that ability fired.
beastToken :: CardName.CardName
beastToken = CardName.MkCardName (Text.pack "Beast Token")

restartSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
restartSpec s registry = Spec.describe s "restart (CR 727)" $ do
  Spec.it s "startGameFromCards: libraries are rebuilt from the existing owned cards, hands drawn" $ do
    -- alice and bob each own 8 cards, all currently on the battlefield. After
    -- startGameFromCards each has a 7-card hand and a 1-card library, the
    -- battlefield is empty, and ownership is unchanged (CR 727.2 / 103.5).
    mountain <- S.printingOf s registry "Mountain"
    let g0 = Setup.emptyGame S.bothPlayers
        g1 = addMany mountain 8 S.alice g0
        g2 = addMany mountain 8 S.bob g1
        after = snd (Engine.runGamePure S.identityAnswer g2 (Setup.startGameFromCards S.performer Set.empty))
        libSize pid = length (Game.zoneMembers Zone.Library pid after)
    Spec.assertEqWith s "alice drew a 7-card opening hand" (S.handSize S.alice after) 7
    Spec.assertEqWith s "bob drew a 7-card opening hand" (S.handSize S.bob after) 7
    Spec.assertEqWith s "alice's library holds the remaining owned card" (libSize S.alice) 1
    Spec.assertEqWith s "bob's library holds the remaining owned card" (libSize S.bob) 1
    Spec.assertEqWith s "the battlefield is empty after the rebuild" (Set.null (GameState.battlefield after)) True
    Spec.assertEqWith s "every rebuilt object is owned by alice or bob (ownership preserved)" (all (\o -> Object.owner o == S.alice || Object.owner o == S.bob) (Map.elems (GameState.objects after))) True

  -- CR 727.2 carries CARDS into the restarted game, and CR 108.2 makes each half
  -- of a melded permanent one: "one object represented by two cards" (CR
  -- 701.42a). So the permanent itself must not come back and both of its cards
  -- must, which is CR 712.21's split read at a funnel that is not a zone change.
  --
  -- alice's five Mountains ride along as the discriminator on the other side: a
  -- rebuild that dropped every object would satisfy the "no melded permanent
  -- survives" assertion on its own.
  Spec.it s "CR 727.2/712.21 a restart carries both cards of a melded permanent into the new game" $ do
    battlements <- S.printingOf s registry "Hanweir Battlements"
    garrison <- S.printingOf s registry "Hanweir Garrison"
    mountain <- S.printingOf s registry "Mountain"
    let (meldedId, board) = meldedBoard (Setup.emptyGame S.bothPlayers) battlements garrison mountain
        components = componentsOn meldedId board
        after = snd (Engine.runGamePure S.identityAnswer board (Setup.restartGame S.performer Set.empty S.alice))
        -- CR 103.5's opening draw runs inside the restart, so a card that came
        -- back is in the library or the hand and which one is the shuffle's
        -- business rather than this rule's.
        hers = Game.zoneMembers Zone.Library S.alice after <> Game.zoneMembers Zone.Hand S.alice after
        sources = sourcesOf hers after
    -- Setup, read off the board going IN, so nothing here can absorb a mutation
    -- of the funnel under test.
    Spec.assertEqWith s "setup: the pair really melded into one permanent representing two cards" (length components) 2
    Spec.assertEqWith s "CR 727.2/712.21 both cards representing the melded permanent are in alice's rebuilt game" (filter (\p -> elem (Source.OfCard p) sources) components) components
    Spec.assertEqWith s "and no object left in the new game is represented by two cards: the permanent is no card at all (CR 108.2)" (all (null . Game.componentsOf . Object.source) (Map.elems (GameState.objects after))) True
    Spec.assertEqWith s "alice's five Mountains came back beside them" (length hers) 7

  Spec.it s "CR 400.7: startGameFromCards puts each card into the library as a NEW object, with no per-incarnation state" $ do
    -- Every one of alice's and bob's 8 owned cards carries the full set of
    -- per-incarnation state -- a chosen colour, a chosen land type, a chosen
    -- name, an attachment, an entry controller, damage, counters, a binding, a
    -- face, an exile permission. startGameFromCards is a hand-written zone
    -- move outside Event.changeZone, so it has to erase all of it (#653).
    --
    -- Dirtying EVERY card rather than one is what makes this deterministic:
    -- the rebuild shuffles and draws opening hands, so which cards stay in the
    -- library is not knowable here. The drawn ones go through changeZone and
    -- come out clean either way; the ones that stay are the ones under test.
    mountain <- S.printingOf s registry "Mountain"
    let g0 = Setup.emptyGame S.bothPlayers
        g1 = addMany mountain 8 S.bob (addMany mountain 8 S.alice g0)
        g2 = g1 {GameState.objects = Map.map (\o -> dirtied (Object.owner o) o) (GameState.objects g1)}
        after = snd (Engine.runGamePure S.identityAnswer g2 (Setup.startGameFromCards S.performer Set.empty))
    -- The discriminator: without this, an assertion over an already-clean pool
    -- would pass no matter what startGameFromCards does.
    Spec.assertEqWith s "the pool going in genuinely carried per-incarnation state" (not (all forgotten (Map.elems (GameState.objects g2)))) True
    Spec.assertEqWith s "every rebuilt object forgot its previous existence" (all forgotten (Map.elems (GameState.objects after))) True

  -- CR 903.9c is the shape the restart's own rule 903.6 needs for a melded
  -- commander: "that permanent and each component representing it that isn't a
  -- commander are put into the appropriate zone, and the card that represents it
  -- and is a commander is put into the command zone". Nothing extra implements
  -- it -- the split runs before startGameFromCards reads the pool, so
  -- Commander.isCommander recognises the component card and the existing CR
  -- 903.6 hold-back does the rest.
  --
  -- A PAIR of restarts differing in the designation alone, so the command zone's
  -- one card cannot be read as something a restart does to any melded permanent.
  Spec.it s "CR 903.6/903.9c a restarted Commander game puts the melded commander's own card into the command zone and its partner into the deck" $ do
    battlements <- S.printingOf s registry "Hanweir Battlements"
    garrison <- S.printingOf s registry "Hanweir Garrison"
    mountain <- S.printingOf s registry "Mountain"
    let (meldedId, board) = meldedBoard (Setup.emptyGame S.bothPlayers) battlements garrison mountain
        components = componentsOn meldedId board
        designating printingId gs = gs {GameState.players = Map.adjust (\p -> p {Player.commander = Just printingId}) S.alice (GameState.players gs)}
        restarted gs = snd (Engine.runGamePure S.identityAnswer gs (Setup.restartGame S.performer Set.empty S.alice))
        deckOf gs = Game.zoneMembers Zone.Library S.alice gs <> Game.zoneMembers Zone.Hand S.alice gs
    case components of
      [first, second] -> do
        let designated = restarted (designating first board)
            control = restarted board
        Spec.assertEqWith s "CR 903.9c the command zone holds the component card alice designated, and only it" (sourcesOf (Set.toAscList (GameState.command designated)) designated) [Source.OfCard first]
        Spec.assertEqWith s "CR 903.6 its partner is in her deck instead" (elem (Source.OfCard second) (sourcesOf (deckOf designated) designated)) True
        Spec.assertEqWith s "and the designated card is not also in her deck" (elem (Source.OfCard first) (sourcesOf (deckOf designated) designated)) False
        -- The control leg: the same board, the same restart, no designation.
        Spec.assertEqWith s "with neither half designated the command zone stays empty (CR 903.6)" (Set.toAscList (GameState.command control)) []
        Spec.assertEqWith s "and both cards are in her deck" (filter (\c -> elem (Source.OfCard c) (sourcesOf (deckOf control) control)) components) components
      _ -> Spec.assertFailure s "the Hanweir pair should meld into a permanent representing two cards"

  Spec.it s "CR 727.1a: the starting player is the restart's controller, at the head of the turn order" $ do
    -- Two restarts of the same board, controlled by different players: the
    -- active player and the head of the turn order follow the controller.
    mountain <- S.printingOf s registry "Mountain"
    let g0 = addMany mountain 8 S.bob (addMany mountain 8 S.alice (Setup.emptyGame S.bothPlayers))
        byBob = snd (Engine.runGamePure S.identityAnswer g0 (Setup.restartGame S.performer Set.empty S.bob))
        byAlice = snd (Engine.runGamePure S.identityAnswer g0 (Setup.restartGame S.performer Set.empty S.alice))
    Spec.assertEqWith s "bob restarted: bob is the new active player" (GameState.activePlayer byBob) S.bob
    Spec.assertEqWith s "bob restarted: bob heads the turn order" (Maybe.listToMaybe (GameState.turnOrder byBob)) (Just S.bob)
    Spec.assertEqWith s "alice restarted: alice is the new active player" (GameState.activePlayer byAlice) S.alice
    Spec.assertEqWith s "alice restarted: alice heads the turn order" (Maybe.listToMaybe (GameState.turnOrder byAlice)) (Just S.alice)

  Spec.it s "CR 727.2: every owned card returns to its owner (library or hand), regardless of prior zone" $ do
    -- alice owns 8 cards, one on the battlefield; bob owns 8, one moved to his
    -- graveyard. CR 400.7 gives drawn cards FRESH ids (Event.changeZone mints a
    -- new object on a zone change), so a specific pre-restart ObjectId need not
    -- survive an opening draw -- CR 727.2 preserves OWNERSHIP, not object ids.
    -- Assert on per-owner counts: after the restart every owned card is in that
    -- owner's library or hand, none on the battlefield or in a graveyard, and
    -- bob's graveyard card is proven to return by his count staying 8.
    mountain <- S.printingOf s registry "Mountain"
    let g0 = Setup.emptyGame S.bothPlayers
        (_aId, g1) = S.addCreature mountain S.alice g0
        (bId, g2) = S.addCreature mountain S.bob g1
        g3 = addMany mountain 7 S.alice (addMany mountain 7 S.bob g2)
        -- move bob's card to his graveyard, to prove zone-independence.
        g4 = snd (Engine.runGamePure S.identityAnswer g3 (Event.changeZone bId Zone.Graveyard))
        after = snd (Engine.runGamePure S.identityAnswer g4 (Setup.restartGame S.performer Set.empty S.alice))
        ownedCount pid = length (filter (\o -> Object.owner o == pid) (Map.elems (GameState.objects after)))
        libHandCount pid = length (Game.zoneMembers Zone.Library pid after) + length (Game.zoneMembers Zone.Hand pid after)
    Spec.assertEqWith s "alice still owns all 8 of her cards" (ownedCount S.alice) 8
    Spec.assertEqWith s "bob still owns all 8 of his cards (incl. the one from his graveyard)" (ownedCount S.bob) 8
    Spec.assertEqWith s "all of alice's cards are in her library or hand" (libHandCount S.alice) 8
    Spec.assertEqWith s "all of bob's cards are in his library or hand" (libHandCount S.bob) 8
    Spec.assertEqWith s "no card is left on the battlefield" (Set.null (GameState.battlefield after)) True
    Spec.assertEqWith s "no graveyard survives the restart" (all null (Map.elems (GameState.graveyard after))) True

  Spec.it s "CR 727.4: the restart settles just before the first untap step, no priority, turn 1, life reset" $ do
    -- Knock bob down to 5 life and give him 3 poison counters before the
    -- restart, so the "back to 20 life / no counters" assertions below are
    -- load-bearing -- Setup.emptyGame already starts players at 20 life with
    -- no counters, so without this mutation the assertions would pass even
    -- if resetPlayer did nothing.
    mountain <- S.printingOf s registry "Mountain"
    let g0 = addMany mountain 8 S.bob (addMany mountain 8 S.alice (Setup.emptyGame S.bothPlayers))
        g1 = S.addPlayerCounter PlayerCounterKind.Poison 3 S.bob g0
        g2 = g1 {GameState.players = Map.adjust (\p -> p {Player.life = 5}) S.bob (GameState.players g1)}
        after = snd (Engine.runGamePure S.identityAnswer g2 (Setup.restartGame S.performer Set.empty S.bob))
    Spec.assertEqWith s "phase is the first turn's untap step" (GameState.phase after) Turn.firstPhase
    Spec.assertEqWith s "no player holds priority" (GameState.priority after) Nothing
    Spec.assertEqWith s "it is turn 1" (GameState.turnNumber after) 1
    Spec.assertEqWith s "the stack is empty" (GameState.stack after) []
    Spec.assertEqWith s "alice is back to 20 life" (S.lifeOf S.alice after) (Just 20)
    Spec.assertEqWith s "bob is back to 20 life" (S.lifeOf S.bob after) (Just 20)
    Spec.assertEqWith s "CR 103.4/727.1: bob's life reset to 20" (S.lifeOf S.bob after) (Just 20)
    Spec.assertEqWith s "bob's poison counters cleared on restart" (S.playerCounterOf PlayerCounterKind.Poison S.bob after) 0

  Spec.it s "CR 727.3: a player owning fewer than seven cards loses at the next SBA check" $ do
    -- bob owns only 3 cards; drawing an opening hand of 7 draws from an empty
    -- library, flagging drewFromEmpty, so the existing SBA path makes bob lose
    -- and alice win. (In live play this fires at the first upkeep, CR 727.3;
    -- here it is asserted at the next explicit SBA check.)
    mountain <- S.printingOf s registry "Mountain"
    let g0 = addMany mountain 3 S.bob (addMany mountain 8 S.alice (Setup.emptyGame S.bothPlayers))
        afterRestart = snd (Engine.runGamePure S.identityAnswer g0 (Setup.restartGame S.performer Set.empty S.alice))
        afterSba = snd (Engine.runGamePure S.identityAnswer afterRestart Engine.checkSba)
    Spec.assertEqWith s "bob drew from an empty library during the opening draw" (Set.member S.bob (GameState.drewFromEmpty afterRestart)) True
    Spec.assertEqWith s "CR 727.3: bob loses, alice wins at the SBA check" (GameState.result afterSba) (Just (Result.Won S.alice))

  Spec.it s "CR 727.1/729.4 #147: a rebuild does not revive a player who had already left" $ do
    -- CR 727.1: "All players in that game when it ended then start a new game
    -- following the procedures set forth in rule 103" -- bob left BEFORE it
    -- ended, so he is not one of them. CR 729.4 says the same for a subgame:
    -- "All players not currently in the subgame are considered outside the
    -- subgame." Today both rebuild paths set every player's status to Playing
    -- unconditionally, so bob comes back at 20 life.
    --
    -- Alice's life is knocked down too, so "alice is back to 20" is a real
    -- assertion and not something Setup.emptyGame already produced.
    let g0 = S.departs Departure.Type.Conceded S.bob (Setup.emptyGame S.threePlayers)
        g1 =
          g0
            { GameState.players =
                Map.adjust (\p -> p {Player.life = 3}) S.bob (Map.adjust (\p -> p {Player.life = 5}) S.alice (GameState.players g0))
            }
        afterRestart = S.runPure S.identityAnswer g1 (Setup.restartGame S.performer Set.empty S.alice)
        sub = Setup.subgameStateFrom S.alice g1
        statusOf pid gs = fmap Player.status (Map.lookup pid (GameState.players gs))
    Spec.assertEqWith s "restart: bob is still departed" (statusOf S.bob afterRestart) (Just (Status.Departed Departure.Type.Conceded))
    Spec.assertEqWith s "restart: and nothing of his is reset" (S.lifeOf S.bob afterRestart) (Just 3)
    Spec.assertEqWith s "restart: alice starts a new game, still playing" (statusOf S.alice afterRestart) (Just Status.Playing)
    Spec.assertEqWith s "restart: at 20 life" (S.lifeOf S.alice afterRestart) (Just 20)
    Spec.assertEqWith s "subgame: bob is still departed" (statusOf S.bob sub) (Just (Status.Departed Departure.Type.Conceded))
    Spec.assertEqWith s "subgame: and nothing of his is reset" (S.lifeOf S.bob sub) (Just 3)
    Spec.assertEqWith s "subgame: alice is playing at 20 life" (S.lifeOf S.alice sub) (Just 20)

  Spec.it s "CR 727.1 #147: a restart rebuilds only the players who were in the game when it ended" $ do
    -- CR 727.1: "All players in that game when it ended then start a new
    -- game." Bob left first, so the new game has two seats. Today he keeps
    -- his seat in the rebuilt turn order, and startGameFromCards therefore
    -- gives him a library, a shuffle and a 7-card opening hand.
    mountain <- S.printingOf s registry "Mountain"
    let g0 = addMany mountain 8 S.carol (addMany mountain 8 S.bob (addMany mountain 8 S.alice (Setup.emptyGame S.threePlayers)))
        g1 = S.departs Departure.Type.Conceded S.bob g0
        after = snd (Engine.runGamePure S.identityAnswer g1 (Setup.restartGame S.performer Set.empty S.alice))
        libSizeOf pid = length (Game.zoneMembers Zone.Library pid after)
    Spec.assertEqWith s "two seats in the rebuilt order, in their seating order" (GameState.turnOrder after) [S.alice, S.carol]
    Spec.assertEqWith s "alice starts it (CR 727.1a)" (GameState.activePlayer after) S.alice
    Spec.assertBool s (List.elem (GameState.activePlayer after) (GameState.turnOrder after)) "the active player is one of the rebuilt seats (totality)"
    Spec.assertEqWith s "bob has no library" (libSizeOf S.bob) 0
    Spec.assertEqWith s "bob drew no opening hand" (S.handSize S.bob after) 0
    Spec.assertEqWith s "alice drew hers" (S.handSize S.alice after) 7
    Spec.assertEqWith s "carol drew hers" (S.handSize S.carol after) 7
    Spec.assertEqWith s "CR 800.1: the rebuilt game has two seats, so no free mulligan" (Mulligan.freeMulligans after) 0
    Spec.assertEqWith s "CR 104.2a: two survivors, so the rebuild decides nothing" (GameState.result after) Nothing
    -- #172's orphan, retired. Before CR 800.4a's object removal, bob's eight
    -- cards stayed in GameState.objects after the restart -- in no library and
    -- undrawable, but counted. Twenty-four objects for sixteen cards in play.
    Spec.assertEqWith s "no orphaned objects survive the rebuild" (Game.objectCount after) 16

-- Move a player's pool onto their LIBRARY (subgameStateFrom reads the library
-- zone, not the battlefield). addMany places cards on the battlefield; this
-- helper then relocates a player's battlefield objects into their library so a
-- test can set up a known library size without drawing. replicate/fold avoid a
-- list comprehension (CLAUDE.md).
poolToLibrary :: PlayerId -> GameState.GameState -> GameState.GameState
poolToLibrary pid gs =
  let mine = Map.keys (Map.filter (\o -> Object.owner o == pid) (GameState.objects gs))
      onLibrary o = o {Object.zone = Zone.Library}
   in gs
        { GameState.objects = List.foldl' (flip (Map.adjust onLibrary)) (GameState.objects gs) mine,
          GameState.battlefield = Set.difference (GameState.battlefield gs) (Set.fromList mine),
          GameState.library = Map.insert pid (Seq.fromList mine) (GameState.library gs)
        }

-- The name printed on Hanweir Battlements' combined back face, which is what the
-- permanent that pair melds into answers to (CR 712.8g).
townshipName :: CardName.CardName
townshipName = CardName.MkCardName (Text.pack "Hanweir, the Writhing Township")

-- alice's Hanweir Battlements and Hanweir Garrison put onto `base` with the five
-- Mountains that pay for it, and the Battlements' printed melding ability
-- activated and resolved: the melded permanent's id, and the board it stands on.
-- The real activation rather than the Meld opcode, so what the two cases below
-- hand these funnels is a board the game can reach.
--
-- Duplicated from Pawl.MeldSpec's `meldedThrough` rather than hoisted into
-- Pawl.Support, which rebuilds every spec in the tree.
meldedBoard :: GameState.GameState -> Printing.Printing -> Printing.Printing -> Printing.Printing -> (Maybe ObjectId.ObjectId, GameState.GameState)
meldedBoard base battlements garrison mountain =
  let (battlementsId, g1) = S.addCreature battlements S.alice base
      (_, g2) = S.addCreature garrison S.alice g1
      board =
        (S.landsFor mountain S.alice 5 g2)
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
   in case Projection.abilitiesOf battlementsId board of
        [_, _, melding] ->
          let after = S.runPure (sparing battlementsId) board (do Activate.activateAbility S.alice battlementsId melding; Stack.resolveTop)
              township = filter (\oid -> fmap S.nameOf (Game.cardOf oid after) == Just townshipName) (Game.zoneMembers Zone.Battlefield S.alice after)
           in (Maybe.listToMaybe township, after)
        _ -> (Nothing, board)

-- CR 605.3a's mana window offers Hanweir Battlements itself, and its "{T}: Add
-- {C}" taps the very permanent whose {T} the melding ability being paid still
-- needs; a payer who takes it can no longer pay that {T} (CR 107.5) and loses the
-- activation, which Pawl.CostSpec's "CR 107.5 tapping the source for mana loses
-- its own {T}" pins. So this board answers with the first offer that is NOT that
-- permanent, named by identity rather than by index, and with nothing at all when
-- it is the only offer -- which fails the payment loudly rather than repairing it.
-- Every other prompt is left to the identity answerer.
--
-- Duplicated from Pawl.MeldSpec's `sparing` rather than hoisted into
-- Pawl.Support, which rebuilds every spec in the tree.
sparing :: ObjectId.ObjectId -> Prompt.Prompt r -> r
sparing oid p = case p of
  Prompt.ChooseManaSource _ _ candidates -> List.find (/= oid) (NonEmpty.toList candidates)
  _ -> S.identityAnswer p

-- The printings representing `oid` on `gs`, which for a melded permanent is CR
-- 701.42a's two cards and for anything else is nothing.
componentsOn :: Maybe ObjectId.ObjectId -> GameState.GameState -> [PrintingId.PrintingId]
componentsOn oid gs = foldMap (Foldable.toList . Game.componentsOf . Object.source) (oid >>= (`Game.lookupObject` gs))

-- What each of `oids` is made of on `gs`, for an assertion that a component card
-- came back as an ordinary card object of its own.
sourcesOf :: [ObjectId.ObjectId] -> GameState.GameState -> [Source.Source]
sourcesOf oids gs = Maybe.mapMaybe (fmap Object.source . (`Game.lookupObject` gs)) oids

subgameSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
subgameSpec s registry = Spec.describe s "subgames (CR 729)" $ do
  Spec.it s "CR 729.2: subgameStateFrom takes ONLY library cards; battlefield/hand do not enter" $ do
    -- alice owns 5 cards: 2 relocated to her library, 3 left on the battlefield.
    -- The subgame state's object pool must be exactly the 2 library cards. The
    -- one other thing CR 729.2 lets in is a commander (CR 729.2c), and no player
    -- here has one designated; Pawl.CommanderSpec's Subgame group covers that.
    mountain <- S.printingOf s registry "Mountain"
    let g0 = Setup.emptyGame S.bothPlayers
        g1 = addMany mountain 5 S.alice g0
        -- relocate exactly 2 of alice's cards to her library, leave 3 on the battlefield
        aliceIds = Map.keys (Map.filter (\o -> Object.owner o == S.alice) (GameState.objects g1))
        (toLib, _rest) = splitAt 2 aliceIds
        onLibrary o = o {Object.zone = Zone.Library}
        g2 =
          g1
            { GameState.objects = List.foldl' (flip (Map.adjust onLibrary)) (GameState.objects g1) toLib,
              GameState.battlefield = Set.difference (GameState.battlefield g1) (Set.fromList toLib),
              GameState.library = Map.insert S.alice (Seq.fromList toLib) (GameState.library g1)
            }
        -- alice starts (the head of the order); this test is about which CARDS
        -- enter the subgame, not who goes first.
        sub = Setup.subgameStateFrom S.alice g2
    Spec.assertEqWith s "the subgame pool holds exactly the 2 library cards" (Map.size (GameState.objects sub)) 2
    Spec.assertEqWith s "every subgame object is one of the 2 library cards" (all (`elem` toLib) (Map.keys (GameState.objects sub))) True
    Spec.assertEqWith s "the subgame battlefield is empty (nothing but the library entered)" (Set.null (GameState.battlefield sub)) True
    Spec.assertEqWith s "the subgame nextObjectId is inherited from the parent" (GameState.nextObjectId sub) (GameState.nextObjectId g2)
    Spec.assertEqWith s "the subgame is a fresh game at turn 1" (GameState.turnNumber sub) 1

  Spec.it s "CR 729.5: funnelBack returns every owned subgame card to its owner's library, ids do not collide" $ do
    -- Parent: alice and bob each own 3 cards on the battlefield plus 2 in their
    -- library (so the parent has non-library objects that must SURVIVE, and
    -- library objects that get REPLACED). finalSub is a GENUINELY PLAYED
    -- subgame -- Setup.startGameFromCards runs on sub0, shuffling each
    -- player's 2-card library and drawing an opening hand (CR 103.5) -- so
    -- Event.drawCard's changeZone mints fresh object ids above the parent's
    -- supply for every card actually drawn (CR 400.7), the way a real
    -- subgame would.
    mountain <- S.printingOf s registry "Mountain"
    let g0 = Setup.emptyGame S.bothPlayers
        g1 = poolToLibrary S.bob (poolToLibrary S.alice (addMany mountain 5 S.bob (addMany mountain 5 S.alice g0)))
        -- move 3 of each back onto the battlefield so the parent has survivors
        reBattlefield pid gg =
          let libIds = Foldable.toList (Map.findWithDefault Seq.empty pid (GameState.library gg))
              (keepLib, toField) = splitAt 2 libIds
              onField o = o {Object.zone = Zone.Battlefield}
           in gg
                { GameState.objects = List.foldl' (flip (Map.adjust onField)) (GameState.objects gg) toField,
                  GameState.battlefield = Set.union (GameState.battlefield gg) (Set.fromList toField),
                  GameState.library = Map.insert pid (Seq.fromList keepLib) (GameState.library gg)
                }
        parent = reBattlefield S.bob (reBattlefield S.alice g1)
        -- alice and bob each start the subgame with exactly 2 library cards;
        -- startGameFromCards draws an opening hand (CR 103.5), so both draw
        -- all 2 and record drewFromEmpty for the other 5 draw attempts --
        -- irrelevant here, this test only checks funnelBack's bookkeeping,
        -- not the CR 727.3/729.3 short-deck loss.
        sub0 = Setup.subgameStateFrom S.alice parent
        (_, finalSub) = Engine.runGamePure S.identityAnswer sub0 (Setup.startGameFromCards S.performer Set.empty)
        after = Setup.funnelBack finalSub parent
        libCount pid = length (Game.zoneMembers Zone.Library pid after)
        battlefieldSurvivors = Set.size (GameState.battlefield after)
    -- alice/bob each still own all their cards, all in their library, none lost
    Spec.assertEqWith s "alice's library holds exactly her 2 subgame cards, returned" (libCount S.alice) 2
    Spec.assertEqWith s "bob's library holds exactly his 2 subgame cards, returned" (libCount S.bob) 2
    Spec.assertEqWith s "the parent's non-library survivors are untouched (6 on the battlefield)" battlefieldSurvivors 6
    Spec.assertEqWith s "no object id collides (object count = survivors + returned cards)" (battlefieldSurvivors + libCount S.alice + libCount S.bob) (Map.size (GameState.objects after))
    Spec.assertEqWith s "the subgame genuinely minted fresh ids (drawCard's changeZone, CR 400.7)" (GameState.nextObjectId finalSub > GameState.nextObjectId sub0) True
    Spec.assertEqWith s "the id supply advanced to exactly the subgame high-water mark" (GameState.nextObjectId after) (GameState.nextObjectId finalSub)

  -- CR 729.5's funnel is the second place a melded permanent has to be read as
  -- CR 108.2's two cards: "each player takes all traditional cards they own that
  -- are in the subgame ... puts them into their main-game library". The subgame
  -- ends with the permanent on its battlefield, so CR 729.5c's command-zone
  -- exception cannot reach it and both cards go to the library.
  Spec.it s "CR 729.5/712.21 a subgame ending with a melded permanent returns both of its cards to the main-game library" $ do
    battlements <- S.printingOf s registry "Hanweir Battlements"
    garrison <- S.printingOf s registry "Hanweir Garrison"
    mountain <- S.printingOf s registry "Mountain"
    let parent = Setup.emptyGame S.bothPlayers
        sub0 = Setup.subgameStateFrom S.alice parent
        (meldedId, finalSub) = meldedBoard sub0 battlements garrison mountain
        components = componentsOn meldedId finalSub
        after = Setup.funnelBack finalSub parent
        hers = Game.zoneMembers Zone.Library S.alice after
        sources = sourcesOf hers after
    Spec.assertEqWith s "setup: the subgame really ended with one permanent representing two cards" (length components) 2
    Spec.assertEqWith s "CR 729.5/712.21 both cards representing the melded permanent are in alice's main-game library" (filter (\p -> elem (Source.OfCard p) sources) components) components
    Spec.assertEqWith s "and no object left in either game is represented by two cards: the permanent is no card at all (CR 108.2)" (all (null . Game.componentsOf . Object.source) (Map.elems (GameState.objects after))) True
    Spec.assertEqWith s "alice's five Mountains came back beside them" (length hers) 7
    Spec.assertEqWith s "no returned object collides with either game's ids" (Map.size (GameState.objects after)) (length hers)
    Spec.assertEqWith s "the split minted its ids above the merged supply" (GameState.nextObjectId after > GameState.nextObjectId finalSub) True

  -- The gate's whole reason to exist (Pawl.Engine.Departure's continuesAfterDeparture
  -- doc comment): a two-player subgame's departure is caught by CR 104.2a
  -- before it can be observed, but a subgame seated with three or more still-
  -- playing parent players is itself CR 800.1 multiplayer, so a departure
  -- INSIDE it is real and CR 800.4a's object removal reaches the departing
  -- player's subgame objects outright. CR 729.5 still requires every card they
  -- owned coming back to their MAIN-game library regardless -- CR 729.4's
  -- second sentence keeps the two games separate populations, and nothing in
  -- the CR removes a card from a player's deck for losing a subgame.
  Spec.it s "CR 729.5/800.4a a player who departs inside a MULTIPLAYER subgame still gets their library back" $ do
    mountain <- S.printingOf s registry "Mountain"
    let g0 = Setup.emptyGame S.threePlayers
        g1 = poolToLibrary S.carol (poolToLibrary S.bob (poolToLibrary S.alice (addMany mountain 3 S.carol (addMany mountain 3 S.bob (addMany mountain 3 S.alice g0)))))
        sub0 = Setup.subgameStateFrom S.alice g1
        (_, seated) = Engine.runGamePure S.identityAnswer sub0 (Setup.startGameFromCards S.performer Set.empty)
        departedSub = S.departs Departure.Type.Lost S.bob seated
        after = Setup.funnelBack departedSub g1
    Spec.assertEqWith s "the subgame really was multiplayer, so CR 800.4a's removal fired" (Departure.continuesAfterDeparture departedSub) True
    Spec.assertEqWith s "bob's own subgame objects are gone" (Game.zoneMembers Zone.Library S.bob departedSub <> Game.zoneMembers Zone.Hand S.bob departedSub) []
    Spec.assertEqWith s "bob's 3-card library comes back whole" (length (Game.zoneMembers Zone.Library S.bob after)) 3
    Spec.assertEqWith s "alice's library is unaffected" (length (Game.zoneMembers Zone.Library S.alice after)) 3
    Spec.assertEqWith s "carol's library is unaffected" (length (Game.zoneMembers Zone.Library S.carol after)) 3

  Spec.it s "CR 400.7: funnelBack returns each card to the library as a NEW object, with no per-incarnation state" $ do
    -- The subgame teardown is the other hand-written zone move outside
    -- Event.changeZone, and it puts cards into a library from TWO pools: the
    -- finished subgame's objects, and -- for a player who departed inside the
    -- subgame, so has no objects left there -- the parent's own library
    -- objects. Both are dirtied here, so both funnels are under test (#653).
    --
    -- Dirtying the subgame AFTER it is played is what makes the first pool
    -- meaningful: startGameFromCards and the opening draws would otherwise
    -- hand funnelBack an already-clean pool and the assertion would pass
    -- without funnelBack doing anything.
    mountain <- S.printingOf s registry "Mountain"
    let g0 = Setup.emptyGame S.threePlayers
        g1 = poolToLibrary S.carol (poolToLibrary S.bob (poolToLibrary S.alice (addMany mountain 3 S.carol (addMany mountain 3 S.bob (addMany mountain 3 S.alice g0)))))
        dirtyPool gs = gs {GameState.objects = Map.map (\o -> dirtied (Object.owner o) o) (GameState.objects gs)}
        parent = dirtyPool g1
        sub0 = Setup.subgameStateFrom S.alice parent
        (_, seated) = Engine.runGamePure S.identityAnswer sub0 (Setup.startGameFromCards S.performer Set.empty)
        departedSub = dirtyPool (S.departs Departure.Type.Lost S.bob seated)
        after = Setup.funnelBack departedSub parent
        libraryObjects pid = Maybe.mapMaybe (`Game.lookupObject` after) (Game.zoneMembers Zone.Library pid after)
        everyone = concatMap libraryObjects [S.alice, S.bob, S.carol]
    -- The discriminators: both pools genuinely carry state going in, and both
    -- genuinely deliver cards to a library.
    Spec.assertEqWith s "the subgame pool going in genuinely carried per-incarnation state" (not (all forgotten (Map.elems (GameState.objects departedSub)))) True
    Spec.assertEqWith s "the parent pool going in genuinely carried per-incarnation state" (not (all forgotten (Map.elems (GameState.objects parent)))) True
    Spec.assertEqWith s "alice and carol come back through the subgame pool" (length (libraryObjects S.alice) + length (libraryObjects S.carol)) 6
    Spec.assertEqWith s "bob, who departed, comes back through the parent pool" (length (libraryObjects S.bob)) 3
    Spec.assertEqWith s "every returned card forgot its previous existence" (all forgotten everyone) True

  -- The narrower door: continuesAfterDeparture reads `finalSub`'s turnOrder
  -- at the END of the subgame, but objectsLeaveWith's own gate fired at
  -- DEPARTURE time -- and a restart resolving INSIDE the subgame AFTER the
  -- departure (Setup.restartGame, reachable via Effect.RestartGame /
  -- Resolve.hs from a still-running subgame) rewrites turnOrder to
  -- Game.stillPlayingInOrder, which DROPS the departed seat. So by the
  -- time funnelBack reads `finalSub`, its turnOrder is back down to two and
  -- continuesAfterDeparture would read False even though bob's objects were
  -- genuinely destroyed earlier. funnelBack's guard must not depend on
  -- `finalSub`'s turnOrder at all for this reason.
  Spec.it s "CR 729.5/800.4a a restart INSIDE the subgame, after the departure, does not un-recover the departed player's library" $ do
    mountain <- S.printingOf s registry "Mountain"
    let g0 = Setup.emptyGame S.threePlayers
        g1 = poolToLibrary S.carol (poolToLibrary S.bob (poolToLibrary S.alice (addMany mountain 3 S.carol (addMany mountain 3 S.bob (addMany mountain 3 S.alice g0)))))
        sub0 = Setup.subgameStateFrom S.alice g1
        (_, seated) = Engine.runGamePure S.identityAnswer sub0 (Setup.startGameFromCards S.performer Set.empty)
        departedSub = S.departs Departure.Type.Lost S.bob seated
        (_, restarted) = Engine.runGamePure S.identityAnswer departedSub (Setup.restartGame S.performer Set.empty S.alice)
        after = Setup.funnelBack restarted g1
    Spec.assertEqWith s "the in-subgame restart really did shrink finalSub's own turnOrder to two" (length (GameState.turnOrder restarted)) 2
    Spec.assertEqWith s "so the naive seam-at-the-end reading would (wrongly) say it is not multiplayer any more" (Departure.continuesAfterDeparture restarted) False
    Spec.assertEqWith s "bob still has nothing anywhere in the restarted subgame" (Map.keys (Map.filter (\o -> Object.owner o == S.bob) (GameState.objects restarted))) []
    Spec.assertEqWith s "bob's 3-card library still comes back whole" (length (Game.zoneMembers Zone.Library S.bob after)) 3
    Spec.assertEqWith s "alice's library is unaffected" (length (Game.zoneMembers Zone.Library S.alice after)) 3
    Spec.assertEqWith s "carol's library is unaffected" (length (Game.zoneMembers Zone.Library S.carol after)) 3

  -- The other half of CR 729.4a, and the reason applyCrossings exists: the wish
  -- inside the subgame took a MAIN-GAME card, and the main game has to lose it.
  -- Driven through Pawl.Engine.OutsideTheGame.bringInFrom, the one writer of
  -- GameState.broughtIn, rather than through a hand-set field -- a hand-set one
  -- would prove applyCrossings reads a Seq and nothing about the road that
  -- fills it.
  Spec.it s "CR 729.4a: a card the subgame brought in leaves the main game when the subgame ends" $ do
    elf <- S.printingOf s registry "Arbor Elf"
    let g0 = Setup.emptyGame S.bothPlayers
        (elfId, g1) = S.addCreature elf S.alice g0
        (bystanderId, parent) = S.addCreature elf S.bob g1
        sub0 = Setup.subgameStateFrom S.alice parent
        (broughtInId, crossedSub) = OutsideTheGame.bringInFrom S.alice elfId sub0
        after = Setup.applyCrossings crossedSub parent
        leftEvents = Maybe.mapMaybe (\logged -> case LoggedEvent.event logged of GameEvent.LeftTheGame oid -> Just oid; _ -> Nothing) (Foldable.toList (GameState.events after))
    -- The fixture's own preconditions, so the assertions below cannot pass for
    -- want of a board: the elf really was in the main game, and the subgame
    -- really did take it.
    Spec.assertEqWith s "the elf started on the main game's battlefield" (Set.member elfId (GameState.battlefield parent)) True
    Spec.assertEqWith s "the subgame's wish minted an object of its own for it (CR 400.7)" (Maybe.isJust broughtInId) True
    Spec.assertEqWith s "the subgame recorded the crossing under the OUTER id" (Foldable.toList (GameState.broughtIn crossedSub)) [elfId]
    -- The gameplay-level claim, first: the main game no longer has the card.
    Spec.assertEqWith s "CR 729.4a: the crossed creature is off the main game's battlefield" (Set.member elfId (GameState.battlefield after)) False
    Spec.assertEqWith s "CR 729.4a: and out of the main game's objects entirely" (Map.member elfId (GameState.objects after)) False
    Spec.assertEqWith s "bob's creature, which nothing took, is untouched" (Set.member bystanderId (GameState.battlefield after)) True
    Spec.assertEqWith s "CR 729.4a: a GameEvent.LeftTheGame names it in the main game's log" leftEvents [elfId]
    Spec.assertEqWith s "CR 608.2h: its last known information is filed under the id it had" (Map.member elfId (GameState.lastKnown after)) True

  -- CR 729.4a: "abilities in the main game that trigger on objects leaving a
  -- main-game zone will trigger, but they won't be put onto the stack until the
  -- main game resumes." A leaves-the-battlefield ability (CR 603.6c) is such an
  -- ability, and the crossing is the OTHER road out of the game -- rule 603.6c's
  -- own second clause names only CR 800.4a's, which Pawl.DepartureSpec covers.
  --
  -- Three seats, each doing a job, exactly as that spec's pair does: alice OWNS
  -- the Thragtusk, carol CONTROLS it and so takes CR 603.3a's trigger, and bob
  -- is the bystander the token count is checked against.
  --
  -- The trigger can only be read from CR 608.2h last known information: the
  -- crossed permanent is not merely off the battlefield by the CR 117.5 scan, it
  -- is out of the main game, so neither the live board nor the per-group sample
  -- has anything to find.
  Spec.it s "CR 729.4a a phased-in permanent a subgame takes triggers its leaves-the-battlefield ability" $ do
    thragtusk <- S.printingOf s registry "Thragtusk"
    let (tusk, g1) = S.addCreature thragtusk S.alice S.threePlayerGame
        parent = S.giveControl tusk S.carol g1
        sub0 = Setup.subgameStateFrom S.alice parent
        (_, crossedSub) = OutsideTheGame.bringInFrom S.alice tusk sub0
        after = resolveTriggers (Setup.applyCrossings crossedSub parent)
    Spec.assertEqWith s "carol controls the creature alice owns" (Projection.controllerOf tusk parent) (Just S.carol)
    Spec.assertEqWith s "it is phased in, so the pair's one difference is genuinely absent here" (Map.member tusk (GameState.phasedOut parent)) False
    Spec.assertEqWith s "the subgame's wish took it out of the main game" (Game.lookupObject tusk after) Nothing
    Spec.assertEqWith s "CR 729.4a: carol got the Beast the leaves-the-battlefield ability makes" (S.countOnBattlefieldByName beastToken S.carol after) 1
    Spec.assertEqWith s "and nobody else did" (fmap (\pid -> S.countOnBattlefieldByName beastToken pid after) [S.alice, S.bob]) [0, 0]

  -- CR 702.26b: "except for rules and effects that specifically mention
  -- phased-out permanents, a phased-out permanent is treated as though it does
  -- not exist. It can't affect or be affected by anything else in the game." CR
  -- 729.4a mentions none, so a main-game ability watching for permanents leaving
  -- the battlefield cannot see this one go -- the same answer CR 702.26k gives
  -- in so many words for the departure road. What CR 702.26d does settle is that
  -- the permanent never changed zones, which is why the crossing has to be gated
  -- on battlefield MEMBERSHIP for the two legs to differ at all.
  --
  -- The paired negative for the case above: the same board, the same seats, the
  -- same crossing, and the one difference is that the Thragtusk is phased out
  -- when the subgame's wish reaches it. Carol's control is asserted on the
  -- phased-out board too, so rule 702.26b's "does not exist" cannot quietly hand
  -- the creature back to alice and answer for a different reason.
  --
  -- Built with Pawl.Engine.Phasing.phaseOut for Pawl.DepartureSpec's reason: no
  -- printing in the pool has both phasing and a leaves-the-battlefield ability.
  Spec.it s "CR 702.26b a phased-out permanent a subgame takes leaves the main game and triggers nothing" $ do
    thragtusk <- S.printingOf s registry "Thragtusk"
    let (tusk, g1) = S.addCreature thragtusk S.alice S.threePlayerGame
        parent = Phasing.phaseOut (PhasedOut.Directly S.carol) tusk (S.giveControl tusk S.carol g1)
        sub0 = Setup.subgameStateFrom S.alice parent
        (broughtInId, crossedSub) = OutsideTheGame.bringInFrom S.alice tusk sub0
        after = resolveTriggers (Setup.applyCrossings crossedSub parent)
    Spec.assertEqWith s "it is phased out" (Phasing.isPhasedOut tusk parent) True
    Spec.assertEqWith s "and carol still controls it, so CR 702.26b is not answering by handing it back" (Projection.controllerOf tusk parent) (Just S.carol)
    -- The fixture's own precondition, and the reading recorded at
    -- Setup.applyCrossings: CR 729.4 puts every main-game object outside the
    -- subgame, so the wish reaches this one and the crossing happens at all.
    Spec.assertEqWith s "the subgame's wish did reach it (CR 729.4)" (Maybe.isJust broughtInId) True
    -- The rule under test, ahead of every proxy below it.
    Spec.assertEqWith s "CR 702.26b: no zone-change ability triggered" (fmap (\pid -> S.countOnBattlefieldByName beastToken pid after) [S.alice, S.bob, S.carol]) [0, 0, 0]
    Spec.assertEqWith s "CR 729.4a: it left the main game just the same" (Game.lookupObject tusk after) Nothing
    Spec.assertEqWith s "and is no longer recorded as phased out either" (Map.member tusk (GameState.phasedOut after)) False
    Spec.assertEqWith s "CR 608.2h: its last known information is still filed" (Map.member tusk (GameState.lastKnown after)) True
  -- CR 702.26b again, this time against CR 604.2's handover rather than the
  -- event: a phased-out permanent "is treated as though it does not exist", so
  -- its static ability was generating no effect there is anything to continue.
  -- One gate serves both, GameState.battlefield membership, which is the reading
  -- Departure.objectsLeaveWith records for the other road out of the game.
  --
  -- A PAIR differing in the phase-out alone -- same Song, same Statue, same
  -- crossing -- since a phased-out Song animates nothing while it is out there
  -- either, and a negative assembled on its own board could not tell the two
  -- readings apart. Pawl.OutsideTheGameSpec's Death Wish case proves the
  -- phased-in leg at gameplay level; this one is here for the gate.
  Spec.it s "CR 702.26b a phased-out Titania's Song a subgame takes hands nothing over" $ do
    titaniasSong <- S.printingOf s registry "Titania's Song"
    jadeStatue <- S.printingOf s registry "Jade Statue"
    let (songId, g1) = S.addCreature titaniasSong S.alice (Setup.emptyGame S.bothPlayers)
        (statueId, phasedIn) = S.addCreature jadeStatue S.alice g1
        phasedOut = Phasing.phaseOut (PhasedOut.Directly S.alice) songId phasedIn
        crossFrom parent = Setup.applyCrossings (snd (OutsideTheGame.bringInFrom S.alice songId (Setup.subgameStateFrom S.alice parent))) parent
        afterIn = crossFrom phasedIn
        afterOut = crossFrom phasedOut
    -- The pair's one difference, on the boards going in.
    Spec.assertEqWith s "CR 613.4b the phased-in Song animates the Statue as a 4/4" (S.powerToughnessOf statueId phasedIn) (Just (4, 4))
    Spec.assertEqWith s "CR 702.26b the phased-out one animates nothing" (S.powerToughnessOf statueId phasedOut) Nothing
    -- The gate, ahead of the proxies.
    Spec.assertEqWith s "CR 702.26b: the phased-out Song hands nothing over as it leaves the game" (S.powerToughnessOf statueId afterOut) Nothing
    Spec.assertEqWith s "CR 604.2: where the phased-in one's effect goes on applying" (S.powerToughnessOf statueId afterIn) (Just (4, 4))
    Spec.assertEqWith s "and it is a stored effect on each leg, none and CR 613.6's four parts" (length (GameState.continuousEffects afterOut), length (GameState.continuousEffects afterIn)) (0, 4)
    Spec.assertEqWith s "CR 729.4a: both legs took the Song out of the main game" (Map.member songId (GameState.objects afterOut), Map.member songId (GameState.objects afterIn)) (False, False)

  -- Why applyCrossings does NOT wrap its batch in Event.simultaneouslyPure, the
  -- one place it parts company with Departure.objectsLeaveWith: CR 800.4a's
  -- objects leave at one instant and share a group, but two wishes cast at two
  -- different moments of the subgame are two events, and CR 603.10a's look-back
  -- triggers have to be able to tell them apart.
  Spec.it s "CR 603.10a: two crossings are two events, read against a running board" $ do
    elf <- S.printingOf s registry "Arbor Elf"
    let g0 = Setup.emptyGame S.bothPlayers
        -- Minted in the opposite order to the one they cross in, so every
        -- assertion below reads the CROSSING order and not the id order.
        (lateCrosser, g1) = S.addCreature elf S.alice g0
        (earlyCrosser, parent) = S.addCreature elf S.alice g1
        sub0 = Setup.subgameStateFrom S.alice parent
        (_, crossed1) = OutsideTheGame.bringInFrom S.alice earlyCrosser sub0
        (_, crossedSub) = OutsideTheGame.bringInFrom S.alice lateCrosser crossed1
        after = Setup.applyCrossings crossedSub parent
        left = Maybe.mapMaybe (\logged -> case LoggedEvent.event logged of GameEvent.LeftTheGame oid -> Just (LoggedEvent.group logged, oid); _ -> Nothing) (Foldable.toList (GameState.events after))
        -- CR 603.10's "objects that exist immediately after an event", as
        -- Event.recordEvent sampled it for each of the two groups.
        sampledAt eventGroup = Map.keysSet (Map.findWithDefault Map.empty eventGroup (GameState.battlefieldWhenTriggered after))
        groupOf oid = fmap fst (List.find (\entry -> snd entry == oid) left)
    Spec.assertEqWith s "both left, in crossing order rather than id order" (fmap snd left) [earlyCrosser, lateCrosser]
    Spec.assertEqWith s "CR 603.10a: they are two event groups, not one simultaneous batch" (length (List.nub (fmap fst left))) 2
    -- The running board, read out of the sample CR 603.10 takes at each event. A
    -- batch that deleted both and only then recorded answers False to the first
    -- of these, and a main-game ability watching the LATE crosser leave the
    -- battlefield would never see the early one go.
    Spec.assertEqWith s "CR 603.10: the early crossing's event was sampled while the late one still stood" (fmap (Set.member lateCrosser . sampledAt) (groupOf earlyCrosser)) (Just True)
    Spec.assertEqWith s "CR 603.10: the late crossing's event was sampled after the early one had gone" (fmap (Set.member earlyCrosser . sampledAt) (groupOf lateCrosser)) (Just False)
    -- CR 603.10's "objects that exist immediately after the event": a group's own
    -- departure is not among the objects existing after it, so each crosser is
    -- absent from its OWN group's sample -- neither assertion above checks that.
    Spec.assertEqWith s "CR 603.10: the early crosser is absent from its own group's sample" (fmap (Set.member earlyCrosser . sampledAt) (groupOf earlyCrosser)) (Just False)
    Spec.assertEqWith s "CR 603.10: the late crosser is absent from its own group's sample" (fmap (Set.member lateCrosser . sampledAt) (groupOf lateCrosser)) (Just False)

  -- CR 608.2h against a RUNNING board, which is the whole reason applyCrossings
  -- is a fold and not a batch: the anthem crosses first, so by the instant the
  -- elf crosses there is nothing pumping it any more and its last known power is
  -- its printed one. A batch reading the pristine parent for every crossing
  -- answers 2, which is the power of a permanent that had already left.
  Spec.it s "CR 608.2h: the second crossing's last known information is read after the first has gone" $ do
    elf <- S.printingOf s registry "Arbor Elf"
    anthem <- S.printingOf s registry "Glorious Anthem"
    let g0 = Setup.emptyGame S.bothPlayers
        (anthemId, g1) = S.addCreature anthem S.alice g0
        (elfId, parent) = S.addCreature elf S.alice g1
        sub0 = Setup.subgameStateFrom S.alice parent
        (_, crossed1) = OutsideTheGame.bringInFrom S.alice anthemId sub0
        (_, crossedSub) = OutsideTheGame.bringInFrom S.alice elfId crossed1
        after = Setup.applyCrossings crossedSub parent
        lastPower oid = fmap (PC.power . LastKnown.characteristics) (Map.lookup oid (GameState.lastKnown after))
    -- The discriminator: the anthem genuinely was pumping the elf in the main
    -- game, so 1 and 2 are two different readings of the same board.
    Spec.assertEqWith s "the anthem really was pumping the elf before either crossed" (Projection.powerOf elfId parent) (Just 2)
    Spec.assertEqWith s "CR 608.2h: the elf left a board the anthem had already left, so its last known power is 1" (lastPower elfId) (Just (Just 1))
    Spec.assertEqWith s "the anthem, which crossed first, left the untouched board" (fmap (const True) (lastPower anthemId)) (Just True)

  -- CR 729.6's nesting, the case a middle frame cannot settle: a wish two levels
  -- down reached a card belonging to the game ABOVE this one, which
  -- subgameStateFrom handed down along with this game's own cards. This frame
  -- holds no such object, so it hands the crossing one level further out instead
  -- of dropping it.
  Spec.it s "CR 729.6: a crossing from further out is passed outward one level, not applied here" $ do
    elf <- S.printingOf s registry "Arbor Elf"
    let g0 = Setup.emptyGame S.bothPlayers
        (elfId, g1) = S.addCreature elf S.alice g0
        (printingId, g2) = Game.intern elf g1
        -- An id belonging to no object of g2's: this game is itself a subgame,
        -- and that card sits in the game holding it.
        outerId = ObjectId.MkObjectId 9001
        parent =
          g2
            { GameState.outsideObjects = Map.singleton outerId (OutsideObject.MkOutsideObject S.alice printingId Facing.FaceUp)
            }
        sub0 = Setup.subgameStateFrom S.alice parent
        (_, crossedSub) = OutsideTheGame.bringInFrom S.alice outerId sub0
        after = Setup.applyCrossings crossedSub parent
        leftEvents = Maybe.mapMaybe (\logged -> case LoggedEvent.event logged of GameEvent.LeftTheGame oid -> Just oid; _ -> Nothing) (Foldable.toList (GameState.events after))
    Spec.assertEqWith s "the subgame really did inherit the outer entry (CR 729.6)" (Map.member outerId (GameState.outsideObjects sub0)) True
    Spec.assertEqWith s "CR 729.6: this game hands the crossing to the frame above it" (Foldable.toList (GameState.broughtIn after)) [outerId]
    Spec.assertEqWith s "CR 729.4a: and stops offering the card it can no longer supply" (Map.member outerId (GameState.outsideObjects after)) False
    Spec.assertEqWith s "nothing left THIS game, which never held the card" leftEvents []
    Spec.assertEqWith s "so its own objects are untouched" (Map.keysSet (GameState.objects after)) (Map.keysSet (GameState.objects parent))
    Spec.assertEqWith s "and its battlefield still holds alice's creature" (Set.member elfId (GameState.battlefield after)) True

  -- CR 400.11b: a sideboard card the subgame's wish spent has left the pool for
  -- good -- funnelBack puts it into the main-game library, so a pool that came
  -- back refilled would have the same card in two places at once.
  Spec.it s "CR 400.11b/729.5: the pool a subgame's wish spent comes back spent, and nothing else does" $ do
    mountain <- S.printingOf s registry "Mountain"
    let g0 = Setup.emptyGame S.bothPlayers
        (printingId, g1) = Game.intern mountain g0
        stocked p = p {Player.outsideTheGame = Map.singleton printingId 2, Player.life = 13}
        parent = g1 {GameState.players = Map.adjust stocked S.alice (GameState.players g1)}
        sub0 = Setup.subgameStateFrom S.alice parent
        -- Inside the subgame: alice's wish brings one copy in, and she loses
        -- life doing it. Only the first of those may follow her out (CR 729.1b).
        (_, spentSub) = OutsideTheGame.bringIn S.alice printingId sub0
        finalSub = spentSub {GameState.players = Map.adjust (\p -> p {Player.life = 5}) S.alice (GameState.players spentSub)}
        after = Setup.funnelBack finalSub parent
        poolOf pid gs = fmap Player.outsideTheGame (Map.lookup pid (GameState.players gs))
    Spec.assertEqWith s "the parent's pool going in genuinely held both copies" (poolOf S.alice parent) (Just (Map.singleton printingId 2))
    Spec.assertEqWith s "the subgame genuinely spent one (CR 400.11b)" (poolOf S.alice spentSub) (Just (Map.singleton printingId 1))
    Spec.assertEqWith s "CR 400.11b: the main game gets the spent pool back, not the full one" (poolOf S.alice after) (Just (Map.singleton printingId 1))
    Spec.assertEqWith s "CR 729.1b: her life total is the main game's, which the subgame never touched" (S.lifeOf S.alice after) (Just 13)
    Spec.assertEqWith s "bob's pool, which no wish reached, is unchanged" (poolOf S.bob after) (Just Map.empty)

  -- The other arm of CR 400.11b, whose second clause takes the card back out of
  -- the game when its owner leaves: bob's departure inside a MULTIPLAYER subgame
  -- is CR 800.4a, so the copy he wished in left with him. Carrying his spent pool
  -- out would lose that card twice over -- Departure.objectsLeaveWith already
  -- deleted the object, so funnelBack has nothing to put in his library either.
  Spec.it s "CR 400.11b/800.4a a player who departs inside the subgame gets their spend undone" $ do
    mountain <- S.printingOf s registry "Mountain"
    let g0 = Setup.emptyGame S.threePlayers
        (printingId, g1) = Game.intern mountain g0
        stocked p = p {Player.outsideTheGame = Map.singleton printingId 2}
        parent = g1 {GameState.players = Map.map stocked (GameState.players g1)}
        sub0 = Setup.subgameStateFrom S.alice parent
        (_, aliceSpent) = OutsideTheGame.bringIn S.alice printingId sub0
        (_, bobSpent) = OutsideTheGame.bringIn S.bob printingId aliceSpent
        finalSub = S.departs Departure.Type.Lost S.bob bobSpent
        after = Setup.funnelBack finalSub parent
        poolOf pid gs = fmap Player.outsideTheGame (Map.lookup pid (GameState.players gs))
    Spec.assertEqWith s "the subgame really was multiplayer, so CR 800.4a's removal fired" (Departure.continuesAfterDeparture finalSub) True
    Spec.assertEqWith s "both of them genuinely spent a copy inside the subgame" (poolOf S.alice bobSpent, poolOf S.bob bobSpent) (Just (Map.singleton printingId 1), Just (Map.singleton printingId 1))
    Spec.assertEqWith s "CR 400.11b: bob left, so the card he wished in is outside the game again" (poolOf S.bob after) (Just (Map.singleton printingId 2))
    Spec.assertEqWith s "alice, who is still playing, keeps her spend" (poolOf S.alice after) (Just (Map.singleton printingId 1))
    Spec.assertEqWith s "carol, who wished for nothing, is unchanged" (poolOf S.carol after) (Just (Map.singleton printingId 2))

  -- The road the pool arm above does NOT restore, pinned so that nobody reads its
  -- silence as an oversight: bob wished a MAIN-GAME card of his own into the
  -- subgame and then departed there. CR 729.4a took the card out of the main game
  -- and CR 800.4a took the subgame's copy out with him, so nothing in either game
  -- represents it afterwards. Nothing was spent from his pool, so the pool arm has
  -- nothing to give back.
  Spec.it s "CR 729.4a/800.4a a main-game card a departed player wished in is in neither game after" $ do
    elf <- S.printingOf s registry "Arbor Elf"
    let g0 = Setup.emptyGame S.threePlayers
        (bobsId, parent) = S.addCreature elf S.bob g0
        sub0 = Setup.subgameStateFrom S.alice parent
        (minted, crossedSub) = OutsideTheGame.bringInFrom S.bob bobsId sub0
        departedSub = S.departs Departure.Type.Lost S.bob crossedSub
        after = Setup.funnelBack departedSub (Setup.applyCrossings departedSub parent)
    Spec.assertEqWith s "the subgame really did mint him a copy" (Maybe.isJust minted) True
    Spec.assertEqWith s "the subgame really was multiplayer, so CR 800.4a's removal fired" (Departure.continuesAfterDeparture departedSub) True
    Spec.assertEqWith s "CR 800.4a: the subgame's copy left with him" (fmap (`Map.member` GameState.objects departedSub) minted) (Just False)
    Spec.assertEqWith s "CR 729.4a: the main game's copy left when the subgame took it" (Map.member bobsId (GameState.objects after)) False
    Spec.assertEqWith s "so bob owns nothing in the main game afterwards" (Map.keys (Map.filter (\o -> Object.owner o == S.bob) (GameState.objects after))) []
    Spec.assertEqWith s "and his pool, which no wish reached, is still empty" (fmap Player.outsideTheGame (Map.lookup S.bob (GameState.players after))) (Just Map.empty)

  Spec.it s "CR 729.2/729.4 #147: a subgame seats only the players still in the main game" $ do
    -- CR 729.2: "Each player takes all the cards in their main-game library,
    -- moves them to their subgame library, and shuffles them." Each player IN
    -- the game -- CR 729.4: "All players not currently in the subgame are
    -- considered outside the subgame." Today the rebuilt order is
    -- rotateTo carol [alice, bob, carol] = [carol, alice, bob], with bob in it.
    let g0 = S.departs Departure.Type.Conceded S.bob (Setup.emptyGame S.threePlayers)
        sub = Setup.subgameStateFrom S.carol g0
    Spec.assertEqWith s "two seats, rotated to the starter" (GameState.turnOrder sub) [S.carol, S.alice]
    Spec.assertEqWith s "carol goes first" (GameState.activePlayer sub) S.carol
    Spec.assertEqWith s "CR 800.1: a two-seat subgame is not a multiplayer game, so no free mulligan" (Mulligan.freeMulligans sub) 0

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Setup" $ do
  setupSpec s registry
  greenBlackSetupSpec s registry
  deckSpec s registry
  restartSpec s registry
  subgameSpec s registry
