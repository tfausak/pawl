-- Covers Pawl.Engine.Setup and Pawl.Types.Deck: setup, deck composition, opening hands.
module Pawl.SetupSpec where

import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Cards as Cards
import qualified Pawl.Engine.Departure as Departure
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Mulligan as Mulligan
import qualified Pawl.Engine.Setup as Setup
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
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Result as Result
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Status as Status
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TapState as TapState
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
    let Deck.MkDeck m = deck
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
    let Deck.MkDeck m = deck
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
    let Deck.MkDeck m = deck
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
      Object.damage = 1,
      Object.sickness = Sickness.Settled pid,
      Object.bindings = Map.singleton (SlotName.MkSlotName (Text.pack "target")) Binding.empty,
      Object.counters = Map.singleton CounterKind.PlusOnePlusOne 1,
      Object.attachedTo = Just (Recipient.ToPlayer pid),
      Object.enteredUnder = Just pid,
      Object.chosenColor = Just Color.Blue,
      Object.chosenSubtype = Just Subtype.Forest,
      Object.chosenNames = Set.singleton (CardName.MkCardName (Text.pack "Mountain")),
      Object.face = Just (CardName.MkCardName (Text.pack "Mountain")),
      Object.playableFromExileBy = Just pid,
      Object.ringBearerFor = Just pid
    }

-- CR 400.7: has this object no memory of a previous existence? Applying the
-- forgetting again changes nothing exactly when the move already applied it in
-- full, so this stays honest as fields are added -- unlike a list of field
-- comparisons, which would have to be extended by hand alongside Object.
forgotten :: Object.Object -> Bool
forgotten object = Object.newIncarnation object == object

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
        after = snd (Engine.runGamePure S.identityAnswer g2 (Setup.startGameFromCards S.performer))
        libSize pid = length (Game.zoneMembers Zone.Library pid after)
    Spec.assertEqWith s "alice drew a 7-card opening hand" (S.handSize S.alice after) 7
    Spec.assertEqWith s "bob drew a 7-card opening hand" (S.handSize S.bob after) 7
    Spec.assertEqWith s "alice's library holds the remaining owned card" (libSize S.alice) 1
    Spec.assertEqWith s "bob's library holds the remaining owned card" (libSize S.bob) 1
    Spec.assertEqWith s "the battlefield is empty after the rebuild" (Set.null (GameState.battlefield after)) True
    Spec.assertEqWith s "every rebuilt object is owned by alice or bob (ownership preserved)" (all (\o -> Object.owner o == S.alice || Object.owner o == S.bob) (Map.elems (GameState.objects after))) True

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
        after = snd (Engine.runGamePure S.identityAnswer g2 (Setup.startGameFromCards S.performer))
    -- The discriminator: without this, an assertion over an already-clean pool
    -- would pass no matter what startGameFromCards does.
    Spec.assertEqWith s "the pool going in genuinely carried per-incarnation state" (not (all forgotten (Map.elems (GameState.objects g2)))) True
    Spec.assertEqWith s "every rebuilt object forgot its previous existence" (all forgotten (Map.elems (GameState.objects after))) True

  Spec.it s "CR 727.1a: the starting player is the restart's controller, at the head of the turn order" $ do
    -- Two restarts of the same board, controlled by different players: the
    -- active player and the head of the turn order follow the controller.
    mountain <- S.printingOf s registry "Mountain"
    let g0 = addMany mountain 8 S.bob (addMany mountain 8 S.alice (Setup.emptyGame S.bothPlayers))
        byBob = snd (Engine.runGamePure S.identityAnswer g0 (Setup.restartGame S.performer S.bob))
        byAlice = snd (Engine.runGamePure S.identityAnswer g0 (Setup.restartGame S.performer S.alice))
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
        after = snd (Engine.runGamePure S.identityAnswer g4 (Setup.restartGame S.performer S.alice))
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
        after = snd (Engine.runGamePure S.identityAnswer g2 (Setup.restartGame S.performer S.bob))
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
        afterRestart = snd (Engine.runGamePure S.identityAnswer g0 (Setup.restartGame S.performer S.alice))
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
    let g0 = Departure.depart Departure.Type.Conceded S.bob (Setup.emptyGame S.threePlayers)
        g1 =
          g0
            { GameState.players =
                Map.adjust (\p -> p {Player.life = 3}) S.bob (Map.adjust (\p -> p {Player.life = 5}) S.alice (GameState.players g0))
            }
        afterRestart = S.runPure S.identityAnswer g1 (Setup.restartGame S.performer S.alice)
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
        g1 = Departure.depart Departure.Type.Conceded S.bob g0
        after = snd (Engine.runGamePure S.identityAnswer g1 (Setup.restartGame S.performer S.alice))
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

subgameSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
subgameSpec s registry = Spec.describe s "subgames (CR 729)" $ do
  Spec.it s "CR 729.2: subgameStateFrom takes ONLY library cards; battlefield/hand do not enter" $ do
    -- alice owns 5 cards: 2 relocated to her library, 3 left on the battlefield.
    -- The subgame state's object pool must be exactly the 2 library cards.
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
        (_, finalSub) = Engine.runGamePure S.identityAnswer sub0 (Setup.startGameFromCards S.performer)
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
        (_, seated) = Engine.runGamePure S.identityAnswer sub0 (Setup.startGameFromCards S.performer)
        departedSub = Departure.depart Departure.Type.Lost S.bob seated
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
        (_, seated) = Engine.runGamePure S.identityAnswer sub0 (Setup.startGameFromCards S.performer)
        departedSub = dirtyPool (Departure.depart Departure.Type.Lost S.bob seated)
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
        (_, seated) = Engine.runGamePure S.identityAnswer sub0 (Setup.startGameFromCards S.performer)
        departedSub = Departure.depart Departure.Type.Lost S.bob seated
        (_, restarted) = Engine.runGamePure S.identityAnswer departedSub (Setup.restartGame S.performer S.alice)
        after = Setup.funnelBack restarted g1
    Spec.assertEqWith s "the in-subgame restart really did shrink finalSub's own turnOrder to two" (length (GameState.turnOrder restarted)) 2
    Spec.assertEqWith s "so the naive seam-at-the-end reading would (wrongly) say it is not multiplayer any more" (Departure.continuesAfterDeparture restarted) False
    Spec.assertEqWith s "bob still has nothing anywhere in the restarted subgame" (Map.keys (Map.filter (\o -> Object.owner o == S.bob) (GameState.objects restarted))) []
    Spec.assertEqWith s "bob's 3-card library still comes back whole" (length (Game.zoneMembers Zone.Library S.bob after)) 3
    Spec.assertEqWith s "alice's library is unaffected" (length (Game.zoneMembers Zone.Library S.alice after)) 3
    Spec.assertEqWith s "carol's library is unaffected" (length (Game.zoneMembers Zone.Library S.carol after)) 3

  Spec.it s "CR 729.2/729.4 #147: a subgame seats only the players still in the main game" $ do
    -- CR 729.2: "Each player takes all the cards in their main-game library,
    -- moves them to their subgame library, and shuffles them." Each player IN
    -- the game -- CR 729.4: "All players not currently in the subgame are
    -- considered outside the subgame." Today the rebuilt order is
    -- rotateTo carol [alice, bob, carol] = [carol, alice, bob], with bob in it.
    let g0 = Departure.depart Departure.Type.Conceded S.bob (Setup.emptyGame S.threePlayers)
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
