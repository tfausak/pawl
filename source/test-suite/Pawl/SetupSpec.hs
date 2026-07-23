-- Covers Pawl.Setup and Pawl.Type.Deck: setup, deck composition, opening hands.
module Pawl.SetupSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Cards as Cards
import qualified Pawl.Engine as Engine
import qualified Pawl.Event as Event
import qualified Pawl.Game as Game
import qualified Pawl.Setup as Setup
import qualified Pawl.Support as S
import qualified Pawl.Turn as Turn
import qualified Pawl.Type.Deck as Deck
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Object as Object
import qualified Pawl.Type.Player as Player
import qualified Pawl.Type.PlayerCounterKind as PlayerCounterKind
import Pawl.Type.PlayerId (PlayerId)
import qualified Pawl.Type.Program as Program
import qualified Pawl.Type.Result as Result
import qualified Pawl.Type.Zone as Zone
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

deckTests :: Cards.Cards -> Tasty.TestTree
deckTests cards =
  Tasty.testGroup
    "Deck"
    [ HU.testCase "the red deck is 60 cards" $
        HU.assertEqual "size" 60 (Setup.deckSize (Cards.redDeck cards)),
      HU.testCase "the green deck is 60 cards" $
        HU.assertEqual "size" 60 (Setup.deckSize (Cards.greenDeck cards)),
      HU.testCase "the black deck is 60 cards" $
        HU.assertEqual "size" 60 (Setup.deckSize (Cards.blackDeck cards)),
      HU.testCase "red deck composition" $
        let Deck.MkDeck m = Cards.redDeck cards
         in do
              HU.assertEqual "mountains" (Just 36) (Map.lookup (Cards.mountainPrinting cards) m)
              HU.assertEqual "pikers" (Just 4) (Map.lookup (Cards.pikerPrinting cards) m)
              HU.assertEqual "maidens" (Just 4) (Map.lookup (Cards.birdMaidenPrinting cards) m)
              HU.assertEqual "bolts" (Just 4) (Map.lookup (Cards.lightningBoltPrinting cards) m)
              HU.assertEqual "blazes" (Just 4) (Map.lookup (Cards.blazePrinting cards) m)
              HU.assertEqual "dragon fodders" (Just 4) (Map.lookup (Cards.dragonFodderPrinting cards) m)
              HU.assertEqual "chaos charms" (Just 4) (Map.lookup (Cards.chaosCharmPrinting cards) m),
      HU.testCase "green deck composition" $
        let Deck.MkDeck m = Cards.greenDeck cards
         in do
              HU.assertEqual "forests" (Just 36) (Map.lookup (Cards.forestPrinting cards) m)
              HU.assertEqual "mammoths" (Just 8) (Map.lookup (Cards.warMammothPrinting cards) m)
              HU.assertEqual "fogs" (Just 4) (Map.lookup (Cards.fogPrinting cards) m)
              HU.assertEqual "giant growths" (Just 4) (Map.lookup (Cards.giantGrowthPrinting cards) m)
              HU.assertEqual "serpent's gifts" (Just 4) (Map.lookup (Cards.serpentsGiftPrinting cards) m)
              HU.assertEqual "battlegrowths" (Just 4) (Map.lookup (Cards.battlegrowthPrinting cards) m),
      HU.testCase "black deck composition" $
        let Deck.MkDeck m = Cards.blackDeck cards
         in do
              HU.assertEqual "swamps" (Just 36) (Map.lookup (Cards.swampPrinting cards) m)
              HU.assertEqual "rats" (Just 8) (Map.lookup (Cards.typhoidRatsPrinting cards) m)
              HU.assertEqual "drudge skeletons" (Just 4) (Map.lookup (Cards.drudgeSkeletonsPrinting cards) m)
              HU.assertEqual "murders" (Just 4) (Map.lookup (Cards.murderPrinting cards) m)
              HU.assertEqual "mind rots" (Just 4) (Map.lookup (Cards.mindRotPrinting cards) m)
              HU.assertEqual "instill infections" (Just 4) (Map.lookup (Cards.instillInfectionPrinting cards) m),
      HU.testCase "36 Mountains per player after a red-red setup" $
        HU.assertEqual "mountains" 36 (S.countByName (Text.pack "Mountain") S.alice (setupState cards)),
      HU.testCase "4 Bird Maidens per player after a red-red setup" $
        HU.assertEqual "maidens" 4 (S.countByName (Text.pack "Bird Maiden") S.alice (setupState cards)),
      HU.testCase "4 Pikers per player after a red-red setup" $
        HU.assertEqual "pikers" 4 (S.countByName (Text.pack "Goblin Piker") S.bob (setupState cards)),
      HU.testCase "4 Lightning Bolts per player after a red-red setup" $
        HU.assertEqual "bolts" 4 (S.countByName (Text.pack "Lightning Bolt") S.alice (setupState cards)),
      HU.testCase "4 Blazes per player after a red-red setup" $
        HU.assertEqual "blazes" 4 (S.countByName (Text.pack "Blaze") S.alice (setupState cards)),
      HU.testCase "4 Dragon Fodders per player after a red-red setup" $
        HU.assertEqual "dragon fodders" 4 (S.countByName (Text.pack "Dragon Fodder") S.alice (setupState cards)),
      HU.testCase "4 Chaos Charms per player after a red-red setup" $
        HU.assertEqual "chaos charms" 4 (S.countByName (Text.pack "Chaos Charm") S.alice (setupState cards))
    ]

setupState :: Cards.Cards -> GameState.GameState
setupState cards =
  Program.foldProgram
    S.identityAnswer
    (State.execStateT (Setup.newGame (S.redRed cards)) (Setup.emptyGame S.bothPlayers))

setupTests :: Cards.Cards -> Tasty.TestTree
setupTests cards =
  Tasty.testGroup
    "Setup"
    [ HU.testCase "120 objects after setup" $
        HU.assertEqual "count" 120 (Game.objectCount (setupState cards)),
      HU.testCase "each library has 53 after opening draws" $
        HU.assertEqual "library" 53 (length (Game.zoneMembers Zone.Library S.alice (setupState cards))),
      HU.testCase "each hand has 7" $
        HU.assertEqual "hand" 7 (length (Game.zoneMembers Zone.Hand S.bob (setupState cards))),
      HU.testCase "active player is first in turn order" $
        HU.assertEqual "active" S.alice (GameState.activePlayer (setupState cards)),
      HU.testCase "runMatch derives the players from the matchup (#24)" $
        let (result, final) = Engine.runMatchPure S.identityAnswer (S.redRed cards)
         in do
              HU.assertBool "has a result" (Maybe.isJust (GameState.result final))
              HU.assertEqual "both players have a life total" 2 (Map.size (GameState.players final))
              HU.assertEqual "the result is the run's result" (Just result) (GameState.result final),
      HU.testCase "CR 122.1 a new player starts with no counters" $
        let gs = Setup.emptyGame S.bothPlayers
         in HU.assertEqual "empty" (Just Map.empty) (fmap Player.counters (Map.lookup S.alice (GameState.players gs))),
      HU.testCase "CR 400.1 a new game's command zone is empty" $
        let gs = Setup.emptyGame S.bothPlayers
         in HU.assertEqual "empty command" mempty (GameState.command gs),
      HU.testCase "CR 725.1 a new game has no monarch" $
        HU.assertEqual "no monarch" Nothing (GameState.monarch (Setup.emptyGame S.bothPlayers))
    ]

greenBlackSetup :: Cards.Cards -> GameState.GameState
greenBlackSetup cards =
  Program.foldProgram
    S.identityAnswer
    (State.execStateT (Setup.newGame (S.greenBlack cards)) (Setup.emptyGame S.bothPlayers))

greenBlackSetupTests :: Cards.Cards -> Tasty.TestTree
greenBlackSetupTests cards =
  Tasty.testGroup
    "GreenBlackSetup"
    [ HU.testCase "alice's green deck deals 36 Forests" $
        HU.assertEqual "forests" 36 (S.countByName (Text.pack "Forest") S.alice (greenBlackSetup cards)),
      HU.testCase "bob's black deck deals 36 Swamps" $
        HU.assertEqual "swamps" 36 (S.countByName (Text.pack "Swamp") S.bob (greenBlackSetup cards)),
      HU.testCase "green-black setup conserves 120 objects" $
        HU.assertEqual "count" 120 (Game.objectCount (greenBlackSetup cards))
    ]

-- Add n Mountains to pid's battlefield, discarding the ids (used to bulk up a
-- pool of owned cards). replicate n () avoids a list comprehension (CLAUDE.md).
addMany :: Cards.Cards -> Int -> PlayerId -> GameState.GameState -> GameState.GameState
addMany cards n pid gs =
  List.foldl' (\g _ -> snd (S.addCreature (Cards.mountainPrinting cards) pid g)) gs (replicate n ())

restartTests :: Cards.Cards -> Tasty.TestTree
restartTests cards =
  Tasty.testGroup
    "restart (CR 727)"
    [ HU.testCase "startGameFromCards: libraries are rebuilt from the existing owned cards, hands drawn" $
        -- alice and bob each own 8 cards, all currently on the battlefield. After
        -- startGameFromCards each has a 7-card hand and a 1-card library, the
        -- battlefield is empty, and ownership is unchanged (CR 727.2 / 103.5).
        let g0 = Setup.emptyGame S.bothPlayers
            g1 = addMany cards 8 S.alice g0
            g2 = addMany cards 8 S.bob g1
            after = snd (Engine.runGamePure S.identityAnswer g2 Setup.startGameFromCards)
            libSize pid = length (Game.zoneMembers Zone.Library pid after)
         in do
              HU.assertEqual "alice drew a 7-card opening hand" 7 (S.handSize S.alice after)
              HU.assertEqual "bob drew a 7-card opening hand" 7 (S.handSize S.bob after)
              HU.assertEqual "alice's library holds the remaining owned card" 1 (libSize S.alice)
              HU.assertEqual "bob's library holds the remaining owned card" 1 (libSize S.bob)
              HU.assertEqual "the battlefield is empty after the rebuild" True (Set.null (GameState.battlefield after))
              HU.assertEqual "every rebuilt object is owned by alice or bob (ownership preserved)" True (all (\o -> Object.owner o == S.alice || Object.owner o == S.bob) (Map.elems (GameState.objects after))),
      HU.testCase "CR 727.1a: the starting player is the restart's controller, at the head of the turn order" $
        -- Two restarts of the same board, controlled by different players: the
        -- active player and the head of the turn order follow the controller.
        let g0 = addMany cards 8 S.bob (addMany cards 8 S.alice (Setup.emptyGame S.bothPlayers))
            byBob = snd (Engine.runGamePure S.identityAnswer g0 (Setup.restartGame S.bob))
            byAlice = snd (Engine.runGamePure S.identityAnswer g0 (Setup.restartGame S.alice))
         in do
              HU.assertEqual "bob restarted: bob is the new active player" S.bob (GameState.activePlayer byBob)
              HU.assertEqual "bob restarted: bob heads the turn order" (Just S.bob) (Maybe.listToMaybe (GameState.turnOrder byBob))
              HU.assertEqual "alice restarted: alice is the new active player" S.alice (GameState.activePlayer byAlice)
              HU.assertEqual "alice restarted: alice heads the turn order" (Just S.alice) (Maybe.listToMaybe (GameState.turnOrder byAlice)),
      HU.testCase "CR 727.2: every owned card returns to its owner (library or hand), regardless of prior zone" $
        -- alice owns 8 cards, one on the battlefield; bob owns 8, one moved to his
        -- graveyard. CR 400.7 gives drawn cards FRESH ids (Event.changeZone mints a
        -- new object on a zone change), so a specific pre-restart ObjectId need not
        -- survive an opening draw -- CR 727.2 preserves OWNERSHIP, not object ids.
        -- Assert on per-owner counts: after the restart every owned card is in that
        -- owner's library or hand, none on the battlefield or in a graveyard, and
        -- bob's graveyard card is proven to return by his count staying 8.
        let g0 = Setup.emptyGame S.bothPlayers
            (_aId, g1) = S.addCreature (Cards.mountainPrinting cards) S.alice g0
            (bId, g2) = S.addCreature (Cards.mountainPrinting cards) S.bob g1
            g3 = addMany cards 7 S.alice (addMany cards 7 S.bob g2)
            -- move bob's card to his graveyard, to prove zone-independence.
            g4 = snd (Engine.runGamePure S.identityAnswer g3 (Event.changeZone bId Zone.Graveyard))
            after = snd (Engine.runGamePure S.identityAnswer g4 (Setup.restartGame S.alice))
            ownedCount pid = length (filter (\o -> Object.owner o == pid) (Map.elems (GameState.objects after)))
            libHandCount pid = length (Game.zoneMembers Zone.Library pid after) + length (Game.zoneMembers Zone.Hand pid after)
         in do
              HU.assertEqual "alice still owns all 8 of her cards" 8 (ownedCount S.alice)
              HU.assertEqual "bob still owns all 8 of his cards (incl. the one from his graveyard)" 8 (ownedCount S.bob)
              HU.assertEqual "all of alice's cards are in her library or hand" 8 (libHandCount S.alice)
              HU.assertEqual "all of bob's cards are in his library or hand" 8 (libHandCount S.bob)
              HU.assertEqual "no card is left on the battlefield" True (Set.null (GameState.battlefield after))
              HU.assertEqual "no graveyard survives the restart" True (all null (Map.elems (GameState.graveyard after))),
      HU.testCase "CR 727.4: the restart settles just before the first untap step, no priority, turn 1, life reset" $
        -- Knock bob down to 5 life and give him 3 poison counters before the
        -- restart, so the "back to 20 life / no counters" assertions below are
        -- load-bearing -- Setup.emptyGame already starts players at 20 life with
        -- no counters, so without this mutation the assertions would pass even
        -- if resetPlayer did nothing.
        let g0 = addMany cards 8 S.bob (addMany cards 8 S.alice (Setup.emptyGame S.bothPlayers))
            g1 = S.addPlayerCounter PlayerCounterKind.Poison 3 S.bob g0
            g2 = g1 {GameState.players = Map.adjust (\p -> p {Player.life = 5}) S.bob (GameState.players g1)}
            after = snd (Engine.runGamePure S.identityAnswer g2 (Setup.restartGame S.bob))
         in do
              HU.assertEqual "phase is the first turn's untap step" Turn.firstPhase (GameState.phase after)
              HU.assertEqual "no player holds priority" Nothing (GameState.priority after)
              HU.assertEqual "it is turn 1" 1 (GameState.turnNumber after)
              HU.assertEqual "the stack is empty" [] (GameState.stack after)
              HU.assertEqual "alice is back to 20 life" (Just 20) (S.lifeOf S.alice after)
              HU.assertEqual "bob is back to 20 life" (Just 20) (S.lifeOf S.bob after)
              HU.assertEqual "CR 103.4/727.1: bob's life reset to 20" (Just 20) (S.lifeOf S.bob after)
              HU.assertEqual "bob's poison counters cleared on restart" 0 (S.playerCounterOf PlayerCounterKind.Poison S.bob after),
      HU.testCase "CR 727.3: a player owning fewer than seven cards loses at the next SBA check" $
        -- bob owns only 3 cards; drawing an opening hand of 7 draws from an empty
        -- library, flagging drewFromEmpty, so the existing SBA path makes bob lose
        -- and alice win. (In live play this fires at the first upkeep, CR 727.3;
        -- here it is asserted at the next explicit SBA check.)
        let g0 = addMany cards 3 S.bob (addMany cards 8 S.alice (Setup.emptyGame S.bothPlayers))
            afterRestart = snd (Engine.runGamePure S.identityAnswer g0 (Setup.restartGame S.alice))
            afterSba = snd (Engine.runGamePure S.identityAnswer afterRestart Engine.checkSba)
         in do
              HU.assertEqual "bob drew from an empty library during the opening draw" True (Set.member S.bob (GameState.drewFromEmpty afterRestart))
              HU.assertEqual "CR 727.3: bob loses, alice wins at the SBA check" (Just (Result.Won S.alice)) (GameState.result afterSba)
    ]

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

subgameTests :: Cards.Cards -> Tasty.TestTree
subgameTests cards =
  Tasty.testGroup
    "subgames (CR 729)"
    [ HU.testCase "CR 729.2: subgameStateFrom takes ONLY library cards; battlefield/hand do not enter" $
        -- alice owns 5 cards: 2 relocated to her library, 3 left on the battlefield.
        -- The subgame state's object pool must be exactly the 2 library cards.
        let g0 = Setup.emptyGame S.bothPlayers
            g1 = addMany cards 5 S.alice g0
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
            sub = Setup.subgameStateFrom g2
         in do
              HU.assertEqual "the subgame pool holds exactly the 2 library cards" 2 (Map.size (GameState.objects sub))
              HU.assertEqual "every subgame object is one of the 2 library cards" True (all (`elem` toLib) (Map.keys (GameState.objects sub)))
              HU.assertEqual "the subgame battlefield is empty (nothing but the library entered)" True (Set.null (GameState.battlefield sub))
              HU.assertEqual "the subgame nextObjectId is inherited from the parent" (GameState.nextObjectId g2) (GameState.nextObjectId sub)
              HU.assertEqual "the subgame is a fresh game at turn 1" 1 (GameState.turnNumber sub),
      HU.testCase "CR 729.5: funnelBack returns every owned subgame card to its owner's library, ids do not collide" $
        -- Parent: alice and bob each own 3 cards on the battlefield plus 2 in their
        -- library (so the parent has non-library objects that must SURVIVE, and
        -- library objects that get REPLACED). finalSub is a GENUINELY PLAYED
        -- subgame -- Setup.startGameFromCards runs on sub0, shuffling each
        -- player's 2-card library and drawing an opening hand (CR 103.5) -- so
        -- Event.drawCard's changeZone mints fresh object ids above the parent's
        -- supply for every card actually drawn (CR 400.7), the way a real
        -- subgame would.
        let g0 = Setup.emptyGame S.bothPlayers
            g1 = poolToLibrary S.bob (poolToLibrary S.alice (addMany cards 5 S.bob (addMany cards 5 S.alice g0)))
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
            sub0 = Setup.subgameStateFrom parent
            (_, finalSub) = Engine.runGamePure S.identityAnswer sub0 Setup.startGameFromCards
            after = Setup.funnelBack finalSub parent
            libCount pid = length (Game.zoneMembers Zone.Library pid after)
            battlefieldSurvivors = Set.size (GameState.battlefield after)
         in do
              -- alice/bob each still own all their cards, all in their library, none lost
              HU.assertEqual "alice's library holds exactly her 2 subgame cards, returned" 2 (libCount S.alice)
              HU.assertEqual "bob's library holds exactly his 2 subgame cards, returned" 2 (libCount S.bob)
              HU.assertEqual "the parent's non-library survivors are untouched (6 on the battlefield)" 6 battlefieldSurvivors
              HU.assertEqual "no object id collides (object count = survivors + returned cards)" (Map.size (GameState.objects after)) (battlefieldSurvivors + libCount S.alice + libCount S.bob)
              HU.assertEqual "the subgame genuinely minted fresh ids (drawCard's changeZone, CR 400.7)" True (GameState.nextObjectId finalSub > GameState.nextObjectId sub0)
              HU.assertEqual "the id supply advanced to exactly the subgame high-water mark" (GameState.nextObjectId finalSub) (GameState.nextObjectId after)
    ]

tests :: Cards.Cards -> Tasty.TestTree
tests cards = Tasty.testGroup "Setup" [setupTests cards, greenBlackSetupTests cards, deckTests cards, restartTests cards, subgameTests cards]
