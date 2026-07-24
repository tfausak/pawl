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
import qualified Pawl.Registry as Registry
import qualified Pawl.Setup as Setup
import qualified Pawl.Support as S
import qualified Pawl.Turn as Turn
import qualified Pawl.Type.Deck as Deck
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Object as Object
import qualified Pawl.Type.Player as Player
import qualified Pawl.Type.PlayerCounterKind as PlayerCounterKind
import Pawl.Type.PlayerId (PlayerId)
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Program as Program
import qualified Pawl.Type.Registry as Registry.Type
import qualified Pawl.Type.Result as Result
import qualified Pawl.Type.Zone as Zone
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

deckTests :: Registry.Type.Registry -> Tasty.TestTree
deckTests registry =
  Tasty.testGroup
    "Deck"
    [ HU.testCase "the red deck is 60 cards" $ do
        deck <- Cards.redDeck registry
        HU.assertEqual "size" 60 (Setup.deckSize deck),
      HU.testCase "the green deck is 60 cards" $ do
        deck <- Cards.greenDeck registry
        HU.assertEqual "size" 60 (Setup.deckSize deck),
      HU.testCase "the black deck is 60 cards" $ do
        deck <- Cards.blackDeck registry
        HU.assertEqual "size" 60 (Setup.deckSize deck),
      HU.testCase "red deck composition" $ do
        mountain <- Registry.printing registry "Mountain"
        piker <- Registry.printing registry "Goblin Piker"
        birdMaiden <- Registry.printing registry "Bird Maiden"
        bolt <- Registry.printing registry "Lightning Bolt"
        blaze <- Registry.printing registry "Blaze"
        dragonFodder <- Registry.printing registry "Dragon Fodder"
        chaosCharm <- Registry.printing registry "Chaos Charm"
        deck <- Cards.redDeck registry
        let Deck.MkDeck m = deck
        HU.assertEqual "mountains" (Just 36) (Map.lookup mountain m)
        HU.assertEqual "pikers" (Just 4) (Map.lookup piker m)
        HU.assertEqual "maidens" (Just 4) (Map.lookup birdMaiden m)
        HU.assertEqual "bolts" (Just 4) (Map.lookup bolt m)
        HU.assertEqual "blazes" (Just 4) (Map.lookup blaze m)
        HU.assertEqual "dragon fodders" (Just 4) (Map.lookup dragonFodder m)
        HU.assertEqual "chaos charms" (Just 4) (Map.lookup chaosCharm m),
      HU.testCase "green deck composition" $ do
        forest <- Registry.printing registry "Forest"
        warMammoth <- Registry.printing registry "War Mammoth"
        fog <- Registry.printing registry "Fog"
        giantGrowth <- Registry.printing registry "Giant Growth"
        serpentsGift <- Registry.printing registry "Serpent's Gift"
        battlegrowth <- Registry.printing registry "Battlegrowth"
        deck <- Cards.greenDeck registry
        let Deck.MkDeck m = deck
        HU.assertEqual "forests" (Just 36) (Map.lookup forest m)
        HU.assertEqual "mammoths" (Just 8) (Map.lookup warMammoth m)
        HU.assertEqual "fogs" (Just 4) (Map.lookup fog m)
        HU.assertEqual "giant growths" (Just 4) (Map.lookup giantGrowth m)
        HU.assertEqual "serpent's gifts" (Just 4) (Map.lookup serpentsGift m)
        HU.assertEqual "battlegrowths" (Just 4) (Map.lookup battlegrowth m),
      HU.testCase "black deck composition" $ do
        swamp <- Registry.printing registry "Swamp"
        typhoidRats <- Registry.printing registry "Typhoid Rats"
        drudgeSkeletons <- Registry.printing registry "Drudge Skeletons"
        murder <- Registry.printing registry "Murder"
        mindRot <- Registry.printing registry "Mind Rot"
        instillInfection <- Registry.printing registry "Instill Infection"
        deck <- Cards.blackDeck registry
        let Deck.MkDeck m = deck
        HU.assertEqual "swamps" (Just 36) (Map.lookup swamp m)
        HU.assertEqual "rats" (Just 8) (Map.lookup typhoidRats m)
        HU.assertEqual "drudge skeletons" (Just 4) (Map.lookup drudgeSkeletons m)
        HU.assertEqual "murders" (Just 4) (Map.lookup murder m)
        HU.assertEqual "mind rots" (Just 4) (Map.lookup mindRot m)
        HU.assertEqual "instill infections" (Just 4) (Map.lookup instillInfection m),
      HU.testCase "36 Mountains per player after a red-red setup" $ do
        gs <- setupState registry
        HU.assertEqual "mountains" 36 (S.countByName (Text.pack "Mountain") S.alice gs),
      HU.testCase "4 Bird Maidens per player after a red-red setup" $ do
        gs <- setupState registry
        HU.assertEqual "maidens" 4 (S.countByName (Text.pack "Bird Maiden") S.alice gs),
      HU.testCase "4 Pikers per player after a red-red setup" $ do
        gs <- setupState registry
        HU.assertEqual "pikers" 4 (S.countByName (Text.pack "Goblin Piker") S.bob gs),
      HU.testCase "4 Lightning Bolts per player after a red-red setup" $ do
        gs <- setupState registry
        HU.assertEqual "bolts" 4 (S.countByName (Text.pack "Lightning Bolt") S.alice gs),
      HU.testCase "4 Blazes per player after a red-red setup" $ do
        gs <- setupState registry
        HU.assertEqual "blazes" 4 (S.countByName (Text.pack "Blaze") S.alice gs),
      HU.testCase "4 Dragon Fodders per player after a red-red setup" $ do
        gs <- setupState registry
        HU.assertEqual "dragon fodders" 4 (S.countByName (Text.pack "Dragon Fodder") S.alice gs),
      HU.testCase "4 Chaos Charms per player after a red-red setup" $ do
        gs <- setupState registry
        HU.assertEqual "chaos charms" 4 (S.countByName (Text.pack "Chaos Charm") S.alice gs)
    ]

setupState :: Registry.Type.Registry -> IO GameState.GameState
setupState registry = do
  matchup <- S.redRed registry
  pure (Program.foldProgram S.identityAnswer (State.execStateT (Setup.newGame matchup) (Setup.emptyGame S.bothPlayers)))

setupTests :: Registry.Type.Registry -> Tasty.TestTree
setupTests registry =
  Tasty.testGroup
    "Setup"
    [ HU.testCase "120 objects after setup" $ do
        gs <- setupState registry
        HU.assertEqual "count" 120 (Game.objectCount gs),
      HU.testCase "each library has 53 after opening draws" $ do
        gs <- setupState registry
        HU.assertEqual "library" 53 (length (Game.zoneMembers Zone.Library S.alice gs)),
      HU.testCase "each hand has 7" $ do
        gs <- setupState registry
        HU.assertEqual "hand" 7 (length (Game.zoneMembers Zone.Hand S.bob gs)),
      HU.testCase "active player is first in turn order" $ do
        gs <- setupState registry
        HU.assertEqual "active" S.alice (GameState.activePlayer gs),
      HU.testCase "runMatch derives the players from the matchup (#24)" $ do
        matchup <- S.redRed registry
        let (result, final) = Engine.runMatchPure S.identityAnswer matchup
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

greenBlackSetup :: Registry.Type.Registry -> IO GameState.GameState
greenBlackSetup registry = do
  matchup <- S.greenBlack registry
  pure (Program.foldProgram S.identityAnswer (State.execStateT (Setup.newGame matchup) (Setup.emptyGame S.bothPlayers)))

greenBlackSetupTests :: Registry.Type.Registry -> Tasty.TestTree
greenBlackSetupTests registry =
  Tasty.testGroup
    "GreenBlackSetup"
    [ HU.testCase "alice's green deck deals 36 Forests" $ do
        gs <- greenBlackSetup registry
        HU.assertEqual "forests" 36 (S.countByName (Text.pack "Forest") S.alice gs),
      HU.testCase "bob's black deck deals 36 Swamps" $ do
        gs <- greenBlackSetup registry
        HU.assertEqual "swamps" 36 (S.countByName (Text.pack "Swamp") S.bob gs),
      HU.testCase "green-black setup conserves 120 objects" $ do
        gs <- greenBlackSetup registry
        HU.assertEqual "count" 120 (Game.objectCount gs)
    ]

-- Add n Mountains to pid's battlefield, discarding the ids (used to bulk up a
-- pool of owned cards). replicate n () avoids a list comprehension (CLAUDE.md).
addMany :: Printing.Printing -> Int -> PlayerId -> GameState.GameState -> GameState.GameState
addMany mountain n pid gs =
  List.foldl' (\g _ -> snd (S.addCreature mountain pid g)) gs (replicate n ())

restartTests :: Registry.Type.Registry -> Tasty.TestTree
restartTests registry =
  Tasty.testGroup
    "restart (CR 727)"
    [ HU.testCase "startGameFromCards: libraries are rebuilt from the existing owned cards, hands drawn" $ do
        -- alice and bob each own 8 cards, all currently on the battlefield. After
        -- startGameFromCards each has a 7-card hand and a 1-card library, the
        -- battlefield is empty, and ownership is unchanged (CR 727.2 / 103.5).
        mountain <- Registry.printing registry "Mountain"
        let g0 = Setup.emptyGame S.bothPlayers
            g1 = addMany mountain 8 S.alice g0
            g2 = addMany mountain 8 S.bob g1
            after = snd (Engine.runGamePure S.identityAnswer g2 Setup.startGameFromCards)
            libSize pid = length (Game.zoneMembers Zone.Library pid after)
        HU.assertEqual "alice drew a 7-card opening hand" 7 (S.handSize S.alice after)
        HU.assertEqual "bob drew a 7-card opening hand" 7 (S.handSize S.bob after)
        HU.assertEqual "alice's library holds the remaining owned card" 1 (libSize S.alice)
        HU.assertEqual "bob's library holds the remaining owned card" 1 (libSize S.bob)
        HU.assertEqual "the battlefield is empty after the rebuild" True (Set.null (GameState.battlefield after))
        HU.assertEqual "every rebuilt object is owned by alice or bob (ownership preserved)" True (all (\o -> Object.owner o == S.alice || Object.owner o == S.bob) (Map.elems (GameState.objects after))),
      HU.testCase "CR 727.1a: the starting player is the restart's controller, at the head of the turn order" $ do
        -- Two restarts of the same board, controlled by different players: the
        -- active player and the head of the turn order follow the controller.
        mountain <- Registry.printing registry "Mountain"
        let g0 = addMany mountain 8 S.bob (addMany mountain 8 S.alice (Setup.emptyGame S.bothPlayers))
            byBob = snd (Engine.runGamePure S.identityAnswer g0 (Setup.restartGame S.bob))
            byAlice = snd (Engine.runGamePure S.identityAnswer g0 (Setup.restartGame S.alice))
        HU.assertEqual "bob restarted: bob is the new active player" S.bob (GameState.activePlayer byBob)
        HU.assertEqual "bob restarted: bob heads the turn order" (Just S.bob) (Maybe.listToMaybe (GameState.turnOrder byBob))
        HU.assertEqual "alice restarted: alice is the new active player" S.alice (GameState.activePlayer byAlice)
        HU.assertEqual "alice restarted: alice heads the turn order" (Just S.alice) (Maybe.listToMaybe (GameState.turnOrder byAlice)),
      HU.testCase "CR 727.2: every owned card returns to its owner (library or hand), regardless of prior zone" $ do
        -- alice owns 8 cards, one on the battlefield; bob owns 8, one moved to his
        -- graveyard. CR 400.7 gives drawn cards FRESH ids (Event.changeZone mints a
        -- new object on a zone change), so a specific pre-restart ObjectId need not
        -- survive an opening draw -- CR 727.2 preserves OWNERSHIP, not object ids.
        -- Assert on per-owner counts: after the restart every owned card is in that
        -- owner's library or hand, none on the battlefield or in a graveyard, and
        -- bob's graveyard card is proven to return by his count staying 8.
        mountain <- Registry.printing registry "Mountain"
        let g0 = Setup.emptyGame S.bothPlayers
            (_aId, g1) = S.addCreature mountain S.alice g0
            (bId, g2) = S.addCreature mountain S.bob g1
            g3 = addMany mountain 7 S.alice (addMany mountain 7 S.bob g2)
            -- move bob's card to his graveyard, to prove zone-independence.
            g4 = snd (Engine.runGamePure S.identityAnswer g3 (Event.changeZone bId Zone.Graveyard))
            after = snd (Engine.runGamePure S.identityAnswer g4 (Setup.restartGame S.alice))
            ownedCount pid = length (filter (\o -> Object.owner o == pid) (Map.elems (GameState.objects after)))
            libHandCount pid = length (Game.zoneMembers Zone.Library pid after) + length (Game.zoneMembers Zone.Hand pid after)
        HU.assertEqual "alice still owns all 8 of her cards" 8 (ownedCount S.alice)
        HU.assertEqual "bob still owns all 8 of his cards (incl. the one from his graveyard)" 8 (ownedCount S.bob)
        HU.assertEqual "all of alice's cards are in her library or hand" 8 (libHandCount S.alice)
        HU.assertEqual "all of bob's cards are in his library or hand" 8 (libHandCount S.bob)
        HU.assertEqual "no card is left on the battlefield" True (Set.null (GameState.battlefield after))
        HU.assertEqual "no graveyard survives the restart" True (all null (Map.elems (GameState.graveyard after))),
      HU.testCase "CR 727.4: the restart settles just before the first untap step, no priority, turn 1, life reset" $ do
        -- Knock bob down to 5 life and give him 3 poison counters before the
        -- restart, so the "back to 20 life / no counters" assertions below are
        -- load-bearing -- Setup.emptyGame already starts players at 20 life with
        -- no counters, so without this mutation the assertions would pass even
        -- if resetPlayer did nothing.
        mountain <- Registry.printing registry "Mountain"
        let g0 = addMany mountain 8 S.bob (addMany mountain 8 S.alice (Setup.emptyGame S.bothPlayers))
            g1 = S.addPlayerCounter PlayerCounterKind.Poison 3 S.bob g0
            g2 = g1 {GameState.players = Map.adjust (\p -> p {Player.life = 5}) S.bob (GameState.players g1)}
            after = snd (Engine.runGamePure S.identityAnswer g2 (Setup.restartGame S.bob))
        HU.assertEqual "phase is the first turn's untap step" Turn.firstPhase (GameState.phase after)
        HU.assertEqual "no player holds priority" Nothing (GameState.priority after)
        HU.assertEqual "it is turn 1" 1 (GameState.turnNumber after)
        HU.assertEqual "the stack is empty" [] (GameState.stack after)
        HU.assertEqual "alice is back to 20 life" (Just 20) (S.lifeOf S.alice after)
        HU.assertEqual "bob is back to 20 life" (Just 20) (S.lifeOf S.bob after)
        HU.assertEqual "CR 103.4/727.1: bob's life reset to 20" (Just 20) (S.lifeOf S.bob after)
        HU.assertEqual "bob's poison counters cleared on restart" 0 (S.playerCounterOf PlayerCounterKind.Poison S.bob after),
      HU.testCase "CR 727.3: a player owning fewer than seven cards loses at the next SBA check" $ do
        -- bob owns only 3 cards; drawing an opening hand of 7 draws from an empty
        -- library, flagging drewFromEmpty, so the existing SBA path makes bob lose
        -- and alice win. (In live play this fires at the first upkeep, CR 727.3;
        -- here it is asserted at the next explicit SBA check.)
        mountain <- Registry.printing registry "Mountain"
        let g0 = addMany mountain 3 S.bob (addMany mountain 8 S.alice (Setup.emptyGame S.bothPlayers))
            afterRestart = snd (Engine.runGamePure S.identityAnswer g0 (Setup.restartGame S.alice))
            afterSba = snd (Engine.runGamePure S.identityAnswer afterRestart Engine.checkSba)
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

subgameTests :: Registry.Type.Registry -> Tasty.TestTree
subgameTests registry =
  Tasty.testGroup
    "subgames (CR 729)"
    [ HU.testCase "CR 729.2: subgameStateFrom takes ONLY library cards; battlefield/hand do not enter" $ do
        -- alice owns 5 cards: 2 relocated to her library, 3 left on the battlefield.
        -- The subgame state's object pool must be exactly the 2 library cards.
        mountain <- Registry.printing registry "Mountain"
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
        HU.assertEqual "the subgame pool holds exactly the 2 library cards" 2 (Map.size (GameState.objects sub))
        HU.assertEqual "every subgame object is one of the 2 library cards" True (all (`elem` toLib) (Map.keys (GameState.objects sub)))
        HU.assertEqual "the subgame battlefield is empty (nothing but the library entered)" True (Set.null (GameState.battlefield sub))
        HU.assertEqual "the subgame nextObjectId is inherited from the parent" (GameState.nextObjectId g2) (GameState.nextObjectId sub)
        HU.assertEqual "the subgame is a fresh game at turn 1" 1 (GameState.turnNumber sub),
      HU.testCase "CR 729.5: funnelBack returns every owned subgame card to its owner's library, ids do not collide" $ do
        -- Parent: alice and bob each own 3 cards on the battlefield plus 2 in their
        -- library (so the parent has non-library objects that must SURVIVE, and
        -- library objects that get REPLACED). finalSub is a GENUINELY PLAYED
        -- subgame -- Setup.startGameFromCards runs on sub0, shuffling each
        -- player's 2-card library and drawing an opening hand (CR 103.5) -- so
        -- Event.drawCard's changeZone mints fresh object ids above the parent's
        -- supply for every card actually drawn (CR 400.7), the way a real
        -- subgame would.
        mountain <- Registry.printing registry "Mountain"
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
            (_, finalSub) = Engine.runGamePure S.identityAnswer sub0 Setup.startGameFromCards
            after = Setup.funnelBack finalSub parent
            libCount pid = length (Game.zoneMembers Zone.Library pid after)
            battlefieldSurvivors = Set.size (GameState.battlefield after)
        -- alice/bob each still own all their cards, all in their library, none lost
        HU.assertEqual "alice's library holds exactly her 2 subgame cards, returned" 2 (libCount S.alice)
        HU.assertEqual "bob's library holds exactly his 2 subgame cards, returned" 2 (libCount S.bob)
        HU.assertEqual "the parent's non-library survivors are untouched (6 on the battlefield)" 6 battlefieldSurvivors
        HU.assertEqual "no object id collides (object count = survivors + returned cards)" (Map.size (GameState.objects after)) (battlefieldSurvivors + libCount S.alice + libCount S.bob)
        HU.assertEqual "the subgame genuinely minted fresh ids (drawCard's changeZone, CR 400.7)" True (GameState.nextObjectId finalSub > GameState.nextObjectId sub0)
        HU.assertEqual "the id supply advanced to exactly the subgame high-water mark" (GameState.nextObjectId finalSub) (GameState.nextObjectId after)
    ]

tests :: Registry.Type.Registry -> Tasty.TestTree
tests registry = Tasty.testGroup "Setup" [setupTests registry, greenBlackSetupTests registry, deckTests registry, restartTests registry, subgameTests registry]
