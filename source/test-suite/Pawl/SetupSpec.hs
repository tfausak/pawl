-- Covers Pawl.Setup and Pawl.Type.Deck: setup, deck composition, opening hands.
module Pawl.SetupSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Pawl.Cards as Cards
import qualified Pawl.Engine as Engine
import qualified Pawl.Game as Game
import qualified Pawl.Setup as Setup
import qualified Pawl.Support as S
import qualified Pawl.Type.Deck as Deck
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Program as Program
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
              HU.assertEqual "the result is the run's result" (Just result) (GameState.result final)
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

tests :: Cards.Cards -> Tasty.TestTree
tests cards = Tasty.testGroup "Setup" [setupTests cards, greenBlackSetupTests cards, deckTests cards]
