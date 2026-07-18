-- Covers Pawl.Setup and Pawl.Type.Deck: setup, deck composition, opening hands.
module Pawl.SetupSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Pawl.Card as Card
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

deckTests :: Tasty.TestTree
deckTests =
  Tasty.testGroup
    "Deck"
    [ HU.testCase "the red deck is 60 cards" $
        HU.assertEqual "size" 60 (Setup.deckSize Setup.redDeck),
      HU.testCase "the green deck is 60 cards" $
        HU.assertEqual "size" 60 (Setup.deckSize Setup.greenDeck),
      HU.testCase "the black deck is 60 cards" $
        HU.assertEqual "size" 60 (Setup.deckSize Setup.blackDeck),
      HU.testCase "red deck composition" $
        let Deck.MkDeck m = Setup.redDeck
         in do
              HU.assertEqual "mountains" (Just 36) (Map.lookup Card.mountainPrinting m)
              HU.assertEqual "pikers" (Just 12) (Map.lookup Card.pikerPrinting m)
              HU.assertEqual "maidens" (Just 8) (Map.lookup Card.birdMaidenPrinting m)
              HU.assertEqual "bolts" (Just 4) (Map.lookup Card.lightningBoltPrinting m),
      HU.testCase "green deck composition" $
        let Deck.MkDeck m = Setup.greenDeck
         in do
              HU.assertEqual "forests" (Just 36) (Map.lookup Card.forestPrinting m)
              HU.assertEqual "mammoths" (Just 16) (Map.lookup Card.warMammothPrinting m)
              HU.assertEqual "giant growths" (Just 4) (Map.lookup Card.giantGrowthPrinting m)
              HU.assertEqual "serpent's gifts" (Just 4) (Map.lookup Card.serpentsGiftPrinting m),
      HU.testCase "black deck composition" $
        let Deck.MkDeck m = Setup.blackDeck
         in do
              HU.assertEqual "swamps" (Just 36) (Map.lookup Card.swampPrinting m)
              HU.assertEqual "rats" (Just 24) (Map.lookup Card.typhoidRatsPrinting m),
      HU.testCase "36 Mountains per player after a red-red setup" $
        HU.assertEqual "mountains" 36 (S.countByName (Text.pack "Mountain") S.alice setupState),
      HU.testCase "8 Bird Maidens per player after a red-red setup" $
        HU.assertEqual "maidens" 8 (S.countByName (Text.pack "Bird Maiden") S.alice setupState),
      HU.testCase "12 Pikers per player after a red-red setup" $
        HU.assertEqual "pikers" 12 (S.countByName (Text.pack "Goblin Piker") S.bob setupState),
      HU.testCase "4 Lightning Bolts per player after a red-red setup" $
        HU.assertEqual "bolts" 4 (S.countByName (Text.pack "Lightning Bolt") S.alice setupState)
    ]

setupState :: GameState.GameState
setupState =
  Program.foldProgram
    S.identityAnswer
    (State.execStateT (Setup.newGame S.redRed) (Setup.emptyGame S.bothPlayers))

setupTests :: Tasty.TestTree
setupTests =
  Tasty.testGroup
    "Setup"
    [ HU.testCase "120 objects after setup" $
        HU.assertEqual "count" 120 (Game.objectCount setupState),
      HU.testCase "each library has 53 after opening draws" $
        HU.assertEqual "library" 53 (length (Game.zoneMembers Zone.Library S.alice setupState)),
      HU.testCase "each hand has 7" $
        HU.assertEqual "hand" 7 (length (Game.zoneMembers Zone.Hand S.bob setupState)),
      HU.testCase "active player is first in turn order" $
        HU.assertEqual "active" S.alice (GameState.activePlayer setupState),
      HU.testCase "runMatch derives the players from the matchup (git-bug 15de615)" $
        let (result, final) = Engine.runMatchPure S.identityAnswer S.redRed
         in do
              HU.assertBool "has a result" (Maybe.isJust (GameState.result final))
              HU.assertEqual "both players have a life total" 2 (Map.size (GameState.players final))
              HU.assertEqual "the result is the run's result" (Just result) (GameState.result final)
    ]

greenBlackSetup :: GameState.GameState
greenBlackSetup =
  Program.foldProgram
    S.identityAnswer
    (State.execStateT (Setup.newGame S.greenBlack) (Setup.emptyGame S.bothPlayers))

greenBlackSetupTests :: Tasty.TestTree
greenBlackSetupTests =
  Tasty.testGroup
    "GreenBlackSetup"
    [ HU.testCase "alice's green deck deals 36 Forests" $
        HU.assertEqual "forests" 36 (S.countByName (Text.pack "Forest") S.alice greenBlackSetup),
      HU.testCase "bob's black deck deals 36 Swamps" $
        HU.assertEqual "swamps" 36 (S.countByName (Text.pack "Swamp") S.bob greenBlackSetup),
      HU.testCase "green-black setup conserves 120 objects" $
        HU.assertEqual "count" 120 (Game.objectCount greenBlackSetup)
    ]

tests :: Tasty.TestTree
tests = Tasty.testGroup "Setup" [setupTests, greenBlackSetupTests, deckTests]
