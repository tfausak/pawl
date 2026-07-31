module Pawl.DecideSpec where

import qualified Data.Map.Strict as Map
import qualified Pawl.Engine.Decide as Decide
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Support as S
import qualified Pawl.Types.Decider as Decider
import qualified Pawl.Types.GameState as GameState
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

tests :: Tasty.TestTree
tests =
  Tasty.testGroup
    "Pawl.Engine.Decide"
    [ HU.testCase "CR 722: with no control, a player decides for themselves" $
        let gs = Setup.emptyGame S.bothPlayers
         in HU.assertEqual "alice decides for alice" (Decider.MkDecider S.alice) (Decide.deciderFor S.alice gs),
      HU.testCase "CR 723.3: an active controlled player's decisions route to the controller" $
        let gs = (Setup.emptyGame S.bothPlayers) {GameState.activePlayer = S.bob, GameState.activeControl = Just (Decider.MkDecider S.alice)}
         in do
              HU.assertEqual "bob's decisions route to alice" (Decider.MkDecider S.alice) (Decide.deciderFor S.bob gs)
              HU.assertEqual "alice still decides for herself" (Decider.MkDecider S.alice) (Decide.deciderFor S.alice gs),
      HU.testCase "control applies only to the active player" $
        let gs = (Setup.emptyGame S.bothPlayers) {GameState.activePlayer = S.alice, GameState.activeControl = Just (Decider.MkDecider S.alice), GameState.pendingControl = Map.empty}
         in HU.assertEqual "bob is not active, so unaffected" (Decider.MkDecider S.bob) (Decide.deciderFor S.bob gs)
    ]
