module Pawl.DecideSpec where

import qualified Data.Map.Strict as Map
import qualified Pawl.Engine.Decide as Decide
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Decider as Decider
import qualified Pawl.Types.GameState as GameState

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Engine.Decide" $ do
  Spec.it s "CR 722: with no control, a player decides for themselves" $ do
    let gs = Setup.emptyGame S.bothPlayers
    Spec.assertEq s (Decide.deciderFor S.alice gs) $ Decider.MkDecider S.alice

  Spec.describe s "CR 723.3: an active controlled player's decisions route to the controller" $ do
    let gs = (Setup.emptyGame S.bothPlayers) {GameState.activePlayer = S.bob, GameState.activeControl = Just (Decider.MkDecider S.alice)}

    Spec.it s "bob's decisions route to alice" $ do
      Spec.assertEq s (Decide.deciderFor S.bob gs) $ Decider.MkDecider S.alice

    Spec.it s "alice still decides for herself" $ do
      Spec.assertEq s (Decide.deciderFor S.alice gs) $ Decider.MkDecider S.alice

  Spec.it s "control applies only to the active player" $ do
    let gs = (Setup.emptyGame S.bothPlayers) {GameState.activePlayer = S.alice, GameState.activeControl = Just (Decider.MkDecider S.alice), GameState.pendingControl = Map.empty}
    Spec.assertEq s (Decide.deciderFor S.bob gs) $ Decider.MkDecider S.bob
