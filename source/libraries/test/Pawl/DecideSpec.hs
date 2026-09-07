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
  Spec.it s "CR 723.1: with no control, a player decides for themselves" $ do
    let gs = Setup.emptyGame S.bothPlayers
    Spec.assertEq s (Decide.deciderFor S.alice gs) $ Decider.MkDecider S.alice

  Spec.describe s "CR 723.3: an active controlled player's decisions route to the controller" $ do
    let gs = (Setup.emptyGame S.bothPlayers) {GameState.activePlayer = S.bob, GameState.control = S.turnControl S.alice S.bob}

    Spec.it s "bob's decisions route to alice" $ do
      Spec.assertEq s (Decide.deciderFor S.bob gs) $ Decider.MkDecider S.alice

    Spec.it s "alice still decides for herself" $ do
      Spec.assertEq s (Decide.deciderFor S.alice gs) $ Decider.MkDecider S.alice

  -- CR 723.2's shape, which the active-player guard deciderFor used to carry
  -- made unreachable (#881): alice controls bob on ALICE's turn, for the length
  -- of one resolution. CR 723.3 is what says this is not a contradiction -- the
  -- rule fixes who the active player is, not who may be controlled.
  Spec.describe s "CR 723.2: a controlled player who is not the active player" $ do
    let gs =
          (Setup.emptyGame S.bothPlayers)
            { GameState.activePlayer = S.alice,
              GameState.control = Map.singleton S.bob (S.resolutionControl S.alice),
              GameState.pendingControl = Map.empty
            }

    Spec.it s "bob's decisions route to alice" $ do
      Spec.assertEq s (Decide.deciderFor S.bob gs) $ Decider.MkDecider S.alice

    -- CR 723.8: the controller keeps making their own.
    Spec.it s "alice still decides for herself" $ do
      Spec.assertEq s (Decide.deciderFor S.alice gs) $ Decider.MkDecider S.alice

  -- The lookup names ONE seat: a row for alice says nothing about bob, which is
  -- what a single Maybe keyed to the active player could not express.
  Spec.it s "a seat with no control row decides for themselves" $ do
    let gs = (Setup.emptyGame S.bothPlayers) {GameState.activePlayer = S.alice, GameState.control = S.turnControl S.carol S.alice, GameState.pendingControl = Map.empty}
    Spec.assertEq s (Decide.deciderFor S.bob gs) $ Decider.MkDecider S.bob
