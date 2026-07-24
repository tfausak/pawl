-- Covers Pawl.Departure: who is still in the game, and the CR 104.2a/104.3
-- consequences of leaving it.
module Pawl.DepartureSpec where

import qualified Data.Map.Strict as Map
import qualified Pawl.Departure as Departure
import qualified Pawl.Setup as Setup
import qualified Pawl.Support as S
import qualified Pawl.Type.Departure as Departure.Type
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Player as Player
import qualified Pawl.Type.PlayerId as PlayerId
import qualified Pawl.Type.Result as Result
import qualified Pawl.Type.Status as Status
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

statusOf :: PlayerId.PlayerId -> GameState.GameState -> Maybe Status.Status
statusOf pid gs = fmap Player.status (Map.lookup pid (GameState.players gs))

tests :: Tasty.TestTree
tests =
  Tasty.testGroup
    "Departure"
    [ HU.testCase "CR 104.3a a conceding player leaves immediately, with Conceded as the reason" $
        let gs = Setup.emptyGame S.bothPlayers
            after = S.runPure S.identityAnswer gs (Departure.leaveGame Departure.Type.Conceded S.alice)
         in HU.assertEqual "alice departed by conceding" (Just (Status.Departed Departure.Type.Conceded)) (statusOf S.alice after),
      HU.testCase "CR 104.2a the last player standing wins, without waiting for a state-based action check" $
        -- leaveGame settles the outcome itself. Nothing runs an SBA pass here.
        let gs = Setup.emptyGame S.bothPlayers
            after = S.runPure S.identityAnswer gs (Departure.leaveGame Departure.Type.Conceded S.alice)
         in HU.assertEqual "bob wins on the spot" (Just (Result.Won S.bob)) (GameState.result after),
      HU.testCase "an already-decided result is not overwritten" $
        let gs = (Setup.emptyGame S.bothPlayers) {GameState.result = Just Result.Drawn}
            after = S.runPure S.identityAnswer gs (Departure.leaveGame Departure.Type.Conceded S.alice)
         in HU.assertEqual "the first result stands" (Just Result.Drawn) (GameState.result after),
      HU.testCase "stillPlaying omits a departed player" $
        let gs = Setup.emptyGame S.bothPlayers
            after = S.runPure S.identityAnswer gs (Departure.leaveGame Departure.Type.Conceded S.alice)
         in HU.assertEqual "only bob remains" [S.bob] (Departure.stillPlaying after)
    ]
