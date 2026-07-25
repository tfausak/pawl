-- Covers Pawl.Departure: who is still in the game, and the CR 104.2a/104.3
-- consequences of leaving it.
module Pawl.DepartureSpec where

import qualified Data.Map.Strict as Map
import qualified Pawl.Departure as Departure
import qualified Pawl.Sba as Sba
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
      -- #142: the SAME precedence, through the OTHER door. Pawl.Sba settles its
      -- own outcome at the end of a state-based-action pass; before this it used
      -- the opposite order from leaveGame, so a pass could replace a result the
      -- game had already reached. Set up a decided draw alongside a player who
      -- would lose to CR 704.5a, so the pass computes a DIFFERENT outcome
      -- (Won bob) and the two orderings disagree about which survives.
      HU.testCase "CR 104.1 a state-based-action pass does not overwrite a decided result" $
        let dying player = player {Player.life = 0}
            gs =
              (Setup.emptyGame S.bothPlayers)
                { GameState.result = Just Result.Drawn,
                  GameState.players = Map.adjust dying S.alice (GameState.players (Setup.emptyGame S.bothPlayers))
                }
            after = S.runPure S.identityAnswer gs Sba.performStateBasedActions
         in do
              HU.assertEqual "the decided draw stands" (Just Result.Drawn) (GameState.result after)
              HU.assertEqual "alice still left the game (CR 704.5a is unaffected)" (Just (Status.Departed Departure.Type.Lost)) (statusOf S.alice after),
      HU.testCase "stillPlaying omits a departed player" $
        let gs = Setup.emptyGame S.bothPlayers
            after = S.runPure S.identityAnswer gs (Departure.leaveGame Departure.Type.Conceded S.alice)
         in HU.assertEqual "only bob remains" [S.bob] (Departure.stillPlaying after),
      HU.testCase "CR 800.4a/800.4m turnOrder is the SEATING roster: a departure does not shorten it" $
        -- The whole of M5.6a rests on this. CR 800.4m needs the departed seat to
        -- know when their turn WOULD have begun, and CR 800.4a's last sentence
        -- needs their position to find their successor. Pruning turnOrder makes
        -- both impossible; every read filters through stillPlaying instead.
        let after = S.runPure S.identityAnswer S.threePlayerGame (Departure.leaveGame Departure.Type.Conceded S.bob)
         in do
              HU.assertEqual "bob keeps his seat" [S.alice, S.bob, S.carol] (GameState.turnOrder after)
              HU.assertEqual "but is no longer playing" [S.alice, S.carol] (Departure.stillPlaying after),
      HU.testCase "CR 104.2a one departure does not decide a three-player game" $
        let after = S.runPure S.identityAnswer S.threePlayerGame (Departure.leaveGame Departure.Type.Conceded S.bob)
            andAnother = S.runPure S.identityAnswer after (Departure.leaveGame Departure.Type.Conceded S.carol)
         in do
              HU.assertEqual "two survivors, no result" Nothing (GameState.result after)
              HU.assertEqual "one survivor, alice wins" (Just (Result.Won S.alice)) (GameState.result andAnother),
      -- CR 725.4: "If the monarch leaves the game, the active player becomes the
      -- monarch at the same time as that player leaves the game."
      HU.testCase "CR 725.4 the monarch departs on someone else's turn: the active player takes the crown" $
        let board = S.withMonarch S.bob S.threePlayerGame
            gone = Departure.depart Departure.Type.Conceded S.bob board
         in do
              HU.assertEqual "alice is the active player on this board" S.alice (GameState.activePlayer board)
              HU.assertEqual "so alice is the monarch" (Just S.alice) (GameState.monarch gone),
      -- CR 725.4: "If the active player is leaving the game or if there is no
      -- active player, the next player in turn order who can become the monarch
      -- becomes the monarch."
      HU.testCase "CR 725.4 the monarch departs on their own turn: the next seat in turn order takes the crown" $
        let board = S.withMonarch S.alice S.threePlayerGame
            gone = Departure.depart Departure.Type.Conceded S.alice board
         in HU.assertEqual "bob, the seat after alice's" (Just S.bob) (GameState.monarch gone),
      HU.testCase "CR 725.4 the walk past the active player's seat skips a seat that has already departed" $
        -- alice is the active player and has already left, so there is no active
        -- player to crown; carol is the monarch and leaves too. The walk starts
        -- after alice's seat: bob, then carol. Bob is the only one still in the
        -- game, so bob takes the crown. Discriminating: a walk anchored on the
        -- DEPARTING MONARCH's seat instead of the active player's would find
        -- alice first and, on filtering her out, still land on bob -- so the
        -- second assertion pins the anchor by making the two disagree.
        let board = S.withMonarch S.carol (Departure.depart Departure.Type.Conceded S.alice S.threePlayerGame)
            gone = Departure.depart Departure.Type.Conceded S.carol board
         in do
              HU.assertEqual "alice is still the active player's seat (CR 800.4j)" S.alice (GameState.activePlayer gone)
              HU.assertEqual "bob takes the crown" (Just S.bob) (GameState.monarch gone),
      -- CR 725.4: "If no player still in the game can become the monarch, the
      -- game continues with no monarch."
      HU.testCase "CR 725.4 the last player standing is the monarch and leaves: no monarch, and no partial head" $
        let twoGone =
              Departure.depart
                Departure.Type.Conceded
                S.bob
                (Departure.depart Departure.Type.Conceded S.alice S.threePlayerGame)
            board = S.withMonarch S.carol twoGone
            gone = Departure.depart Departure.Type.Conceded S.carol board
         in do
              HU.assertEqual "nobody is left to become the monarch" Nothing (GameState.monarch gone)
              HU.assertEqual "and the roster is untouched" [S.alice, S.bob, S.carol] (GameState.turnOrder gone)
    ]
