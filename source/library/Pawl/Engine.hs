{-# LANGUAGE RankNTypes #-}

module Pawl.Engine where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Pawl.Action as Action
import qualified Pawl.Decide as Decide
import qualified Pawl.Game as Game
import qualified Pawl.Sba as Sba
import qualified Pawl.Setup as Setup
import qualified Pawl.Turn as Turn
import qualified Pawl.Type.Action as Action.Type
import qualified Pawl.Type.BeginningStep as BeginningStep
import qualified Pawl.Type.EndingStep as EndingStep
import Pawl.Type.Game (Game)
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Object as Object
import Pawl.Type.ObjectId (ObjectId)
import qualified Pawl.Type.Phase as Phase
import Pawl.Type.PlayerId (PlayerId)
import qualified Pawl.Type.Program as Program
import Pawl.Type.Prompt (Prompt)
import qualified Pawl.Type.Prompt as Prompt
import Pawl.Type.Result (Result)
import qualified Pawl.Type.TapState as TapState
import qualified Pawl.Type.Zone as Zone

-- The interpreter seam: every decision the engine suspends on is answered here.
runGame :: (Monad m) => (forall r. Prompt r -> m r) -> GameState -> Game a -> m (a, GameState)
runGame answer gs game = Program.foldProgramM answer (State.runStateT game gs)

runGamePure :: (forall r. Prompt r -> r) -> GameState -> Game a -> (a, GameState)
runGamePure answer gs game = Program.foldProgram answer (State.runStateT game gs)

-- The next entry of a cyclic order after 'pid'. Falls back to 'pid' when the
-- order is empty or does not mention it, keeping the function total.
nextInOrder :: [PlayerId] -> PlayerId -> PlayerId
nextInOrder order pid = case dropWhile (/= pid) order of
  _ : y : _ -> y
  _ -> case order of
    h : _ -> h
    [] -> pid

-- The next player after 'pid' in APNAP order who has not left the game.
nextStillPlaying :: GameState -> PlayerId -> PlayerId
nextStillPlaying gs pid =
  let playing = Sba.stillPlaying gs
      order = filter (`List.elem` playing) (GameState.turnOrder gs)
   in nextInOrder order pid

checkSba :: Game ()
checkSba = State.modify' Sba.checkStateBasedActions

drawFor :: PlayerId -> Game ()
drawFor pid = do
  gs <- State.get
  case Game.zoneMembers Zone.Library pid gs of
    -- CR 121.3: the draw fails and is remembered; CR 704.5b turns it into a loss.
    [] -> State.put gs {GameState.drewFromEmpty = Set.insert pid (GameState.drewFromEmpty gs)}
    top : _ -> State.put (Game.changeZone top Zone.Hand gs)

untapAll :: PlayerId -> Game ()
untapAll pid = do
  gs <- State.get
  let untap :: Object.Object -> Object.Object
      untap obj = obj {Object.tapped = TapState.Untapped}
      ids = Game.zoneMembers Zone.Battlefield pid gs
  State.put gs {GameState.objects = foldr (Map.adjust untap) (GameState.objects gs) ids}

-- CR 514.2. In M0 every card is a Mountain, so "which to discard" has no
-- meaningful choice: trim from the front of hand without prompting.
discardToHandSize :: PlayerId -> Game ()
discardToHandSize pid = do
  gs <- State.get
  let held = Game.zoneMembers Zone.Hand pid gs
      excess = length held - Setup.openingHand
      toGraveyard :: GameState -> ObjectId -> GameState
      toGraveyard g oid = Game.changeZone oid Zone.Graveyard g
  Monad.when (excess > 0) $
    State.put (List.foldl' toGraveyard gs (take excess held))

-- CR 103.7a: the starting player skips their first draw step.
skipsDraw :: GameState -> Bool
skipsDraw gs =
  GameState.turnNumber gs == 1
    && case GameState.turnOrder gs of
      starter : _ -> starter == GameState.activePlayer gs
      [] -> False

runTurnBasedActions :: Phase.Phase -> Game ()
runTurnBasedActions phase = do
  active <- State.gets GameState.activePlayer
  case phase of
    Phase.Beginning BeginningStep.Untap -> do
      untapAll active
      State.modify' $ \gs ->
        gs {GameState.landPlayed = Set.delete active (GameState.landPlayed gs)}
    Phase.Beginning BeginningStep.DrawStep -> do
      skip <- State.gets skipsDraw
      Monad.unless skip (drawFor active)
    Phase.Ending EndingStep.Cleanup -> discardToHandSize active
    _ -> pure ()

-- Ask the priority holder for an action until every still-playing player has
-- passed in succession. The stack is always empty in M0, so a full round of
-- passes simply ends the step.
priorityLoop :: Game ()
priorityLoop = do
  active <- State.gets GameState.activePlayer
  State.modify' $ \gs -> gs {GameState.priority = Just active, GameState.passes = 0}
  let loop :: Game ()
      loop = do
        gs <- State.get
        case GameState.priority gs of
          Nothing -> pure ()
          Just p -> do
            let decider = Decide.deciderFor p gs
                actions = Action.legalActions p gs
            chosen <- Trans.lift (Program.prompt (Prompt.ChooseAction decider p actions))
            case chosen of
              Action.Type.Pass -> do
                let passes = GameState.passes gs + 1
                    playing = length (Sba.stillPlaying gs)
                if passes >= fromIntegral playing
                  then State.put gs {GameState.priority = Nothing, GameState.passes = passes}
                  else do
                    State.put
                      gs
                        { GameState.passes = passes,
                          GameState.priority = Just (nextStillPlaying gs p)
                        }
                    loop
              Action.Type.Play oid -> do
                -- CR 305.1: playing a land is a special action; it resolves
                -- immediately and the active player regains priority.
                let played = Game.changeZone oid Zone.Battlefield gs
                State.put
                  played
                    { GameState.landPlayed = Set.insert p (GameState.landPlayed played),
                      GameState.passes = 0,
                      GameState.priority = Just (GameState.activePlayer played)
                    }
                loop
  loop

handoffTurn :: Game ()
handoffTurn = State.modify' $ \gs ->
  gs
    { GameState.activePlayer = nextInOrder (GameState.turnOrder gs) (GameState.activePlayer gs),
      GameState.turnNumber = GameState.turnNumber gs + 1,
      GameState.phase = Turn.firstPhase
    }

advance :: Phase.Phase -> Game ()
advance phase = case Turn.next phase of
  Just p -> State.modify' (\gs -> gs {GameState.phase = p})
  Nothing -> handoffTurn

-- One step: turn-based actions, then priority (if the step grants it), then
-- state-based actions, then move on. Bails out as soon as the game has a result.
runStep :: Game ()
runStep = do
  phase <- State.gets GameState.phase
  runTurnBasedActions phase
  checkSba
  finished <- State.gets (Maybe.isJust . GameState.result)
  Monad.unless finished $ do
    Monad.when (Turn.grantsPriority phase) priorityLoop
    checkSba
    stillFinished <- State.gets (Maybe.isJust . GameState.result)
    Monad.unless stillFinished (advance phase)

-- Terminates because libraries are finite, each turn draws at most one card, and
-- drawing from an empty library is a loss (CR 704.5b).
playGame :: Game Result
playGame =
  let loop :: Game Result
      loop = do
        outcome <- State.gets GameState.result
        case outcome of
          Just r -> pure r
          Nothing -> do
            runStep
            loop
   in loop

playFrom :: NonEmpty.NonEmpty PlayerId -> Game Result
playFrom order = do
  Setup.newGame order
  playGame
