{-# LANGUAGE RankNTypes #-}

module Pawl.Engine where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Pawl.Action as Action
import qualified Pawl.Activate as Activate
import qualified Pawl.Cast as Cast
import qualified Pawl.Combat as Combat
import qualified Pawl.Damage as Damage
import qualified Pawl.Decide as Decide
import qualified Pawl.Event as Event
import qualified Pawl.Game as Game
import qualified Pawl.Mana as Mana
import qualified Pawl.Projection as Projection
import qualified Pawl.Sba as Sba
import qualified Pawl.Setup as Setup
import qualified Pawl.Stack as Stack
import qualified Pawl.Turn as Turn
import qualified Pawl.Type.Action as Action.Type
import qualified Pawl.Type.BeginningStep as BeginningStep
import qualified Pawl.Type.CombatStep as CombatStep
import qualified Pawl.Type.Deck as Deck
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
import qualified Pawl.Type.Sickness as Sickness
import qualified Pawl.Type.Source as Source
import qualified Pawl.Type.TapState as TapState
import qualified Pawl.Type.TriggeredAbility as TriggeredAbility
import qualified Pawl.Type.Zone as Zone

-- The interpreter seam: every decision the engine suspends on is answered here.
runGame :: (Monad m) => (forall r. Prompt r -> m r) -> GameState -> Game a -> m (a, GameState)
runGame answer gs game = Program.foldProgramM answer (State.runStateT game gs)

runGamePure :: (forall r. Prompt r -> r) -> GameState -> Game a -> (a, GameState)
runGamePure answer gs game = Program.foldProgram answer (State.runStateT game gs)

-- One entry point from matchup to played game: the player list is DERIVED from
-- the matchup, so a matchup player without a Player record is unrepresentable
-- here (git-bug 15de615). Setup.emptyGame stays public as the deckless fixture
-- door, where no deck agreement exists to violate.
runMatch :: (Monad m) => (forall r. Prompt r -> m r) -> NonEmpty.NonEmpty (PlayerId, Deck.Deck) -> m (Result, GameState)
runMatch answer matchup =
  runGame answer (Setup.emptyGame (NonEmpty.map fst matchup)) (playFrom matchup)

runMatchPure :: (forall r. Prompt r -> r) -> NonEmpty.NonEmpty (PlayerId, Deck.Deck) -> (Result, GameState)
runMatchPure answer matchup =
  runGamePure answer (Setup.emptyGame (NonEmpty.map fst matchup)) (playFrom matchup)

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
    top : _ -> State.put (Event.changeZone top Zone.Hand gs)

untapAll :: PlayerId -> Game ()
untapAll pid = do
  gs <- State.get
  let untap obj = obj {Object.tapped = TapState.Untapped}
      ids = Game.zoneMembers Zone.Battlefield pid gs
  State.put gs {GameState.objects = foldr (Map.adjust untap) (GameState.objects gs) ids}

-- CR 302.6: permanents the active player has controlled since their turn began
-- are no longer summoning sick. The untap step is where that becomes true.
--
-- EXPIRES at M3: control held CONTINUOUSLY is the actual rule, so a control
-- change must reset this. Nothing in M1b can change control.
settleAll :: PlayerId -> Game ()
settleAll pid = do
  gs <- State.get
  let settle obj = obj {Object.sickness = Sickness.Settled}
      ids = Game.zoneMembers Zone.Battlefield pid gs
  State.put gs {GameState.objects = foldr (Map.adjust settle) (GameState.objects gs) ids}

-- CR 514.2. Non-identical cards now share a hand (Mountains and Pikers), so
-- trimming front-of-hand would be the engine choosing what to pitch -- policy in
-- the rules core, not canonicalization. The choice is the player's.
--
-- The answer is filtered to cards actually in hand and capped at the excess, so
-- a misbehaving interpreter cannot discard someone else's card or overshoot. An
-- interpreter that returns too few simply discards too few; that is its bug, and
-- inventing a fallback here would put the policy back.
discardToHandSize :: PlayerId -> Game ()
discardToHandSize pid = do
  gs <- State.get
  let held = Game.zoneMembers Zone.Hand pid gs
      excess = length held - Setup.openingHand
  Monad.when (excess > 0) $ do
    let decider = Decide.deciderFor pid gs
    chosen <- Trans.lift (Program.prompt (Prompt.ChooseDiscard decider pid held (fromIntegral excess)))
    let inHand oid = List.elem oid held
        toDiscard = take excess (filter inHand chosen)
        toGraveyard g oid = Event.changeZone oid Zone.Graveyard g
    State.modify' (\g -> List.foldl' toGraveyard g toDiscard)

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
      settleAll active
      State.modify' $ \gs ->
        gs {GameState.landPlayed = Set.delete active (GameState.landPlayed gs)}
    Phase.Beginning BeginningStep.DrawStep -> do
      skip <- State.gets skipsDraw
      Monad.unless skip (drawFor active)
    Phase.Combat CombatStep.DeclareAttackers -> do
      Combat.declareAttackers active
      -- CR 508.8: with the attacker set now final, drop the two combat steps that
      -- have nothing to do if nobody attacked.
      State.modify' Combat.skipEmptyCombat
    Phase.Combat CombatStep.DeclareBlockers -> Combat.declareBlockers
    Phase.Combat CombatStep.CombatDamage -> do
      -- CR 510.4: deal this step's damage; if it was the first-strike step,
      -- splice a second combat damage step in after it. The between-steps
      -- priority (CR 510.3) and SBA check come free from the step machinery.
      needSecond <- Damage.dealCombatDamage
      Monad.when needSecond $
        State.modify' (\gs -> gs {GameState.remaining = Turn.spliceSecondDamage (GameState.remaining gs)})
    -- CR 511.3: creatures stop being attacking and blocking.
    Phase.Combat CombatStep.EndOfCombat -> State.modify' Combat.clearCombat
    Phase.Ending EndingStep.Cleanup -> do
      discardToHandSize active
      -- CR 514.2: damage wears off AND until-end-of-turn effects end,
      -- simultaneously.
      State.modify' Damage.removeAllDamage
      State.modify' Projection.dropEndOfTurnEffects
    _ -> pure ()

-- Ask the priority holder for an action until every still-playing player has
-- passed in succession (CR 117.4). A full round of passes resolves the top of
-- the stack and hands priority back to the active player; only an EMPTY stack
-- ends the step. M0 could skip that distinction because its stack was always
-- empty.
-- CR 603.3: put each triggered ability that fired since the last placement on the
-- stack, in APNAP order (CR 603.3b). M3f has at most one trigger controlled by one
-- player, so the ordering is trivial and the own-order/two-part choice (CR 603.3b)
-- is elided until a second simultaneous trigger exists. Draining zoneChanges makes
-- an event fire its triggers once (CR 603.2c). Targets are chosen as the ability is
-- placed (CR 603.3d); no M3f trigger targets. Returns whether any were placed.
placePendingTriggers :: Game Bool
placePendingTriggers = do
  gs <- State.get
  let changes = GameState.zoneChanges gs
      pending = Event.triggersFrom changes gs
  State.modify' (\g -> g {GameState.zoneChanges = []})
  Monad.mapM_ placeOne (apnapOrder gs pending)
  pure (not (null pending))

-- Put one triggered ability on the stack as a fresh OfTrigger object.
placeOne :: (ObjectId, PlayerId, TriggeredAbility.TriggeredAbility) -> Game ()
placeOne (srcId, controller, ability) = do
  gs <- State.get
  let (abilId, gs1) = Game.freshObjectId gs
      (ts, gs2) = Game.freshTimestamp gs1
      obj =
        Object.MkObject
          { Object.owner = controller,
            Object.source = Source.OfTrigger srcId ability,
            Object.zone = Zone.Stack,
            Object.tapped = TapState.Untapped,
            Object.damage = 0,
            Object.sickness = Sickness.Settled,
            Object.targets = Map.empty,
            Object.chosenSubtypes = Map.empty,
            Object.timestamp = ts
          }
  State.put gs2 {GameState.objects = Map.insert abilId obj (GameState.objects gs2), GameState.stack = abilId : GameState.stack gs2}

-- CR 603.3b: active player's triggers first, then the others. Stable within a
-- controller (M3f never has two from one controller, so order within is moot).
apnapOrder :: GameState -> [(a, PlayerId, b)] -> [(a, PlayerId, b)]
apnapOrder gs pend =
  let active = GameState.activePlayer gs
      mine (_, p, _) = p == active
   in filter mine pend ++ filter (not . mine) pend

-- CR 117.5: each time a player would receive priority, perform state-based
-- actions, then put triggered abilities on the stack, repeating until neither
-- does anything. Then priority is granted (by the caller).
settleForPriority :: Game ()
settleForPriority = do
  before <- State.get
  checkSba
  placed <- placePendingTriggers
  after <- State.get
  Monad.when (placed || before /= after) settleForPriority

priorityLoop :: Game ()
priorityLoop = do
  active <- State.gets GameState.activePlayer
  State.modify' $ \gs -> gs {GameState.priority = Just active, GameState.passes = 0}
  let loop = do
        settleForPriority
        finished <- State.gets (Maybe.isJust . GameState.result)
        if finished
          then State.modify' (\gs -> gs {GameState.priority = Nothing})
          else do
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
                      then case GameState.stack gs of
                        [] -> State.put gs {GameState.priority = Nothing, GameState.passes = passes}
                        _ -> do
                          Stack.resolveTop
                          State.modify' (\g -> g {GameState.passes = 0, GameState.priority = Just (GameState.activePlayer g)})
                          loop
                      else do
                        State.put gs {GameState.passes = passes, GameState.priority = Just (nextStillPlaying gs p)}
                        loop
                  Action.Type.Play oid -> do
                    State.modify' (\g -> let played = Event.changeZone oid Zone.Battlefield g in played {GameState.landPlayed = Set.insert p (GameState.landPlayed played), GameState.passes = 0, GameState.priority = Just p})
                    loop
                  Action.Type.Cast oid -> do
                    Cast.castSpell p oid
                    State.modify' (\g -> g {GameState.passes = 0, GameState.priority = Just p})
                    loop
                  Action.Type.Activate oid ability -> do
                    Activate.activateAbility p oid ability
                    State.modify' (\g -> g {GameState.passes = 0, GameState.priority = Just p})
                    loop
  loop

handoffTurn :: Game ()
handoffTurn = State.modify' $ \gs ->
  gs
    { GameState.activePlayer = nextInOrder (GameState.turnOrder gs) (GameState.activePlayer gs),
      GameState.turnNumber = GameState.turnNumber gs + 1,
      GameState.phase = Turn.firstPhase,
      GameState.remaining = Turn.laterPhases
    }

-- Consume the schedule: the next step becomes current. An empty schedule means
-- the turn is over, so hand off. Replaces the old `Turn.next` walk -- the turn is
-- data now, and this is the only thing that reads its order.
advance :: Game ()
advance = do
  gs <- State.get
  case Seq.viewl (GameState.remaining gs) of
    p Seq.:< rest -> State.put gs {GameState.phase = p, GameState.remaining = rest}
    Seq.EmptyL -> handoffTurn

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
    -- CR 500.4: each player's mana pool empties at the end of every step and
    -- phase. Nothing floats mana in M1a, so this is unobservable today; it is
    -- the rule, and it is a one-liner.
    State.modify' Mana.emptyManaPools
    checkSba
    stillFinished <- State.gets (Maybe.isJust . GameState.result)
    Monad.unless stillFinished advance

-- Terminates because libraries are finite, each turn draws at most one card, and
-- drawing from an empty library is a loss (CR 704.5b).
playGame :: Game Result
playGame =
  let loop = do
        outcome <- State.gets GameState.result
        case outcome of
          Just r -> pure r
          Nothing -> do
            runStep
            loop
   in loop

playFrom :: NonEmpty.NonEmpty (PlayerId, Deck.Deck) -> Game Result
playFrom matchup = do
  Setup.newGame matchup
  playGame
