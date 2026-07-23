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
import Numeric.Natural (Natural)
import qualified Pawl.Action as Action
import qualified Pawl.Activate as Activate
import qualified Pawl.Binding as Binding
import qualified Pawl.Cast as Cast
import qualified Pawl.Combat as Combat
import qualified Pawl.Damage as Damage
import qualified Pawl.Decide as Decide
import qualified Pawl.Event as Event
import qualified Pawl.Expiry as Expiry
import qualified Pawl.Game as Game
import qualified Pawl.Mana as Mana
import qualified Pawl.Modal as Modal
import qualified Pawl.Monarch as Monarch
import qualified Pawl.PlayerEffect as PlayerEffect
import qualified Pawl.Projection as Projection
import qualified Pawl.Resolve as Resolve
import qualified Pawl.Sba as Sba
import qualified Pawl.Setup as Setup
import qualified Pawl.Stack as Stack
import qualified Pawl.Target as Target
import qualified Pawl.Turn as Turn
import qualified Pawl.Type.Action as Action.Type
import qualified Pawl.Type.BeginningStep as BeginningStep
import qualified Pawl.Type.CombatStep as CombatStep
import qualified Pawl.Type.Deck as Deck
import qualified Pawl.Type.EndingStep as EndingStep
import Pawl.Type.Game (Game)
import qualified Pawl.Type.GameEvent as GameEvent
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Object as Object
import qualified Pawl.Type.PendingTrigger as PendingTrigger
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
-- here (#24). Setup.emptyGame stays public as the deckless fixture
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
checkSba = Sba.checkStateBasedActions

untapAll :: PlayerId -> Game ()
untapAll pid = do
  gs <- State.get
  let untap obj = obj {Object.tapped = TapState.Untapped}
      ids = Projection.controls pid gs
  State.put gs {GameState.objects = foldr (Map.adjust untap) (GameState.objects gs) ids}

-- CR 302.6: permanents the active player has controlled since their turn began
-- are no longer summoning sick. The untap step is where that becomes true.
--
-- Control-change re-sickening is handled (M4.5 P1): `GainControl` re-Sicks its
-- target at resolution, and this settle now iterates `Projection.controls`, so
-- it clears sickness for whoever currently controls the permanent, not its
-- owner. A P1 control effect is until-end-of-turn, so it always wears off
-- (CR 514.2) before the thief's own next untap step -- the creature is back
-- under the owner's control by then, and settles normally there. Settling a
-- permanent held under INDEFINITE control, across the thief's own untap step,
-- is the Auras / Control Magic phase.
settleAll :: PlayerId -> Game ()
settleAll pid = do
  gs <- State.get
  let settle obj = obj {Object.sickness = Sickness.Settled}
      ids = Projection.controls pid gs
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
  -- CR 402.2, not CR 103.5: the maximum hand size is its own rule and its own
  -- seven, and an effect may remove it entirely (Reliquary Tower). A player with
  -- no maximum discards nothing and is never asked.
  case PlayerEffect.maximumHandSize pid gs of
    Nothing -> pure ()
    Just limit -> do
      let held = Game.zoneMembers Zone.Hand pid gs
          excess = length held - fromIntegral limit
      Monad.when (excess > 0) $ do
        let decider = Decide.deciderFor pid gs
        chosen <- Trans.lift (Program.prompt (Prompt.ChooseDiscard decider pid held (fromIntegral excess)))
        let inHand oid = List.elem oid held
            toDiscard = take excess (filter inHand chosen)
        Monad.mapM_ (\oid -> Event.changeZone oid Zone.Graveyard) toDiscard

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
      Monad.unless skip (Event.drawCard active)
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
      -- simultaneously. One sweep over both carriers (Pawl.Expiry).
      State.modify' Damage.removeAllDamage
      State.modify' Expiry.dropAtCleanup
    _ -> pure ()

-- Ask the priority holder for an action until every still-playing player has
-- passed in succession (CR 117.4). A full round of passes resolves the top of
-- the stack and hands priority back to the active player; only an EMPTY stack
-- ends the step. M0 could skip that distinction because its stack was always
-- empty.
-- CR 603.3: put each triggered ability that fired since the last placement on the
-- stack, in APNAP order (CR 603.3b): active player's triggers first, then each
-- other player's in turn order (apnapPlayers). Within one controller's own set,
-- that player chooses the order (orderPending), asked only when they control two
-- or more -- CR 603.3b's own-order choice is a real prompt now, not an elision.
-- CR 603.3b's other half -- first place the triggers whose condition ISN'T
-- another ability triggering, then the rest, as a separate pass -- is not
-- implemented here (#49); it is vacuous while nothing in the card pool triggers
-- off another ability triggering. Advancing
-- scannedThrough makes an event fire its triggers once (CR 603.2c) WITHOUT
-- discarding the record. Targets are chosen as the ability is placed (CR 603.3d).
-- Returns whether any were placed.
placePendingTriggers :: Game Bool
placePendingTriggers = do
  gs <- State.get
  let evs = Event.unscannedEvents gs
      (pending, surviving) = Event.gatherTriggers evs gs
      -- CR 725.2: the monarch's inherent triggers hang on no object, so the
      -- normal ObjectId-sourced pending pipeline can't carry them. Gather them
      -- from the SAME unscanned-event snapshot, before the watermark bump.
      inherent = Monarch.inherentMonarchPending evs gs
  State.put
    gs
      { GameState.scannedThrough = fromIntegral (Seq.length (GameState.events gs)),
        GameState.delayedTriggers = surviving
      }
  ordered <- orderPending pending
  Monad.mapM_ placeOne ordered
  Monad.mapM_ (\(p, ab, b) -> Monarch.placeInherent p ab b) inherent
  pure (not (null pending) || not (null inherent))

-- Put one triggered ability on the stack as a fresh OfTrigger object, choosing
-- its mode(s) and their targets as it is placed (CR 603.3d). This mirrors
-- Cast.castSpell's cast-time flow: CR 700.2b -- the controller chooses the mode
-- when the ability triggers (elided, forced and unprompted, exactly when the
-- fillable modes are no more than the selection demands, #50) -- then CR 603.3d --
-- targets for the CHOSEN mode(s) are chosen now, at placement.
--
-- CR 603.3c/700.2b: "If no mode is chosen, the ability is removed from the
-- stack." This is the trigger-only half of the rule -- a SPELL that can't
-- choose a mode is simply never offered for casting in the first place (CR
-- 601.2c, M4g); a TRIGGER is already placed on the stack (CR 603.3d puts it
-- there before modes/targets are chosen), so an unfillable one must instead be
-- taken back OFF the stack. The guard precedes the mode prompt: a removed
-- trigger must never be asked to choose.
placeOne :: PendingTrigger.PendingTrigger -> Game ()
placeOne pending = do
  gs <- State.get
  let srcId = PendingTrigger.source pending
      controller = PendingTrigger.controller pending
      ability = PendingTrigger.ability pending
      (abilId, gs1) = Game.freshObjectId gs
      (ts, gs2) = Game.freshTimestamp gs1
      decider = Decide.deciderFor controller gs
      modal = TriggeredAbility.modal ability
      legal = Target.fillableModes srcId modal gs
      count = Modal.selectionCount modal
      obj =
        Object.MkObject
          { Object.owner = controller,
            Object.source = Source.OfTrigger srcId ability,
            Object.zone = Zone.Stack,
            Object.tapped = TapState.Untapped,
            Object.damage = 0,
            Object.sickness = Sickness.Settled,
            Object.bindings = Map.empty,
            Object.counters = Map.empty,
            Object.timestamp = ts
          }
  State.put gs2 {GameState.objects = Map.insert abilId obj (GameState.objects gs2), GameState.stack = abilId : GameState.stack gs2}
  if Set.size legal < fromIntegral count
    then -- CR 603.3c: fewer legal modes than the selection demands -- for
    -- ChooseExactly 1, no legal mode at all -- removes the ability.
      State.modify' (Resolve.cease abilId)
    else do
      -- CR 700.2b: forced when there is nothing to choose (as many legal modes
      -- as the selection demands), prompted otherwise.
      chosenModes <-
        if Set.size legal <= fromIntegral count
          then pure legal
          else Trans.lift (Program.prompt (Prompt.ChooseModes decider controller abilId legal count))
      -- CR 603.3d: targets for the chosen mode(s) only, chosen as the ability
      -- is placed. A mode with no target slots (Create/Draw) asks nothing.
      let sets = Target.legalSetsExcluding srcId (Modal.modesTargetSpecs chosenModes modal) gs
      chosen <-
        if Map.null sets
          then pure Map.empty
          else Trans.lift (Program.prompt (Prompt.ChooseTargets decider controller abilId sets))
      -- CR 113.7: the ability's SOURCE is bound under the reserved slot as it is
      -- placed, so "this creature" resolves as an ordinary slot read even after
      -- the source has left the battlefield.
      --
      -- CR 603.7c: a delayed ability's CAPTURED environment (its "it") rides
      -- alongside the targets/modes chosen now for THIS placement; the source
      -- slot is stamped over the top regardless. The two DO collide: the
      -- captured environment is built by the same Binding.fromChoices the
      -- arming spell used, so it carries that spell's OWN reserved slots
      -- (chosenModes, variableX) whenever the spell used them (Tidal Wave's
      -- arming mode is unconditional). Map.union is left-biased, so
      -- placement-time bindings must be the LEFT argument -- they are this
      -- ability's own choices, made just now for this ability; the captured
      -- environment's only job is to carry forward object references (CR
      -- 603.7c's "it") that placement-time can never supply. Getting the
      -- order backwards silently substitutes the arming spell's chosen mode /
      -- X for the delayed ability's own -- unobserved by any test to date only
      -- because Tidal Wave and its one delayed ability both ever choose mode
      -- 0.
      State.modify' (\g -> g {GameState.objects = Map.adjust (\o -> o {Object.bindings = Binding.setYou controller (Binding.setTriggerSource srcId (Map.union (Binding.fromChoices chosen Map.empty Nothing chosenModes) (PendingTrigger.bindings pending)))}) abilId (GameState.objects g)})

-- CR 101.4 / 603.3b: the players who control a pending trigger, active player
-- first and then the rest in turn order. Replaces M3f's apnapOrder, which sorted
-- the triggers directly -- with a within-controller ORDER now being a choice, the
-- pass has to group by controller first.
apnapPlayers :: GameState -> [PendingTrigger.PendingTrigger] -> [PlayerId]
apnapPlayers gs pending =
  let order = GameState.turnOrder gs
      active = GameState.activePlayer gs
      rotated = dropWhile (/= active) order ++ takeWhile (/= active) order
      controls pid = any (\pt -> PendingTrigger.controller pt == pid) pending
   in filter controls rotated

-- CR 603.3b: APNAP across controllers, and within one controller's set, that
-- player's chosen order. Asked only when they control two or more.
orderPending :: [PendingTrigger.PendingTrigger] -> Game [PendingTrigger.PendingTrigger]
orderPending pending = do
  gs <- State.get
  groups <- Monad.mapM (orderFor gs pending) (apnapPlayers gs pending)
  pure (concat groups)

orderFor :: GameState -> [PendingTrigger.PendingTrigger] -> PlayerId -> Game [PendingTrigger.PendingTrigger]
orderFor gs pending pid = do
  let mine = filter (\pt -> PendingTrigger.controller pt == pid) pending
  if length mine < 2
    then pure mine
    else do
      let decider = Decide.deciderFor pid gs
      answer <- Trans.lift (Program.prompt (Prompt.OrderTriggers decider pid (map PendingTrigger.source mine)))
      pure (permute mine answer)

-- Reject-not-repair, as payment already does: only a genuine permutation of the
-- offered indices is honoured. Anything else -- a short answer, a duplicate, an
-- out-of-range index -- leaves the canonical order standing rather than dropping
-- or duplicating a trigger.
permute :: [a] -> [Natural] -> [a]
permute xs order =
  let canonical :: [Natural]
      canonical = map fromIntegral (take (length xs) [0 :: Int ..])
      at i = case drop (fromIntegral i) xs of
        h : _ -> Just h
        [] -> Nothing
   in if List.sort order == canonical
        then Maybe.mapMaybe at order
        else xs

-- CR 117.5: each time a player would receive priority, sweep expired "for as
-- long as" effects, perform state-based actions, then put triggered abilities
-- on the stack, repeating until none of the three does anything. Then priority
-- is granted (by the caller). The repeat is gated on three cheap booleans --
-- whether the conditional sweep changed anything, whether an SBA fired and
-- whether a trigger was placed -- so a settle that changes nothing (the common
-- case) costs one board projection and one length comparison per carrier, NOT
-- a deep GameState equality check.
--
-- CR 611.2b's condition is checked continuously, and CR 704.3 makes "whenever
-- a player would get priority" the coarsest moment anything could observe it,
-- so settling here is indistinguishable from checking continuously. The sweep
-- runs FIRST, before the SBA check: a "for as long as you control this" effect
-- ending (Master Thief) returns a permanent to another player's control, and a
-- control-scoped state-based action must see the post-sweep control, not the
-- moment before it reverted. CR 704.5j's legend rule ("controlled by the same
-- player") is the rule that will read it; this engine does not check it yet
-- (#64), so the ordering is not yet observable -- it is the right order for the
-- one that lands first. The
-- loop re-runs whenever ANYTHING fired, because an SBA can itself be what
-- falsifies a condition (e.g. a permanent the condition names is destroyed). A
-- game with no While stored pays one list scan.
--
-- P5 removed the third gate this comment used to describe. The as-enters copy
-- drain used to run here, first, because a copied permanent's characteristics
-- had to be locked in before any SBA or trigger observed it (CR 614.12a). The
-- entry loop now runs inside the zone change itself, before the Moved event
-- exists, so there is nothing left to drain.
settleForPriority :: Game ()
settleForPriority = do
  swept <- Expiry.sweepConditional
  returned <- Monarch.returnExiledForMonarch
  acted <- Sba.performStateBasedActions
  placed <- placePendingTriggers
  Monad.when (swept || returned || acted || placed) settleForPriority

priorityLoop :: Game ()
priorityLoop = do
  active <- State.gets GameState.activePlayer
  State.modify' $ \gs -> gs {GameState.priority = Just active, GameState.passes = 0}
  -- settleForPriority (CR 117.5) runs where the board can CHANGE -- once at entry,
  -- and after each resolution or board-changing action -- never after a bare
  -- priority pass. A pass leaves the game state untouched, so re-running the SBA
  -- projection then would be pure waste; the last settle already saw this exact
  -- state. This is the standard "check SBAs only after a game event" reading of
  -- CR 117.5, observably identical to settling on every priority grant.
  let loop = do
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
                          settleForPriority
                          State.modify' (\g -> g {GameState.passes = 0, GameState.priority = Just (GameState.activePlayer g)})
                          loop
                      else do
                        State.put gs {GameState.passes = passes, GameState.priority = Just (nextStillPlaying gs p)}
                        loop
                  Action.Type.Play oid -> do
                    Event.changeZone oid Zone.Battlefield
                    State.modify' (\g -> g {GameState.landPlayed = Set.insert p (GameState.landPlayed g), GameState.passes = 0, GameState.priority = Just p})
                    settleForPriority
                    loop
                  Action.Type.Cast oid -> do
                    Cast.castSpell p oid
                    State.modify' (\g -> g {GameState.passes = 0, GameState.priority = Just p})
                    settleForPriority
                    loop
                  Action.Type.Activate oid ability -> do
                    Activate.activateAbility p oid ability
                    State.modify' (\g -> g {GameState.passes = 0, GameState.priority = Just p})
                    settleForPriority
                    loop
  settleForPriority
  loop

handoffTurn :: Game ()
handoffTurn = State.modify' $ \gs ->
  let newActive = nextInOrder (GameState.turnOrder gs) (GameState.activePlayer gs)
   in -- CR 611.2a: with activePlayer already advanced, drop every "until your
      -- next turn" effect belonging to the player whose turn just began. The
      -- transition IS the event, known exactly here; see Pawl.Expiry.
      Expiry.dropAtHandoff $
        gs
          { GameState.activePlayer = newActive,
            GameState.turnNumber = GameState.turnNumber gs + 1,
            -- CR 608.2i is why a log exists at all -- "some effects look back in
            -- time and require information about previous game states and
            -- actions." It does not itself say how far back; the ONE-turn scope is
            -- this engine's choice, made because every history-reading card in the
            -- pool asks "this turn" (Khabál Ghoul: "creatures that died this
            -- turn"). Cleared here, with both watermarks, and never at cleanup --
            -- cleanup is still part of this turn and CR 514.1's discard is itself
            -- an event of it. Engine.advance settles immediately before calling
            -- this, so nothing unscanned is discarded.
            GameState.events = Seq.empty,
            GameState.scannedThrough = 0,
            GameState.damageScannedThrough = 0,
            GameState.phase = Turn.firstPhase,
            GameState.remaining = Turn.laterPhases,
            -- CR 723.1/723.1b: the new active player's pending control (if any)
            -- becomes this turn's active control; overwriting activeControl every
            -- turn is what ends a prior control at the next turn's start (CR 723.1).
            GameState.activeControl = Map.lookup newActive (GameState.pendingControl gs),
            GameState.pendingControl = Map.delete newActive (GameState.pendingControl gs)
          }

-- Consume the schedule: the next step becomes current. An empty schedule means
-- the turn is over, so hand off. Replaces the old `Turn.next` walk -- the turn is
-- data now, and this is the only thing that reads its order.
advance :: Game ()
advance = do
  gs <- State.get
  case Seq.viewl (GameState.remaining gs) of
    p Seq.:< rest -> State.put gs {GameState.phase = p, GameState.remaining = rest}
    -- CR 514.3 (partial) / 117.5: the turn is over. Settle once more so every
    -- event the cleanup step's turn-based actions emitted is scanned BEFORE
    -- handoffTurn clears the log -- an unscanned event discarded at handoff is a
    -- lost trigger. CR 514.3a's extra cleanup step and its priority round are not
    -- built (#51), so a trigger placed here resolves at the next turn's first
    -- priority rather than during this cleanup.
    Seq.EmptyL -> do
      settleForPriority
      handoffTurn

-- One step: turn-based actions, then priority (if the step grants it), then
-- state-based actions, then move on. Bails out as soon as the game has a result.
runStep :: Game ()
runStep = do
  phase <- State.gets GameState.phase
  -- CR 603.2b: the step began. Recorded BEFORE the step's turn-based actions, so
  -- the first priority boundary of this step scans it. No player receives
  -- priority during the untap step (CR 502.4), so an ability that triggers then
  -- is held until the next time a player would receive priority -- usually
  -- upkeep, where CR 503.1a puts it on the stack before the active player gets
  -- priority.
  State.modify' (\gs -> Event.recordEvent (GameEvent.StepBegan phase (GameState.activePlayer gs)) gs)
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
