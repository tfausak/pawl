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
import qualified Pawl.Departure as Departure
import qualified Pawl.Event as Event
import qualified Pawl.Expiry as Expiry
import qualified Pawl.Game as Game
import qualified Pawl.Mana as Mana
import qualified Pawl.Modal as Modal
import qualified Pawl.Monarch as Monarch
import qualified Pawl.Mulligan as Mulligan
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
import qualified Pawl.Type.Concession as Concession
import qualified Pawl.Type.Decider as Decider
import qualified Pawl.Type.Deck as Deck
import qualified Pawl.Type.Departure as Departure.Type
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
import qualified Pawl.Type.RestartSignal as RestartSignal
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
  runGame answer (Setup.emptyGame (fmap fst matchup)) (playFrom matchup)

runMatchPure :: (forall r. Prompt r -> r) -> NonEmpty.NonEmpty (PlayerId, Deck.Deck) -> (Result, GameState)
runMatchPure answer matchup =
  runGamePure answer (Setup.emptyGame (fmap fst matchup)) (playFrom matchup)

-- The next entry of a cyclic order after 'pid'. Falls back to 'pid' when the
-- order is empty or does not mention it, keeping the function total.
nextInOrder :: [PlayerId] -> PlayerId -> PlayerId
nextInOrder order pid = case dropWhile (/= pid) order of
  _ : y : _ -> y
  _ -> case order of
    h : _ -> h
    [] -> pid

-- CR 800.4a (last sentence): "If the player who left the game had priority at
-- the time they left, priority passes to the next player in turn order who's
-- still in the game."
--
-- The seat is looked up in the FULL seating order (GameState.turnOrder is never
-- shortened -- see Pawl.Type.GameState), so a player who has ALREADY departed
-- still has a position from which to find their successor. That is exactly the
-- case priorityLoop's concede arm calls this in, and it is simultaneously the
-- correct implementation for the ordinary pass case, where `pid` is still
-- playing and the answer is unchanged.
--
-- Monarch.reassignOnDeparture walks the same seating order, for CR 725.4's
-- monarch successor -- a different rule, but the same shape of walk -- and
-- this is deliberately NOT shared with it, for three reasons: this function
-- anchors on the DEPARTING seat (`pid`) and includes it in the wrap, so it
-- can return that seat, where reassignOnDeparture anchors on the ACTIVE seat
-- and excludes it; this function is total in PlayerId, where
-- reassignOnDeparture must return a Maybe because CR 725.4's third sentence
-- lets the game continue with no monarch; and this function reads
-- Departure.stillPlaying directly, where reassignOnDeparture takes `playing`
-- injected, because Pawl.Monarch may not import Pawl.Departure. Changing
-- either walk without checking the other risks reintroducing this
-- duplication with a mismatch baked in.
--
-- Total: falls back to `pid` when nobody is still playing.
nextStillPlaying :: GameState -> PlayerId -> PlayerId
nextStillPlaying gs pid =
  let order = GameState.turnOrder gs
      playing = Departure.stillPlaying gs
      -- The cyclic scan: everyone after `pid`, then the whole order again so the
      -- wrap is covered. A `pid` absent from the order simply starts at the head.
      scan = drop 1 (dropWhile (/= pid) order) <> order
   in case filter (\p -> List.elem p playing) scan of
        h : _ -> h
        [] -> pid

-- CR 800.4j: "If a player leaves the game during their turn, that turn continues
-- to its completion without an active player. If the active player would receive
-- priority, instead the next player in turn order receives priority, or the top
-- object on the stack resolves, or the phase or step ends, whichever is
-- appropriate."
--
-- GameState.activePlayer is deliberately NOT widened to a Maybe: the turn still
-- BELONGS to that seat -- CR 800.4m's durations and CR 101.4's APNAP anchor both
-- reference it -- and making it optional would ripple through every consumer to
-- express something none of them needs. This one helper covers the difference.
priorityHolder :: GameState -> PlayerId
priorityHolder gs =
  let active = GameState.activePlayer gs
   in if List.elem active (Departure.stillPlaying gs)
        then active
        else nextStillPlaying gs active

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

-- CR 103.8a: "In a two-player game, the player who plays first skips the draw
-- step (see rule 504, "Draw Step") of their first turn." CR 103.8c: "In all
-- other multiplayer games, no player skips the draw step of their first turn."
-- CR 800.7 says the same from the multiplayer side.
--
-- CR 800.1: "A multiplayer game is a game that begins with more than two
-- players." GameState.turnOrder is the permanent seating roster (see
-- Pawl.Type.GameState), so counting seats answers "begins with" directly: a
-- three-player game that has dropped to two survivors still does not skip, and a
-- rebuilt game (CR 727.1, CR 729.2) is seated from the players who were in the
-- game it came from and so answers for itself. Not more than two seats is CR
-- 103.8a's arm, which is also where a degenerate one-seat subgame lands.
--
-- CR 103.8b grants the same skip to a TEAM in a Two-Headed Giant game -- the same
-- capability for a third reason, which would be another arm of this function.
-- pawl has no teams or variants to read from (#175), so nothing else needs
-- to know.
skipsDraw :: GameState -> Bool
skipsDraw gs =
  GameState.turnNumber gs == 1
    && length (GameState.turnOrder gs) <= 2
    && case GameState.turnOrder gs of
      starter : _ -> starter == GameState.activePlayer gs
      [] -> False

runTurnBasedActions :: Phase.Phase -> Game ()
runTurnBasedActions phase = do
  active <- State.gets GameState.activePlayer
  -- CR 800.4j: a turn whose player has left the game "continues to its
  -- completion without an active player", so the turn-based actions the rules
  -- assign to THE ACTIVE PLAYER -- untap (CR 703.4c), draw (CR 703.4d), choose
  -- the defending player (CR 703.4h/CR 507.1), declare attackers (CR 703.4i),
  -- the cleanup discard (CR 703.4n) -- have no subject. Declare
  -- blockers (CR 703.4j) belongs to the defending player, and CR 703.4p's
  -- damage/until-end-of-turn sweep is the GAME's action, not the active
  -- player's; neither is guarded.
  --
  -- CR 800.4j alone does not license skipping any of them; it is a PRIORITY
  -- rule. CR 800.4h is what reaches the CHOICES on that list -- a choice a rule
  -- requires of a player who has left is made by the next player in turn order --
  -- and skipping them instead diverges from it (#181). The list splits three
  -- ways: the draw is not a choice at all, so CR 800.4h never reaches it; untap
  -- and the cleanup discard are choices over the departed player's own permanents
  -- and hand, and declare attackers over their creatures, all of which CR 800.4a
  -- took, so those three are vacuous; only the defending-player choice is real,
  -- and it is unobservable rather than vacuous for the reason on
  -- Pawl.Type.Combat's defender field.
  hasActive <- State.gets (\gs -> List.elem active (Departure.stillPlaying gs))
  case phase of
    Phase.Beginning BeginningStep.Untap -> Monad.when hasActive $ do
      untapAll active
      settleAll active
      State.modify' $ \gs ->
        gs {GameState.landPlayed = Set.delete active (GameState.landPlayed gs)}
    Phase.Beginning BeginningStep.DrawStep -> Monad.when hasActive $ do
      skip <- State.gets skipsDraw
      Monad.unless skip (Event.drawCard active)
    -- CR 703.4h: choose the defending player. The active player's action
    -- (CR 507.1), so it takes the same guard as the others.
    --
    -- hasActive and chooseDefender's own guard are the same value BY
    -- EQUIVALENCE, not merely observed to agree: hasActive is bound once, at
    -- the top of this function, from the GameState in scope here; only a pure
    -- `case` on `phase` runs between that bind and this arm; and
    -- Combat.chooseDefender opens with its own State.get and computes the
    -- identical `List.elem (GameState.activePlayer gs) (Departure.stillPlaying
    -- gs)` over that same state. A green suite is deliberately not what this
    -- rests on -- the declareAttackers arm below carries a guard (CR 703.4i)
    -- that was once wrongly called dead code on exactly that basis, and review
    -- found it load-bearing at two seats a direct call can reach but the game
    -- loop cannot.
    --
    -- Removal is therefore safe on this path, and still declined: this arm is
    -- where the enumeration of CR 800.4j's actions above is read off, and an
    -- arm silently missing the wrapper while that list still names it would be
    -- the worse artifact. chooseDefender's own copy is untouched by this
    -- decision -- M5.6d's review ruled it load-bearing for a direct caller
    -- that has no engine wrapper (a spec, or a second combat phase spliced by
    -- an effect) -- and its comment states this same argument from the other
    -- end.
    Phase.Combat CombatStep.BeginningOfCombat -> Monad.when hasActive Combat.chooseDefender
    Phase.Combat CombatStep.DeclareAttackers -> do
      Monad.when hasActive (Combat.declareAttackers active)
      -- CR 508.8: with the attacker set now final, drop the two combat steps that
      -- have nothing to do if nobody attacked. NOT guarded: a turn with no active
      -- player declares no attackers, which is precisely 508.8's condition.
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
      Monad.when hasActive (discardToHandSize active)
      -- CR 514.2: damage wears off AND until-end-of-turn effects end,
      -- simultaneously. One sweep over both carriers (Pawl.Expiry). NOT
      -- guarded: CR 703.4p is the game's action, not the active player's.
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
--
-- CR 800.4d, SECOND sentence: "If a triggered ability that would be
-- controlled by a player who has left the game would be put onto the stack,
-- it isn't put on the stack." No separate filter is needed here:
-- `orderPending` groups `pending` by `apnapPlayers`, which already restricts
-- every group to a still-playing controller (Departure.stillPlaying), so a
-- PendingTrigger whose controller has left never appears in `ordered` and
-- `placeOne` never sees it. Under the pipeline `orderPending` feeds --
-- `apnapPlayers` -> `orderFor` -> `permute`, none of which can ever
-- INTRODUCE an entry -- a delayed ability is the only carrier that can reach
-- this with a departed controller (once more than two players are seated,
-- Departure.continuesAfterDeparture; at two, CR 800.4a's clauses never run
-- and CR 104.2a ends the game before an end step returns, so the question is
-- moot there, and apnapPlayers's filter would catch it regardless if it
-- somehow arose) -- see Pawl.Departure's objectsLeaveWith haddock -- because
-- eventTriggers/stateTriggers re-derive the controller live from
-- Projection.controllerOf, and a departed player controls nothing after CR
-- 800.4a. The entry is still CONSUMED regardless: `surviving` above already
-- dropped it from delayedTriggers, because CR 603.7b spends the one shot on
-- the trigger event, which happened. (The monarch's `inherent` triggers below
-- bypass this pipeline entirely and are argued safe separately, at their own
-- gather site.)
--
-- The Bool this returns is computed from `ordered`, not the pre-filter
-- `pending`, so it still tells the truth ("were any placed") in exactly the
-- case CR 800.4d creates: a departed player's delayed ability firing with
-- nothing else pending would otherwise report `True` on a step that put
-- nothing on the stack.
--
-- NOT implemented: CR 800.4d's FIRST sentence ("If an object that would be
-- owned by a player who has left the game would be created in any zone, it
-- isn't created") and CR 800.4b's SECOND sentence ("If a token would be
-- created under the control of a player who has left the game, no token is
-- created"). `Event.createTokens` is the unguarded producer -- it takes one
-- player and uses it for both control and ownership, so for tokens those two
-- sentences coincide exactly and one guard would satisfy both (#177); a token
-- is also the only object in this pool that either sentence has a producer
-- for at all.
--
-- CR 800.4b's THIRD sentence (an object put onto the battlefield or the stack
-- under a departed player's control) is producerless, so nothing tracks it:
-- Event.changeZoneReturning is the sole zone-change primitive and takes the
-- destination controller from `Object.owner`, never from a named player, so
-- no opcode can express the situation.
placePendingTriggers :: Game Bool
placePendingTriggers = do
  gs <- State.get
  let evs = Event.unscannedEvents gs
      (pending, surviving) = Event.gatherTriggers evs gs
      -- CR 725.2: the monarch's inherent triggers hang on no object, so the
      -- normal ObjectId-sourced pending pipeline can't carry them. Gather them
      -- from the SAME unscanned-event snapshot, before the watermark bump.
      -- They bypass orderPending/apnapPlayers entirely, so CR 800.4d's filter
      -- above does not cover this path -- but no separate guard is needed
      -- here either: Monarch.reassignOnDeparture runs inside Departure.depart
      -- with Departure.stillPlayingInOrder, so the crown can never rest on a
      -- departed seat in the first place, and inherentMonarchPending's
      -- controller is always the current monarch.
      inherent = Monarch.inherentMonarchPending evs gs
  State.put
    gs
      { GameState.scannedThrough = fromIntegral (Seq.length (GameState.events gs)),
        GameState.delayedTriggers = surviving
      }
  ordered <- orderPending pending
  Monad.mapM_ placeOne ordered
  Monad.mapM_ (\(p, ab, b) -> Monarch.placeInherent p ab b) inherent
  pure (not (null ordered) || not (null inherent))

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
            Object.attachedTo = Nothing,
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
      let sets = Target.legalSets srcId (Modal.modesTargetSpecs chosenModes modal) gs
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
-- pass has to group by controller first. A departed seat is not in the APNAP
-- order at all -- CR 101.4 orders the active player and the nonactive players,
-- and CR 102.1 makes a player "one of the people in the game" -- and CR 800.4d
-- keeps its triggers off the stack. Since turnOrder is the permanent seating
-- roster, this filter is what enforces both.
apnapPlayers :: GameState -> [PendingTrigger.PendingTrigger] -> [PlayerId]
apnapPlayers gs pending =
  let order = GameState.turnOrder gs
      active = GameState.activePlayer gs
      rotated = dropWhile (/= active) order <> takeWhile (/= active) order
      -- turnOrder is the permanent SEATING roster, so the rotation still names
      -- departed seats. A player who has left the game is not in APNAP order and
      -- is never asked to order triggers (CR 800.4a leaves them nothing to
      -- control in the first place, which this does not depend on).
      playing = Departure.stillPlaying gs
      controls pid = List.elem pid playing && any (\pt -> PendingTrigger.controller pt == pid) pending
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
      answer <- Trans.lift (Program.prompt (Prompt.OrderTriggers decider pid (fmap PendingTrigger.source mine)))
      pure (permute mine answer)

-- Reject-not-repair, as payment already does: only a genuine permutation of the
-- offered indices is honoured. Anything else -- a short answer, a duplicate, an
-- out-of-range index -- leaves the canonical order standing rather than dropping
-- or duplicating a trigger.
permute :: [a] -> [Natural] -> [a]
permute xs order =
  let canonical :: [Natural]
      canonical = fmap fromIntegral (take (length xs) [0 :: Int ..])
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
  -- CR 800.4j: the active player, unless they have left the game.
  holder <- State.gets priorityHolder
  State.modify' $ \gs -> gs {GameState.priority = Just holder, GameState.passes = 0}
  -- settleForPriority (CR 117.5) runs where the board can CHANGE -- once at entry,
  -- and after each resolution or board-changing action -- never after a bare
  -- priority pass. A pass leaves the game state untouched, so re-running the SBA
  -- projection then would be pure waste; the last settle already saw this exact
  -- state. This is the standard "check SBAs only after a game event" reading of
  -- CR 117.5, observably identical to settling on every priority grant.
  let loop = do
        finished <- State.gets (Maybe.isJust . GameState.result)
        restarted <- State.gets GameState.restartSignal
        case restarted of
          -- CR 727.4: a restart resolved under this loop, so the game it was
          -- running has ended (CR 727.1) and been rebuilt. Stop without granting
          -- priority -- "No player has priority" -- and without touching
          -- GameState.priority, which the rebuild already set to Nothing.
          RestartSignal.Restarted -> pure ()
          RestartSignal.Playing ->
            if finished
              then State.modify' (\gs -> gs {GameState.priority = Nothing})
              else do
                gs <- State.get
                case GameState.priority gs of
                  Nothing -> pure ()
                  Just p ->
                    if List.elem p (Departure.stillPlaying gs)
                      then do
                        -- CR 104.3a: asked before anything else, and keyed to `p` -- the
                        -- TRUE player, never `Decide.deciderFor p`. Prompt.Concede carries
                        -- no Decider precisely so this cannot be got wrong (CR 723.6): a
                        -- controller may not concede for the player they control, though
                        -- that player may still concede themselves.
                        concession <- Trans.lift (Program.prompt (Prompt.Concede p))
                        case concession of
                          Concession.Concedes -> do
                            -- CR 104.3a: leaves the game IMMEDIATELY. Not a state-based
                            -- action (that is CR 104.3b), so it does not wait for a settle,
                            -- and nothing goes on the stack -- there is nothing to respond
                            -- to. leaveGame settles CR 104.2a on the spot; the loop's own
                            -- `finished` check then unwinds on the next iteration.
                            Departure.leaveGame Departure.Type.Conceded p
                            -- CR 800.4a: priority passes to the next player in turn
                            -- order who's still in the game. nextStillPlaying looks
                            -- `p` up in the full seating order, so it finds p's
                            -- SUCCESSOR even though leaveGame has already run.
                            --
                            -- CR 117.4 defines passing "in succession" as passing
                            -- without taking any actions in between passing. A
                            -- concession is an action that
                            -- changes the board (CR 800.4a removes the departing
                            -- player's objects from the game), so the passes already
                            -- counted this cycle no longer form a succession and the
                            -- count restarts -- as it does in the Play/Cast/Activate
                            -- arms below. The CR does not settle this directly; not
                            -- resetting risks resolving a spell that a player who has
                            -- just watched a board disappear would have responded to,
                            -- and resetting costs at most one redundant pass prompt.
                            State.modify' (\g -> g {GameState.passes = 0, GameState.priority = Just (nextStillPlaying g p)})
                            loop
                          Concession.Continues -> do
                            let decider = Decide.deciderFor p gs
                                actions = Action.legalActions p gs
                            chosen <- Trans.lift (Program.prompt (Prompt.ChooseAction decider p actions))
                            case chosen of
                              Action.Type.Pass -> do
                                let passes = GameState.passes gs + 1
                                    playing = length (Departure.stillPlaying gs)
                                if passes >= fromIntegral playing
                                  then case GameState.stack gs of
                                    [] -> State.put gs {GameState.priority = Nothing, GameState.passes = passes}
                                    _ -> do
                                      Stack.resolveTopWith playSubgame
                                      settleForPriority
                                      State.modify' (\g -> g {GameState.passes = 0, GameState.priority = Just (priorityHolder g)})
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
                      else do
                        -- CR 800.4a (last sentence): "If the player who left the
                        -- game had priority at the time they left, priority passes
                        -- to the next player in turn order who's still in the
                        -- game." p was written as the holder and then departed --
                        -- e.g. paying a life cost inside settleForPriority (CR
                        -- 119.4) -- before ever being asked anything.
                        -- Departure.depart does not touch GameState.priority, so
                        -- the stale `Just p` would otherwise survive to the Concede
                        -- prompt below. nextStillPlaying scans the full seating
                        -- roster and is correct for a departed argument (see its
                        -- haddock), so the successor is found without asking p
                        -- anything.
                        State.modify' (\g -> g {GameState.priority = Just (nextStillPlaying g p)})
                        loop
  settleForPriority
  loop

-- CR 800.4k / CR 800.4m: hand the turn to the next SEAT in the seating order
-- (GameState.turnOrder, which is never shortened -- see Pawl.Type.GameState)
-- whose player is still in the game.
--
-- CR 800.4k: "If a player who has left the game would begin a turn, that turn
-- doesn't begin." So a departed seat is walked past, not made active.
--
-- CR 800.4m: "any continuous effects with durations that last until that
-- player's next turn ... will last until that turn WOULD have begun." So
-- Expiry.dropAtTurnOf fires at EVERY seat the walk passes, including the ones
-- whose turn never begins. For the seat that does begin a turn, the same call is
-- CR 611.2a.
handoffTurn :: Game ()
handoffTurn = State.modify' $ \gs ->
  walkToNextTurn (length (GameState.turnOrder gs)) (GameState.activePlayer gs) gs

-- One seat at a time, bounded by the number of seats, so it terminates even when
-- every seat has departed. The fallback returns the state without beginning a
-- turn, keeping the sweeps already applied -- it does NOT roll back to the
-- pre-walk state, since `swept` (each seat's dropAtTurnOf) threads forward with
-- every recursive call. Total with no partial head -- written as an explicit
-- bounded recursion rather than `cycle`/`head` for exactly that reason.
-- Unreachable while the game is running: a game with no survivors already has a
-- Result (CR 104.2a / 104.4a).
walkToNextTurn :: Int -> PlayerId -> GameState -> GameState
walkToNextTurn seatsLeft seat gs =
  if seatsLeft <= 0
    then gs
    else
      let next = nextInOrder (GameState.turnOrder gs) seat
          swept = Expiry.dropAtTurnOf next gs
       in if List.elem next (Departure.stillPlaying swept)
            then beginTurnOf next swept
            else walkToNextTurn (seatsLeft - 1) next swept

-- The turn actually begins for `pid`. Split out of handoffTurn so the CR 800.4k
-- seat walk has exactly one place to land.
beginTurnOf :: PlayerId -> GameState -> GameState
beginTurnOf pid gs =
  let -- CR 800.4b: "If a player would be controlled by a player who has left the
      -- game, they aren't." A pending Decider naming a departed player is not
      -- promoted; the stale entry is dropped either way, below.
      --
      -- CR 800.4a's second clause -- effects giving a departing player control of
      -- objects or players end -- clears the entry at the departure itself
      -- (Departure.controlEffectsEnd), so in play the two rules reach the same
      -- outcome. This guard is the one that answers for CR 800.4b, and it is the
      -- only thing that answers when the entry arrives without a departure having
      -- run over it.
      promoted = case Map.lookup pid (GameState.pendingControl gs) of
        Nothing -> Nothing
        Just decider -> case decider of
          Decider.MkDecider d ->
            if List.elem d (Departure.stillPlaying gs)
              then Just decider
              else Nothing
   in gs
        { GameState.activePlayer = pid,
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
          -- CR 800.4b: unless its decider has left the game, in which case it
          -- is not promoted (see `promoted`, above).
          GameState.activeControl = promoted,
          GameState.pendingControl = Map.delete pid (GameState.pendingControl gs)
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
  -- CR 727.4: "The effect that restarts the game finishes resolving just before
  -- the first turn's untap step." If the previous step unwound on a restart, this
  -- is that untap step, and the rebuilt game is played from here like any other --
  -- so lower the signal before doing anything else.
  State.modify' (\gs -> gs {GameState.restartSignal = RestartSignal.Playing})
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
    restarted <- State.gets GameState.restartSignal
    case restarted of
      -- CR 727.4: a restart replaced the game during this step's priority round.
      -- Unwind: the rebuilt state is already positioned at turn 1's untap step
      -- with empty mana pools and nothing to settle, and `advance` in particular
      -- MUST NOT run -- it would pop the FRESH schedule and skip that untap step
      -- entirely. playGame's next iteration re-enters runStep, which lowers the
      -- signal and plays the new game from its first step.
      RestartSignal.Restarted -> pure ()
      RestartSignal.Playing -> do
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

-- CR 729: play a subgame as a FUNCTION CALL. Construct the subgame from the
-- parent's library cards (CR 729.2, subgameStateFrom), then run its setup and
-- whole game as `runStateT (startGameFromCards >> playGame)` LIFTED into the
-- parent's StateT -- so the subgame's prompts flow through the SAME Program
-- interpreter and Replay fold (untagged; the design). The parent GameState sits
-- untouched in the outer frame while the subgame runs (CR 729.1a). At the end,
-- funnel each owner's cards back to their main-game library (CR 729.5) and
-- reshuffle (Prompt.Shuffle). A subgame within a subgame (CR 729.6) is free: the
-- nested playGame's own priorityLoop re-supplies playSubgame.
-- Nesting terminates: subgameStateFrom draws each level's library from the
-- PARENT level's library zone at cast time (CR 729.2), already reduced by the
-- >= 7 cards its own opening hand consumed, so nesting depth is bounded by
-- roughly |library| / 7 -- it cannot recurse forever (the CR 729.6 gate rests
-- on this bound).
-- Cards brought into a subgame from the main game, and the main-game triggers
-- their removal queues, are not implemented (#152). Nontraditional/Vanguard/
-- Commander subgame movement is not implemented (#131).
playSubgame :: Game Result
playSubgame = do
  parent <- State.get
  -- CR 729.2: "Randomly determine which player goes first." The engine asks; the
  -- interpreter rolls. Only the players still in the main game are in the subgame
  -- (CR 729.4), so only they can be rolled (fixed by #147). Not asked when the answer is
  -- forced -- a lone candidate goes first no matter what randomness says, and
  -- where the rules leave nothing to determine, don't prompt.
  starter <- case NonEmpty.nonEmpty (Departure.stillPlayingInOrder parent) of
    Nothing -> pure (GameState.activePlayer parent)
    Just order -> case order of
      only NonEmpty.:| [] -> pure only
      _ -> Trans.lift (Program.prompt (Prompt.RandomFirstPlayer order))
  let sub0 = Setup.subgameStateFrom starter parent
  (result, finalSub) <- Trans.lift (State.runStateT (Setup.startGameFromCards Resolve.performHandAction >> playGame) sub0)
  State.modify' (Setup.funnelBack finalSub)
  -- CR 729.5: "each player takes all traditional cards they own that are in the
  -- subgame ... puts them into their main-game library, then shuffles them." Each
  -- player who was IN the subgame: a player outside it (CR 729.4) took nothing
  -- into it and is not asked to shuffle their main-game library (fixed by #147).
  seated <- State.gets Departure.stillPlayingInOrder
  Monad.forM_ seated Mulligan.shuffleLibrary
  pure result

playFrom :: NonEmpty.NonEmpty (PlayerId, Deck.Deck) -> Game Result
playFrom matchup = do
  Setup.newGame Resolve.performHandAction matchup
  playGame
