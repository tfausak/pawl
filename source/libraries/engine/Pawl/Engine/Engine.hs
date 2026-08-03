{-# LANGUAGE RankNTypes #-}

module Pawl.Engine.Engine where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Action as Action
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Cast as Cast
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Damage as Damage
import qualified Pawl.Engine.Decide as Decide
import qualified Pawl.Engine.Departure as Departure
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Expiry as Expiry
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Mana as Mana
import qualified Pawl.Engine.Modal as Modal
import qualified Pawl.Engine.Monarch as Monarch
import qualified Pawl.Engine.Mulligan as Mulligan
import qualified Pawl.Engine.PlayerEffect as PlayerEffect
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Replacement as Replacement
import qualified Pawl.Engine.Resolve as Resolve
import qualified Pawl.Engine.Sba as Sba
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Engine.Target as Target
import qualified Pawl.Engine.Turn as Turn
import qualified Pawl.Extra.Int as Int
import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.Types.Action as Action.Type
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.Concession as Concession
import qualified Pawl.Types.Decider as Decider
import qualified Pawl.Types.Deck as Deck
import qualified Pawl.Types.Departure as Departure.Type
import qualified Pawl.Types.DiscardCause as DiscardCause
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.ExtraTurn as ExtraTurn
import Pawl.Types.Game (Game)
import qualified Pawl.Types.GameEvent as GameEvent
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PendingTrigger as PendingTrigger
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PhaseSelector as PhaseSelector
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.Program as Program
import Pawl.Types.Prompt (Prompt)
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.RestartSignal as RestartSignal
import Pawl.Types.Result (Result)
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TriggerEntry as TriggerEntry
import qualified Pawl.Types.TriggerSource as TriggerSource
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.Zone as Zone

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
-- shortened -- see Pawl.Types.GameState), so a player who has ALREADY departed
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
-- Game.stillPlaying directly, where reassignOnDeparture takes `playing`
-- injected so the departing caller's snapshot is explicit. Changing
-- either walk without checking the other risks reintroducing this
-- duplication with a mismatch baked in.
--
-- Total: falls back to `pid` when nobody is still playing.
nextStillPlaying :: GameState -> PlayerId -> PlayerId
nextStillPlaying gs pid =
  let order = GameState.turnOrder gs
      playing = Game.stillPlaying gs
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
   in if List.elem active (Game.stillPlaying gs)
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
-- The record names `pid`, so it answers CR 302.6 only for `pid` -- the rule's
-- subject is a player ("under ITS CONTROLLER'S control since THEIR most recent
-- turn began"), and a settle made for one player says nothing about another.
--
-- It iterates `Projection.controls`, so it settles for whoever currently
-- controls the permanent, not its owner. That reading is control-duration-
-- agnostic: an until-end-of-turn effect (Act of Treason) always wears off
-- (CR 514.2) before the thief's own next untap step, so the creature settles
-- back under its owner there, but an Aura's static ability (Control Magic)
-- grants control INDEFINITELY, so the creature can still be the thief's at
-- their own untap step and settles for them instead -- covered by CR 302.6
-- (#62) in `Pawl.AuraSpec`.
settleAll :: PlayerId -> Game ()
settleAll pid = do
  gs <- State.get
  let settle obj = obj {Object.sickness = Sickness.Settled pid}
      ids = Projection.controls pid gs
  State.put gs {GameState.objects = foldr (Map.adjust settle) (GameState.objects gs) ids}

-- CR 302.6 asks for control held CONTINUOUSLY, so a settle must not outlive the
-- control it was made about. This drops any `Settled p` on the battlefield whose
-- object `p` no longer controls.
--
-- It samples rather than hooks because control is DERIVED: a control-granting
-- static ability (Control Magic's `SetControllerToSource`) is re-read live by the
-- projection, so a control change has no event to hang a re-sickening on -- there
-- is no moment to observe (#198). `settleForPriority` is where it samples, which
-- is every point the board can CHANGE rather than literally every priority grant;
-- a bare priority pass leaves the state untouched, so the previous sample already
-- saw this exact board. That is the same reading the settle loop's own header
-- defends for the CR 117.5 state-based-action check.
--
-- It only ever CLEARS. That asymmetry is what makes the sampling sound: a
-- discrepancy proves control changed, so clearing is always right, while
-- granting from a sample would invent continuity across the gap between two
-- samples. It is also what catches control leaving and returning inside one turn
-- (bob's Control Magic, then alice's Angelic Edict on it): the record is gone by
-- the time control comes back, and only alice's next untap step restores it.
--
-- Battlefield-scoped: nothing off the battlefield has a controller to compare
-- against, and CR 302.6 is a restriction on permanents.
--
-- Hoists the grant list and calls `controllerOfGiven`, exactly as
-- `Projection.controls` does -- linear in the battlefield rather than one
-- `controlGrants` scan per object. That is not only speed: `settleAll` writes
-- through `Projection.controls`, so deriving the check the same way makes the
-- two structurally unable to disagree about who controls what.
checkControlContinuity :: Game ()
checkControlContinuity = do
  gs <- State.get
  let grants = Projection.controlGrants gs
      interrupted oid objs = case Map.lookup oid objs of
        Nothing -> objs
        Just obj -> case Object.sickness obj of
          Sickness.Sick -> objs
          Sickness.Settled p ->
            if Projection.controllerOfGiven grants Set.empty oid gs == Just p
              then objs
              else Map.insert oid obj {Object.sickness = Sickness.Sick} objs
  State.put gs {GameState.objects = foldr interrupted (GameState.objects gs) (Set.toList (GameState.battlefield gs))}

-- CR 514.1's cleanup discard ("if the active player's hand contains more cards
-- than their maximum hand size ... they discard enough cards to reduce their hand
-- size to that number") -- NOT CR 514.2, which is the damage-removal and
-- end-of-turn sweep beside it. Non-identical cards now share a hand (Mountains
-- and Pikers), so
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
          excess = length held - Natural.toIntSaturating limit
      Monad.when (excess > 0) $ do
        let decider = Decide.deciderFor pid gs
        chosen <- Trans.lift (Program.prompt (Prompt.ChooseDiscard decider pid held (Int.toNaturalSaturating excess)))
        let inHand oid = List.elem oid held
            toDiscard = take excess (filter inHand chosen)
        -- CR 701.9a, through the shared discard funnel: a cleanup discard is a
        -- discard, so it records one for a rule 701.9a trigger to read. A trigger
        -- this fires is one "waiting to be put onto the stack" during the cleanup
        -- step, which is CR 514.3a's condition -- see `cleanupException`, and
        -- Megrim in Pawl.GameSpec for the proof that it is reached from here.
        Monad.mapM_ (Event.discard DiscardCause.Ordinary pid) toDiscard

-- CR 103.8a: "In a two-player game, the player who plays first skips the draw
-- step (see rule 504, "Draw Step") of their first turn." CR 103.8c: "In all
-- other multiplayer games, no player skips the draw step of their first turn."
-- CR 800.7 says the same from the multiplayer side.
--
-- CR 800.1: "A multiplayer game is a game that begins with more than two
-- players." GameState.turnOrder is the permanent seating roster (see
-- Pawl.Types.GameState), so counting seats answers "begins with" directly: a
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
  -- Pawl.Types.Combat's defender field.
  hasActive <- State.gets (\gs -> List.elem active (Game.stillPlaying gs))
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
    -- identical `List.elem (GameState.activePlayer gs) (Game.stillPlaying
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
    Phase.Combat CombatStep.DeclareAttackers -> Monad.when hasActive (Combat.declareAttackers active)
    Phase.Combat CombatStep.DeclareBlockers -> Combat.declareBlockers
    Phase.Combat CombatStep.CombatDamage -> do
      -- CR 510.4: deal this step's damage; if it was the first-strike step,
      -- splice a second combat damage step in after it. The between-steps
      -- priority (CR 510.3) and SBA check come free from the step machinery.
      needSecond <- Damage.dealCombatDamage
      Monad.when needSecond $
        State.modify' (\gs -> gs {GameState.remaining = Turn.spliceSecondDamage (GameState.remaining gs)})
    -- CR 511.1: "the end of combat step has no turn-based actions", so it has no
    -- arm here, deliberately. CR 511.3's removal from combat is an end-of-STEP
    -- action and runStep performs it there, beside CR 500.5's mana emptying.
    Phase.Ending EndingStep.Cleanup -> do
      Monad.when hasActive (discardToHandSize active)
      -- CR 514.2: damage wears off AND until-end-of-turn effects end,
      -- simultaneously. One sweep over both carriers (Pawl.Engine.Expiry). NOT
      -- guarded: CR 703.4p is the game's action, not the active player's.
      --
      -- This is the pool's one reachable CR 603.3a window, and the reason
      -- GameState.controlWhenTriggered exists: the discard above has already
      -- fired a rule 701.9a trigger, CR 514.3a does not place it until after this
      -- line, and an "until end of turn" control effect the sweep ends here would
      -- otherwise have the scan credit that trigger to whoever got the permanent
      -- BACK. Pawl.TriggerSpec's controllerAtTriggerSpec is the proof, with a
      -- Megrim stolen by Zealous Conscripts.
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
-- The abilities that fired include CR 725.2's sourceless inherent monarch pair:
-- gathered apart (see `inherent` below), but ordered and placed with everything
-- else, in ONE batch, because CR 603.3b gives the choice to a controller over
-- every triggered ability they control, not over some subset of them.
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
-- every group to a still-playing controller (Game.stillPlaying), so a
-- PendingTrigger whose controller has left never appears in `ordered` and
-- `placeOne` never sees it. Under the pipeline `orderPending` feeds --
-- `apnapPlayers` -> `orderFor` -> `permute`, none of which can ever
-- INTRODUCE an entry. TWO carriers can reach this with a departed controller,
-- and only ever once more than two players are seated
-- (Departure.continuesAfterDeparture; at two, CR 800.4a's clauses never run and
-- CR 104.2a ends the game before an end step returns, so the question is moot
-- there, and apnapPlayers's filter would catch either of them regardless if it
-- somehow arose) -- see Pawl.Engine.Departure's objectsLeaveWith haddock:
--
--   * a DELAYED ability, whose controller CR 603.7d fixed as the spell that
--     created it resolved ("the player who controlled that spell as it
--     resolved"), which is why it can outlive that player.
--   * an OBJECT-BORNE ability out of Event.eventTriggers, which reads CR 603.3a's
--     controller from GameState.controlWhenTriggered -- who controlled the source
--     when it TRIGGERED -- rather than live. A player who controlled it then and
--     has left the game since is exactly the case CR 800.4d's second sentence
--     describes, and dropping the trigger is what that rule asks for; the live
--     read this replaced would instead have placed it under whoever CR 800.4a
--     handed the permanent back to, an ability CR 603.3a says was never theirs.
--     No board in the pool reaches it (#604).
--
-- Event.stateTriggers is not a third: CR 603.8 evaluates a state trigger AT this
-- scan, so its live read is the right moment by construction.
--
-- The DELAYED entry is still CONSUMED regardless: `surviving` above already
-- dropped it from delayedTriggers, because CR 603.7b spends the one shot on
-- the trigger event, which happened. The monarch's `inherent` triggers go
-- through the same filter, since they are merged into the batch before
-- `orderPending` runs; belt and braces, because CR 725.4 already keeps the
-- crown off a departed seat (see the gather site below).
--
-- The Bool this returns is computed from `ordered`, not the pre-filter
-- `pending`, so it still tells the truth ("were any placed") in exactly the
-- case CR 800.4d creates: a departed player's delayed ability firing with
-- nothing else pending would otherwise report `True` on a step that put
-- nothing on the stack.
--
-- CR 800.4d's FIRST sentence ("If an object that would be owned by a player who
-- has left the game would be created in any zone, it isn't created") and CR
-- 800.4b's SECOND sentence ("If a token would be created under the control of a
-- player who has left the game, no token is created") are both enforced by the
-- guard at the head of `Event.createTokens`. By CR 111.2 a token's owner and
-- controller are the same player, so for a token those two sentences coincide
-- and one guard satisfies both; a token is the only object in this pool that
-- either sentence has a producer for at all.
--
-- That guard is DEFENCE IN DEPTH, not a reachable path. The filter described
-- above stops a departed player's ability one step earlier: it never reaches the
-- stack, so it never resolves, so its Effect.Create never runs. The guard states
-- the rule where the rule belongs -- at the single place a token is minted --
-- rather than leaving CR 800.4b's second sentence resting on the CR 800.4d
-- filter continuing to hold.
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
      -- CR 725.2: the monarch's inherent triggers hang on no object, so
      -- Event.gatherTriggers -- which asks each battlefield permanent what it
      -- triggers -- has nowhere to find them. Gathered separately, from the SAME
      -- unscanned-event snapshot and before the watermark bump, then merged into
      -- the one batch below: CR 603.3b gives a player the order among ALL the
      -- abilities they control that triggered since they last had priority, and
      -- CR 725.2 makes these abilities theirs like any other. Placing them after
      -- the ordered batch would make them resolve first, by the engine's choice
      -- rather than the player's.
      --
      -- Their controller is always the current monarch, and CR 725.4 --
      -- Monarch.reassignOnDeparture, run inside Departure.depart over
      -- Game.stillPlayingInOrder -- keeps the crown off a departed seat, so
      -- CR 800.4d has nothing to catch here; apnapPlayers filters them anyway.
      inherent = Monarch.inherentMonarchPending evs gs
  State.put
    gs
      { GameState.scannedThrough = Natural.length (GameState.events gs),
        -- CR 603.3a's sample is spent with the batch it was taken for. Cleared
        -- rather than left standing, so the next event to open a batch takes a
        -- fresh one (Event.recordEvent samples only when nothing is unscanned)
        -- and a stale reading can never outlive the events it described.
        GameState.controlWhenTriggered = Map.empty,
        GameState.delayedTriggers = surviving
      }
  ordered <- orderPending (pending <> inherent)
  Monad.mapM_ placeOne ordered
  pure (not (null ordered))

-- Put one triggered ability from the ordered batch on the stack. What it hangs
-- on decides how: an ability BORNE by an object goes through placeBorne, where
-- every step is keyed to that object -- CR 113.7's reserved source binding, and
-- the fillableModes/legalSets pair that reads modes and targets relative to it.
-- CR 725.2's sourceless pair has no such object and takes the other arm.
placeOne :: PendingTrigger.PendingTrigger -> Game ()
placeOne pending = case PendingTrigger.source pending of
  TriggerSource.Sourceless -> Monarch.placeInherent pending
  TriggerSource.OfObject srcId -> placeBorne srcId pending

-- Put one object-borne triggered ability on the stack as a fresh OfTrigger
-- object, choosing its mode(s) and their targets as it is placed (CR 603.3d).
-- This mirrors Cast.castSpell's cast-time flow: CR 700.2b -- the controller
-- chooses the mode when the ability triggers (elided, forced and unprompted,
-- exactly when the fillable modes are no more than the selection demands, #50)
-- -- then CR 603.3d -- targets for the CHOSEN mode(s) are chosen now, at
-- placement.
--
-- CR 603.3c/700.2b: "If no mode is chosen, the ability is removed from the
-- stack." This is the trigger-only half of the rule -- a SPELL that can't
-- choose a mode is simply never offered for casting in the first place (CR
-- 601.2c, M4g); a TRIGGER is already placed on the stack (CR 603.3d puts it
-- there before modes/targets are chosen), so an unfillable one must instead be
-- taken back OFF the stack. The guard precedes the mode prompt: a removed
-- trigger must never be asked to choose.
placeBorne :: ObjectId.ObjectId -> PendingTrigger.PendingTrigger -> Game ()
placeBorne srcId pending = do
  gs <- State.get
  let controller = PendingTrigger.controller pending
      ability = PendingTrigger.ability pending
      (abilId, gs1) = Game.freshObjectId gs
      (ts, gs2) = Game.freshTimestamp gs1
      decider = Decide.deciderFor controller gs
      modal = TriggeredAbility.modal ability
      legal = Target.fillableModes (Just controller) srcId Map.empty modal gs
      count = Modal.selectionCount modal
      obj =
        Object.MkObject
          { Object.owner = controller,
            Object.enteredUnder = Nothing,
            Object.source = Source.OfTrigger srcId ability,
            Object.zone = Zone.Stack,
            Object.tapped = TapState.Untapped,
            Object.damage = 0,
            Object.sickness = Sickness.Settled controller,
            Object.bindings = Map.empty,
            Object.counters = Map.empty,
            Object.attachedTo = Nothing,
            Object.timestamp = ts
          }
  State.put gs2 {GameState.objects = Map.insert abilId obj (GameState.objects gs2), GameState.stack = abilId : GameState.stack gs2}
  if Natural.length legal < count
    then -- CR 603.3c: fewer legal modes than the selection demands -- for
    -- ChooseExactly 1, no legal mode at all -- removes the ability.
      State.modify' (Game.cease abilId)
    else do
      -- CR 700.2b: forced when there is nothing to choose (as many legal modes
      -- as the selection demands), prompted otherwise.
      chosenModes <-
        if Natural.length legal <= count
          then pure legal
          else Trans.lift (Program.prompt (Prompt.ChooseModes decider controller abilId legal count))
      -- CR 603.3d: targets for the chosen mode(s) only, chosen as the ability
      -- is placed. A mode with no target slots (Create, or a Draw that names its
      -- drawer without targeting) asks nothing.
      let sets = Target.legalSets (Just controller) srcId (Modal.modesTargetSpecs chosenModes modal) gs
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
      State.modify' (\g -> g {GameState.objects = Map.adjust (\o -> o {Object.bindings = Binding.setYou controller (Binding.setTriggerSource srcId (Map.union (Binding.fromChoices chosen Nothing chosenModes) (PendingTrigger.bindings pending)))}) abilId (GameState.objects g)})

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
  let rotated = Game.apnapOrder gs
      -- turnOrder is the permanent SEATING roster, so the rotation still names
      -- departed seats. A player who has left the game is not in APNAP order and
      -- is never asked to order triggers (CR 800.4a leaves them nothing to
      -- control in the first place, which this does not depend on).
      playing = Game.stillPlaying gs
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
      answer <- Trans.lift (Program.prompt (Prompt.OrderTriggers decider pid (fmap entryOf mine)))
      pure (permute mine answer)

-- What one pending trigger looks like to the player being asked for CR 603.3b's
-- order: its source and WHICH ABILITY it is (#61), and neither of the two fields
-- that would say nothing or too much -- see Pawl.Types.TriggerEntry.
--
-- Passing the ability THROUGH is not the rules core reading it. Nothing here
-- cases on what the ability does, and nothing may: the engine's own use of the
-- answer is the index permutation below.
entryOf :: PendingTrigger.PendingTrigger -> TriggerEntry.TriggerEntry
entryOf pending =
  TriggerEntry.MkTriggerEntry
    { TriggerEntry.source = PendingTrigger.source pending,
      TriggerEntry.ability = PendingTrigger.ability pending
    }

-- Reject-not-repair, as payment already does: only a genuine permutation of the
-- offered indices is honoured. Anything else -- a short answer, a duplicate, an
-- out-of-range index -- leaves the canonical order standing rather than dropping
-- or duplicating a trigger.
permute :: [a] -> [Natural] -> [a]
permute xs order =
  let canonical :: [Natural]
      canonical = zipWith const [0 ..] xs
      at i = case List.genericDrop i xs of
        h : _ -> Just h
        [] -> Nothing
   in if List.sort order == canonical
        then Maybe.mapMaybe at order
        else xs

-- CR 117.5's settle, discarding the report. The name all but one caller uses;
-- `performSettle` below is the same act, and carries the whole account of it.
settleForPriority :: Game ()
settleForPriority = Monad.void performSettle

-- CR 117.5: each time a player would receive priority, sweep expired "for as
-- long as" effects, perform state-based actions, then put triggered abilities
-- on the stack, repeating until none of the three does anything. Then priority
-- is granted (by the caller). The repeat is gated on four cheap booleans --
-- whether the conditional sweep changed anything, whether a monarch exile
-- returned, whether an SBA fired and whether a trigger was placed -- so a settle
-- that changes nothing (the common case) costs one board projection and one
-- length comparison per carrier, NOT a deep GameState equality check.
--
-- On top of that, every pass pays two samples of derived state, because a derived
-- change to control or to card types has nothing else to notice it. The CR 302.6
-- continuity scan (checkControlContinuity) is unconditional and linear in the
-- battlefield -- a real addition to this path's cost, not a free rider. The
-- CR 506.4 removal-from-combat scan (Combat.removeChanged) costs a control-grant
-- scan and a gather while creatures are in combat, and nothing at all when none
-- are, which is most of the game.
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
--
-- It also REPORTS whether it performed any state-based action or placed any
-- triggered ability on the stack. Same split as Sba.checkStateBasedActions /
-- Sba.performStateBasedActions, and for the same reason: one caller needs the
-- answer and the rest do not.
--
-- That caller is `cleanupException` (CR 514.3a), and the two things reported are
-- exactly the two the rule asks about. The CR 611.2b conditional sweep and the
-- monarch exile return (Monarch.returnExiledForMonarch, a DURATION ending rather
-- than an ability triggering) also happen here and also make the loop repeat, but
-- neither is a state-based action and neither is a triggered ability, so neither
-- answers CR 514.3a's question and neither is reported.
--
-- Reported across EVERY pass, where CR 704.3's last sentence names "the step's
-- first check". The two agree wherever the conditional sweep is inert, which is
-- every board the CR itself describes -- the CR does not put a CR 611.2b sweep
-- inside this check at all, so under its own reading a first check that performs
-- nothing is a loop that does nothing. Where pawl's extra sweep makes them
-- differ, this errs toward granting priority, so a trigger placed on a later pass
-- still gets the CR 514.3a round it is entitled to instead of resolving a turn
-- late.
performSettle :: Game Bool
performSettle = do
  swept <- Expiry.sweepConditional
  returned <- Monarch.returnExiledForMonarch
  acted <- Sba.performStateBasedActions
  placed <- placePendingTriggers
  -- Last, and for the same reason the conditional sweep runs first: both read
  -- state this settle can still change, and are placed to see what it leaves
  -- behind rather than what some earlier step saw. Both read CONTROL, and the
  -- CR 506.4 scan also reads CARD TYPES -- where the sweep is what ends a "for as
  -- long as" animation, and so what makes an attacker stop being a creature.
  --
  -- Outside the recursion guard on purpose -- neither makes further work, so
  -- neither is a reason to loop, and both must run even on a pass where nothing
  -- fired. That last part is also what keeps the placement a matter of doing the
  -- work in ONE pass rather than of correctness: the settle stops only on a pass
  -- where nothing fired, and these two ran on that pass, against the finished
  -- board, before priority is granted.
  --
  -- Order between the two does not matter and is not load-bearing: CR 506.4 asks
  -- about combat and CR 302.6 about summoning sickness, and neither reads what the
  -- other writes.
  State.modify' Combat.removeChanged
  checkControlContinuity
  more <- if swept || returned || acted || placed then performSettle else pure False
  pure (acted || placed || more)

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
                    if List.elem p (Game.stillPlaying gs)
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
                            answered <- Trans.lift (Program.prompt (Prompt.ChooseAction decider p actions))
                            -- FILTERED, NOT TRUSTED. Everything Action.legalActions
                            -- computed -- the controller check, CR 302.6's
                            -- tap-sickness gate, CR 307.5 timing, cost payability,
                            -- CR 305.2's one land per turn, CR 117.1a's casting
                            -- timing and every prohibition -- is enforced here.
                            -- Acting on an unoffered answer would make all of it
                            -- advisory (#219). Cast.castSpell and
                            -- Activate.activateAbility re-validate modes, targets
                            -- and payment on their own; what only this guard can
                            -- catch is an action that was never on the menu.
                            --
                            -- Rejecting to Pass rather than failing: it is always a
                            -- legal action, it keeps this loop total, and it cannot
                            -- wedge the game, because a full round of passes
                            -- resolves the stack or ends the step. Same
                            -- reject-not-repair posture Combat.declareAttackers and
                            -- Cost.payComponents take toward their own answers.
                            let chosen = if List.elem answered actions then answered else Action.Type.Pass
                            case chosen of
                              Action.Type.Pass -> do
                                let passes = GameState.passes gs + 1
                                    playing = Natural.length (Game.stillPlaying gs)
                                if passes >= playing
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

-- CR 500.7 / 800.4k / 800.4m: this turn is over, so begin the next one -- a
-- pending EXTRA turn if there is one, and otherwise the turn of the next SEAT in
-- the seating order (GameState.turnOrder, which is never shortened -- see
-- Pawl.Types.GameState) whose player is still in the game. Which of the two it
-- is, is takeNextTurn's question.
--
-- CR 800.4k: "If a player who has left the game would begin a turn, that turn
-- doesn't begin." So a departed seat is walked past, not made active, and a
-- departed player's extra turn is spent without beginning.
--
-- CR 800.4m: "any continuous effects with durations that last until that
-- player's next turn ... will last until that turn WOULD have begun." So
-- Expiry.dropAtTurnOf fires at EVERY seat the walk passes and at every extra
-- turn popped, including the ones whose turn never begins. For the seat that
-- does begin a turn, the same call is CR 611.2a.
handoffTurn :: Game ()
handoffTurn = State.modify' takeNextTurn

-- CR 500.7 / 103.1: the seat the ordinary turn order resumes from. Read through
-- one function so the two callers below cannot drift: the anchor is the active
-- player unless an extra turn is under way, in which case it is the seat that
-- extra turn was inserted after (see GameState.turnAnchor).
turnAnchorOf :: GameState -> PlayerId
turnAnchorOf gs = Maybe.fromMaybe (GameState.activePlayer gs) (GameState.turnAnchor gs)

-- CR 500.7 first: "Some effects can give a player extra turns. They do this by
-- adding the turns directly after the specified turn." Every extra-turn effect
-- in the pool specifies the turn it resolves in, so an entry in
-- GameState.extraTurns is a turn scheduled directly after THIS one -- which
-- makes popping it here, before the seating order is consulted at all, the whole
-- of "directly after".
--
-- CR 500.7 last: "the most recently created turn will be taken first" -- so the
-- store is a stack and this takes its HEAD. Resolve's TakeExtraTurn arm is the
-- other half; between them, two turns created in one turn come out in the
-- reverse of the order they were created in.
--
-- The anchor does NOT move (see GameState.turnAnchor): CR 500.7 adds a turn and
-- removes none, so the turn that would have followed the specified turn still
-- follows it. That is only observable when the taker is not the active player --
-- Time Warp aimed at an opponent -- and it is what stops an extra turn from
-- silently eating that player's ordinary one.
--
-- CR 805.8 (shared team turns) and CR 807.4i/j (Grand Melee's turn markers, which
-- can make a player's extra turn wait) each rewrite this rule for their own
-- option or variant. Neither is implemented, because pawl has no format or
-- variant to read one from (#175).
--
-- CR 800.4k applies to an extra turn exactly as it does to an ordinary one: a
-- departed player's extra turn does not begin. The entry is still SPENT, and
-- Expiry.dropAtTurnOf still fires for CR 800.4m's "would have begun" -- the same
-- two things walkToNextTurn does for a seat it walks past.
--
-- Total: each recursive call consumes one entry, and the empty case falls
-- through to walkToNextTurn, which is bounded by the seat count.
takeNextTurn :: GameState -> GameState
takeNextTurn gs = case GameState.extraTurns gs of
  [] -> walkToNextTurn (length (GameState.turnOrder gs)) (turnAnchorOf gs) gs
  entry : rest ->
    let pid = ExtraTurn.taker entry
        anchor = turnAnchorOf gs
        swept = Expiry.dropAtTurnOf pid gs {GameState.extraTurns = rest}
        anchored = swept {GameState.turnAnchor = Just anchor}
     in if List.elem pid (Game.stillPlaying swept)
          then -- CR 500.11: this turn's OWN skips (Savor the Moment's "skip the
          -- untap step of that turn") come into being as it begins, and only on
          -- this branch -- the CR 800.4k entry the `else` spends belongs to a
          -- turn that never begins, so there is nothing for a skip of it to do.
          -- Nothing of this turn has started yet, so CR 614.10's "once a step,
          -- phase, or turn has started, it can no longer be skipped" is not yet
          -- in the way.
            Replacement.installTurnSkips entry (beginTurnOf pid anchored)
          else takeNextTurn swept

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
       in if List.elem next (Game.stillPlaying swept)
            then -- CR 500.7 / 103.1: this turn IS the ordinary rotation, so the
            -- seat it is dealt to is the one the next walk starts from and
            -- there is nothing left to remember (see GameState.turnAnchor).
              beginTurnOf next swept {GameState.turnAnchor = Nothing}
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
            if List.elem d (Game.stillPlaying gs)
              then Just decider
              else Nothing
   in -- CR 603.7a: the one moment a delayed ability armed for "your next turn"
      -- can learn which turn that is, and the one moment an entry whose turn has
      -- passed can be retired. Applied to the UPDATED state, since both answers
      -- are read off this turn's number and active player.
      Event.settleOnsets
        gs
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
            -- Cleared with the log it describes: the settle Engine.advance runs
            -- immediately before this leaves nothing unscanned, so the sample has
            -- no batch left to answer for.
            GameState.controlWhenTriggered = Map.empty,
            GameState.damageScannedThrough = 0,
            -- GameState.lastKnown is deliberately NOT cleared alongside them. The
            -- log's one-turn scope is a choice about what "look back in time"
            -- (CR 608.2i) has to reach; CR 608.2h's last known information is not
            -- history a card asks after, it is the substitute identity of an object
            -- that no longer exists, and it stays needed for as long as anything can
            -- still name that object -- a delayed triggered ability's source
            -- (CR 603.7d) outlives the turn it was armed in. Pawl.Engine.Setup clears it at
            -- the three points a NEW game begins, which is the only place it means
            -- nothing.
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
    -- CR 117.5: the turn is over. Settle once more so every event the terminal
    -- step's turn-based actions emitted is scanned BEFORE handoffTurn clears the
    -- log -- an unscanned event discarded at handoff is a lost trigger. This is
    -- also where CR 704.3 catches a state-based action raised by those same
    -- turn-based actions (e.g. an Aura's CR 704.5m fall-off): the settle loops
    -- rather than checking once.
    --
    -- The step this lands after is USUALLY the cleanup step, whose own CR 514.3a
    -- check (`cleanupException`) has already run this settle and, if it found
    -- anything, granted priority and scheduled another cleanup step -- so on that
    -- path this one finds nothing left to do. It is kept anyway, because the
    -- schedule can empty at a step that is NOT the cleanup step: CR 500.11's "as
    -- though it didn't exist" over a skipped ending phase (skipWholePhase) drops
    -- the cleanup step along with the rest of the phase, and the handoff then
    -- follows the postcombat main phase directly. Stating the guarantee here
    -- keeps it from resting on an enumeration of what each such step can leave
    -- behind.
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
  active <- State.gets GameState.activePlayer
  -- CR 614.1b: "effects that use the word 'skip' are replacement effects ...
  -- what events, steps, phases, or turns will be replaced with nothing", and CR
  -- 500.11: "to skip a step, phase, or turn is to proceed past it as though it
  -- didn't exist". So the question is asked HERE, of the replacement system, and
  -- a `False` answer means the whole of `runStepThatBegan` -- the CR 603.2b
  -- beginning event, the turn-based actions, the priority round, CR 500.5's mana
  -- emptying -- is not merely empty but never runs. Eon Hub is the card, and CR
  -- 500.6's "at the beginning of" triggers never triggering is the observable
  -- that distinguishes this from a step that happened and did nothing.
  --
  -- CR 614.10's "once a step, phase, or turn has started, it can no longer be
  -- skipped" is what pins the question to this line. `advance` has already
  -- written the step into GameState.phase by now, but nothing has yet observed
  -- it: `advance` records no event and grants no priority, and playGame does
  -- nothing between the two but read GameState.result. So the step is scheduled,
  -- not started, and this is its last unobserved moment.
  --
  -- A skipped STEP is left popped off the schedule by `advance` below rather
  -- than dropped from GameState.remaining the way CR 508.8's combat skip is
  -- (Turn.dropSkippedCombatSteps). Both reach "as though it didn't exist"; the
  -- difference is that CR 508.8 is a RULE, known one step ahead, while a
  -- replacement effect has to be asked at the moment the event would happen,
  -- because CR 616.1's loop reads the board as it then is. A skipped PHASE is
  -- the case where popping one entry is not enough, and `skipWholePhase` below
  -- says what it does instead.
  --
  -- TWO questions, in CR 500.1's own order: a phase that has steps is offered
  -- first, then the step. `Turn.phaseBeginningAt` answers Just only at a stepped
  -- phase's FIRST step, so the phase question is asked once per phase and never
  -- once the phase is under way -- CR 614.10 again, read at phase grain. A main
  -- phase raises only the step question, because CR 505.2 makes it one schedule
  -- entry and asking twice about it would be asking the same thing twice.
  --
  -- Both questions are asked even when the phase one says yes, and in that order,
  -- because CR 500.1 nests the step inside the phase: Stasis skipping an untap
  -- step must still take it during a beginning phase nobody skipped.
  phaseBegins <- case Turn.phaseBeginningAt phase of
    Nothing -> pure True
    Just selector -> Replacement.beginsPhase selector active
  if not phaseBegins
    then skipWholePhase phase
    else do
      begins <- Replacement.beginsPhase (PhaseSelector.Step phase) active
      if not begins
        then advance
        else runStepThatBegan phase

-- CR 500.11: proceed past a SKIPPED PHASE "as though it didn't exist" -- so the
-- rest of its steps leave the schedule and `advance` picks up whatever CR
-- 500.1's fixed order puts after the phase -- the postcombat main phase, for the
-- combat one Stonehorn Dignitary takes.
--
-- Positional, via Turn.dropRestOfPhase, not a filter: CR 500.8 lets a second
-- combat phase be added later in the same turn, and skipping this one says
-- nothing about that one -- the same reason CR 508.8's step skip is positional.
--
-- Nothing about the skipped phase is announced. CR 614.6 makes a replaced event
-- one that "never happens", and CR 500.6's "at the beginning of" triggers hang
-- off the CR 603.2b step records `runStepThatBegan` writes, none of which this
-- path reaches.
--
-- GameState.combat is left ALONE, and does not go stale by being left alone.
-- Combat.clearCombat runs as the end of combat step ends (CR 511.3), which this
-- path bypasses along with the rest of the phase -- but every writer of that
-- record (Combat.declareAttackers, declareBlockers, chooseDefender,
-- putOntoBattlefieldAttacking, Damage's first-strike mark) runs inside a combat
-- step, so a phase whose steps never begin writes nothing and the record is
-- still whatever the last end of combat step emptied it to. Skipping the end of
-- combat STEP on its own is the case that would strand it, and no card in the
-- pool names that step (#447).
skipWholePhase :: Phase.Phase -> Game ()
skipWholePhase phase = do
  State.modify' (\gs -> gs {GameState.remaining = Turn.dropRestOfPhase phase (GameState.remaining gs)})
  advance

-- The body of a step that was not skipped, split out only so `runStep`'s CR
-- 614.1b check reads as a guard rather than as a nesting level.
runStepThatBegan :: Phase.Phase -> Game ()
runStepThatBegan phase = do
  -- CR 603.2b: the step began. Recorded BEFORE the step's turn-based actions, so
  -- the first priority boundary of this step scans it. No player receives
  -- priority during the untap step (CR 502.4), so an ability that triggers then
  -- is held until the next time a player would receive priority -- usually
  -- upkeep, where CR 503.1a puts it on the stack before the active player gets
  -- priority.
  State.modify' (\gs -> Event.recordEvent (GameEvent.StepBegan phase (GameState.activePlayer gs)) gs)
  runTurnBasedActions phase
  -- Asked BEFORE the CR 704.3 check below, not after it. For every step but one
  -- the order is free -- this line is pure there -- and for the cleanup step it
  -- is forced: CR 704.3's last sentence keys that step's outcome to "the step's
  -- FIRST check", so nothing may perform a state-based action ahead of CR
  -- 514.3a's. Running the ordinary check first buries the creature that CR 514.2
  -- just dropped to zero toughness (CR 704.5f), and CR 514.3a then finds an
  -- already-settled board and grants nothing -- see Pawl.GameSpec's "a
  -- state-based action alone fires the exception", which is what caught it.
  grants <- grantsPriorityNow phase
  -- CR 704.3, one unlooped pass (Sba.checkStateBasedActions says why one is
  -- enough at this site). Nothing left for a cleanup step to do, since
  -- `grantsPriorityNow` has just settled it to a fixpoint; the step's own check
  -- for every other.
  checkSba
  finished <- State.gets (Maybe.isJust . GameState.result)
  Monad.unless finished $ do
    Monad.when grants priorityLoop
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
        -- CR 500.5, whole and in its own order: "As a step or phase ends, if
        -- there are effects that last until the end of that step or phase, those
        -- effects expire. Then any unspent mana left in a player's mana pool
        -- empties."
        --
        -- TWO windows end here, not one, and CR 500.5a is why the difference
        -- matters: "effects that last 'until end of combat' expire at the end of
        -- the combat phase, not at the beginning of the end of combat step" (CR
        -- 511.2 again). So the step that just ran is swept, and then -- only if
        -- it was the phase's last step -- the phase is, which is the question
        -- Turn.phaseEndingAt answers. An "until end of combat" animation is
        -- therefore live for the whole of the end of combat step, including the
        -- priority round CR 511.1 grants there, and gone as that step's end
        -- carries the phase's with it.
        --
        -- The step is swept before the phase because CR 500.1 nests the one
        -- inside the other and the inner window ends first -- the reverse of the
        -- order runStep asks the two SKIP questions in, where the phase is
        -- offered before the step it contains. Nothing observes the order in
        -- either case: the two sweeps match disjoint sets of stored effects
        -- (equality on the selector), and no state-based action, trigger or
        -- priority round runs between them.
        State.modify' (Expiry.dropAtEndOf (PhaseSelector.Step phase))
        Foldable.traverse_ (State.modify' . Expiry.dropAtEndOf) (Turn.phaseEndingAt phase)
        -- CR 703.4q: emptying the pool is a turn-based action that does not use
        -- the stack, and CR 500.5's "Then" puts it AFTER the expiries above. This
        -- line says only WHEN; WHICH mana empties is the action itself, and
        -- Mana.emptyManaPools decides it per player and per unit (Upwelling's
        -- whole pool, Omnath Locus of Mana's green).
        --
        -- The ordering is observable, and Pawl.ExpirySpec's "CR 500.5 an
        -- end-of-combat retention effect expires BEFORE the pool empties" is
        -- what proves it: PlayerEffect.DontLoseUnspentMana is read live by
        -- Mana.emptyManaPools, so a retention effect that expires here keeps
        -- nothing, while one swept afterwards would keep the pool across a
        -- boundary it no longer covers. No CARD in the pool prints that
        -- combination (CR
        -- 702.189a firebending is the shape that would); the test installs the
        -- stored effect directly.
        State.modify' Mana.emptyManaPools
        -- CR 511.3: "as soon as the end of combat step ends, all creatures,
        -- battles, and planeswalkers are removed from combat" -- so it belongs
        -- here, at the step's END, and not in runTurnBasedActions at its start.
        -- The two lines share that timing for opposite reasons: CR 703.4q makes
        -- the mana emptying above a turn-based action whose own moment is "as
        -- each step or phase ends", while this is not a turn-based action at all,
        -- because CR 511.1 says this step has none. Start versus end is the
        -- observable part: creatures stay attacking for the whole of the step,
        -- including the priority round CR 511.1 grants, where an instant may
        -- still read them (Kill Shot).
        --
        -- The two orderings of this line and the mana emptying above it are
        -- indistinguishable -- nothing reads a mana pool through the combat
        -- record or the reverse -- and neither raises a state-based action, so
        -- checkSba below is unaffected.
        --
        -- Order against CR 500.5's expiry sweeps is free for a subtler reason,
        -- and it is worth stating because this is the one step where both fire:
        -- an "until end of combat" animation expiring first makes its source
        -- stop being a creature, which CR 506.4 would remove from combat -- but
        -- this line empties the whole record a moment later regardless, and no
        -- priority round, trigger or state-based-action check sits between the
        -- two for anything to observe the difference in.
        Monad.when (phase == Phase.Combat CombatStep.EndOfCombat) (State.modify' Combat.clearCombat)
        -- CR 508.8: drop the two combat steps that have nothing to do if nobody
        -- attacked. Asked as the declare attackers step ENDS, not when its
        -- turn-based action finishes, because the rule's condition has two
        -- clauses: "no creatures are declared as attackers OR put onto the
        -- battlefield attacking", and the second can only happen in the priority
        -- round this line sits after -- an attack trigger (Hanweir Garrison)
        -- resolving. Asking earlier answered the first clause and assumed the
        -- second away (#30).
        --
        -- NOT guarded by hasActive: a turn with no active player declares no
        -- attackers, which is precisely CR 508.8's condition.
        --
        -- Order against the lines above is free -- neither the mana emptying nor
        -- CR 511.3's removal happens in this step, and CR 500.5's expiry sweeps
        -- touch only the four expiry carriers while this touches only
        -- GameState.remaining -- and it is before `advance`, which is what CR
        -- 500.11's "as though it didn't exist" needs.
        Monad.when (phase == Phase.Combat CombatStep.DeclareAttackers) (State.modify' Combat.skipEmptyCombat)
        checkSba
        stillFinished <- State.gets (Maybe.isJust . GameState.result)
        Monad.unless stillFinished advance

-- Whether THIS step grants a priority round. Every step but one answers from the
-- phase alone (Turn.grantsPriority); the cleanup step's answer is CR 514.3's
-- "normally, no player receives priority" qualified by CR 514.3a's exception,
-- which is a question about the board and so has to be asked here.
grantsPriorityNow :: Phase.Phase -> Game Bool
grantsPriorityNow phase = case phase of
  Phase.Ending EndingStep.Cleanup -> cleanupException
  _ -> pure (Turn.grantsPriority phase)

-- CR 514.3a: "At this point, the game checks to see if any state-based actions
-- would be performed and/or any triggered abilities are waiting to be put onto
-- the stack (including those that trigger 'at the beginning of the next cleanup
-- step'). If so, those state-based actions are performed, then those triggered
-- abilities are put on the stack, then the active player gets priority. Players
-- may cast spells and activate abilities. Once the stack is empty and all players
-- pass in succession, another cleanup step begins."
--
-- Checking and performing are ONE act here. `performSettle` is precisely CR
-- 117.5's "perform the state-based actions, then put the waiting triggered
-- abilities on the stack, repeating until neither does anything", and it reports
-- whether it did either. The rule reads as a question asked before its own answer
-- is acted on, but the two orders are indistinguishable: the consequent performs
-- exactly what the question asked about, and nothing observes the board in
-- between. CR 704.3's last sentence states the same procedure from the other end
-- -- "This process also occurs during the cleanup step (see rule 514), except
-- that if no state-based actions are performed as the result of the step's first
-- check and no triggered abilities are waiting to be put on the stack, then no
-- player gets priority and the step ends."
--
-- Returns whether the exception fired, which is what the caller turns into a
-- priority round.
--
-- WHY THIS TERMINATES, given that a cleanup step can now schedule another one.
-- The chain advances only on new work: a cleanup step schedules a successor only
-- when this check found a state-based action to perform or a trigger to place,
-- so a cleanup step that finds nothing ends the turn, and the settle is
-- idempotent -- running it twice over an unchanged board reports False the second
-- time. The turn-based actions the successor re-runs cannot manufacture that work
-- either: CR 514.1 finds the hand already at its maximum and discards nothing,
-- and CR 514.2 finds no marked damage and no "until end of turn" effect left. So
-- a third cleanup step needs the SECOND one's priority round to have produced
-- something the first did not -- a card drawn back over the hand size, a
-- permanent put onto the battlefield that then dies.
--
-- The pool has a producer for each half of the check, and neither can produce a
-- third step. The trigger half is CR 514.1's discard (Megrim), which fires only
-- on the first cleanup step, because the second finds the hand already trimmed.
-- The state-based half is CR 514.2 ending an "until end of turn" pump out from
-- under a creature (CR 704.5f), which the first step's own settle takes to a
-- fixpoint -- including whatever that death raises in turn -- and which the
-- second step has no effect left to end. So every game this engine can play ends
-- its turns in at most two cleanup steps (Pawl.GameSpec's "the second cleanup
-- step finds nothing waiting and ends the turn").
--
-- It is NOT bounded in general, and Magic does not bound it either: an ability
-- that triggers at the beginning of each cleanup step loops forever, and CR
-- 104.4b's draw is the rules' answer to that rather than a bound on the loop.
-- pawl does not detect such a loop (#484), which is the same engine-liveness gap
-- #338 opened from the turn side.
cleanupException :: Game Bool
cleanupException = do
  fired <- performSettle
  Monad.when fired $
    -- "Once the stack is empty and all players pass in succession, another
    -- cleanup step begins." Scheduled HERE, before the priority round rather than
    -- after it, exactly as CR 510.4's second combat damage step is
    -- (Turn.spliceSecondDamage, spliced from runTurnBasedActions).
    --
    -- The two placements are indistinguishable, which is why the existing
    -- precedent decides it rather than an argument. The only thing between here
    -- and `advance` that touches GameState.remaining is CR 500.8's phase splice
    -- (Turn.splicePhases), and it lands the added phases behind this step either
    -- way: with the entry already present it splits at index 0 and appends after
    -- it, and with `remaining` still empty Turn.thisPhase yields an empty prefix
    -- and appends at the front, which this then goes in front of. Both give the
    -- second cleanup step and then the added phase, which is CR 500.8's "directly
    -- after the specified phase" -- the second cleanup step is still part of that
    -- ending phase. The paths that never reach `advance` discard the schedule
    -- wholesale regardless: a restart rebuilds the GameState (CR 727.4), and a
    -- game that has ended never advances again.
    State.modify' (\gs -> gs {GameState.remaining = Turn.spliceExtraCleanup (GameState.remaining gs)})
  pure fired

-- Terminates because libraries are finite, each turn draws at most one card, and
-- drawing from an empty library is a loss (CR 704.5b). That argument rests on the
-- DRAW step being reached, and a CR 614.1b skip of it (runStep's check above)
-- suspends it, exactly as a real Stasis lock suspends a real game.
--
-- Fatigue skips a draw step, so the argument no longer covers every game this
-- engine can be asked to play; there is still no progress bound behind it
-- (#338). It is not yet a way to hang this loop: Fatigue's skip is CR 614.10a's
-- "next", spent on one occurrence and then gone, so each copy cast postpones the
-- draw by one turn rather than stopping it, and a finite number of copies still
-- runs the library out. A card whose skip is unbounded -- a permanent's static,
-- as Eon Hub's upkeep skip is -- would be the one that hangs it.
--
-- CR 500.7's extra turns leave the argument intact. An extra turn is a turn like
-- any other and reaches its own draw step, so the bound is still one card off a
-- finite library per turn; and the schedule cannot refill itself, because
-- GameState.extraTurns is pushed only by a resolving effect and popped only by
-- handoffTurn, so a finite number of resolutions buys a finite number of turns.
-- The card to re-examine this against would be one whose extra turn comes from
-- an ability that triggers every turn, which the pool does not have.
--
-- A turn-scoped skip (Pawl.Types.ExtraTurn's `skipped`, Savor the Moment) leaves
-- it intact too, on both counts. It cannot suspend the draw the way Fatigue's can
-- unless a card names the DRAW step, and none does -- Savor the Moment names the
-- untap step, which the bound never mentioned. And it cannot outlive the turn it
-- rode in on: installTurnSkips arms each row Uses.Once and Expiry.AtCleanup, so
-- the cleanup sweep (Expiry.dropAtCleanup, and see installTurnSkips for why that
-- expiry and not Never) ends whatever the turn did not spend.
--
-- CR 514.3a's extra CLEANUP steps are the one repetition that does not go through
-- a draw step at all, so the library argument says nothing about them. They carry
-- their own termination argument, at `cleanupException`, and it is bounded for
-- the pool rather than in general (#484).
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
  starter <- case NonEmpty.nonEmpty (Game.stillPlayingInOrder parent) of
    Nothing -> pure (GameState.activePlayer parent)
    Just order -> case order of
      only NonEmpty.:| [] -> pure only
      _ -> do
        answer <- Trans.lift (Program.prompt (Prompt.RandomFirstPlayer order))
        -- Filtered, not trusted (#222): a subgame cannot start with a player who
        -- is not seated in it.
        pure (if List.elem answer (NonEmpty.toList order) then answer else NonEmpty.head order)
  let sub0 = Setup.subgameStateFrom starter parent
  (result, finalSub) <- Trans.lift (State.runStateT (Setup.startGameFromCards Resolve.performHandAction >> playGame) sub0)
  State.modify' (Setup.funnelBack finalSub)
  -- CR 729.5: "each player takes all traditional cards they own that are in the
  -- subgame ... puts them into their main-game library, then shuffles them." Each
  -- player who was IN the subgame: a player outside it (CR 729.4) took nothing
  -- into it and is not asked to shuffle their main-game library (fixed by #147).
  seated <- State.gets Game.stillPlayingInOrder
  Monad.forM_ seated Mulligan.shuffleLibrary
  pure result

playFrom :: NonEmpty.NonEmpty (PlayerId, Deck.Deck) -> Game Result
playFrom matchup = do
  Setup.newGame Resolve.performHandAction matchup
  playGame
