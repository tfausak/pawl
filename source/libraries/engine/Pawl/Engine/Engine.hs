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
import qualified Pawl.Engine.Cost as Cost
import qualified Pawl.Engine.Damage as Damage
import qualified Pawl.Engine.Daytime as Daytime
import qualified Pawl.Engine.Decide as Decide
import qualified Pawl.Engine.Departure as Departure
import qualified Pawl.Engine.Dungeon as Dungeon
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Expiry as Expiry
import qualified Pawl.Engine.FaceDown as FaceDown
import qualified Pawl.Engine.Foretell as Foretell
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Ignore as Ignore
import qualified Pawl.Engine.Mana as Mana
import qualified Pawl.Engine.Modal as Modal
import qualified Pawl.Engine.Monarch as Monarch
import qualified Pawl.Engine.Mulligan as Mulligan
import qualified Pawl.Engine.Phasing as Phasing
import qualified Pawl.Engine.PlayerEffect as PlayerEffect
import qualified Pawl.Engine.Plot as Plot
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Rad as Rad
import qualified Pawl.Engine.Replacement as Replacement
import qualified Pawl.Engine.Resolve as Resolve
import qualified Pawl.Engine.Ring as Ring
import qualified Pawl.Engine.Room as Room
import qualified Pawl.Engine.Saga as Saga
import qualified Pawl.Engine.Sba as Sba
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Speed as Speed
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Engine.Target as Target
import qualified Pawl.Engine.Turn as Turn
import qualified Pawl.Engine.UntapRestriction as UntapRestriction
import qualified Pawl.Extra.Int as Int
import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.Types.AbilityTriggered as AbilityTriggered
import qualified Pawl.Types.Action as Action.Type
import qualified Pawl.Types.Asked as Asked
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.Concession as Concession
import qualified Pawl.Types.ControlChanged as ControlChanged
import qualified Pawl.Types.CounterCause as CounterCause
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Decider as Decider
import qualified Pawl.Types.Deck as Deck
import qualified Pawl.Types.Departure as Departure.Type
import qualified Pawl.Types.DiscardCause as DiscardCause
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.ExtraTurn as ExtraTurn
import qualified Pawl.Types.Facing as Facing
import Pawl.Types.Game (Game)
import qualified Pawl.Types.GameEvent as GameEvent
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Mana as Mana
import qualified Pawl.Types.Modal as Modal.Type
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeSelection as ModeSelection
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PendingTrigger as PendingTrigger
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PhaseSelector as PhaseSelector
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.Program as Program
import qualified Pawl.Types.ProjectedCharacteristics as PC
import Pawl.Types.Prompt (Prompt)
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.RestartSignal as RestartSignal
import Pawl.Types.Result (Result)
import qualified Pawl.Types.Result as Result
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.StackObjectKind as StackObjectKind
import qualified Pawl.Types.StepBegan as StepBegan
import qualified Pawl.Types.Supertype as Supertype
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Timestamp as Timestamp
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.TriggerEntry as TriggerEntry
import qualified Pawl.Types.TriggerLimit as TriggerLimit
import qualified Pawl.Types.TriggerSource as TriggerSource
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.Zone as Zone

-- The interpreter seam: every decision the engine suspends on is answered here.
-- The PRIMITIVE form, and the only one that sees which game asked -- an Asked
-- carries the asking game's state and the games it is nested inside (CR 729.1a),
-- which CR 723.4's visibility split needs.
runGameAsked :: (Monad m) => (forall r. Asked.Asked r -> m r) -> GameState -> Game a -> m (a, GameState)
runGameAsked answer gs game = Program.foldProgramM answer (State.runStateT game gs)

runGameAskedPure :: (forall r. Asked.Asked r -> r) -> GameState -> Game a -> (a, GameState)
runGameAskedPure answer gs game = Program.foldProgram answer (State.runStateT game gs)

-- The seam for an answerer that does not care which game it is in.
runGame :: (Monad m) => (forall r. Prompt r -> m r) -> GameState -> Game a -> m (a, GameState)
runGame answer = runGameAsked (answer . Asked.prompt)

runGamePure :: (forall r. Prompt r -> r) -> GameState -> Game a -> (a, GameState)
runGamePure answer = runGameAskedPure (answer . Asked.prompt)

-- One entry point from matchup to played game: the player list is DERIVED from it,
-- so a matchup player without a Player record is unrepresentable.
runMatch :: (Monad m) => (forall r. Prompt r -> m r) -> NonEmpty.NonEmpty (PlayerId, Deck.Deck) -> m (Result, GameState)
runMatch answer matchup =
  runGame answer (Setup.emptyGame (fmap fst matchup)) (playFrom matchup)

runMatchPure :: (forall r. Prompt r -> r) -> NonEmpty.NonEmpty (PlayerId, Deck.Deck) -> (Result, GameState)
runMatchPure answer matchup =
  runGamePure answer (Setup.emptyGame (fmap fst matchup)) (playFrom matchup)

-- The next entry of a cyclic order after 'pid', falling back to 'pid' when the
-- order is empty or does not mention it.
nextInOrder :: [PlayerId] -> PlayerId -> PlayerId
nextInOrder order pid = case dropWhile (/= pid) order of
  _ : y : _ -> y
  _ -> case order of
    h : _ -> h
    [] -> pid

-- CR 800.4a (last sentence): priority passes to the next player in turn order
-- who's still in the game. The seat is looked up in the FULL seating order
-- (GameState.turnOrder is never shortened), so a player who has ALREADY departed
-- still has a position from which to find their successor -- which is how
-- priorityLoop's concede arm calls this. Total. Deliberately NOT shared with
-- Monarch.reassignOnDeparture (CR 725.4), which anchors on the ACTIVE seat,
-- excludes it, and must return a Maybe.
nextStillPlaying :: GameState -> PlayerId -> PlayerId
nextStillPlaying gs pid =
  let order = GameState.turnOrder gs
      playing = Game.stillPlaying gs
      -- Everyone after `pid`, then the whole order again so the wrap is covered.
      scan = drop 1 (dropWhile (/= pid) order) <> order
   in case filter (\p -> List.elem p playing) scan of
        h : _ -> h
        [] -> pid

-- CR 800.4j: a turn whose player has left continues without an active player, and
-- where they would receive priority the next player in turn order does instead.
-- GameState.activePlayer is deliberately NOT widened to a Maybe -- the turn still
-- BELONGS to that seat, CR 800.4m's durations and CR 101.4's APNAP anchor both
-- referencing it.
priorityHolder :: GameState -> PlayerId
priorityHolder gs =
  let active = GameState.activePlayer gs
   in if List.elem active (Game.stillPlaying gs)
        then active
        else nextStillPlaying gs active

-- The step's own CR 704.3 check. Samples CR 704.5k's clock first, this site
-- following the turn-based actions and CR 514.2's sweep being able to change who
-- is world.
checkSba :: Game ()
checkSba = sampleWorldSince >> Sba.checkStateBasedActions

-- CR 502.3's untap, and its own "effects can keep one or more of a player's
-- permanents from untapping": such an effect takes the permanent out of this fold
-- and leaves it as it was (Pawl.Engine.UntapRestriction), CR 101.2 being what
-- makes the "can't" beat the turn-based action.
--
-- THREE carriers, subtracted together. The printed static one is re-derived live;
-- the other two are stored on the victim, and this is where each both applies and
-- ENDS, CR 701.43b putting the expiry in the untap step it bites in. They are two
-- fields because they name different seats: Object.doesNotUntapNext says "its
-- controller's next untap step", so it is applied and cleared against `ids`,
-- while Object.exertedBy says "YOUR next untap step" (CR 701.43a), so `pid` is
-- dropped from EVERY permanent -- one that changed hands is neither held back at
-- its new controller's step nor left carrying a rider past the step that ends it.
untapAll :: PlayerId -> Game ()
untapAll pid = do
  gs <- State.get
  let untap obj = obj {Object.tapped = TapState.Untapped}
      clear obj = obj {Object.doesNotUntapNext = False}
      unexert obj = obj {Object.exertedBy = Set.delete pid (Object.exertedBy obj)}
      ids = Projection.controls pid gs
      prohibited = UntapRestriction.doesNotUntap ids gs
      asks f oid = maybe False f (Game.lookupObject oid gs)
      oneShot = asks Object.doesNotUntapNext
      exerted = asks (Set.member pid . Object.exertedBy)
      untapping = filter (\oid -> not (Set.member oid prohibited) && not (oneShot oid) && not (exerted oid)) ids
      expiring = filter oneShot ids
      untapped = foldr (Map.adjust clear) (foldr (Map.adjust untap) (GameState.objects gs) untapping) expiring
      objects =
        if any (Set.member pid . Object.exertedBy) untapped
          then Map.map unexert untapped
          else untapped
  State.put gs {GameState.objects = objects}

-- CR 302.6: permanents the active player has controlled since their turn began
-- are no longer summoning sick, and the untap step is where that becomes true.
-- The record names `pid`, so it answers CR 302.6 only for `pid`, and it iterates
-- `Projection.controls`, so it settles for whoever CURRENTLY controls the
-- permanent: Control Magic grants control indefinitely, so the creature can still
-- be the thief's at their own untap step.
settleAll :: PlayerId -> Game ()
settleAll pid = do
  gs <- State.get
  let settle obj = obj {Object.sickness = Sickness.Settled pid}
      ids = Projection.controls pid gs
  State.put gs {GameState.objects = foldr (Map.adjust settle) (GameState.objects gs) ids}

-- CR 302.6 asks for control held CONTINUOUSLY, so a settle must not outlive the
-- control it was made about. This drops any `Settled p` on the battlefield whose
-- object `p` no longer controls, and samples rather than hooks because control is
-- DERIVED, no resolution announcing a change for a re-sickening to hang on. It
-- only ever CLEARS, which is what makes the sampling sound: a discrepancy proves
-- control changed, while granting from a sample would invent continuity across
-- the gap between two samples. Derives the check the same way `settleAll` writes
-- it, so the two cannot disagree about who controls what.
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

-- CR 704.5k asks how long each permanent has "had the world supertype", and
-- world-ness is DERIVED (layer 4, CR 613.1d) while the clock must be STORED -- so
-- this samples it into Object.worldSince, as checkControlContinuity samples
-- control. Unlike that one it both WRITES and CLEARS, the sample being taken
-- BEFORE the rule reads it; a stamp-only version would latch, leaving a permanent
-- that lost the supertype and regained it claiming the older clock. ONE fresh
-- timestamp for the whole pass, CR 704.5k's tie clause making permanents that
-- became world simultaneously compare equal, and no special case for a PRINTED
-- world supertype -- seeding from Object.timestamp would make one clock an ENTRY
-- instant and the other a SETTLE.
sampleWorldSince :: Game ()
sampleWorldSince = do
  gs <- State.get
  let pcs = Projection.projectAll gs
      isWorld oid = case Map.lookup oid pcs of
        Nothing -> False
        Just pc -> Set.member Supertype.World (PC.supertypes pc)
      isStamped oid = case Map.lookup oid (GameState.objects gs) of
        Nothing -> False
        Just obj -> Maybe.isJust (Object.worldSince obj)
      ids = Set.toList (GameState.battlefield gs)
      toStamp = filter (\oid -> isWorld oid && not (isStamped oid)) ids
      toClear = filter (\oid -> isStamped oid && not (isWorld oid)) ids
      unstamp obj = obj {Object.worldSince = Nothing}
      cleared = foldr (Map.adjust unstamp) (GameState.objects gs) toClear
  case toStamp of
    [] -> State.put gs {GameState.objects = cleared}
    _ -> do
      let (ts, gs1) = Game.freshTimestamp gs
          stamp obj = obj {Object.worldSince = Just ts}
      State.put gs1 {GameState.objects = foldr (Map.adjust stamp) cleared toStamp}

-- The OBSERVATION POINT for a control change (CR 613.1b puts control in layer 2,
-- so the projection re-reads it live and nothing announces the answer changing).
-- Diffs the live controller of every battlefield permanent against
-- GameState.controlSample, records a GameEvent.ControlChanged for each
-- difference, stores the new snapshot, and reports whether it recorded anything.
--
-- MINTING AN EVENT is what makes this different from the three samplers beside it
-- (checkControlContinuity, Combat.removeChanged, Ring.endOnControlChange), which
-- each clear one piece of stored state and tell nobody: a trigger condition cannot
-- be written against stored state that quietly changes, so CR 603.2 needs an entry
-- in the log. FIRST SIGHTING IS NOT A CHANGE, so a permanent entering mints no
-- event, and the snapshot is REBUILT from the battlefield -- CR 400.7 gives what
-- comes back a new id, so a creature borrowed, killed and reanimated does not
-- come home. Terminates: it only mints on a difference.
sampleControl :: Game Bool
sampleControl = do
  gs <- State.get
  let grants = Projection.controlGrants gs
      sampled =
        Map.fromList
          [ (oid, pid)
          | oid <- Set.toList (GameState.battlefield gs),
            Just pid <- [Projection.controllerOfGiven grants Set.empty oid gs]
          ]
      changes =
        [ GameEvent.ControlChanged (ControlChanged.MkControlChanged oid before after)
        | (oid, after) <- Map.toList sampled,
          Just before <- [Map.lookup oid (GameState.controlSample gs)],
          before /= after
        ]
  State.put gs {GameState.controlSample = sampled}
  -- CR 603.2's simultaneity: two permanents whose control reverted in the same CR
  -- 514.2 sweep changed hands at the same moment, so the batch is one event group.
  Monad.unless (null changes) . Event.simultaneously $
    State.modify' (\g -> List.foldl' (flip Event.recordEvent) g changes)
  pure (not (null changes))

-- CR 514.1's cleanup discard -- NOT CR 514.2, the sweep beside it. The choice is
-- the player's: trimming front-of-hand would be the engine choosing what to
-- pitch. The answer is filtered to cards actually in hand and capped at the
-- excess, so a misbehaving interpreter cannot discard someone else's card or
-- overshoot; one that returns too few simply discards too few.
discardToHandSize :: PlayerId -> Game ()
discardToHandSize pid = do
  gs <- State.get
  -- CR 402.2, not CR 103.5: the maximum hand size is its own rule and its own
  -- seven, and an effect may remove it entirely (Reliquary Tower).
  case PlayerEffect.maximumHandSize pid gs of
    Nothing -> pure ()
    Just limit -> do
      let held = Game.zoneMembers Zone.Hand pid gs
          excess = length held - Natural.toIntSaturating limit
      Monad.when (excess > 0) $ do
        let decider = Decide.deciderFor pid gs
        chosen <- Game.choose (Prompt.ChooseDiscard decider pid held (Int.toNaturalSaturating excess))
        let inHand oid = List.elem oid held
            toDiscard = take excess (filter inHand chosen)
        -- CR 701.9a, through the shared discard funnel, so it records a discard
        -- for a rule 701.9a trigger -- then a trigger waiting during the cleanup
        -- step, CR 514.3a's condition (`cleanupException`).
        Monad.mapM_ (Event.discard DiscardCause.Ordinary pid) toDiscard

-- CR 103.8a: in a two-player game the player who plays first skips the draw step
-- of their first turn; CR 103.8c and CR 800.7, in other multiplayer games nobody
-- does. CR 800.1 makes a multiplayer game one that BEGINS with more than two
-- players, and turnOrder is the permanent roster, so a three-player game down to
-- two survivors still does not skip.
--
-- Not implemented: CR 103.8b's same skip for a TEAM in Two-Headed Giant, pawl
-- having no teams or variants to read from (#175).
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
  -- CR 800.4j: a turn whose player has left the game continues to its completion
  -- without an active player, so the turn-based actions the rules assign to THE
  -- ACTIVE PLAYER -- untap (CR 703.4c), draw (CR 703.4d), choose the defending
  -- player (CR 703.4h), declare attackers (CR 703.4i), the cleanup discard (CR
  -- 703.4n) -- have no subject. Declare blockers (CR 703.4j) belongs to the
  -- defending player and CR 703.4p's sweep is the GAME's action, so neither is
  -- guarded.
  --
  -- Not implemented: CR 800.4h, which reaches the CHOICES on that list -- a choice
  -- a rule requires of a departed player is made by the next player in turn order
  -- (#181). Only the defending-player choice is real, and unobservable.
  hasActive <- State.gets (List.elem active . Game.stillPlaying)
  case phase of
    Phase.Beginning BeginningStep.Untap -> do
      -- CR 502.1 / 703.4a: phasing, immediately after the step begins. Inside the
      -- CR 800.4j guard, rule 502.1 ranging over what THE ACTIVE PLAYER controls;
      -- a row keyed to a player who left EARLIER never reaches it, CR 800.4k
      -- giving that seat no turn, so CR 702.26n's reschedule is at the walk.
      Monad.when hasActive (State.modify' (Phasing.phasingEvent active))
      -- CR 502.2 / 703.4b: the day/night check, second in the step and BEFORE the
      -- untap itself (CR 502.3 / 703.4c). Outside the CR 800.4j guard, rule 703.4b
      -- making it the GAME's action.
      _ <- Daytime.untapCheck Event.recordTransformed
      Monad.when hasActive $ do
        untapAll active
        settleAll active
        State.modify' $ \gs ->
          -- CR 305.2: the allowance is per TURN. DELETED rather than set to 0, so
          -- "has played none" has one representation.
          gs {GameState.landsPlayed = Map.delete active (GameState.landsPlayed gs)}
    Phase.Beginning BeginningStep.DrawStep -> Monad.when hasActive $ do
      skip <- State.gets skipsDraw
      Monad.unless skip (Event.drawCard active)
    -- CR 703.4h: choose the defending player. The active player's action (CR
    -- 507.1), so it takes the same guard -- redundantly, Combat.chooseDefender
    -- computing the identical test, and kept so this arm still reads off CR
    -- 800.4j's enumeration above.
    Phase.Combat CombatStep.BeginningOfCombat -> Monad.when hasActive Combat.chooseDefender
    Phase.Combat CombatStep.DeclareAttackers -> Monad.when hasActive (Combat.declareAttackers active)
    Phase.Combat CombatStep.DeclareBlockers -> Combat.declareBlockers
    Phase.Combat CombatStep.CombatDamage -> do
      -- CR 510.4: deal this step's damage; if it was the first-strike step,
      -- splice a second combat damage step in after it. CR 510.3's between-steps
      -- priority and its SBA check come free from the step machinery.
      needSecond <- Damage.dealCombatDamage
      -- CR 615.5's "immediately afterward": a shield this wave spent runs its
      -- additional effect HERE, before the step's SBA check, so Test of Faith's
      -- +1/+1 counters are on the blocker before CR 704.5g asks about lethality.
      Resolve.runPreventionRiders
      Monad.when needSecond $
        State.modify' (\gs -> gs {GameState.remaining = Turn.spliceSecondDamage (GameState.remaining gs)})
    -- CR 505.4 / 703.4f / 714.3c: a lore counter onto each Saga its controller
    -- has with chapter abilities. A turn-based action, not a trigger, which is why
    -- it lives here and not in the gatherer; the active player's, so it takes the
    -- CR 800.4j guard. Not implemented: CR 703.4g's Attraction roll, which that
    -- rule puts immediately after this and which has no producer (#175).
    Phase.PrecombatMain -> Monad.when hasActive (advanceSagas active)
    -- CR 511.1: the end of combat step has no turn-based actions, so no arm here.
    -- CR 511.3's removal from combat is an end-of-STEP action; runStep does it.
    Phase.Ending EndingStep.Cleanup -> do
      Monad.when hasActive (discardToHandSize active)
      -- CR 514.2: damage wears off AND until-end-of-turn effects end,
      -- simultaneously -- one sweep over the stored-effect carriers, one over
      -- marked damage, one over the mana pools. NOT guarded: CR 703.4p is the
      -- game's action, and their order is not observable. Mana.endManaRetention
      -- only ENDS the retention -- the mana itself is taken by this same step's
      -- CR 500.5 sweep at its end, which makes retained mana outlive every
      -- earlier step. This is the one reachable CR 603.3a window that
      -- GameState.battlefieldWhenTriggered closes: the discard above has fired a
      -- rule 701.9a trigger, CR 514.3a does not place it until after this line,
      -- and an "until end of turn" control effect ending here would otherwise
      -- credit it to whoever got the permanent back.
      State.modify' Damage.removeAllDamage
      State.modify' Expiry.dropAtCleanup
      State.modify' Mana.endManaRetention
    _ -> pure ()

-- CR 505.4 / 703.4f / 714.3c's ACTION half: one lore counter onto each Saga this
-- player controls that has one or more chapter abilities. Here rather than in
-- Pawl.Engine.Saga, which sits below Pawl.Engine.Event in the import graph and so
-- can classify which Sagas advance but cannot call the CR 122.6 placement funnel.
-- Through Event.putCounters, so the placement records the event CR 714.2b's
-- chapter abilities are gathered from.
--
-- ByRule, which is CR 614.16's answer: that rule's replacement effects reach a
-- placement made by a resolving spell or ability, and CR 609.1 makes a turn-based
-- action neither. So Doubling Season does NOT advance a Saga two chapters a turn,
-- though it DOES double the lore counter CR 714.3a's replacement gives it as it
-- enters. The player it carries is rule 714.3c's "that player", which is
-- load-bearing: a clause naming a PLAYER (Vorinclex) does reach this placement.
advanceSagas :: PlayerId -> Game ()
advanceSagas pid = do
  gs <- State.get
  let pcs = Projection.projectAll gs
  Monad.mapM_ (\oid -> Event.putCounters (CounterCause.ByRule pid) oid CounterKind.Lore 1) (Saga.advancing (\oid -> Projection.controllerOf oid gs) pid pcs gs)

-- CR 603.3: put each triggered ability that fired since the last placement on the
-- stack, in APNAP order (CR 603.3b), with each controller choosing the order
-- among their own. The sourceless inherent abilities are gathered apart but
-- placed in ONE batch with everything else, CR 603.3b giving a controller the
-- choice over every ability they control. Its TWO-PART process runs in the rule's
-- own order: first every trigger whose condition isn't another ability
-- triggering, then the rest. Both passes come out of ONE gathered batch, so the
-- split is by CONDITION -- Event.reactsToAbilityTriggering is the classification,
-- exhaustive so a new condition has to choose its pass.
--
-- CR 800.4d, SECOND sentence, needs no separate filter: `orderPending` groups by
-- `apnapPlayers`, which restricts every group to a still-playing controller. Two
-- carriers can still reach it with a departed controller -- a DELAYED ability,
-- proved by Pawl.TriggerSpec's "CR 800.4d a departed player's delayed ability
-- triggers, is consumed, and is not put on the stack", and an OBJECT-BORNE one
-- reading CR 603.3a's controller from GameState.battlefieldWhenTriggered, proved
-- by Pawl.DepartureSpec's "CR 603.3a/800.4d a borrowed permanent's trigger is
-- the departing player's, so it is never put on the stack". CR 800.4d's FIRST
-- sentence and CR 800.4b's SECOND are enforced at the head of
-- `Event.createTokens`.
placePendingTriggers :: Game Bool
placePendingTriggers = do
  gs <- State.get
  let evs = Event.unscannedEvents gs
      -- The GROUPED view of the same snapshot, which the CR 603.10a look-back in
      -- Event.eventTriggers needs and the three inherent gatherers below do not.
      (pending, surviving) = Event.gatherTriggers (Event.unscannedGrouped gs) gs
      -- CR 725.2: the monarch's inherent triggers hang on no object, so
      -- Event.gatherTriggers -- which asks each battlefield permanent what it
      -- triggers -- has nowhere to find them. Gathered separately, from the SAME
      -- snapshot and before the watermark bump, then merged into the one batch
      -- below: placing them after the ordered batch would make them resolve
      -- first, by the engine's choice rather than the player's.
      inherent = Monarch.inherentMonarchPending evs gs
      -- CR 702.179d, the rulebook's third inherent ability, gathered for the
      -- reason above. At most one entry, and only for the active player.
      revving = Speed.inherentPending evs gs
      -- CR 728.1, the fourth, gathered for the same reason. At most one entry,
      -- and only for the active player.
      irradiated = Rad.inherentPending evs gs
      -- CR 309.4c's room abilities, gathered separately because
      -- Event.gatherTriggers reads the command zone for CR 114.4's emblems alone
      -- and a dungeon card is not one (#1411). Unlike the four above these DO have
      -- a source, so they carry TriggerSource.OfObject and go through placeBorne.
      entered = Dungeon.roomPending evs gs
  State.put
    gs
      { GameState.scannedThrough = Natural.length (GameState.events gs),
        -- CR 702.179d's "this ability triggers only once each turn", marked as the
        -- trigger is GATHERED rather than as it resolves, so an instance countered
        -- on the stack has still spent the turn's one. Cleared at beginTurnOf.
        GameState.speedIncreasedThisTurn = List.foldl' (flip Set.insert) (GameState.speedIncreasedThisTurn gs) (fmap PendingTrigger.controller revving),
        -- CR 603.10's per-group samples are spent with the batch they were taken
        -- for. Cleared rather than left standing, which bounds both the map and
        -- the game states its unforced thunks retain.
        GameState.battlefieldWhenTriggered = Map.empty,
        GameState.delayedTriggers = surviving
      }
  gathered <- reactions (pending <> inherent <> revving <> irradiated <> entered)
  -- CR 603.3b's two sentences, run one after the other rather than ordered
  -- together and placed at the end: the rule's first sentence PUTS its abilities
  -- on the stack before its second is reached, which is observable both in the
  -- ordering prompt and in CR 603.3d's target choices.
  first <- orderPending (filter (not . reacts) gathered)
  Monad.mapM_ placeOne first
  second <- orderPending (filter reacts gathered)
  Monad.mapM_ placeOne second
  pure (not (null first) || not (null second))

-- CR 603.3b's classification, asked of one pending trigger: does its condition
-- name another ability triggering? A SOURCELESS inherent ability is classified
-- through its own condition like any other, so it needs no special case.
reacts :: PendingTrigger.PendingTrigger -> Bool
reacts = Event.reactsToAbilityTriggering . TriggeredAbility.condition . PendingTrigger.ability

-- CR 603.3b: "if MULTIPLE ABILITIES HAVE TRIGGERED since the last time a player
-- received priority" -- so an ability that triggers off another ability
-- triggering is part of the SAME batch, and has to be gathered before any of the
-- batch goes on the stack. So: record a GameEvent.AbilityTriggered for every
-- trigger gathered so far, scan those new events for the abilities they in turn
-- fire, and repeat until a round finds nothing. MINTING AN EVENT rather than
-- inspecting the pending set directly, because an ability triggering IS a game
-- event -- rule 603.3b says so by making it a trigger condition -- so the
-- existing machinery applies unchanged. Only EVENT triggers are scanned. No
-- artificial bound on the rounds, a pair of abilities each triggering off the
-- other being a genuine loop in the rules too (CR 104.4b's draw). Every round is
-- filtered by `withinTurnLimit` first, the ONE place a "triggers only once each
-- turn" rider is spent.
--
-- Not implemented: a CR 603.7 DELAYED ability whose trigger event is another
-- ability triggering (#1026).
reactions :: [PendingTrigger.PendingTrigger] -> Game [PendingTrigger.PendingTrigger]
reactions incoming = do
  before <- State.get
  case withinTurnLimit before incoming of
    [] -> pure []
    batch -> do
      -- One CR 704.3 event group EACH, not one `Event.simultaneously` bracket
      -- around the lot: a batch is "the abilities that have triggered since the
      -- last time a player received priority" (CR 603.3b), which can have
      -- triggered off different events at different moments.
      State.modify' (\g -> List.foldl' (flip Event.recordEvent) g (Maybe.mapMaybe triggeredEvent batch))
      gs <- State.get
      let fresh = Event.reactionTriggers (Event.unscannedGrouped gs) gs
      -- The round's own events are consumed here, CR 603.10's samples with them.
      State.modify'
        ( \g ->
            g
              { GameState.scannedThrough = Natural.length (GameState.events g),
                GameState.battlefieldWhenTriggered = Map.empty
              }
        )
      rest <- reactions fresh
      pure (batch <> rest)

-- The printed rider "This ability triggers only once each turn"
-- (Pawl.Types.TriggerLimit), applied to one gathered batch: drop every entry
-- whose ability carries the rider and has already triggered this turn. No stored
-- flag -- the record is CR 603.3b's own log, and GameState.events is cleared at
-- the turn handoff, which makes "in the log" mean "this turn". CR 702.179d's twin
-- (GameState.speedIncreasedThisTurn) is a stored flag; see Pawl.Engine.Speed for
-- why a sourceless trigger needs it. Keyed on the SOURCE and the CONDITION, so
-- two permanents with the same printed ability spend separate limits (CR 113.7)
-- and one that leaves and returns re-arms (CR 400.7), while a change of CONTROL
-- spends nothing. Spent on TRIGGERING.
--
-- Not implemented: a SOURCELESS trigger is never limited here, the log holding
-- no record of one (#1026); and two DISTINCT abilities on one source that share
-- a trigger condition are one key, so an unlimited one firing can spend a
-- limited one's turn (#1664).
withinTurnLimit :: GameState -> [PendingTrigger.PendingTrigger] -> [PendingTrigger.PendingTrigger]
withinTurnLimit gs = go (Set.fromList (Maybe.mapMaybe (fmap spentKey . abilityTriggeredOf . snd) (Foldable.toList (GameState.events gs))))
  where
    spentKey record = (AbilityTriggered.source record, AbilityTriggered.condition record)
    go _ [] = []
    go spent (pending : rest) = case limitedKey pending of
      Nothing -> pending : go spent rest
      Just key
        | Set.member key spent -> go spent rest
        | otherwise -> pending : go (Set.insert key spent) rest

-- The key one pending trigger spends, or Nothing when it spends none -- because
-- its ability prints no rider, or because it is sourceless.
limitedKey :: PendingTrigger.PendingTrigger -> Maybe (ObjectId.ObjectId, TriggerCondition.TriggerCondition)
limitedKey pending = case TriggeredAbility.limit (PendingTrigger.ability pending) of
  TriggerLimit.Unlimited -> Nothing
  TriggerLimit.OncePerTurn -> case PendingTrigger.source pending of
    TriggerSource.Sourceless -> Nothing
    TriggerSource.OfObject oid -> Just (oid, TriggeredAbility.condition (PendingTrigger.ability pending))

-- `triggeredEvent` read back: the record an event carries if it is one ability
-- triggering (CR 603.3b), and nothing otherwise.
abilityTriggeredOf :: GameEvent.GameEvent -> Maybe AbilityTriggered.AbilityTriggered
abilityTriggeredOf event = case event of
  GameEvent.AbilityTriggered record -> Just record
  GameEvent.SpellCast {} -> Nothing
  GameEvent.HalfUnlocked {} -> Nothing
  GameEvent.TurnedFaceUp _ -> Nothing
  GameEvent.Transformed {} -> Nothing
  GameEvent.BecameDesignated {} -> Nothing
  GameEvent.Evolved _ -> Nothing
  GameEvent.Mentored {} -> Nothing
  GameEvent.Trained _ -> Nothing
  GameEvent.PermanentSacrificed {} -> Nothing
  GameEvent.Moved {} -> Nothing
  GameEvent.DamageDealt _ -> Nothing
  GameEvent.DamagePrevented {} -> Nothing
  GameEvent.StepBegan {} -> Nothing
  GameEvent.BecameMonarch _ -> Nothing
  GameEvent.Discarded {} -> Nothing
  GameEvent.Drew {} -> Nothing
  GameEvent.Revealed {} -> Nothing
  GameEvent.AttackerDeclared {} -> Nothing
  GameEvent.BlockerDeclared {} -> Nothing
  GameEvent.BlocksDeclared {} -> Nothing
  GameEvent.AttackerBlocked {} -> Nothing
  GameEvent.AttackerUnblocked _ -> Nothing
  GameEvent.SpellCountered _ -> Nothing
  GameEvent.LoyaltyAbilityActivated _ -> Nothing
  GameEvent.LifeLost {} -> Nothing
  GameEvent.LifeGained {} -> Nothing
  GameEvent.CountersPut {} -> Nothing
  GameEvent.CountersRemoved {} -> Nothing
  GameEvent.ControlChanged {} -> Nothing
  GameEvent.VentureMarkerEntered {} -> Nothing
  GameEvent.BecameTarget {} -> Nothing
  GameEvent.BecameAttached {} -> Nothing
  GameEvent.LeftTheGame _ -> Nothing
  GameEvent.Milled {} -> Nothing
  GameEvent.Scried _ -> Nothing
  GameEvent.Surveiled _ -> Nothing
  GameEvent.Plotted _ -> Nothing
  GameEvent.Explored _ -> Nothing
  GameEvent.Exerted _ -> Nothing
  GameEvent.BecameAttacked _ -> Nothing

-- CR 603.3b's record of one ability triggering: its source (CR 113.7), its
-- controller as it triggered (CR 603.3a) and its trigger condition. Nothing for a
-- SOURCELESS ability, which is the one shape the event cannot describe, there
-- being no id to name (#1026).
triggeredEvent :: PendingTrigger.PendingTrigger -> Maybe GameEvent.GameEvent
triggeredEvent pending = case PendingTrigger.source pending of
  TriggerSource.Sourceless -> Nothing
  TriggerSource.OfObject oid ->
    Just
      ( GameEvent.AbilityTriggered
          AbilityTriggered.MkAbilityTriggered
            { AbilityTriggered.source = oid,
              AbilityTriggered.controller = PendingTrigger.controller pending,
              AbilityTriggered.condition = TriggeredAbility.condition (PendingTrigger.ability pending)
            }
      )

-- Put one triggered ability from the ordered batch on the stack. What it hangs
-- on decides how: an ability BORNE by an object goes through placeBorne, where
-- every step is keyed to that object -- CR 113.7's reserved source binding, and
-- the fillableModes/legalSets pair. A sourceless ability has no such object.
placeOne :: PendingTrigger.PendingTrigger -> Game ()
placeOne pending = case PendingTrigger.source pending of
  -- Monarch.placeInherent names no rule of its own: it is the generic sourceless
  -- placement, which rule 702.179d's ability rides too.
  TriggerSource.Sourceless -> Monarch.placeInherent pending
  TriggerSource.OfObject srcId -> placeBorne srcId pending

-- Put one object-borne triggered ability on the stack as a fresh OfTrigger
-- object, choosing its mode(s) and their targets as it is placed (CR 603.3d).
-- Mirrors Cast.castSpell's cast-time flow: CR 700.2b's mode choice, forced and
-- unprompted exactly when the fillable modes are no more than the selection
-- demands, then CR 603.3d's targets. ModalSpec's "M4h trigger modal" proves a
-- real choice is really asked. CR 603.3c/700.2b: if no mode is chosen, the
-- ability is removed from the stack -- the trigger-only half of the rule, a SPELL
-- that can't choose a mode never being offered for casting (CR 601.2c). The guard
-- precedes the mode prompt.
placeBorne :: ObjectId.ObjectId -> PendingTrigger.PendingTrigger -> Game ()
placeBorne srcId pending = do
  gs <- State.get
  let controller = PendingTrigger.controller pending
      ability = PendingTrigger.ability pending
      (abilId, gs1) = Game.freshObjectId gs
      (ts, gs2) = Game.freshTimestamp gs1
      decider = Decide.deciderFor controller gs
      -- CR 603.2/603.3d: the event's own bindings are known before a target is
      -- chosen, so a slot saying "that player controls" is baked before either
      -- the mode gate or the target prompt reads it. Baked HERE and not stored:
      -- the stack object keeps the printed ability, and CR 608.2b's re-check
      -- bakes again from the same bindings.
      modal = Target.bakeModal (Binding.playerSlots (PendingTrigger.bindings pending)) (TriggeredAbility.modal ability)
      -- The OBJECT half of the same sentence. A slot that reads a bound OBJECT
      -- (Harness the Storm's "the same name as that spell") cannot be baked,
      -- the answer depending on the candidate -- so the bindings are handed to the
      -- matcher instead, and to the mode gate as well as the target prompt, both
      -- of which would otherwise see an empty map and admit nothing.
      bound = fmap (Set.singleton . Recipient.ToObject) (Binding.objectSlots (PendingTrigger.bindings pending))
      legal = Target.fillableModes (Just controller) bound srcId Map.empty modal gs
      selection = Modal.Type.selection modal
      obj =
        Object.MkObject
          { Object.owner = controller,
            Object.enteredUnder = Nothing,
            Object.source = Source.OfTrigger srcId ability,
            Object.zone = Zone.Stack,
            Object.tapped = TapState.Untapped,
            Object.facing = Facing.FaceUp,
            Object.exiledFaceDown = False,
            Object.damage = 0,
            Object.sickness = Sickness.Settled controller,
            Object.bindings = Map.empty,
            Object.counters = Map.empty,
            Object.counterTimestamps = Map.empty,
            Object.attachedTo = Nothing,
            Object.chosenColor = Nothing,
            Object.chosenSubtype = Nothing,
            Object.chosenNames = Set.empty,
            Object.chosenPlayer = Nothing,
            Object.timestamp = ts,
            Object.face = Nothing,
            Object.turnedOverAt = Nothing,
            Object.worldSince = Nothing,
            Object.playableFromExile = Nothing,
            Object.plotted = Nothing,
            Object.foretold = Nothing,
            Object.ringBearerFor = Nothing,
            Object.protector = Nothing,
            Object.ventureRoom = Nothing,
            Object.classLevel = Nothing,
            Object.unlockedHalves = Set.empty,
            Object.designations = Set.empty,
            Object.kicked = False,
            Object.phyrexianLifePaid = 0,
            Object.manaSpent = Mana.MkMana [],
            Object.announcedX = Nothing,
            Object.detainedUntil = Set.empty,
            Object.goadedBy = Set.empty,
            Object.doesNotUntapNext = False,
            Object.exertedBy = Set.empty
          }
  State.put gs2 {GameState.objects = Map.insert abilId obj (GameState.objects gs2), GameState.stack = abilId : GameState.stack gs2}
  if not (Modal.selectionPossible legal selection)
    then -- CR 603.3c: no selection satisfies the instruction.
      State.modify' (Game.cease abilId)
    else do
      -- CR 700.2b: forced when there is nothing to choose, prompted otherwise.
      -- Sorted on the way in, for Cast.castProposed's reason (CR 608.2c, 700.2d).
      chosenModes <- case Modal.forcedSelection legal selection of
        Just forced -> pure forced
        Nothing -> fmap Seq.sort (Game.choose (Prompt.ChooseModes decider controller abilId legal selection))
      -- CR 603.3d: targets for the chosen mode(s) only, chosen as the ability is
      -- placed. A mode with no target slots asks nothing.
      let slots = Modal.modesTargetSlots chosenModes modal
          sets = Target.legalSets (Just controller) bound srcId slots gs
      chosen <- Target.chooseTargets decider controller abilId slots sets
      -- CR 113.7: the ability's SOURCE is bound under the reserved slot as it is
      -- placed, so "this creature" resolves as an ordinary slot read even after
      -- the source has left. CR 603.7c: a delayed ability's CAPTURED environment
      -- (its "it") rides alongside the choices made now, and the two DO collide,
      -- the captured environment carrying the arming spell's OWN reserved slots.
      -- Binding.mergeBinding is left-biased per FIELD, so placement-time bindings
      -- must be the LEFT argument; backwards, the arming spell's mode or X wins.
      -- unionWith, not Map.union, which would drop the whole captured entry.
      State.modify' (\g -> g {GameState.objects = Map.adjust (\o -> o {Object.bindings = Binding.setYou controller (Binding.setTriggerSource srcId (Map.unionWith Binding.mergeBinding (Binding.fromChoices chosen Nothing chosenModes) (PendingTrigger.bindings pending)))}) abilId (GameState.objects g)})
      -- CR 601.2c through CR 603.3d: each chosen object became a target, which is
      -- what CR 702.21a's ward watches. Nothing here can reject the placement
      -- afterwards -- CR 603.3d's removal is the `selectionPossible` branch above.
      Event.becameTarget abilId StackObjectKind.Ability controller chosen

-- CR 101.4 / 603.3b: the players who control a pending trigger, active player
-- first and then the rest in turn order, grouped by controller because the
-- within-controller ORDER is itself a choice. A departed seat is not in the APNAP
-- order at all (CR 101.4 with CR 102.1) and CR 800.4d keeps its triggers off the
-- stack; turnOrder being the permanent roster, the filter below enforces both.
apnapPlayers :: GameState -> [PendingTrigger.PendingTrigger] -> [PlayerId]
apnapPlayers gs pending =
  let rotated = Game.apnapOrder gs
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
      entries = fmap entryOf mine
  if length mine < 2 || interchangeable entries
    then pure mine
    else do
      let decider = Decide.deciderFor pid gs
      answer <- Game.choose (Prompt.OrderTriggers decider pid entries)
      pure (Game.permute mine answer)

-- CR 603.3b: is every permutation of this batch the same game, so that the prompt
-- is a question with one answer? The entries must be EQUAL -- one ability of one
-- source, the discriminator Pawl.Types.TriggerEntry carries -- but equality alone
-- is NOT enough, and that is the trap: CR 603.6a fires one trigger per entrant,
-- so Aether Flash watching two creatures enter contributes two EQUAL entries
-- whose bindings name different creatures, and CR 117.3b hands priority back
-- between the two resolutions. So the ability must also be order-inert: nothing
-- about it may be decided as it is placed, and nothing it does may read what a
-- sibling has already done.
interchangeable :: [TriggerEntry.TriggerEntry] -> Bool
interchangeable entries = case entries of
  [] -> True
  entry : rest -> all (== entry) rest && orderInert (TriggerEntry.ability entry)

-- CR 603.3b: may a batch of EQUAL entries of this ability be put on the stack in
-- the engine's canonical order without asking? Five conditions -- no intervening
-- "if" (CR 603.4), which is re-checked as the ability resolves; one mode and a
-- ChooseExactly 1 selection, so CR 603.3c / 700.2b has nothing to announce; no
-- target slots, CR 603.3d importing CR 601.2c; the mode reads NO slot at all
-- (Resolve.modeSlots); and that answer is complete (Resolve.slotsAreExhaustive),
-- which is where an opcode carrying a nested payload has to be caught. Reading no
-- slot AT ALL rather than "no slot that varies across the batch" is deliberately
-- conservative -- Binding.you and Binding.triggerSource are constant across a
-- batch of equal entries -- so do not widen it without a card that needs the
-- width. Not a case on any effect's IDENTITY.
orderInert :: TriggeredAbility.TriggeredAbility Card.Card -> Bool
orderInert ability =
  let modal = TriggeredAbility.modal ability
   in Maybe.isNothing (TriggeredAbility.intervening ability)
        && Modal.Type.selection modal == ModeSelection.ChooseExactly 1
        && case Foldable.toList (Modal.Type.modes modal) of
          [mode] ->
            Map.null (Mode.targetSlots mode)
              && Map.null (Resolve.modeSlots mode)
              && all Resolve.slotsAreExhaustive (Mode.allEffects mode)
          _ -> False

-- What one pending trigger looks like to the player asked for CR 603.3b's order:
-- its source and WHICH ABILITY it is. Passing the ability THROUGH is not the
-- rules core reading it -- nothing here cases on what the ability does.
entryOf :: PendingTrigger.PendingTrigger -> TriggerEntry.TriggerEntry
entryOf pending =
  TriggerEntry.MkTriggerEntry
    { TriggerEntry.source = PendingTrigger.source pending,
      TriggerEntry.ability = PendingTrigger.ability pending
    }

-- CR 117.5's settle, discarding the report; `performSettle` below is the same act.
settleForPriority :: Game ()
settleForPriority = Monad.void performSettle

-- CR 117.5: each time a player would receive priority, sweep expired "for as
-- long as" effects, perform state-based actions, then put triggered abilities on
-- the stack, repeating until none of the three does anything. Every pass also
-- pays five samples of derived state (sampleWorldSince for CR 704.5k,
-- sampleControl for CR 603.2, checkControlContinuity for CR 302.6,
-- Combat.removeChanged for CR 506.4, Ring.endOnControlChange for CR 701.54a).
--
-- CR 704.3 makes "whenever a player would get priority" the coarsest moment
-- anything could observe CR 611.2b's condition, so settling here is
-- indistinguishable from checking continuously. The sweep runs FIRST, before the
-- SBA check: a "for as long as you control this" effect ending (Master Thief)
-- returns a permanent to another player's control, and CR 704.5j's legend rule
-- must see the post-sweep control. It REPORTS whether it acted or placed a
-- trigger, which is what `cleanupException` (CR 514.3a) asks about.
performSettle :: Game Bool
performSettle = do
  -- CR 614.1c: an as-enters rewrite that ran an effect queued it, Event being
  -- unable to run one. Drained FIRST, so the effects land before the SBA pass --
  -- Monstrous War-Leech's mill decides what CR 704.5f then reads.
  Resolve.runEntryEffects
  swept <- Expiry.sweepConditional
  returned <- Monarch.returnExiledForMonarch
  -- CR 702.145c/d/f/g, checked here for CR 704.3's reason and not because they are
  -- state-based actions -- both rules say they are not. Before the SBA pass, since
  -- turning a permanent over changes its power and toughness.
  dayNight <- Daytime.settle Event.recordTransformed
  -- Also before the SBA pass: a permanent that just became world must be stamped
  -- before CR 704.5k reads the clock. No reason to loop.
  sampleWorldSince
  -- Unlike the four other samples here it MAKES WORK: a GameEvent.ControlChanged
  -- it mints has to be scanned, so it runs before placePendingTriggers and joins
  -- the recursion guard below.
  sampledControl <- sampleControl
  acted <- Sba.performStateBasedActions
  placed <- placePendingTriggers
  -- Last, and for the same reason the conditional sweep runs first: all three read
  -- state this settle can still change -- CONTROL for all three, and CARD TYPES
  -- for the CR 506.4 scan. Outside the recursion guard on purpose: none makes
  -- further work, and all must run even on a pass where nothing fired, which is
  -- the only pass the settle stops on. Ring.endOnControlChange clearing a
  -- designation after the SBA pass is not a missed check either: both readers of
  -- the mark ask CR 701.54e's "under your control" alongside it, so an unlifted
  -- mark answers False regardless. Order among the three does not matter -- CR
  -- 506.4 asks about combat, CR 302.6 about summoning sickness and CR 701.54a
  -- about the designation, and none reads what another writes.
  State.modify' Combat.removeChanged
  checkControlContinuity
  Ring.endOnControlChange
  more <- if swept || returned || dayNight || sampledControl || acted || placed then performSettle else pure False
  pure (acted || placed || more)

-- CR 104.4b: how many events may happen with no player able to decide anything
-- before the game is declared a loop of mandatory actions.
--
-- A HEURISTIC, and deliberately a crude one: detecting such a loop in general is
-- the halting problem, so the question this answers is "has this gone on longer
-- than any real game would?". The MARGIN is what makes it safe.
-- GameState.nextTimestamp advances on the events CR 104.4b names (CR 613.7a,
-- 613.7c, 613.7d), and the slowest real ending -- decking out from a 60-card
-- library -- spends on the order of fifty of them, twenty times under the limit,
-- where a two-card recursion loop arrives in a few hundred.
mandatoryLoopLimit :: Natural
mandatoryLoopLimit = 1000

-- CR 104.4b: a game that has entered a loop of mandatory actions is a draw. Never
-- overwrites a result the game already has -- a won game is not looping, and CR
-- 104.4a's simultaneous loss is a different draw. The subtraction cannot
-- underflow: GameState.lastChoice only ever takes a past GameState.nextTimestamp,
-- and that supply only counts up.
checkMandatoryLoop :: Game ()
checkMandatoryLoop = State.modify' $ \gs ->
  let gap = Timestamp.unwrap (GameState.nextTimestamp gs) - Timestamp.unwrap (GameState.lastChoice gs)
   in if Maybe.isNothing (GameState.result gs) && gap >= mandatoryLoopLimit
        then gs {GameState.result = Just Result.Drawn}
        else gs

-- Ask the priority holder for an action until every still-playing player has
-- passed in succession (CR 117.4). A full round of passes resolves the top of the
-- stack and hands priority back to the active player; only an EMPTY stack ends
-- the step.
priorityLoop :: Game ()
priorityLoop = do
  -- CR 800.4j: the active player, unless they have left the game.
  holder <- State.gets priorityHolder
  State.modify' $ \gs -> gs {GameState.priority = Just holder, GameState.passes = 0}
  -- settleForPriority (CR 117.5) runs where the board can CHANGE -- once at entry,
  -- and after each resolution or board-changing action -- never after a bare
  -- priority pass, which leaves the game state untouched. Observably identical to
  -- settling on every priority grant.
  let loop = do
        -- CR 104.4b, checked HERE as well as at playGame's loop head: a
        -- resolution cycle repeats inside one priority round and never reaches it.
        checkMandatoryLoop
        finished <- State.gets (Maybe.isJust . GameState.result)
        restarted <- State.gets GameState.restartSignal
        case restarted of
          -- CR 727.4: a restart resolved under this loop, so the game it was
          -- running has ended (CR 727.1) and been rebuilt. Stop without granting
          -- priority and without touching GameState.priority, which the rebuild
          -- already set to Nothing.
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
                        -- CR 104.3a: asked before anything else, and keyed to `p` --
                        -- the TRUE player, never `Decide.deciderFor p`.
                        -- Prompt.Concede carries no Decider precisely so this
                        -- cannot be got wrong (CR 723.6).
                        concession <- Game.ask (Prompt.Concede p)
                        case concession of
                          Concession.Concedes -> do
                            -- CR 104.3a: leaves the game IMMEDIATELY. Not a
                            -- state-based action (CR 104.3b), so it does not wait
                            -- for a settle and nothing goes on the stack.
                            Departure.leaveGame Departure.Type.Conceded p
                            -- CR 800.4a: priority passes to the next still-playing
                            -- player; nextStillPlaying looks `p` up in the full
                            -- seating order, so it finds the successor even though
                            -- leaveGame has already run. The pass count restarts
                            -- (CR 117.4); the CR does not settle that directly,
                            -- but not resetting risks resolving a spell a player
                            -- would have responded to.
                            State.modify' (\g -> g {GameState.passes = 0, GameState.priority = Just (nextStillPlaying g p)})
                            loop
                          Concession.Continues -> do
                            let decider = Decide.deciderFor p gs
                                actions = Action.legalActions p gs
                            -- CR 104.4b: a menu that is only Pass offers no
                            -- optional action, so it does not break a loop of
                            -- mandatory actions -- still ASKED, just not RECORDED
                            -- as a choice. Matched two-deep rather than `length
                            -- actions > 1`: Action.legalActions is lazy, so taking
                            -- the length would force the whole spine at every
                            -- grant -- a 70% slowdown on Pawl.ReplaySpec.
                            let offersAChoice = case actions of
                                  _ : _ : _ -> True
                                  _ -> False
                            answered <-
                              if offersAChoice
                                then Game.choose (Prompt.ChooseAction decider p actions)
                                else Game.ask (Prompt.ChooseAction decider p actions)
                            -- FILTERED, NOT TRUSTED: everything Action.legalActions
                            -- computed -- CR 302.6's tap-sickness gate, CR 307.5
                            -- timing, cost payability, CR 305.2's land allowance,
                            -- CR 117.1a's casting timing and every prohibition --
                            -- is enforced here, acting on an unoffered answer
                            -- making all of it advisory. Rejecting to Pass keeps
                            -- the loop total and cannot wedge the game.
                            let chosen = if List.elem answered actions then answered else Action.Type.Pass
                            case chosen of
                              Action.Type.Pass -> do
                                let passes = GameState.passes gs + 1
                                    playing = Natural.length (Game.stillPlaying gs)
                                -- modify' over `State.put gs {...}`: `gs` predates
                                -- the prompt, and putting it back would discard
                                -- GameState.lastChoice (CR 104.4b), making every
                                -- loop look mandatory.
                                if passes >= playing
                                  then case GameState.stack gs of
                                    [] -> State.modify' (\g -> g {GameState.priority = Nothing, GameState.passes = passes})
                                    _ -> do
                                      Stack.resolveTopWith playSubgame
                                      settleForPriority
                                      State.modify' (\g -> g {GameState.passes = 0, GameState.priority = Just (priorityHolder g)})
                                      loop
                                  else do
                                    State.modify' (\g -> g {GameState.passes = passes, GameState.priority = Just (nextStillPlaying gs p)})
                                    loop
                              Action.Type.Play oid mName -> do
                                -- CR 712.12: a modal double-faced card played as a
                                -- land chooses a land face first and enters with
                                -- it up. NOT changeZoneEntering, where CR 712.14b
                                -- turns a put-onto-the-battlefield instruction
                                -- away: playing a land is a special action.
                                Monad.void (Event.changeZoneShowing oid Zone.Battlefield mName)
                                -- CR 305.2a counts the lands played this turn, so
                                -- this TALLIES rather than flagging. CR 305.4:
                                -- the only tally, an effect that PUTS a land onto
                                -- the battlefield not being one.
                                State.modify' (\g -> g {GameState.landsPlayed = Map.insertWith (+) p 1 (GameState.landsPlayed g), GameState.passes = 0, GameState.priority = Just p})
                                settleForPriority
                                loop
                              Action.Type.Cast oid name facing -> do
                                Cast.castSpell p oid name facing
                                State.modify' (\g -> g {GameState.passes = 0, GameState.priority = Just p})
                                settleForPriority
                                loop
                              -- CR 116.2b / 702.37e / 701.40b: a special action, so
                              -- nothing goes on the stack and no player gets a
                              -- window to respond. Priority is retained and the
                              -- pass count restarts, CR 117.4's "passing in
                              -- succession" meaning without actions in between.
                              Action.Type.TurnFaceUp oid procedure -> do
                                FaceDown.turnFaceUp p procedure oid
                                State.modify' (\g -> g {GameState.passes = 0, GameState.priority = Just p})
                                settleForPriority
                                loop
                              -- CR 116.2m / 709.5e: a special action too, the
                              -- TurnFaceUp arm's shape. What CR 709.5h's trigger
                              -- sees is the DESIGNATION, which settleForPriority
                              -- gathers like any other.
                              Action.Type.Unlock oid half -> do
                                Room.unlock p oid half
                                State.modify' (\g -> g {GameState.passes = 0, GameState.priority = Just p})
                                settleForPriority
                                loop
                              -- CR 116.2e: a special action too, the TurnFaceUp
                              -- arm's shape. The discard goes through the CR
                              -- 701.9a funnel rather than a zone move, so an
                              -- ability that triggers on a discard sees it --
                              -- Ordinary and not ToPayCyclingCost, CR 702.29c's
                              -- cause being a cycling ability's cost.
                              Action.Type.DiscardFromHand oid -> do
                                Event.discard DiscardCause.Ordinary p oid
                                State.modify' (\g -> g {GameState.passes = 0, GameState.priority = Just p})
                                settleForPriority
                                loop
                              -- CR 116.2k / 702.170b: a special action too, the
                              -- TurnFaceUp arm's shape.
                              Action.Type.Plot oid -> do
                                Plot.plot p oid
                                State.modify' (\g -> g {GameState.passes = 0, GameState.priority = Just p})
                                settleForPriority
                                loop
                              -- CR 116.2h / 702.143b: a special action too.
                              Action.Type.Foretell oid -> do
                                Foretell.foretell p oid
                                State.modify' (\g -> g {GameState.passes = 0, GameState.priority = Just p})
                                settleForPriority
                                loop
                              -- CR 116.2d: a special action too.
                              Action.Type.Ignore oid -> do
                                Ignore.ignore p oid
                                State.modify' (\g -> g {GameState.passes = 0, GameState.priority = Just p})
                                settleForPriority
                                loop
                              -- CR 605.3a's first window, and CR 605.3b's
                              -- immediacy: the ability does not go on the stack,
                              -- so activating it is over by the time this returns.
                              -- Cost.tapForMana is the same activation CR 605.3a's
                              -- other two windows take, so this arm decides only
                              -- WHEN it may happen; its answer is discarded, a
                              -- mana ability whose cost went unpaid having changed
                              -- nothing (CR 601.2h).
                              Action.Type.ActivateManaAbility oid -> do
                                Monad.void (Cost.tapForMana oid)
                                State.modify' (\g -> g {GameState.passes = 0, GameState.priority = Just p})
                                settleForPriority
                                loop
                              Action.Type.Activate oid ability -> do
                                Activate.activateAbility p oid ability
                                State.modify' (\g -> g {GameState.passes = 0, GameState.priority = Just p})
                                settleForPriority
                                loop
                      else do
                        -- CR 800.4a (last sentence): `p` was written as the holder
                        -- and then departed -- e.g. paying a life cost inside
                        -- settleForPriority (CR 119.4) -- before being asked
                        -- anything. Departure.depart does not touch
                        -- GameState.priority, so the stale `Just p` would
                        -- otherwise survive to the Concede prompt.
                        State.modify' (\g -> g {GameState.priority = Just (nextStillPlaying g p)})
                        loop
  settleForPriority
  loop

-- CR 500.7 / 800.4k / 800.4m: this turn is over, so begin the next one -- a
-- pending EXTRA turn if there is one, and otherwise the turn of the next SEAT in
-- the seating order whose player is still in the game (takeNextTurn's question).
-- CR 800.4k: a turn a departed player would begin does not begin, so such a seat
-- is walked past. CR 800.4m: durations lasting until that player's next turn last
-- until that turn WOULD have begun, so Expiry.dropAtTurnOf fires at EVERY seat
-- the walk passes and at every extra turn popped (for the seat that does begin
-- one, the same call is CR 611.2a). CR 702.26n hangs off that same moment, so
-- Phasing.orphanSchedule fires at both places -- but only where the turn does not
-- begin, a seat that begins one being nobody's orphan.
handoffTurn :: Game ()
handoffTurn = State.modify' takeNextTurn

-- CR 500.7 / 103.1: the seat the ordinary turn order resumes from, read through
-- one function so the two callers cannot drift -- the active player, unless an
-- extra turn is under way, when it is the seat that turn was inserted after.
turnAnchorOf :: GameState -> PlayerId
turnAnchorOf gs = Maybe.fromMaybe (GameState.activePlayer gs) (GameState.turnAnchor gs)

-- CR 500.7: extra turns are added directly after the specified turn, and every
-- extra-turn effect in `data/cards/` specifies the turn it resolves in -- so
-- popping the entry here, before the seating order is consulted, is the whole of
-- "directly after". The rule's last sentence makes the store a stack, so this
-- takes its HEAD. The anchor does NOT move (see GameState.turnAnchor): CR 500.7
-- adds a turn and removes none, so the turn that would have followed still
-- follows it -- observable only when the taker is not the active player, where it
-- stops the extra turn eating that player's ordinary one. CR 800.4k applies as to
-- an ordinary turn: it does not begin, the entry is still SPENT, and both "would
-- have begun" rules still fire.
--
-- Not implemented: CR 805.8's shared team turns and CR 807.4i/j's Grand Melee
-- turn markers, each of which rewrites this rule for a variant pawl has no
-- format to read one from (#175).
takeNextTurn :: GameState -> GameState
takeNextTurn gs = case GameState.extraTurns gs of
  [] -> walkToNextTurn (length (GameState.turnOrder gs)) (turnAnchorOf gs) gs
  entry : rest ->
    let pid = ExtraTurn.taker entry
        anchor = turnAnchorOf gs
        swept = Expiry.dropAtTurnOf pid gs {GameState.extraTurns = rest}
        anchored = swept {GameState.turnAnchor = Just anchor}
     in if List.elem pid (Game.stillPlaying swept)
          then -- CR 500.11: this turn's OWN skips (Savor the Moment) come into
          -- being as it begins, and only on this branch -- the CR 800.4k entry
          -- the `else` spends belongs to a turn that never begins. Nothing of
          -- this turn has started yet, so CR 614.10 is not yet in the way.
            Replacement.installTurnSkips entry (beginTurnOf pid anchored)
          else -- CR 702.26n, as on walkToNextTurn's skip branch below: this
          -- entry is a turn `pid` would have begun, so a row keyed to them is
          -- rescheduled here too. No board reaches it, an extra turn for a
          -- departed player needing a phased-out permanent besides, so it is
          -- the rule written out at its second site rather than a proved one.
            takeNextTurn (Phasing.orphanSchedule pid swept)

-- One seat at a time, bounded by the number of seats, so it terminates even when
-- every seat has departed. The fallback returns the state without beginning a
-- turn, keeping the sweeps already applied. Written as an explicit bounded
-- recursion rather than `cycle`/`head` so it is total; unreachable while the game
-- is running, a game with no survivors already having a Result (CR 104.2a).
walkToNextTurn :: Int -> PlayerId -> GameState -> GameState
walkToNextTurn seatsLeft seat gs =
  if seatsLeft <= 0
    then gs
    else
      let next = nextInOrder (GameState.turnOrder gs) seat
          swept = Expiry.dropAtTurnOf next gs
          -- CR 702.26n: the phasing analogue of the sweep above, on the same line
          -- for the same reason -- this is the moment the turn WOULD have begun.
          -- A row keyed to a seat this walk passes is rescheduled and phases in at
          -- the untap step of whichever seat the walk lands on. This branch only:
          -- a seat that does begin a turn is nobody's orphan.
          orphaned = Phasing.orphanSchedule next swept
       in if List.elem next (Game.stillPlaying swept)
            then -- CR 500.7 / 103.1: this turn IS the ordinary rotation, so there
            -- is nothing left to remember.
              beginTurnOf next swept {GameState.turnAnchor = Nothing}
            else walkToNextTurn (seatsLeft - 1) next orphaned

-- The turn actually begins for `pid`, split out so the CR 800.4k seat walk has
-- exactly one place to land.
beginTurnOf :: PlayerId -> GameState -> GameState
beginTurnOf pid gs =
  let -- CR 800.4b: a player who would be controlled by a departed player isn't, so
      -- a pending Decider naming one is not promoted. CR 800.4a's second clause
      -- clears the entry at the departure itself; this guard answers otherwise.
      promoted = case Map.lookup pid (GameState.pendingControl gs) of
        Nothing -> Nothing
        Just decider -> case decider of
          Decider.MkDecider d ->
            if List.elem d (Game.stillPlaying gs)
              then Just decider
              else Nothing
   in -- CR 603.7a: the one moment a delayed ability armed for "your next turn"
      -- can learn which turn that is, and an entry whose turn has passed be
      -- retired. Applied to the UPDATED state, both answers being read off this
      -- turn's number and active player.
      Event.settleOnsets
        gs
          { GameState.activePlayer = pid,
            GameState.turnNumber = GameState.turnNumber gs + 1,
            -- CR 608.2i is why a log exists at all. It does not say how far back;
            -- the ONE-turn scope is this engine's choice, every history-reading
            -- card in `data/cards/` asking "this turn". Cleared here and never at
            -- cleanup -- cleanup is still part of this turn, and CR 514.1's
            -- discard is itself an event of it.
            GameState.events = Seq.empty,
            -- CR 702.179d's "only once each turn", cleared beside the log for the
            -- same reason: a new turn starts with nobody's ability spent.
            GameState.speedIncreasedThisTurn = Set.empty,
            -- CR 121.1's per-turn draw tally, cleared for EVERY player: a player
            -- draws on turns that are not theirs, so "each turn" is the whole map.
            GameState.drawsThisTurn = Map.empty,
            -- CR 502.2 / 731.2: the count the NEXT turn's untap step asks about
            -- "the previous turn's active player", taken here because the log it
            -- is folded from is cleared by this same record update. `gs` still
            -- holds the OUTGOING active player.
            GameState.spellsCastLastTurn = PlayerEffect.castsThisTurn (GameState.activePlayer gs) gs,
            GameState.scannedThrough = 0,
            -- Cleared with the log it describes: the settle Engine.advance runs
            -- immediately before this leaves nothing unscanned.
            GameState.battlefieldWhenTriggered = Map.empty,
            GameState.damageScannedThrough = 0,
            -- GameState.lastKnown is deliberately NOT cleared alongside them: CR
            -- 608.2h's last known information is the substitute identity of an
            -- object that no longer exists rather than history, and a delayed
            -- ability's source (CR 603.7d) outlives the turn it was armed in.
            -- Setup clears it where a NEW game begins.
            GameState.phase = Turn.firstPhase,
            GameState.remaining = Turn.laterPhases,
            -- CR 723.1/723.1b: the new active player's pending control becomes
            -- this turn's active control, and overwriting it every turn is what
            -- ends a prior control at the next turn's start -- unless CR 800.4b
            -- stops the promotion (`promoted`, above).
            GameState.activeControl = promoted,
            GameState.pendingControl = Map.delete pid (GameState.pendingControl gs)
          }

-- Consume the schedule: the next step becomes current. An empty schedule means
-- the turn is over, so hand off. The turn is data, and this is the only thing
-- that reads its order.
advance :: Game ()
advance = do
  gs <- State.get
  case Seq.viewl (GameState.remaining gs) of
    p Seq.:< rest -> State.put gs {GameState.phase = p, GameState.remaining = rest}
    -- CR 117.5: the turn is over. Settle once more so every event the terminal
    -- step's turn-based actions emitted is scanned BEFORE handoffTurn clears the
    -- log -- an unscanned event discarded at handoff is a lost trigger -- and so
    -- CR 704.3 catches a state-based action those same actions raised. Usually
    -- redundant, the cleanup step's own CR 514.3a check having settled already;
    -- kept because the schedule can empty at a step that is NOT the cleanup step,
    -- CR 500.11's skip over an ending phase dropping it with the rest.
    Seq.EmptyL -> do
      settleForPriority
      handoffTurn

-- One step: turn-based actions, then priority (if the step grants it), then
-- state-based actions, then move on. Bails out as soon as the game has a result.
runStep :: Game ()
runStep = do
  -- CR 727.4: the effect that restarts the game finishes resolving just before the
  -- first turn's untap step, so if the previous step unwound on a restart, this is
  -- that untap step. Lower the signal first.
  State.modify' (\gs -> gs {GameState.restartSignal = RestartSignal.Playing})
  phase <- State.gets GameState.phase
  active <- State.gets GameState.activePlayer
  -- CR 614.1b makes "skip" a replacement effect, and CR 500.11 makes skipping a
  -- step proceeding past it as though it didn't exist. So the question is asked
  -- HERE, of the replacement system, and a `False` answer means the whole of
  -- `runStepThatBegan` never runs -- CR 500.6's triggers never triggering is what
  -- distinguishes that from a step that happened and did nothing (Eon Hub). CR
  -- 614.10 pins the question to this line: `advance` has written the step into
  -- GameState.phase but records no event and grants no priority, so the step is
  -- scheduled, not started.
  --
  -- TWO questions, in CR 500.1's own order: `Turn.phaseBeginningAt` answers Just
  -- only at a stepped phase's FIRST step, and a main phase raises only the step
  -- question (CR 505.2). Both are asked even when the phase says yes -- Stasis
  -- skipping an untap step must still take it in an unskipped phase.
  phaseBegins <- case Turn.phaseBeginningAt phase of
    Nothing -> pure True
    Just selector -> Event.beginsPhase selector active
  if not phaseBegins
    then skipWholePhase phase
    else do
      begins <- Event.beginsPhase (PhaseSelector.Step phase) active
      if not begins
        then advance
        else runStepThatBegan phase

-- CR 500.11: proceed past a SKIPPED PHASE as though it didn't exist -- so the
-- rest of its steps leave the schedule and `advance` picks up whatever CR 500.1's
-- fixed order puts after it. Positional, via Turn.dropRestOfPhase, not a filter:
-- CR 500.8 lets a second combat phase be added later in the same turn, and
-- skipping this one says nothing about that one. Nothing about the skipped phase
-- is announced -- CR 614.6 makes a replaced event one that never happens, and CR
-- 500.6's triggers hang off the CR 603.2b step records this path never reaches.
--
-- GameState.combat is left ALONE and does not go stale for it: every writer that
-- ADDS to that record runs inside a combat step, so a phase whose steps never
-- begin writes nothing. (Combat.clearAttackedThisStep does run outside one, at
-- every step's end, but a clearer cannot strand anything.) Not implemented:
-- skipping the end of combat step on its own, which strands the phase-scoped
-- half of the record and which no card names (#2010).
skipWholePhase :: Phase.Phase -> Game ()
skipWholePhase phase = do
  State.modify' (\gs -> gs {GameState.remaining = Turn.dropRestOfPhase phase (GameState.remaining gs)})
  advance

-- The body of a step that was not skipped, split out only so `runStep`'s CR
-- 614.1b check reads as a guard rather than as a nesting level.
runStepThatBegan :: Phase.Phase -> Game ()
runStepThatBegan phase = do
  -- CR 603.2b: the step began. Recorded BEFORE the step's turn-based actions, so
  -- the first priority boundary of this step scans it. No player receives priority
  -- during the untap step (CR 502.4), so an ability that triggers then is held
  -- until upkeep, where CR 503.1a puts it on the stack first.
  State.modify' (\gs -> Event.recordEvent (GameEvent.StepBegan (StepBegan.MkStepBegan phase (GameState.activePlayer gs))) gs)
  runTurnBasedActions phase
  -- Asked BEFORE the CR 704.3 check below. For every step but one the order is
  -- free -- this line is pure there -- and for the cleanup step it is forced: CR
  -- 704.3's last sentence keys that step's outcome to the step's FIRST check, so
  -- nothing may act ahead of CR 514.3a's. Running the ordinary check first buries
  -- the creature CR 514.2 just dropped to zero toughness (CR 704.5f), and CR
  -- 514.3a then finds a settled board and grants nothing.
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
      -- Unwind. `advance` in particular MUST NOT run -- it would pop the FRESH
      -- schedule and skip turn 1's untap step entirely.
      RestartSignal.Restarted -> pure ()
      RestartSignal.Playing -> do
        -- CR 500.5, whole and in its own order: as a step or phase ends, effects
        -- lasting until the end of it expire, then any unspent mana empties. TWO
        -- windows end here, not one, and CR 500.5a is why: an "until end of
        -- combat" effect expires at the end of the combat PHASE, not at the
        -- beginning of the end of combat step (CR 511.2), so such an animation is
        -- live for the whole of that step. The step is swept before the phase, CR
        -- 500.1 nesting the one inside the other; nothing observes the order.
        State.modify' (Expiry.dropAtEndOf (PhaseSelector.Step phase))
        Foldable.traverse_ (State.modify' . Expiry.dropAtEndOf) (Turn.phaseEndingAt phase)
        -- CR 703.4q: emptying the pool is a turn-based action that does not use
        -- the stack, and CR 500.5's "Then" puts it AFTER the expiries above. This
        -- line says only WHEN; WHICH mana empties is Mana.emptyManaPools', off
        -- both retention carriers -- per player (Upwelling) and per unit (Shizuko,
        -- Caller of Autumn). The ordering against a retention that ENDS at this
        -- same boundary is observable, and both carriers are read live: one swept
        -- afterwards would keep the pool across a boundary it no longer covers.
        State.modify' Mana.emptyManaPools
        -- CR 511.3: as soon as the end of combat step ends, creatures, battles and
        -- planeswalkers are removed from combat -- so it belongs at the step's END
        -- and not in runTurnBasedActions. Not a turn-based action at all, CR 511.1
        -- giving this step none. Start versus end is observable: creatures stay
        -- attacking for the whole step, including the priority round where an
        -- instant may still read them (Kill Shot).
        Monad.when (phase == Phase.Combat CombatStep.EndOfCombat) (State.modify' Combat.clearCombat)
        -- CR 508.6 read on CR 500.1's span: the step-scoped half of the attack
        -- record ends with the step, so this runs at EVERY step's end and not
        -- just at combat's. See Pawl.Engine.Combat.clearAttackedThisStep for why
        -- the wider reach is the point rather than laziness.
        State.modify' Combat.clearAttackedThisStep
        -- CR 508.8: drop the two combat steps that have nothing to do if nobody
        -- attacked. Asked as the declare attackers step ENDS, not when its
        -- turn-based action finishes, because the rule's second clause -- put onto
        -- the battlefield attacking -- can only happen in the priority round this
        -- line sits after (Hanweir Garrison). NOT guarded by hasActive: a turn
        -- with no active player declares no attackers, CR 508.8's own condition.
        Monad.when (phase == Phase.Combat CombatStep.DeclareAttackers) (State.modify' Combat.skipEmptyCombat)
        checkSba
        stillFinished <- State.gets (Maybe.isJust . GameState.result)
        Monad.unless stillFinished advance

-- Whether THIS step grants a priority round. Every step but one answers from the
-- phase alone (Turn.grantsPriority); the cleanup step's answer is CR 514.3
-- qualified by CR 514.3a's exception, a question about the board.
grantsPriorityNow :: Phase.Phase -> Game Bool
grantsPriorityNow phase = case phase of
  Phase.Ending EndingStep.Cleanup -> cleanupException
  _ -> pure (Turn.grantsPriority phase)

-- CR 514.3a: the cleanup step checks whether any state-based actions would be
-- performed or any triggered abilities are waiting; if so, those are performed
-- and placed, the active player gets priority, and once the stack is empty and
-- all players pass another cleanup step begins. Checking and performing are ONE
-- act here: `performSettle` is precisely CR 117.5's loop and reports whether it
-- did either, and the two orders are indistinguishable.
--
-- WHY THIS TERMINATES, given that a cleanup step can schedule another one: the
-- chain advances only on new work and the settle is idempotent. NOT bounded in
-- general, and Magic does not bound it either -- an ability triggering at the
-- beginning of each cleanup step loops forever, and CR 104.4b's draw, which
-- `checkMandatoryLoop` applies heuristically, is the rules' answer.
cleanupException :: Game Bool
cleanupException = do
  fired <- performSettle
  Monad.when fired $
    -- Scheduled HERE, before the priority round, exactly as CR 510.4's second
    -- combat damage step is -- indistinguishable either way, CR 500.8's phase
    -- splice landing behind this step regardless.
    State.modify' (\gs -> gs {GameState.remaining = Turn.spliceExtraCleanup (GameState.remaining gs)})
  pure fired

-- Ordinarily terminates because libraries are finite, each turn draws at most one
-- card, and drawing from an empty library is a loss (CR 704.5b). That argument
-- rests on the DRAW step being reached, and a CR 614.1b skip of it suspends it,
-- exactly as a real Stasis lock suspends a real game.
--
-- `checkMandatoryLoop` at this loop's head is the backstop for every repetition
-- the argument does not cover, and it is a HEURISTIC rather than a bound: a game
-- two players CAN still act in is left alone, so a Stasis lock is not ended by it
-- either. `Pawl.GameSpec`'s "a mandatory loop (CR 104.4b)" group holds both
-- halves; CR 514.3a's extra CLEANUP steps argue for themselves.
playGame :: Game Result
playGame =
  let loop = do
        checkMandatoryLoop
        outcome <- State.gets GameState.result
        case outcome of
          Just r -> pure r
          Nothing -> do
            runStep
            loop
   in loop

-- CR 729: play a subgame as a FUNCTION CALL. Built from the parent's library
-- cards and commanders (CR 729.2 / 729.2c), then run LIFTED into the parent's
-- StateT, so its prompts flow through the SAME Program interpreter and Replay
-- fold and a transcript stays replayable across one. The parent GameState sits
-- untouched in the outer frame (CR 729.1a); at the end cards funnel back to
-- their owner's library (CR 729.5) and commanders to the command zone (CR
-- 729.5c). Nesting terminates: each level's library comes from the parent's at
-- cast time, so depth is bounded by |library| / 7.
--
-- Not implemented: cards brought into a subgame from the main game, and the
-- main-game triggers their removal queues (#152). Of CR 729.2a-c and CR
-- 729.5a-c's command-zone residents, commanders are the kind Setup carries both
-- ways, and a dungeon rides Player.dungeon rather than the command zone; planes
-- and phenomena (#934), schemes (#935), vanguards (#936) and conspiracies (#937)
-- do not exist.
playSubgame :: Game Result
playSubgame = do
  parent <- State.get
  -- CR 729.2: randomly determine which player goes first. The engine asks; the
  -- interpreter rolls. Only the players still in the main game are in the subgame
  -- (CR 729.4). Not asked when the answer is forced.
  starter <- case NonEmpty.nonEmpty (Game.stillPlayingInOrder parent) of
    Nothing -> pure (GameState.activePlayer parent)
    Just order -> case order of
      only NonEmpty.:| [] -> pure only
      _ -> do
        answer <- Game.ask (Prompt.RandomFirstPlayer order)
        -- Filtered, not trusted: a subgame cannot start with an unseated player.
        pure (if List.elem answer (NonEmpty.toList order) then answer else NonEmpty.head order)
  let sub0 = Setup.subgameStateFrom starter parent
  -- CR 729.1a: every question the subgame raises passes outward through this
  -- frame, the one place that knows both games, so this is where the parent is
  -- pushed onto the tag -- once per level a nested question climbs (CR 729.6).
  -- The CR 729.2 roll above is deliberately NOT wrapped: it is asked before the
  -- subgame's state exists, in the main game.
  (result, finalSub) <-
    Trans.lift
      ( Program.mapProgram
          (Asked.under parent)
          (State.runStateT (Setup.startGameFromCards Resolve.performHandAction Set.empty >> playGame) sub0)
      )
  State.modify' (Setup.funnelBack finalSub)
  -- CR 729.5: each player who was IN the subgame takes the traditional cards they
  -- own back to their main-game library and shuffles.
  seated <- State.gets Game.stillPlayingInOrder
  Monad.forM_ seated Mulligan.shuffleLibrary
  pure result

playFrom :: NonEmpty.NonEmpty (PlayerId, Deck.Deck) -> Game Result
playFrom matchup = do
  Setup.newGame Resolve.performHandAction matchup
  playGame
