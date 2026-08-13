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
import qualified Pawl.Types.Action as Action.Type
import qualified Pawl.Types.Asked as Asked
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.Concession as Concession
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
import qualified Pawl.Types.RestartSignal as RestartSignal
import Pawl.Types.Result (Result)
import qualified Pawl.Types.Result as Result
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.Supertype as Supertype
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Timestamp as Timestamp
import qualified Pawl.Types.TriggerEntry as TriggerEntry
import qualified Pawl.Types.TriggerSource as TriggerSource
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.Zone as Zone

-- The interpreter seam: every decision the engine suspends on is answered here.
--
-- The PRIMITIVE form, and the only one that sees which game asked (#153): an
-- Asked carries the asking game's state and the games it is nested inside (CR
-- 729.1a). An interface answering here can show a subgame's question as a
-- subgame's, which is what CR 723.4's visibility split needs.
runGameAsked :: (Monad m) => (forall r. Asked.Asked r -> m r) -> GameState -> Game a -> m (a, GameState)
runGameAsked answer gs game = Program.foldProgramM answer (State.runStateT game gs)

runGameAskedPure :: (forall r. Asked.Asked r -> r) -> GameState -> Game a -> (a, GameState)
runGameAskedPure answer gs game = Program.foldProgram answer (State.runStateT game gs)

-- The seam for an answerer that does not care which game it is in: the question
-- alone, with the tag dropped. Every answerer in the test suite and the
-- benchmark is one of these.
runGame :: (Monad m) => (forall r. Prompt r -> m r) -> GameState -> Game a -> m (a, GameState)
runGame answer = runGameAsked (answer . Asked.prompt)

runGamePure :: (forall r. Prompt r -> r) -> GameState -> Game a -> (a, GameState)
runGamePure answer = runGameAskedPure (answer . Asked.prompt)

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

-- CR 800.4a (last sentence): priority passes to the next player in turn order
-- who's still in the game.
--
-- The seat is looked up in the FULL seating order (GameState.turnOrder is never
-- shortened -- see Pawl.Types.GameState), so a player who has ALREADY departed
-- still has a position from which to find their successor. That is the case
-- priorityLoop's concede arm calls this in, and it is unchanged for the ordinary
-- pass case, where `pid` is still playing.
--
-- Deliberately NOT shared with Monarch.reassignOnDeparture (CR 725.4), which
-- walks the same seating order: this anchors on the DEPARTING seat and includes
-- it in the wrap, so it can return that seat; it is total in PlayerId; and it
-- reads Game.stillPlaying directly. That one anchors on the ACTIVE seat and
-- excludes it, must return a Maybe because CR 725.4 lets the game continue with
-- no monarch, and takes `playing` injected. Changing either walk without
-- checking the other risks reintroducing the duplication with a mismatch.
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

-- CR 800.4j: a turn whose player has left continues to its completion without an
-- active player, and where the active player would receive priority the next
-- player in turn order does instead.
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

-- The step's own CR 704.3 check. Samples CR 704.5k's clock first, for the reason
-- performSettle does: this site follows the turn-based actions, and CR 514.2's
-- sweep ending an "until end of turn" grant is one that can change who is world.
checkSba :: Game ()
checkSba = sampleWorldSince >> Sba.checkStateBasedActions

-- CR 502.3's untap, and its own "effects can keep one or more of a player's
-- permanents from untapping": an effect saying a permanent doesn't untap during
-- its controller's untap step takes it out of this fold and leaves it exactly as
-- it was (Tsabo's Web, Pawl.Engine.UntapRestriction). CR 101.2 is what makes the
-- "can't" beat the turn-based action.
--
-- The prohibition is asked of the ids this step would otherwise untap, so a
-- permanent this player does not control is never a candidate -- CR 502.3 untaps
-- the active player's permanents, and the printed sentence says "its controller's
-- untap step" of that same player.
untapAll :: PlayerId -> Game ()
untapAll pid = do
  gs <- State.get
  let untap obj = obj {Object.tapped = TapState.Untapped}
      ids = Projection.controls pid gs
      prohibited = UntapRestriction.doesNotUntap ids gs
  State.put gs {GameState.objects = foldr (Map.adjust untap) (GameState.objects gs) (filter (\oid -> not (Set.member oid prohibited)) ids)}

-- CR 302.6: permanents the active player has controlled since their turn began
-- are no longer summoning sick. The untap step is where that becomes true.
--
-- The record names `pid`, so it answers CR 302.6 only for `pid` -- the rule asks
-- about a creature relative to its controller, and a settle made for one player
-- says nothing about another.
--
-- It iterates `Projection.controls`, so it settles for whoever currently
-- controls the permanent, not its owner. That reading is control-duration-
-- agnostic: an until-end-of-turn effect (Act of Treason) always wears off
-- (CR 514.2) before the thief's own next untap step, but an Aura's static
-- ability (Control Magic) grants control INDEFINITELY, so the creature can
-- still be the thief's at their own untap step and settles for them (#62).
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
-- static ability is re-read live by the projection, so no resolution announces a
-- control change for a re-sickening to hang on. `settleForPriority` is where it
-- samples, which is every point the board can CHANGE; a bare priority pass leaves
-- the state untouched, so the previous sample already saw this exact board.
--
-- `sampleControl` mints a GameEvent.ControlChanged off the same difference for CR
-- 603.2's benefit, and this deliberately does NOT read it. They are diffs of the
-- same two boards, so consuming the log would add an ordering dependency between two
-- samplers and buy nothing -- and they run at different moments on purpose: the event
-- has to be minted before the trigger scan, while this has to land after it, on the
-- pass where the board finally stops moving.
--
-- It only ever CLEARS. That asymmetry is what makes the sampling sound: a
-- discrepancy proves control changed, so clearing is always right, while
-- granting from a sample would invent continuity across the gap between two
-- samples. It is also what catches control leaving and returning inside one turn.
--
-- Battlefield-scoped: nothing off the battlefield has a controller to compare
-- against, and CR 302.6 is a restriction on permanents.
--
-- Hoists the grant list and calls `controllerOfGiven`, exactly as
-- `Projection.controls` does -- linear in the battlefield, and deriving the
-- check the same way `settleAll` writes it makes the two structurally unable to
-- disagree about who controls what.
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
-- world-ness is DERIVED (a layer-4 projection, CR 613.1d) while the clock must be
-- STORED -- so this samples it into Object.worldSince, the way
-- checkControlContinuity above samples control. A supertype granted by a static
-- ability is re-read live by the projection and has no event to hang a stamp on.
--
-- Unlike checkControlContinuity this both WRITES and CLEARS, and the asymmetry
-- that keeps it sound is different: the sample is taken at every point the board
-- can change and BEFORE the rule reads it, so a stamp is minted at the first
-- settle after the permanent became world and dropped at the first settle after it
-- stopped. A stamp-only version would latch, leaving a permanent that lost the
-- supertype and regained it claiming the older clock.
--
-- ONE fresh timestamp for the whole pass, minted only if something is actually
-- stamped: CR 704.5k's tie clause is a sentence of the rule, so permanents that
-- became world simultaneously must compare equal.
--
-- No special case for a PRINTED world supertype. Such a permanent is world from
-- the moment it exists, so the first settle after it enters stamps it like any
-- other -- one writer, one clock. Seeding worldSince from Object.timestamp at
-- entry instead would make a printed world permanent's clock an ENTRY instant and
-- a granted one's a SETTLE instant, and the two are not comparable.
--
-- Battlefield-scoped: CR 704.5k is about permanents, and Object.newIncarnation
-- clears the stamp on the way out (CR 400.7).
--
-- One Projection.projectAll, the same whole-board gather Sba.performStateBasedActions
-- already pays on every pass -- not a fresh projection per object.
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
-- GameState.controlSample and records a GameEvent.ControlChanged for each
-- difference, then stores the new snapshot. Reports whether it recorded anything.
--
-- MINTING AN EVENT is what makes this different from the three samplers beside it
-- (checkControlContinuity, Combat.removeChanged, Ring.endOnControlChange), which
-- each clear one piece of stored state and tell nobody. A trigger condition cannot
-- be written against stored state that quietly changes, so CR 603.2 needs an entry
-- in the log; `Engine.reactions` already mints GameEvent.AbilityTriggered for the
-- same reason, so that the ordinary trigger machinery applies unchanged.
--
-- FIRST SIGHTING IS NOT A CHANGE. An id absent from the previous snapshot is
-- recorded and nothing else, so a permanent entering -- under its owner or, by CR
-- 110.2's Object.enteredUnder, under somebody else -- mints no event. That is the
-- rule and not a conservatism: CR 603.2's event is control CHANGING, and a
-- permanent that has just entered has not had two controllers.
--
-- The snapshot is REBUILT from the battlefield, which prunes it: a permanent that
-- left has no entry to go stale, and CR 400.7 gives what comes back a new id this
-- has never seen, so a creature borrowed, killed and reanimated does not read as
-- coming home.
--
-- WHERE IT RUNS: inside `performSettle`'s recursion guard and BEFORE
-- placePendingTriggers, because a minted event has to be scanned by a later pass
-- of the same settle -- the three samplers performSettle runs LAST sit outside the
-- guard precisely
-- because they make no further work, and this one does. It terminates because it
-- only mints on a difference and writes the difference away, so a board that stops
-- changing mints nothing.
--
-- Recording through Event.recordEvent means the CR 603.3a control sample
-- (GameState.controlWhenTriggered) can be taken here, on a settle where nothing else
-- was unscanned. That is the right reading and not an accident: an ability triggering
-- off a control change triggered when control had already moved, so the controller
-- the scan reads is the post-change one.
--
-- One Projection.controlGrants hoisted over the walk, as
-- checkControlContinuity and Projection.controls both do.
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
        [ GameEvent.ControlChanged oid before after
        | (oid, after) <- Map.toList sampled,
          Just before <- [Map.lookup oid (GameState.controlSample gs)],
          before /= after
        ]
  State.put gs {GameState.controlSample = sampled}
  -- CR 603.2's simultaneity: two permanents whose control reverted in the same CR
  -- 514.2 sweep changed hands at the same moment, so the batch is one event group
  -- and CR 603.3b orders the abilities they trigger.
  Monad.unless (null changes) . Event.simultaneously $
    State.modify' (\g -> List.foldl' (flip Event.recordEvent) g changes)
  pure (not (null changes))

-- CR 514.1's cleanup discard -- NOT CR 514.2, the damage-removal and
-- end-of-turn sweep beside it. Non-identical cards share a hand, so trimming
-- front-of-hand would be the engine choosing what to pitch: policy in the rules
-- core, not canonicalization. The choice is the player's.
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
        chosen <- Game.choose (Prompt.ChooseDiscard decider pid held (Int.toNaturalSaturating excess))
        let inHand oid = List.elem oid held
            toDiscard = take excess (filter inHand chosen)
        -- CR 701.9a, through the shared discard funnel: a cleanup discard is a
        -- discard, so it records one for a rule 701.9a trigger to read. Such a
        -- trigger is one waiting to be put on the stack during the cleanup step,
        -- which is CR 514.3a's condition -- see `cleanupException`.
        Monad.mapM_ (Event.discard DiscardCause.Ordinary pid) toDiscard

-- CR 103.8a: in a two-player game the player who plays first skips the draw step
-- of their first turn. CR 103.8c and CR 800.7: in other multiplayer games,
-- nobody does.
--
-- CR 800.1 makes a multiplayer game one that BEGINS with more than two players.
-- GameState.turnOrder is the permanent seating roster, so counting seats answers
-- that directly: a three-player game that has dropped to two survivors still
-- does not skip, and a rebuilt game (CR 727.1, CR 729.2) answers for itself.
-- Not more than two seats is CR 103.8a's arm, which is also where a degenerate
-- one-seat subgame lands.
--
-- CR 103.8b grants the same skip to a TEAM in Two-Headed Giant, which would be
-- another arm of this function; pawl has no teams or variants to read from
-- (#175).
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
  -- player (CR 703.4h/CR 507.1), declare attackers (CR 703.4i), the cleanup
  -- discard (CR 703.4n) -- have no subject. Declare blockers (CR 703.4j) belongs
  -- to the defending player, and CR 703.4p's sweep is the GAME's action; neither
  -- is guarded.
  --
  -- CR 800.4j is a PRIORITY rule and licenses skipping none of them. CR 800.4h
  -- is what reaches the CHOICES on that list -- a choice a rule requires of a
  -- departed player is made by the next player in turn order -- and skipping
  -- them instead diverges from it (#181). The draw is not a choice at all; untap,
  -- the cleanup discard and declare attackers range over permanents, a hand and
  -- creatures CR 800.4a already took, so those three are vacuous; only the
  -- defending-player choice is real, and it is unobservable rather than vacuous
  -- for the reason on Pawl.Types.Combat's defender field.
  hasActive <- State.gets (\gs -> List.elem active (Game.stillPlaying gs))
  case phase of
    Phase.Beginning BeginningStep.Untap -> do
      -- CR 502.1 / 703.4a: phasing, immediately after the step begins and so
      -- first of all. Inside the CR 800.4j guard, because rule 502.1 ranges over
      -- what THE ACTIVE PLAYER controls and controlled -- vacuous under the guard
      -- rather than merely skipped for the permanents CR 800.4a's first clause
      -- reaches: it has already taken everything a departed player OWNED, and CR
      -- 702.26k took their phased-out ones with it. It is NOT vacuous where owner
      -- and controller come apart -- a permanent somebody else owns that phased
      -- out under a player who has since left keeps a GameState.phasedOut row
      -- keyed to a player with no further untap step, which CR 800.4c and CR
      -- 702.26n between them say what to do about (#931).
      Monad.when hasActive (State.modify' (Phasing.phasingEvent active))
      -- CR 502.2 / 703.4b: the day/night check, second in the step and BEFORE the
      -- untap itself (CR 502.3 / 703.4c). Outside the CR 800.4j guard the actions
      -- below take, because rule 703.4b makes it the GAME's action rather than the
      -- active player's -- CR 703.4p's sweep is the other one of those.
      --
      -- Rule 703.4b's "immediately after the phasing action has been completed"
      -- is the line above, so the two are in the rule's order.
      _ <- Daytime.untapCheck
      Monad.when hasActive $ do
        untapAll active
        settleAll active
        State.modify' $ \gs ->
          -- CR 305.2: the allowance is per TURN, so the count that turn compares
          -- against starts again at zero. DELETED rather than set to 0 -- an
          -- absent row and a zero row are the same answer to CR 305.2a, and
          -- deleting keeps exactly one representation of "has played none".
          gs {GameState.landsPlayed = Map.delete active (GameState.landsPlayed gs)}
    Phase.Beginning BeginningStep.DrawStep -> Monad.when hasActive $ do
      skip <- State.gets skipsDraw
      Monad.unless skip (Event.drawCard active)
    -- CR 703.4h: choose the defending player. The active player's action
    -- (CR 507.1), so it takes the same guard as the others.
    --
    -- hasActive and chooseDefender's own guard are the same value BY
    -- EQUIVALENCE, not merely observed to agree: hasActive is bound at the top
    -- of this function, only a pure `case` on `phase` runs between that bind and
    -- this arm, and Combat.chooseDefender opens with its own State.get and
    -- computes the identical test over that same state.
    --
    -- Removal is therefore safe on this path, and still declined: this arm is
    -- where the enumeration of CR 800.4j's actions above is read off, and an
    -- arm silently missing the wrapper while that list still names it would be
    -- the worse artifact. chooseDefender's own copy stays for a direct caller
    -- that has no engine wrapper (a spec, or a second combat phase spliced by
    -- an effect).
    Phase.Combat CombatStep.BeginningOfCombat -> Monad.when hasActive Combat.chooseDefender
    Phase.Combat CombatStep.DeclareAttackers -> Monad.when hasActive (Combat.declareAttackers active)
    Phase.Combat CombatStep.DeclareBlockers -> Combat.declareBlockers
    Phase.Combat CombatStep.CombatDamage -> do
      -- CR 510.4: deal this step's damage; if it was the first-strike step,
      -- splice a second combat damage step in after it. The between-steps
      -- priority (CR 510.3) and SBA check come free from the step machinery.
      needSecond <- Damage.dealCombatDamage
      -- CR 615.5's "immediately afterward": a shield this wave spent runs its
      -- additional effect HERE, before the step's SBA check, so Test of Faith's
      -- +1/+1 counters are on the blocker before CR 704.5g asks whether the
      -- damage it did take was lethal.
      Resolve.runPreventionRiders
      Monad.when needSecond $
        State.modify' (\gs -> gs {GameState.remaining = Turn.spliceSecondDamage (GameState.remaining gs)})
    -- CR 505.4 / 703.4f / 714.3c: "immediately after a player's precombat main
    -- phase begins, that player puts a lore counter on each Saga enchantment they
    -- control with one or more chapter abilities". A turn-based action, not a
    -- trigger, which is why it lives here and not in the gatherer.
    --
    -- The active player's, so it takes the CR 800.4j guard the list above names:
    -- CR 505.4 says "the active player", and only they have a precombat main
    -- phase. Vacuous under that guard rather than merely skipped -- CR 800.4a has
    -- already taken a departed player's permanents, so there is no Saga they
    -- control left to advance.
    --
    -- CR 703.4g's Attraction roll, which that rule says happens immediately
    -- after this, has no producer in the pool and no site here.
    Phase.PrecombatMain -> Monad.when hasActive (advanceSagas active)
    -- CR 511.1: the end of combat step has no turn-based actions, so it has no
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
      -- BACK.
      State.modify' Damage.removeAllDamage
      State.modify' Expiry.dropAtCleanup
    _ -> pure ()

-- CR 505.4 / 703.4f / 714.3c's ACTION half: one lore counter onto each Saga this
-- player controls that has one or more chapter abilities.
--
-- Here rather than in Pawl.Engine.Saga for the reason that module's header gives:
-- it sits below Pawl.Engine.Event in the import graph, so it can classify which
-- Sagas advance but cannot call the CR 122.6 placement funnel. Pawl.Engine.Speed
-- and Pawl.Engine.Sba are split the same way.
--
-- Through Event.putCounters, so the placement records the CR 122.6 event CR
-- 714.2b's chapter abilities are gathered from.
--
-- ByRule, which is CR 614.16's answer and not a shortcut: that rule's replacement
-- effects reach a placement made by a resolving spell or ability, or by another
-- replacement or prevention effect, and CR 609.1 makes a turn-based action neither.
-- So Doubling Season does NOT advance a Saga two chapters a turn, though it DOES
-- double the lore counter CR 714.3a's replacement gives it as it enters -- and that
-- asymmetry is the whole reason Pawl.Types.CounterCause exists.
--
-- The player it carries is rule 714.3c's "that player", who is both the one whose
-- phase is beginning and the one controlling every Saga this advances. That is not
-- bookkeeping: a clause naming a PLAYER rather than an effect -- Vorinclex,
-- Monstrous Raider's -- does reach this placement, because 714.3c has a player make
-- it.
--
-- The Sagas are fixed from ONE projection taken before any counter goes on, which
-- is CR 703.4f's own reading: the whole placement is a single turn-based action,
-- so a Saga whose chapter ability would give its controller another Saga does not
-- advance that one too. Those abilities have not even triggered yet -- CR 714.3c
-- says the action does not use the stack, and nothing resolves until priority.
advanceSagas :: PlayerId -> Game ()
advanceSagas pid = do
  gs <- State.get
  let pcs = Projection.projectAll gs
  Monad.mapM_ (\oid -> Event.putCounters (CounterCause.ByRule pid) oid CounterKind.Lore 1) (Saga.advancing (\oid -> Projection.controllerOf oid gs) pid pcs gs)

-- CR 603.3: put each triggered ability that fired since the last placement on the
-- stack, in APNAP order (CR 603.3b): active player's triggers first, then each
-- other player's in turn order (apnapPlayers). Within one controller's own set,
-- that player chooses the order (orderPending), asked only when they control two
-- or more. The abilities that fired include the sourceless inherent ones the
-- rulebook states without a card -- CR 725.2's monarch pair and CR 702.179d's
-- speed increase, gathered apart (see `inherent` and `revving` below) but ordered
-- and placed with everything else, in ONE batch, because CR 603.3b gives the
-- choice to a controller over every triggered ability they control, not over some
-- subset.
--
-- CR 603.3b's TWO-PART process, in the rule's own order: first every trigger
-- whose condition isn't another ability triggering, APNAP with each controller's
-- own order; then all the rest, APNAP again. Both passes come out of ONE gathered
-- batch (`gathered` below), so the split is by CONDITION and never by which round
-- of the gather found a trigger -- Event.reactsToAbilityTriggering is the
-- classification, exhaustive so a new condition has to choose its pass.
--
-- The second pass has something to hold because `reactions` records a
-- GameEvent.AbilityTriggered for every trigger in the batch and scans it before
-- anything is placed; see that function. Advancing scannedThrough makes an event fire
-- its triggers once (CR 603.2c) WITHOUT discarding the record. Targets are
-- chosen as the ability is placed (CR 603.3d). Returns whether any were placed.
--
-- CR 800.4d, SECOND sentence: a triggered ability that would be controlled by a
-- player who has left the game is not put onto the stack. No separate filter is
-- needed: `orderPending` groups `pending` by `apnapPlayers`, which already
-- restricts every group to a still-playing controller, and nothing in the
-- `apnapPlayers` -> `orderFor` -> `Game.permute` pipeline can INTRODUCE an entry. Two
-- carriers can reach this with a departed controller -- a DELAYED ability, whose
-- controller CR 603.7d fixed as the arming spell resolved, and an OBJECT-BORNE
-- ability reading CR 603.3a's controller from GameState.controlWhenTriggered
-- rather than live. No board in the pool reaches either (#604);
-- Event.stateTriggers is not a third, since CR 603.8 evaluates a state trigger
-- AT this scan. The DELAYED entry is still CONSUMED regardless, because CR
-- 603.7b spends the one shot on the trigger event, which happened.
--
-- The Bool this returns is computed from the two placed passes, not from the
-- pre-filter `pending`, so it still tells the truth in exactly the case CR 800.4d
-- creates:
-- a departed player's delayed ability firing with nothing else pending would
-- otherwise report `True` on a step that put nothing on the stack.
--
-- CR 800.4d's FIRST sentence and CR 800.4b's SECOND are both enforced by the
-- guard at the head of `Event.createTokens` -- by CR 111.2 a token's owner and
-- controller are the same player, so one guard satisfies both. That guard is
-- DEFENCE IN DEPTH: the filter above already stops a departed player's ability
-- one step earlier, but the rule belongs at the single place a token is minted.
--
-- CR 800.4b's THIRD sentence (an object put onto the battlefield or the stack
-- under a departed player's control) is producerless, so nothing tracks it:
-- Event.changeZoneReturning is the sole zone-change primitive and takes the
-- destination controller from `Object.owner`, never from a named player.
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
      -- unscanned-event snapshot and before the watermark bump, then merged into
      -- the one batch below: CR 603.3b gives a player the order among ALL the
      -- abilities they control that triggered, and CR 725.2 makes these theirs
      -- like any other. Placing them after the ordered batch would make them
      -- resolve first, by the engine's choice rather than the player's.
      --
      -- Their controller is always the current monarch, and CR 725.4
      -- (Monarch.reassignOnDeparture) keeps the crown off a departed seat, so
      -- CR 800.4d has nothing to catch here; apnapPlayers filters them anyway.
      inherent = Monarch.inherentMonarchPending evs gs
      -- CR 702.179d, the rulebook's third inherent ability after CR 725.2's two,
      -- and gathered for exactly the reason above: it hangs on no object either.
      -- At most one entry, and only for the active player.
      revving = Speed.inherentPending evs gs
      -- CR 728.1, the rulebook's fourth inherent ability, gathered for the same
      -- reason as the three above. At most one entry, and only for the active
      -- player, who is the only one with a precombat main phase this turn.
      irradiated = Rad.inherentPending evs gs
      -- CR 309.4c's room abilities, gathered separately for the reason the four
      -- above are: Event.gatherTriggers reads the command zone for CR 114.4's
      -- emblems alone, and a dungeon card is not one -- nor is a room ability
      -- printed on its face (#1411). Unlike them these DO have a source, so they carry
      -- TriggerSource.OfObject and go on the stack through placeBorne.
      entered = Dungeon.roomPending evs gs
  State.put
    gs
      { GameState.scannedThrough = Natural.length (GameState.events gs),
        -- CR 702.179d's "this ability triggers only once each turn", marked as the
        -- trigger is gathered rather than as it resolves: the limit is on
        -- TRIGGERING, so an instance countered on the stack has still spent the
        -- turn's one. Cleared at the turn handoff (beginTurnOf).
        GameState.speedIncreasedThisTurn = List.foldl' (flip Set.insert) (GameState.speedIncreasedThisTurn gs) (fmap PendingTrigger.controller revving),
        -- CR 603.3a's sample is spent with the batch it was taken for. Cleared
        -- rather than left standing, so the next event to open a batch takes a
        -- fresh one (Event.recordEvent samples only when nothing is unscanned)
        -- and a stale reading can never outlive the events it described.
        GameState.controlWhenTriggered = Map.empty,
        GameState.delayedTriggers = surviving
      }
  gathered <- reactions (pending <> inherent <> revving <> irradiated <> entered)
  -- CR 603.3b's two sentences, run one after the other rather than ordered
  -- together and placed at the end. The rule's first sentence PUTS its abilities
  -- on the stack before its second is reached, and that is observable twice over:
  -- Pawl.Types.Asked hands the ordering prompt the game as it stands, so a player
  -- choosing the second pass's order sees the first pass already on the stack;
  -- and CR 603.3d has each ability choose its targets as it is put there, against
  -- that same stack.
  first <- orderPending (filter (not . reacts) gathered)
  Monad.mapM_ placeOne first
  second <- orderPending (filter reacts gathered)
  Monad.mapM_ placeOne second
  pure (not (null first) || not (null second))

-- CR 603.3b's classification, asked of one pending trigger: does its condition
-- name another ability triggering? A SOURCELESS inherent ability (CR 725.2, CR
-- 702.179d, CR 728.1) is classified the same way as any other, through its own
-- condition, so it needs no special case.
reacts :: PendingTrigger.PendingTrigger -> Bool
reacts = Event.reactsToAbilityTriggering . TriggeredAbility.condition . PendingTrigger.ability

-- CR 603.3b: "if MULTIPLE ABILITIES HAVE TRIGGERED since the last time a player
-- received priority" -- so an ability that triggers off another ability
-- triggering is part of the SAME batch, not of the next one, and has to be
-- gathered before any of the batch goes on the stack.
--
-- That is what this does: record a GameEvent.AbilityTriggered for every trigger
-- gathered so far, scan those new events for the abilities they in turn fire, and
-- repeat until a round finds nothing. The result is every trigger of the batch,
-- which `placePendingTriggers` then splits into the rule's two passes by
-- condition.
--
-- MINTING AN EVENT rather than having the second pass inspect the pending set
-- directly. An ability triggering is a game event -- rule 603.3b says so by
-- making it a trigger condition -- so it belongs in CR 608.2i's log beside every
-- other event, and every existing piece of machinery then applies to it
-- unchanged: Event.eventTriggers' battlefield-and-graveyard scan, CR 603.4's
-- intervening "if", CR 603.3a's controller sample, eventBindings. A private scan
-- over the pending set would have been a second matching path for one condition,
-- recording nothing a later reader could see.
--
-- Only EVENT triggers are scanned in these rounds. CR 603.8's state triggers are
-- already gathered once by `gatherTriggers`, and a state trigger's condition is a
-- fact about the game state rather than an event, so no round could add one. A CR
-- 603.7 DELAYED ability whose trigger event is another ability triggering is not
-- gathered here and has no producer (#1026).
--
-- No artificial bound on the rounds. A pair of abilities each triggering off the
-- other is a genuine loop in the rules too (CR 104.4b's draw), and stopping short
-- would be the engine deciding a game the rules do not; nothing in the pool
-- reaches a second round, since a chapter ability's triggering fires only
-- Historian's Boon and the Boon's triggering fires nothing.
reactions :: [PendingTrigger.PendingTrigger] -> Game [PendingTrigger.PendingTrigger]
reactions batch
  | null batch = pure []
  | otherwise = do
      -- One CR 704.3 event group EACH, not one `Event.simultaneously` bracket
      -- around the lot: a batch is "the abilities that have triggered since the
      -- last time a player received priority" (CR 603.3b), and those can have
      -- triggered off different events at different moments. Nothing reads the
      -- grouping of these records today -- CR 603.10a's look-back is about a
      -- permanent leaving the battlefield, and `looksBack` puts this condition
      -- outside its four families.
      State.modify' (\g -> List.foldl' (flip Event.recordEvent) g (Maybe.mapMaybe triggeredEvent batch))
      gs <- State.get
      let fresh = Event.reactionTriggers (Event.unscannedGrouped gs) gs
      -- The round's own events are consumed here, and CR 603.3a's sample is
      -- spent with them for the reason the batch's own is cleared above: the
      -- next round's recordEvent re-takes it, this having left nothing unscanned.
      State.modify'
        ( \g ->
            g
              { GameState.scannedThrough = Natural.length (GameState.events g),
                GameState.controlWhenTriggered = Map.empty
              }
        )
      rest <- reactions fresh
      pure (batch <> rest)

-- CR 603.3b's record of one ability triggering: its source (CR 113.7), its
-- controller as it triggered (CR 603.3a) and its trigger condition.
--
-- Nothing for a SOURCELESS ability, which is the one shape the event cannot
-- describe -- CR 725.2's monarch pair, CR 702.179d's speed increase and CR
-- 728.1's rad counters hang on no object, so there is no id to name (#1026).
triggeredEvent :: PendingTrigger.PendingTrigger -> Maybe GameEvent.GameEvent
triggeredEvent pending = case PendingTrigger.source pending of
  TriggerSource.Sourceless -> Nothing
  TriggerSource.OfObject oid ->
    Just
      ( GameEvent.AbilityTriggered
          oid
          (PendingTrigger.controller pending)
          (TriggeredAbility.condition (PendingTrigger.ability pending))
      )

-- Put one triggered ability from the ordered batch on the stack. What it hangs
-- on decides how: an ability BORNE by an object goes through placeBorne, where
-- every step is keyed to that object -- CR 113.7's reserved source binding, and
-- the fillableModes/legalSets pair that reads modes and targets relative to it.
-- A sourceless ability (CR 725.2's pair, CR 702.179d's) has no such object and
-- takes the other arm.
placeOne :: PendingTrigger.PendingTrigger -> Game ()
placeOne pending = case PendingTrigger.source pending of
  -- Monarch.placeInherent names no rule of its own -- it is the generic
  -- sourceless placement, and rule 702.179d's ability rides it too.
  TriggerSource.Sourceless -> Monarch.placeInherent pending
  TriggerSource.OfObject srcId -> placeBorne srcId pending

-- Put one object-borne triggered ability on the stack as a fresh OfTrigger
-- object, choosing its mode(s) and their targets as it is placed (CR 603.3d).
-- This mirrors Cast.castSpell's cast-time flow: CR 700.2b's mode choice as the
-- ability triggers (forced and unprompted exactly when the fillable modes are no
-- more than the selection demands -- a choice between indistinguishable options,
-- since every fillable mode must be taken), then CR 603.3d's targets for the
-- CHOSEN mode(s), chosen now at placement. Aether Channeler's ETB is the pool's
-- proof that a real choice is really asked (ModalSpec, "M4h trigger modal").
--
-- CR 603.3c/700.2b: if no mode is chosen, the ability is removed from the stack.
-- This is the trigger-only half of the rule -- a SPELL that can't choose a mode
-- is simply never offered for casting (CR 601.2c); a TRIGGER is already on the
-- stack by the time modes are chosen, so an unfillable one must instead be taken
-- back OFF it. The guard precedes the mode prompt: a removed trigger must never
-- be asked to choose.
placeBorne :: ObjectId.ObjectId -> PendingTrigger.PendingTrigger -> Game ()
placeBorne srcId pending = do
  gs <- State.get
  let controller = PendingTrigger.controller pending
      ability = PendingTrigger.ability pending
      (abilId, gs1) = Game.freshObjectId gs
      (ts, gs2) = Game.freshTimestamp gs1
      decider = Decide.deciderFor controller gs
      -- CR 603.2/603.3d: the event's own bindings are known before a target is
      -- chosen, so a slot saying "that player controls" is baked to the player the
      -- event named before either the mode gate or the target prompt reads it
      -- (Pawl.Engine.Target.bakeModal). Baked HERE and not stored: the stack
      -- object keeps the printed ability, and CR 608.2b's re-check bakes again
      -- from the same bindings.
      modal = Target.bakeModal (Binding.playerSlots (PendingTrigger.bindings pending)) (TriggeredAbility.modal ability)
      legal = Target.fillableModes (Just controller) srcId Map.empty modal gs
      selection = Modal.Type.selection modal
      obj =
        Object.MkObject
          { Object.owner = controller,
            Object.enteredUnder = Nothing,
            Object.source = Source.OfTrigger srcId ability,
            Object.zone = Zone.Stack,
            Object.tapped = TapState.Untapped,
            Object.facing = Facing.FaceUp,
            Object.damage = 0,
            Object.sickness = Sickness.Settled controller,
            Object.bindings = Map.empty,
            Object.counters = Map.empty,
            Object.attachedTo = Nothing,
            Object.chosenColor = Nothing,
            Object.chosenSubtype = Nothing,
            Object.chosenNames = Set.empty,
            Object.timestamp = ts,
            Object.face = Nothing,
            Object.turnedOverAt = Nothing,
            Object.worldSince = Nothing,
            Object.playableFromExile = Nothing,
            Object.plotted = Nothing,
            Object.ringBearerFor = Nothing,
            Object.protector = Nothing,
            Object.ventureRoom = Nothing,
            Object.unlockedHalves = Set.empty,
            Object.designations = Set.empty,
            Object.kicked = False
          }
  State.put gs2 {GameState.objects = Map.insert abilId obj (GameState.objects gs2), GameState.stack = abilId : GameState.stack gs2}
  if not (Modal.selectionPossible legal selection)
    then -- CR 603.3c: no selection satisfies the instruction -- ordinarily fewer
    -- legal modes than it demands, and for ChooseExactly 1 no legal mode at all
    -- -- which removes the ability.
      State.modify' (Game.cease abilId)
    else do
      -- CR 700.2b: forced when there is nothing to choose, prompted otherwise;
      -- Modal.forcedSelection tells the two apart. Sorted on the way in, for the
      -- reason Cast.castProposed gives (CR 608.2c, CR 700.2d).
      chosenModes <- case Modal.forcedSelection legal selection of
        Just forced -> pure forced
        Nothing -> fmap Seq.sort (Game.choose (Prompt.ChooseModes decider controller abilId legal selection))
      -- CR 603.3d: targets for the chosen mode(s) only, chosen as the ability
      -- is placed. A mode with no target slots (Create, or a Draw that names its
      -- drawer without targeting) asks nothing.
      let specs = Modal.modesTargetSpecs chosenModes modal
          sets = Target.legalSets (Just controller) srcId specs gs
      chosen <- Target.chooseTargets decider controller abilId specs sets
      -- CR 113.7: the ability's SOURCE is bound under the reserved slot as it is
      -- placed, so "this creature" resolves as an ordinary slot read even after
      -- the source has left the battlefield.
      --
      -- CR 603.7c: a delayed ability's CAPTURED environment (its "it") rides
      -- alongside the targets/modes chosen now for THIS placement; the source
      -- slot is stamped over the top regardless. The two DO collide: the
      -- captured environment is built by the same Binding.fromChoices the arming
      -- spell used, so it carries that spell's OWN reserved slots (chosenModes,
      -- variableX). Binding.mergeBinding is left-biased per FIELD, so
      -- placement-time bindings must be the LEFT argument -- they are this
      -- ability's own choices; the captured environment's only job is to carry
      -- forward object references placement-time can never supply. Getting the
      -- order backwards silently substitutes the arming spell's chosen mode or X
      -- for this ability's own.
      --
      -- unionWith mergeBinding rather than a left-biased Map.union, which would
      -- drop the WHOLE captured entry on a name collision: the two sides can now
      -- carry disjoint FIELDS of one slot -- a delayed ability declaring a target
      -- spec under the name a Create bound its minted tokens to would keep only
      -- the target and lose the group. Per-field, both survive.
      State.modify' (\g -> g {GameState.objects = Map.adjust (\o -> o {Object.bindings = Binding.setYou controller (Binding.setTriggerSource srcId (Map.unionWith Binding.mergeBinding (Binding.fromChoices chosen Nothing chosenModes) (PendingTrigger.bindings pending)))}) abilId (GameState.objects g)})

-- CR 101.4 / 603.3b: the players who control a pending trigger, active player
-- first and then the rest in turn order. Grouped by controller because the
-- within-controller ORDER is itself a choice. A departed seat is not in the
-- APNAP order at all -- CR 101.4 orders the active and the nonactive players,
-- and CR 102.1 makes a player one of the people in the game -- and CR 800.4d
-- keeps its triggers off the stack. Since turnOrder is the permanent seating
-- roster, this filter is what enforces both.
apnapPlayers :: GameState -> [PendingTrigger.PendingTrigger] -> [PlayerId]
apnapPlayers gs pending =
  let rotated = Game.apnapOrder gs
      -- turnOrder is the permanent SEATING roster, so the rotation still names
      -- departed seats. A player who has left the game is not in APNAP order and
      -- is never asked to order triggers.
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

-- CR 603.3b: is every permutation of this batch the same game, so that the
-- prompt is a question with one answer? Two conditions, and both are needed.
--
-- The entries must be EQUAL -- one ability of one source, the discriminator
-- Pawl.Types.TriggerEntry carries (#61). Equality alone is NOT enough, and that
-- is the whole trap here: CR 603.6a fires one trigger per entrant, so Aether
-- Flash watching two creatures enter contributes two EQUAL entries whose
-- bindings name different creatures, and CR 117.3b hands priority back between
-- the two resolutions -- so which of them was damaged first is on the board an
-- opponent may respond to.
--
-- So the ability must also be order-inert: nothing about it may be decided as it
-- is placed, and nothing it does may read what a sibling has already done.
interchangeable :: [TriggerEntry.TriggerEntry] -> Bool
interchangeable entries = case entries of
  [] -> True
  entry : rest -> all (== entry) rest && orderInert (TriggerEntry.ability entry)

-- CR 603.3b: may a batch of EQUAL entries of this ability be put on the stack in
-- the engine's canonical order without asking? Five conditions:
--
--   * no intervening "if" (CR 603.4), which is re-checked as the ability
--     resolves and so can read a board an earlier sibling changed;
--   * one mode and a ChooseExactly 1 selection, so CR 603.3c / 700.2b's mode
--     announcement -- made as the ability is put on the stack, in the order
--     chosen -- has nothing to announce;
--   * no target specs, since CR 603.3d imports CR 601.2c and targets are chosen
--     as each ability is placed, in the order chosen;
--   * the mode reads NO slot at all (Resolve.modeSlots);
--   * and that answer is complete (Resolve.slotsAreExhaustive), which is where
--     an opcode carrying a nested payload has to be caught.
--
-- Reading no slot AT ALL rather than "no slot that varies across the batch" is
-- deliberately conservative. Binding.you and Binding.triggerSource really are
-- constant across a batch of equal entries -- one controller, one source -- so
-- admitting them would be sound too; the conservative form needs no argument
-- about which slots vary, and Soul Warden reads none. Do not widen it without a
-- card that needs the width.
--
-- Not a case on any effect's IDENTITY: this asks two classifications and never
-- which opcode it is looking at.
orderInert :: TriggeredAbility.TriggeredAbility Card.Card -> Bool
orderInert ability =
  let modal = TriggeredAbility.modal ability
   in Maybe.isNothing (TriggeredAbility.intervening ability)
        && Modal.Type.selection modal == ModeSelection.ChooseExactly 1
        && case Foldable.toList (Modal.Type.modes modal) of
          [mode] ->
            Map.null (Mode.targetSpecs mode)
              && Map.null (Resolve.modeSlots mode)
              && all Resolve.slotsAreExhaustive (Mode.allEffects mode)
          _ -> False

-- What one pending trigger looks like to the player being asked for CR 603.3b's
-- order: its source and WHICH ABILITY it is (#61) -- see
-- Pawl.Types.TriggerEntry.
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

-- CR 117.5's settle, discarding the report. The name all but one caller uses;
-- `performSettle` below is the same act, and carries the whole account of it.
settleForPriority :: Game ()
settleForPriority = Monad.void performSettle

-- CR 117.5: each time a player would receive priority, sweep expired "for as
-- long as" effects, perform state-based actions, then put triggered abilities
-- on the stack, repeating until none of the three does anything. Then priority
-- is granted (by the caller). The repeat is gated on cheap booleans -- the
-- conditional sweep, a monarch exile returning, a day/night check acting, the
-- control sample minting an event, an SBA firing, a trigger being placed -- so a
-- settle that changes nothing costs one board projection and one length comparison
-- per carrier, NOT a deep GameState equality check. On top of
-- that, every pass pays five samples of derived state (sampleWorldSince for
-- CR 704.5k, sampleControl for CR 603.2's control-change event,
-- checkControlContinuity for CR 302.6, Combat.removeChanged for
-- CR 506.4, Ring.endOnControlChange for CR 701.54a), because a derived change to
-- control, to card types or to supertypes has nothing else to notice it.
--
-- CR 611.2b's condition is checked continuously, and CR 704.3 makes "whenever
-- a player would get priority" the coarsest moment anything could observe it,
-- so settling here is indistinguishable from checking continuously. The sweep
-- runs FIRST, before the SBA check: a "for as long as you control this" effect
-- ending (Master Thief) returns a permanent to another player's control, and a
-- control-scoped state-based action must see the post-sweep control. CR 704.5j's
-- legend rule is the rule that reads it, and Sba does check it, so the ordering
-- is observable rather than theoretical. The loop re-runs whenever
-- ANYTHING fired, because an SBA can itself be what falsifies a condition.
--
-- It also REPORTS whether it performed any state-based action or placed any
-- triggered ability, because one caller needs the answer and the rest do not.
-- That caller is `cleanupException` (CR 514.3a), and those two are exactly what
-- the rule asks about; the conditional sweep, the monarch exile return, the
-- day/night check and the control sample also make the loop repeat but are none of
-- them, so none is reported. The control sample is the one whose omission could
-- cost a CR 514.3a round, and it cannot: an event it mints is scanned by the very
-- next pass, so a trigger it produces is reported as `placed` there.
--
-- Reported across EVERY pass, where CR 704.3's last sentence names the step's
-- first check. The two agree wherever the conditional sweep is inert, which is
-- every board the CR itself describes. Where pawl's extra sweep makes them
-- differ, this errs toward granting priority, so a trigger placed on a later pass
-- still gets the CR 514.3a round it is entitled to.
performSettle :: Game Bool
performSettle = do
  swept <- Expiry.sweepConditional
  returned <- Monarch.returnExiledForMonarch
  -- CR 702.145c/d/f/g, checked here for CR 704.3's reason and not because they are
  -- state-based actions -- both rules say they are not. Before the SBA pass, since
  -- turning a permanent over changes its power and toughness and CR 704.5f must
  -- read the board the turn leaves behind.
  dayNight <- Daytime.settle
  -- Before the SBA pass for Daytime.settle's reason: a permanent that just became
  -- world must be stamped before CR 704.5k reads the clock. Inside the recursion
  -- guard because it runs on every pass, but not part of it -- stamping makes no
  -- further work, so it is never a reason to loop.
  sampleWorldSince
  -- The control-change observation point, and unlike the four other samples in
  -- this function it MAKES WORK: a GameEvent.ControlChanged it mints has to be
  -- scanned, so it runs before placePendingTriggers and joins the recursion guard
  -- below. See `sampleControl`.
  sampledControl <- sampleControl
  acted <- Sba.performStateBasedActions
  placed <- placePendingTriggers
  -- Last, and for the same reason the conditional sweep runs first: all three
  -- read state this settle can still change, and are placed to see what it leaves
  -- behind. All three read CONTROL, and the CR 506.4 scan also reads CARD TYPES --
  -- where the sweep is what ends a "for as long as" animation, and so what makes
  -- an attacker stop being a creature.
  --
  -- Outside the recursion guard on purpose -- none makes further work, so none is
  -- a reason to loop, and all must run even on a pass where nothing fired. That
  -- stops being true the moment CR 701.54c's base ability lands: clearing the
  -- Ring-bearer designation would then REMOVE a legendary supertype, which is
  -- input to the CR 704.5j legend rule that already ran on this pass (#707). The

  -- settle stops only on a pass where nothing fired, and these three ran on that
  -- pass, against the finished board, before priority is granted.
  --
  -- Order among the three does not matter: CR 506.4 asks about combat, CR 302.6
  -- about summoning sickness and CR 701.54a about the Ring-bearer designation, and
  -- none reads what another writes.
  State.modify' Combat.removeChanged
  checkControlContinuity
  Ring.endOnControlChange
  more <- if swept || returned || dayNight || sampledControl || acted || placed then performSettle else pure False
  pure (acted || placed || more)

-- CR 104.4b: how many events may happen with no player able to decide anything
-- before the game is declared a loop of mandatory actions.
--
-- A HEURISTIC, and deliberately a crude one. Detecting such a loop in general is
-- the halting problem, so the question this answers is not "is this a loop?" but
-- "has this gone on longer than any real game would?". The MARGIN is what makes
-- it safe. GameState.nextTimestamp advances on the events CR 104.4b names -- an
-- object entering a zone (CR 613.7d) and a continuous effect beginning (CR
-- 613.7a) -- and a game in which no player can act issues about one per turn, the
-- draw. So the slowest way a real game ends, decking out from a 60-card library
-- (CR 704.5b), spends on the order of fifty of these, roughly twenty times under
-- the limit. A two-card recursion loop issues several per cycle and arrives in a
-- few hundred.
--
-- Counting engine iterations instead was rejected for want of that margin: a game
-- whose players pass every step until someone decks visits a loop head on the
-- order of a thousand times, which is this same order.
--
-- Not configurable. There is one caller and one sensible value, and a knob
-- threaded through GameState would be a second thing to keep right.
mandatoryLoopLimit :: Natural
mandatoryLoopLimit = 1000

-- CR 104.4b: a game that has entered a loop of mandatory actions is a draw.
--
-- Never overwrites a result the game already has. A won game is not looping, and
-- CR 104.4a's simultaneous loss is a different draw arrived at by a different
-- path (Departure.leaveGame).
--
-- The subtraction cannot underflow: GameState.lastChoice is only ever written to
-- the value GameState.nextTimestamp then had, and that supply only counts up.
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
  -- priority pass, which leaves the game state untouched. This is the standard
  -- "check SBAs only after a game event" reading of CR 117.5, observably
  -- identical to settling on every priority grant.
  let loop = do
        -- CR 104.4b, checked HERE as well as at playGame's loop head: a
        -- resolution cycle repeats inside one priority round and never reaches
        -- that one.
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
                        -- CR 104.3a: asked before anything else, and keyed to `p` -- the
                        -- TRUE player, never `Decide.deciderFor p`. Prompt.Concede carries
                        -- no Decider precisely so this cannot be got wrong (CR 723.6): a
                        -- controller may not concede for the player they control, though
                        -- that player may still concede themselves.
                        concession <- Game.ask (Prompt.Concede p)
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
                            -- CR 117.4 defines passing in succession as passing
                            -- without taking actions in between. A concession
                            -- changes the board (CR 800.4a removes the departing
                            -- player's objects), so the passes already counted
                            -- this cycle no longer form a succession and the
                            -- count restarts -- as in the Play/Cast/Activate arms
                            -- below. The CR does not settle this directly; not
                            -- resetting risks resolving a spell a player would
                            -- have responded to, and resetting costs at most one
                            -- redundant pass prompt.
                            State.modify' (\g -> g {GameState.passes = 0, GameState.priority = Just (nextStillPlaying g p)})
                            loop
                          Concession.Continues -> do
                            let decider = Decide.deciderFor p gs
                                actions = Action.legalActions p gs
                            -- CR 104.4b: a menu that is only Pass offers no
                            -- optional action, so it does not break a loop of
                            -- mandatory actions. Still ASKED either way -- the
                            -- interpreter and the Replay fold see every priority
                            -- grant -- just not RECORDED as a choice, which is
                            -- what Game.choose does and what
                            -- checkMandatoryLoop reads.
                            --
                            -- Matched two-deep rather than `length actions > 1`:
                            -- Action.legalActions is lazy and Pass is its head,
                            -- so `List.elem answered actions` below stops at the
                            -- first cons for a passing answer. Taking the length
                            -- would force the whole spine -- castableSpells and
                            -- the activation walk -- at every priority grant,
                            -- which measured as a 70% slowdown on Pawl.ReplaySpec's
                            -- played-out game.
                            let offersAChoice = case actions of
                                  _ : _ : _ -> True
                                  _ -> False
                            answered <-
                              if offersAChoice
                                then Game.choose (Prompt.ChooseAction decider p actions)
                                else Game.ask (Prompt.ChooseAction decider p actions)
                            -- FILTERED, NOT TRUSTED. Everything Action.legalActions
                            -- computed -- the controller check, CR 302.6's
                            -- tap-sickness gate, CR 307.5 timing, cost payability,
                            -- CR 305.2's land allowance, CR 117.1a's casting
                            -- timing and every prohibition -- is enforced here.
                            -- Acting on an unoffered answer would make all of it
                            -- advisory (#219). What only this guard can catch is
                            -- an action that was never on the menu.
                            --
                            -- Rejecting to Pass rather than failing: it is always a
                            -- legal action, it keeps this loop total, and it cannot
                            -- wedge the game, because a full round of passes
                            -- resolves the stack or ends the step.
                            let chosen = if List.elem answered actions then answered else Action.Type.Pass
                            case chosen of
                              Action.Type.Pass -> do
                                let passes = GameState.passes gs + 1
                                    playing = Natural.length (Game.stillPlaying gs)
                                -- modify' over `State.put gs {...}`: `gs` is the
                                -- snapshot taken BEFORE the prompt, and putting
                                -- it back would discard whatever the prompt
                                -- wrote. Game.choose writes GameState.lastChoice
                                -- there (CR 104.4b), and a clobbered marker
                                -- would make every loop look mandatory. The
                                -- values still read off the snapshot -- `passes`
                                -- and the successor seat -- are fields no prompt
                                -- touches.
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
                                -- CR 712.12: "A player playing a modal
                                -- double-faced card ... as a land chooses one of
                                -- its faces that's a land before putting it onto
                                -- the battlefield. It enters the battlefield with
                                -- that face up." The choice was made when the
                                -- action was chosen (Action.playableLands offers
                                -- one per face), so the move is what carries it --
                                -- the door CR 709.3a's chosen half already uses.
                                --
                                -- NOT changeZoneEntering, which is where CR
                                -- 712.14b turns a put-onto-the-battlefield
                                -- instruction away: playing a land is a special
                                -- action (CR 305.1), not such an instruction, and
                                -- CR 712.12 permits exactly the card CR 712.14b
                                -- stops.
                                Monad.void (Event.changeZoneShowing oid Zone.Battlefield mName)
                                -- CR 305.2a counts the lands played this turn,
                                -- so this TALLIES rather than flagging: the
                                -- second land Exploration allows has to be
                                -- distinguishable from the first.
                                --
                                -- CR 305.4: this arm is the only tally, and that
                                -- is the rule rather than an oversight -- an
                                -- effect that PUTS a land onto the battlefield
                                -- is not a land played and must not count.
                                State.modify' (\g -> g {GameState.landsPlayed = Map.insertWith (+) p 1 (GameState.landsPlayed g), GameState.passes = 0, GameState.priority = Just p})
                                settleForPriority
                                loop
                              Action.Type.Cast oid name facing -> do
                                Cast.castSpell p oid name facing
                                State.modify' (\g -> g {GameState.passes = 0, GameState.priority = Just p})
                                settleForPriority
                                loop
                              -- CR 116.2b / 702.37e: a special action, so
                              -- nothing goes on the stack and no player gets a
                              -- window to respond to it -- the same shape as
                              -- CR 116.2a's land play above. Priority is
                              -- retained and the pass count restarts, because
                              -- CR 117.4's "passing in succession" means
                              -- passing without taking actions in between and
                              -- this was an action.
                              Action.Type.TurnFaceUp oid -> do
                                FaceDown.turnFaceUp p oid
                                State.modify' (\g -> g {GameState.passes = 0, GameState.priority = Just p})
                                settleForPriority
                                loop
                              -- CR 116.2m / 709.5e: a special action too, so the
                              -- TurnFaceUp arm's shape above applies unchanged --
                              -- nothing goes on the stack and no player gets a
                              -- window to respond to the payment. What CR 709.5h's
                              -- trigger sees is the DESIGNATION, which
                              -- settleForPriority then gathers like any other.
                              Action.Type.Unlock oid half -> do
                                Room.unlock p oid half
                                State.modify' (\g -> g {GameState.passes = 0, GameState.priority = Just p})
                                settleForPriority
                                loop
                              -- CR 116.2e: a special action too, so the
                              -- TurnFaceUp arm's shape above applies unchanged.
                              -- The discard goes through the CR 701.9a funnel
                              -- rather than a zone move, so an ability that
                              -- triggers on a discard sees it.
                              --
                              -- Ordinary, not ToPayCyclingCost: CR 702.29c's
                              -- cause is a cycling ability's cost, and this is
                              -- neither a cost nor a cycle.
                              Action.Type.DiscardFromHand oid -> do
                                Event.discard DiscardCause.Ordinary p oid
                                State.modify' (\g -> g {GameState.passes = 0, GameState.priority = Just p})
                                settleForPriority
                                loop
                              -- CR 116.2d: a special action too, so the
                              -- TurnFaceUp arm's shape above applies unchanged --
                              -- the payment goes on nothing and no player gets a
                              -- window to respond to it.
                              -- CR 116.2k / 702.170b: a special action too, so the
                              -- TurnFaceUp arm's shape above applies unchanged --
                              -- nothing goes on the stack and no player gets a
                              -- window to respond to the payment or the exile.
                              Action.Type.Plot oid -> do
                                Plot.plot p oid
                                State.modify' (\g -> g {GameState.passes = 0, GameState.priority = Just p})
                                settleForPriority
                                loop
                              Action.Type.Ignore oid -> do
                                Ignore.ignore p oid
                                State.modify' (\g -> g {GameState.passes = 0, GameState.priority = Just p})
                                settleForPriority
                                loop
                              -- CR 605.3a's first window, and CR 605.3b's
                              -- immediacy: the ability does not go on the stack,
                              -- so activating it is over by the time this
                              -- returns and the mana is already in the pool.
                              -- CR 117.3c keeps priority with the activator and
                              -- restarts the pass count, the shape every arm
                              -- above shares.
                              --
                              -- Cost.tapForMana is the same activation CR
                              -- 605.3a's other two windows take (Cost.payMana
                              -- calls it), which is why nothing here is about
                              -- mana: this arm decides only WHEN it may happen.
                              -- Its answer is discarded because a mana ability
                              -- whose cost went unpaid changed nothing and the
                              -- player simply has priority again (CR 601.2h).
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
                        -- CR 800.4a (last sentence): priority passes to the next
                        -- still-playing player in turn order. `p` was written as
                        -- the holder and then departed -- e.g. paying a life cost
                        -- inside settleForPriority (CR 119.4) -- before ever being
                        -- asked anything. Departure.depart does not touch
                        -- GameState.priority, so the stale `Just p` would
                        -- otherwise survive to the Concede prompt below.
                        -- nextStillPlaying is correct for a departed argument.
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
-- CR 800.4k: a turn a departed player would begin does not begin, so such a seat
-- is walked past rather than made active and their extra turn is spent without
-- beginning.
--
-- CR 800.4m: durations lasting until that player's next turn last until that
-- turn WOULD have begun, so Expiry.dropAtTurnOf fires at EVERY seat the walk
-- passes and at every extra turn popped, including the ones whose turn never
-- begins. For the seat that does begin a turn, the same call is CR 611.2a.
handoffTurn :: Game ()
handoffTurn = State.modify' takeNextTurn

-- CR 500.7 / 103.1: the seat the ordinary turn order resumes from. Read through
-- one function so the two callers below cannot drift: the anchor is the active
-- player unless an extra turn is under way, in which case it is the seat that
-- extra turn was inserted after (see GameState.turnAnchor).
turnAnchorOf :: GameState -> PlayerId
turnAnchorOf gs = Maybe.fromMaybe (GameState.activePlayer gs) (GameState.turnAnchor gs)

-- CR 500.7: extra turns are added directly after the specified turn. Every
-- extra-turn effect in the pool specifies the turn it resolves in, so an entry
-- in GameState.extraTurns is a turn scheduled directly after THIS one -- which
-- makes popping it here, before the seating order is consulted at all, the whole
-- of "directly after". The rule's last sentence makes the store a stack: the
-- most recently created turn is taken first, so this takes its HEAD, and
-- Resolve's TakeExtraTurn arm is the other half.
--
-- The anchor does NOT move (see GameState.turnAnchor): CR 500.7 adds a turn and
-- removes none, so the turn that would have followed the specified turn still
-- follows it. That is only observable when the taker is not the active player --
-- Time Warp aimed at an opponent -- and it is what stops an extra turn from
-- silently eating that player's ordinary one.
--
-- CR 805.8 (shared team turns) and CR 807.4i/j (Grand Melee's turn markers) each
-- rewrite this rule for their own option or variant. Neither is implemented,
-- because pawl has no format or variant to read one from (#175).
--
-- CR 800.4k applies to an extra turn exactly as it does to an ordinary one: a
-- departed player's extra turn does not begin. The entry is still SPENT, and
-- Expiry.dropAtTurnOf still fires for CR 800.4m -- the same two things
-- walkToNextTurn does for a seat it walks past.
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
          then -- CR 500.11: this turn's OWN skips (Savor the Moment) come into
          -- being as it begins, and only on this branch -- the CR 800.4k entry
          -- the `else` spends belongs to a turn that never begins. Nothing of
          -- this turn has started yet, so CR 614.10 is not yet in the way.
            Replacement.installTurnSkips entry (beginTurnOf pid anchored)
          else takeNextTurn swept

-- One seat at a time, bounded by the number of seats, so it terminates even when
-- every seat has departed. The fallback returns the state without beginning a
-- turn, keeping the sweeps already applied -- `swept` threads forward with every
-- recursive call. Written as an explicit bounded recursion rather than
-- `cycle`/`head` so it is total with no partial head. Unreachable while the game
-- is running: a game with no survivors already has a Result (CR 104.2a /
-- 104.4a).
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
  let -- CR 800.4b: a player who would be controlled by a departed player isn't.
      -- A pending Decider naming a departed player is not promoted; the stale
      -- entry is dropped either way, below.
      --
      -- CR 800.4a's second clause clears the entry at the departure itself
      -- (Departure.controlEffectsEnd), so in play the two rules reach the same
      -- outcome. This guard is the one that answers for CR 800.4b, and the only
      -- thing that answers when the entry arrives without a departure having run
      -- over it.
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
            -- CR 608.2i is why a log exists at all: some effects look back in
            -- time. It does not say how far back; the ONE-turn scope is this
            -- engine's choice, made because every history-reading card in the
            -- pool asks "this turn" (Khabal Ghoul). Cleared here, with both
            -- watermarks, and never at cleanup -- cleanup is still part of this
            -- turn and CR 514.1's discard is itself an event of it.
            -- Engine.advance settles immediately before calling this, so nothing
            -- unscanned is discarded.
            GameState.events = Seq.empty,
            -- CR 702.179d's "only once each turn", cleared beside the log it sits
            -- next to and for the same reason: this is the handoff, so a new turn
            -- starts with nobody's speed-increase ability spent.
            GameState.speedIncreasedThisTurn = Set.empty,
            -- CR 121.1's per-turn draw tally, cleared here for the same reason and
            -- for EVERY player: a player draws on turns that are not theirs, so
            -- "each turn" is the whole map rather than the incoming seat's row.
            GameState.drawsThisTurn = Map.empty,
            -- CR 502.2 / 731.2: the count the NEXT turn's untap step asks about
            -- "the previous turn's active player", taken here because the log it
            -- is folded from is cleared by this same record update and that check
            -- runs afterwards. The player is the OUTGOING active player, which is what `gs`
            -- still holds -- the new one is assigned in this same record update.
            GameState.spellsCastLastTurn = PlayerEffect.castsThisTurn (GameState.activePlayer gs) gs,
            GameState.scannedThrough = 0,
            -- Cleared with the log it describes: the settle Engine.advance runs
            -- immediately before this leaves nothing unscanned, so the sample has
            -- no batch left to answer for.
            GameState.controlWhenTriggered = Map.empty,
            GameState.damageScannedThrough = 0,
            -- GameState.lastKnown is deliberately NOT cleared alongside them.
            -- CR 608.2h's last known information is not history a card asks
            -- after, it is the substitute identity of an object that no longer
            -- exists, and it stays needed for as long as anything can still name
            -- that object -- a delayed triggered ability's source (CR 603.7d)
            -- outlives the turn it was armed in. Pawl.Engine.Setup clears it at
            -- the three points a NEW game begins.
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
-- the turn is over, so hand off. The turn is data, and this is the only thing
-- that reads its order.
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
    -- check (`cleanupException`) has already run this settle, so on that path
    -- this one finds nothing left to do. It is kept anyway, because the schedule
    -- can empty at a step that is NOT the cleanup step: CR 500.11's skip over an
    -- ending phase (skipWholePhase) drops the cleanup step along with the rest of
    -- the phase, and the handoff then follows the postcombat main phase directly.
    Seq.EmptyL -> do
      settleForPriority
      handoffTurn

-- One step: turn-based actions, then priority (if the step grants it), then
-- state-based actions, then move on. Bails out as soon as the game has a result.
runStep :: Game ()
runStep = do
  -- CR 727.4: the effect that restarts the game finishes resolving just before
  -- the first turn's untap step. If the previous step unwound on a restart, this
  -- is that untap step, and the rebuilt game is played from here like any other --
  -- so lower the signal before doing anything else.
  State.modify' (\gs -> gs {GameState.restartSignal = RestartSignal.Playing})
  phase <- State.gets GameState.phase
  active <- State.gets GameState.activePlayer
  -- CR 614.1b makes "skip" a replacement effect, and CR 500.11 makes skipping a
  -- step proceeding past it as though it didn't exist. So the question is asked
  -- HERE, of the replacement system, and a `False` answer means the whole of
  -- `runStepThatBegan` -- the CR 603.2b beginning event, the turn-based actions,
  -- the priority round, CR 500.5's mana emptying -- is not merely empty but never
  -- runs. Eon Hub is the card, and CR 500.6's "at the beginning of" triggers
  -- never triggering is the observable that distinguishes this from a step that
  -- happened and did nothing.
  --
  -- CR 614.10 -- a step that has started can no longer be skipped -- is what pins
  -- the question to this line. `advance` has already written the step into
  -- GameState.phase by now, but nothing has yet observed it: `advance` records no
  -- event and grants no priority, and playGame does nothing between the two but
  -- read GameState.result. So the step is scheduled, not started.
  --
  -- A skipped STEP is left popped off the schedule by `advance` below rather
  -- than dropped from GameState.remaining the way CR 508.8's combat skip is
  -- (Turn.dropSkippedCombatSteps): CR 508.8 is a RULE, known one step ahead,
  -- while a replacement effect has to be asked at the moment the event would
  -- happen, because CR 616.1's loop reads the board as it then is. A skipped
  -- PHASE is the case where popping one entry is not enough; `skipWholePhase`
  -- says what it does instead.
  --
  -- TWO questions, in CR 500.1's own order: a phase that has steps is offered
  -- first, then the step. `Turn.phaseBeginningAt` answers Just only at a stepped
  -- phase's FIRST step, so the phase question is asked once per phase -- CR
  -- 614.10 again, at phase grain -- and a main phase raises only the step
  -- question, because CR 505.2 makes it one schedule entry. Both are asked even
  -- when the phase one says yes, because CR 500.1 nests the step inside the
  -- phase: Stasis skipping an untap step must still take it during a beginning
  -- phase nobody skipped.
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
-- rest of its steps leave the schedule and `advance` picks up whatever CR
-- 500.1's fixed order puts after the phase (the postcombat main phase, for the
-- combat one Stonehorn Dignitary takes).
--
-- Positional, via Turn.dropRestOfPhase, not a filter: CR 500.8 lets a second
-- combat phase be added later in the same turn, and skipping this one says
-- nothing about that one -- the same reason CR 508.8's step skip is positional.
--
-- Nothing about the skipped phase is announced. CR 614.6 makes a replaced event
-- one that never happens, and CR 500.6's "at the beginning of" triggers hang off
-- the CR 603.2b step records `runStepThatBegan` writes, none of which this path
-- reaches.
--
-- GameState.combat is left ALONE, and does not go stale for it.
-- Combat.clearCombat runs as the end of combat step ends (CR 511.3), which this
-- path bypasses along with the rest of the phase -- but every writer of that
-- record runs inside a combat step, so a phase whose steps never begin writes
-- nothing and the record is still whatever the last end of combat step emptied
-- it to. Skipping the end of combat STEP on its own is the case that would
-- strand it, and no card in the pool names that step (#447).
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
  -- is forced: CR 704.3's last sentence keys that step's outcome to the step's
  -- FIRST check, so nothing may perform a state-based action ahead of CR
  -- 514.3a's. Running the ordinary check first buries the creature that CR 514.2
  -- just dropped to zero toughness (CR 704.5f), and CR 514.3a then finds an
  -- already-settled board and grants nothing.
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
        -- CR 500.5, whole and in its own order: as a step or phase ends, effects
        -- lasting until the end of that step or phase expire, and then any
        -- unspent mana empties.
        --
        -- TWO windows end here, not one, and CR 500.5a is why the difference
        -- matters: an "until end of combat" effect expires at the end of the
        -- combat PHASE, not at the beginning of the end of combat step (CR
        -- 511.2). So the step that just ran is swept, and then -- only if it was
        -- the phase's last step -- the phase is (Turn.phaseEndingAt). An "until
        -- end of combat" animation is therefore live for the whole of the end of
        -- combat step, including the priority round CR 511.1 grants there.
        --
        -- The step is swept before the phase because CR 500.1 nests the one
        -- inside the other and the inner window ends first. Nothing observes the
        -- order: the two sweeps match disjoint sets of stored effects, and no
        -- state-based action, trigger or priority round runs between them.
        State.modify' (Expiry.dropAtEndOf (PhaseSelector.Step phase))
        Foldable.traverse_ (State.modify' . Expiry.dropAtEndOf) (Turn.phaseEndingAt phase)
        -- CR 703.4q: emptying the pool is a turn-based action that does not use
        -- the stack, and CR 500.5's "Then" puts it AFTER the expiries above. This
        -- line says only WHEN; WHICH mana empties is the action itself, and
        -- Mana.emptyManaPools decides it per player and per unit (Upwelling's
        -- whole pool, Omnath Locus of Mana's green).
        --
        -- The ordering is observable: PlayerEffect.DontLoseUnspentMana is read
        -- live by Mana.emptyManaPools, so a retention effect that expires here
        -- keeps nothing, while one swept afterwards would keep the pool across a
        -- boundary it no longer covers. No CARD in the pool prints that
        -- combination (CR 702.189a firebending is the shape that would).
        State.modify' Mana.emptyManaPools
        -- CR 511.3: as soon as the end of combat step ends, creatures, battles
        -- and planeswalkers are removed from combat -- so it belongs here, at the
        -- step's END, and not in runTurnBasedActions at its start. Unlike the
        -- mana emptying above it, this is not a turn-based action at all, because
        -- CR 511.1 says this step has none. Start versus end is the observable
        -- part: creatures stay attacking for the whole of the step, including the
        -- priority round CR 511.1 grants, where an instant may still read them
        -- (Kill Shot).
        --
        -- Its order against the mana emptying is indistinguishable -- nothing
        -- reads a mana pool through the combat record or the reverse -- and
        -- neither raises a state-based action. Order against CR 500.5's expiry
        -- sweeps is free too: an "until end of combat" animation expiring first
        -- makes its source stop being a creature, which CR 506.4 would remove
        -- from combat, but this line empties the whole record a moment later
        -- regardless and nothing sits between the two to observe the difference.
        Monad.when (phase == Phase.Combat CombatStep.EndOfCombat) (State.modify' Combat.clearCombat)
        -- CR 508.8: drop the two combat steps that have nothing to do if nobody
        -- attacked. Asked as the declare attackers step ENDS, not when its
        -- turn-based action finishes, because the rule's condition has two
        -- clauses -- no creatures declared as attackers OR put onto the
        -- battlefield attacking -- and the second can only happen in the priority
        -- round this line sits after, an attack trigger (Hanweir Garrison)
        -- resolving. Asking earlier answered the first clause and assumed the
        -- second away (#30).
        --
        -- NOT guarded by hasActive: a turn with no active player declares no
        -- attackers, which is precisely CR 508.8's condition.
        --
        -- Order against the lines above is free -- they touch disjoint state --
        -- and this is before `advance`, which is what CR 500.11 needs.
        Monad.when (phase == Phase.Combat CombatStep.DeclareAttackers) (State.modify' Combat.skipEmptyCombat)
        checkSba
        stillFinished <- State.gets (Maybe.isJust . GameState.result)
        Monad.unless stillFinished advance

-- Whether THIS step grants a priority round. Every step but one answers from the
-- phase alone (Turn.grantsPriority); the cleanup step's answer is CR 514.3
-- qualified by CR 514.3a's exception, which is a question about the board and so
-- has to be asked here.
grantsPriorityNow :: Phase.Phase -> Game Bool
grantsPriorityNow phase = case phase of
  Phase.Ending EndingStep.Cleanup -> cleanupException
  _ -> pure (Turn.grantsPriority phase)

-- CR 514.3a: the cleanup step checks whether any state-based actions would be
-- performed or any triggered abilities are waiting to be put on the stack
-- (including those that trigger at the beginning of the next cleanup step). If
-- so, those are performed and placed, the active player gets priority, and once
-- the stack is empty and all players pass in succession another cleanup step
-- begins.
--
-- Checking and performing are ONE act here. `performSettle` is precisely CR
-- 117.5's loop, and it reports whether it did either. The rule reads as a
-- question asked before its own answer is acted on, but the two orders are
-- indistinguishable: the consequent performs exactly what the question asked
-- about, and nothing observes the board in between. CR 704.3's last sentence
-- states the same procedure from the other end.
--
-- Returns whether the exception fired, which is what the caller turns into a
-- priority round.
--
-- WHY THIS TERMINATES, given that a cleanup step can now schedule another one.
-- The chain advances only on new work: a successor is scheduled only when this
-- check found something, and the settle is idempotent. The turn-based actions the
-- successor re-runs cannot manufacture work either -- CR 514.1 finds the hand
-- already trimmed and CR 514.2 no marked damage and no "until end of turn" effect
-- left -- so a third step needs the SECOND one's priority round to have produced
-- something the first did not. The pool's producer for each half cannot: the
-- trigger half is CR 514.1's discard (Megrim), which fires only on the first
-- step, and the state-based half is CR 514.2 ending an "until end of turn" pump
-- (CR 704.5f), which the first step's settle takes to a fixpoint. So every game
-- this engine can play ends its turns in at most two cleanup steps.
--
-- It is NOT bounded in general, and Magic does not bound it either: an ability
-- that triggers at the beginning of each cleanup step loops forever, and CR
-- 104.4b's draw is the rules' answer to that rather than a bound on the loop.
-- That draw is what `checkMandatoryLoop` applies, so a chain no card in the pool
-- can build today would end the game rather than hang it -- heuristically, since
-- deciding it exactly is the halting problem. The bound above is still the reason
-- ordinary turns end in one or two cleanup steps; the guard is only the backstop
-- for the ones it does not cover.
cleanupException :: Game Bool
cleanupException = do
  fired <- performSettle
  Monad.when fired $
    -- The successor cleanup step is scheduled HERE, before the priority round
    -- rather than after it, exactly as CR 510.4's second combat damage step is
    -- (Turn.spliceSecondDamage).
    --
    -- The two placements are indistinguishable, which is why the existing
    -- precedent decides it rather than an argument. The only thing between here
    -- and `advance` that touches GameState.remaining is CR 500.8's phase splice
    -- (Turn.splicePhases), and it lands the added phases behind this step either
    -- way -- which is CR 500.8's "directly after the specified phase", the second
    -- cleanup step still being part of that ending phase. The paths that never
    -- reach `advance` discard the schedule wholesale regardless.
    State.modify' (\gs -> gs {GameState.remaining = Turn.spliceExtraCleanup (GameState.remaining gs)})
  pure fired

-- Ordinarily terminates because libraries are finite, each turn draws at most one
-- card, and drawing from an empty library is a loss (CR 704.5b). That argument
-- rests on the DRAW step being reached, and a CR 614.1b skip of it (runStep's
-- check above) suspends it, exactly as a real Stasis lock suspends a real game.
--
-- `checkMandatoryLoop` at this loop's head is the backstop for every repetition
-- the argument does not cover, and it is a HEURISTIC rather than a bound: CR
-- 104.4b's draw fires once events have repeated far past the point at which any
-- player could have chosen otherwise. A game two players CAN still act in is left
-- alone to run as long as they like, which is what that rule's second sentence
-- asks for -- so a Stasis lock is not ended by it either. `Pawl.GameSpec`'s
-- "a mandatory loop (CR 104.4b)" group holds both halves.
--
-- Fatigue is not yet a way to hang this loop: its skip is CR 614.10a's "next",
-- spent on one occurrence, so each copy postpones the draw by one turn rather
-- than stopping it. A card whose skip is unbounded -- a permanent's static, as
-- Eon Hub's upkeep skip is -- would be the one that hangs it.
--
-- CR 500.7's extra turns leave the argument intact: an extra turn reaches its own
-- draw step, and the schedule cannot refill itself, because GameState.extraTurns
-- is pushed only by a resolving effect and popped only by handoffTurn. The card
-- to re-examine this against would be one whose extra turn comes from an ability
-- that triggers every turn, which the pool does not have. A turn-scoped skip
-- (Pawl.Types.ExtraTurn's `skipped`, Savor the Moment) leaves it intact too: no
-- card names the DRAW step, and installTurnSkips arms each row Uses.Once and
-- Expiry.AtCleanup.
--
-- CR 514.3a's extra CLEANUP steps are the one repetition that does not go through
-- a draw step at all, so the library argument says nothing about them. They carry
-- their own termination argument, at `cleanupException`, and it is bounded for
-- the pool rather than in general -- the guard above is what covers the rest.
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

-- CR 729: play a subgame as a FUNCTION CALL. Construct the subgame from the
-- parent's library cards (CR 729.2, subgameStateFrom), then run its setup and
-- whole game LIFTED into the parent's StateT -- so the subgame's prompts flow
-- through the SAME Program interpreter and Replay fold, which is what keeps a
-- transcript replayable across one. They are not indistinguishable from the
-- parent's for all that: Asked.under below stamps every one of them with the
-- parent game, so an answerer can tell a subgame's question from a main-game
-- one (#153). The parent GameState sits untouched in the outer frame while the
-- subgame runs (CR 729.1a). At the end, funnel each owner's cards back to
-- their main-game library (CR 729.5) and reshuffle. A subgame within a subgame
-- (CR 729.6) is free: the nested playGame's own priorityLoop re-supplies
-- playSubgame.
--
-- Nesting terminates: subgameStateFrom draws each level's library from the
-- PARENT level's library zone at cast time (CR 729.2), already reduced by the
-- >= 7 cards its own opening hand consumed, so nesting depth is bounded by
-- roughly |library| / 7 (the CR 729.6 gate rests on this bound).
--
-- Cards brought into a subgame from the main game, and the main-game triggers
-- their removal queues, are not implemented (#152). Neither is CR 729.5's other
-- half, the command-zone residents moving into and back out of a subgame: none of
-- them exists to move, with one exception: COMMANDERS now do, and neither
-- direction carries them (#943). The five other kinds of command-zone card are
-- still unbuilt -- #933 (dungeons), #934 (planes and phenomena), #935 (schemes),
-- #936 (vanguards) and #937 (conspiracies).
playSubgame :: Game Result
playSubgame = do
  parent <- State.get
  -- CR 729.2: randomly determine which player goes first. The engine asks; the
  -- interpreter rolls. Only the players still in the main game are in the
  -- subgame (CR 729.4), so only they can be rolled. Not asked when the
  -- answer is forced -- a lone candidate goes first no matter what randomness
  -- says.
  starter <- case NonEmpty.nonEmpty (Game.stillPlayingInOrder parent) of
    Nothing -> pure (GameState.activePlayer parent)
    Just order -> case order of
      only NonEmpty.:| [] -> pure only
      _ -> do
        answer <- Game.ask (Prompt.RandomFirstPlayer order)
        -- Filtered, not trusted (#222): a subgame cannot start with a player who
        -- is not seated in it.
        pure (if List.elem answer (NonEmpty.toList order) then answer else NonEmpty.head order)
  let sub0 = Setup.subgameStateFrom starter parent
  -- CR 729.1a: every question the subgame raises passes outward through this
  -- frame, which is the one place that knows both games -- so this is where the
  -- parent is pushed onto the tag (#153). One pass over the whole subgame
  -- program, so a question from a nested subgame (CR 729.6) is wrapped once per
  -- level it climbs and arrives naming every game it is inside.
  --
  -- The CR 729.2 roll above is deliberately NOT wrapped: it is asked before the
  -- subgame's state exists, in the main game, which is the game whose player is
  -- being asked to roll.
  (result, finalSub) <-
    Trans.lift
      ( Program.mapProgram
          (Asked.under parent)
          (State.runStateT (Setup.startGameFromCards Resolve.performHandAction >> playGame) sub0)
      )
  State.modify' (Setup.funnelBack finalSub)
  -- CR 729.5: each player takes the traditional cards they own that are in the
  -- subgame, puts them into their main-game library and shuffles. Each player who
  -- was IN the subgame: a player outside it (CR 729.4) took nothing into it and
  -- is not asked to shuffle their main-game library.
  seated <- State.gets Game.stillPlayingInOrder
  Monad.forM_ seated Mulligan.shuffleLibrary
  pure result

playFrom :: NonEmpty.NonEmpty (PlayerId, Deck.Deck) -> Game Result
playFrom matchup = do
  Setup.newGame Resolve.performHandAction matchup
  playGame
