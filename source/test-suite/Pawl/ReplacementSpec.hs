{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers: Pawl.Engine.Replacement (the CR 616.1 loop, its buckets and its prompt) and
-- the funnels that raise proposed events through it. Mostly gameplay-level --
-- put a board together, cast or resolve, assert on game state -- but a case
-- reaches for a more direct construction whenever gameplay cannot produce the
-- exact shape the property under test needs. Where a case departs from
-- gameplay-level testing, it justifies itself at the point it happens, rather
-- than here.
module Pawl.ReplacementSpec where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Numeric.Natural as Natural
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Cast as Cast
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Damage as Damage
import qualified Pawl.Engine.Departure as Departure
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.FaceDown as FaceDown
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Keyword as Keyword.Engine
import qualified Pawl.Engine.Projection as Projection
-- Aliased Filter.Type, not Filter, per the project-wide convention (FilterSpec):
-- the evaluator module Pawl.Engine.Filter may later be imported and must not collide.

import qualified Pawl.Engine.Replacement as Replacement
import qualified Pawl.Engine.Replay as Replay
import qualified Pawl.Engine.Resolve as Resolve
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Engine.Turn as Turn
import qualified Pawl.Extra.Int as Int
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as Action
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.ActiveReplacement as ActiveReplacement
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Combat as Combat.Type
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.DamagePattern as DamagePattern
import qualified Pawl.Types.DamageR as DamageR
import qualified Pawl.Types.DamageRewrite as DamageRewrite
import qualified Pawl.Types.Daytime as Daytime
import qualified Pawl.Types.Departure as Departure.Type
import qualified Pawl.Types.DestructionCause as DestructionCause
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.DurationRef as DurationRef
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.EntryOption as EntryOption
import qualified Pawl.Types.EntryR as EntryR
import qualified Pawl.Types.EntryRewrite as EntryRewrite
import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.FaceDownReason as FaceDownReason
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.Game as Game.Type
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.KickerDecision as KickerDecision
import qualified Pawl.Types.LifeChange as LifeChange
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeSelection as ModeSelection
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect
import qualified Pawl.Types.ReplacementEntry as ReplacementEntry
import qualified Pawl.Types.ReplacementOrigin as ReplacementOrigin
import qualified Pawl.Types.Response as Response
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.StepBegan as StepBegan
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Timestamp as Timestamp
import qualified Pawl.Types.TurnUpProcedure as TurnUpProcedure
import qualified Pawl.Types.Uses as Uses
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChangePattern as ZoneChangePattern
import qualified Pawl.Types.ZoneChangeR as ZoneChangeR

-- Every answer the engine asked for, in order -- so a test can assert that a
-- prompt WAS raised (the engine did not decide) or was NOT (the choice was
-- indistinguishable and correctly elided).
answersFor :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> Game.Type.Game a -> [Response.Response]
answersFor answer gs game = snd (Replay.record answer gs game)

-- The single activated ability of a printing (Drudge Skeletons and Liquimetal
-- Coating each have exactly one). Total: the empty-ability fallback is
-- unreachable in this fixture.
-- Same shape as ActivateSpec.theAbility -- duplicated per this test suite's
-- existing convention of group-local helpers (ActivateSpec and ManaSpec
-- already duplicate singleModeAbility the same way) rather than centralizing
-- a helper this small in Support.
theAbility :: Printing.Printing -> ActivatedAbility.ActivatedAbility Card.Card
theAbility p = case Face.activatedAbilities (S.combinedFace p) of
  ab : _ -> ab
  [] -> ActivatedAbility.MkActivatedAbility (Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) []) (Modal.MkModal (Seq.singleton (Mode.MkMode Seq.empty Map.empty)) (ModeSelection.ChooseExactly 1)) [] Nothing

wasAskedToReplace :: [Response.Response] -> Bool
wasAskedToReplace responses =
  let isReplacement r = case r of
        Response.ChoseReplacement _ -> True
        _ -> False
   in any isReplacement responses

wasAskedForEntryOption :: [Response.Response] -> Bool
wasAskedForEntryOption responses =
  let isEntryOption r = case r of
        Response.ChoseEntryOption _ -> True
        _ -> False
   in any isEntryOption responses

-- alice controls one Forest plus `mine`; bob controls `theirs`; alice holds one
-- Battlegrowth ({G} instant: put a +1/+1 counter on target creature). Returns the
-- state, Battlegrowth's hand id, and the two id lists in the order given.
counterBoard :: Printing.Printing -> Printing.Printing -> [Printing.Printing] -> [Printing.Printing] -> (GameState.GameState, ObjectId.ObjectId, [ObjectId.ObjectId], [ObjectId.ObjectId])
counterBoard forest battlegrowth mine theirs =
  let addAll pid ps gs =
        List.foldl'
          (\(ids, g) p -> let (oid, g1) = S.addCreature p pid g in (ids <> [oid], g1))
          ([], gs)
          ps
      (ours, gs1) = addAll S.alice mine (S.landsInPlay forest 1)
      (yours, gs2) = addAll S.bob theirs gs1
      (gs3, spellId) = S.handOne battlegrowth gs2
   in (gs3, spellId, ours, yours)

-- Aim every target slot at `victim`, and answer a CR 616.1 race by picking the
-- candidate whose SOURCE is `preferred` -- by id, so the assertion does not
-- depend on the engine's canonical candidate order.
raceAnswer :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
raceAnswer preferred victim p = case p of
  Prompt.ChooseReplacement _ _ entries -> maybe 0 Int.toNaturalSaturating (List.findIndex ((== preferred) . ReplacementEntry.source) entries)
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToCreature victim))) sets
  _ -> S.identityAnswer p

-- Aim every target slot at one object. Recipient.ToObject, not ToCreature as
-- raceAnswer above uses: both slots this answers -- Liquimetal Coating's and
-- Skilled Animator's -- are Pool.Permanents, and a recipient tagged for the wrong
-- pool is not in the legal set at all.
aimObject :: ObjectId.ObjectId -> Prompt.Prompt r -> r
aimObject oid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToObject oid))) sets
  _ -> S.identityAnswer p

-- What each COUNTDOWN shield on the board has left (CR 615.7), read off the
-- floating rows themselves. An empty list is a shield spent to 0 and dropped,
-- which is why the count of rows would not say the same thing; a Fog-shaped or
-- counter-backed prevention does not appear here at all, having no amount on its
-- row to report.
shieldsLeft :: GameState.GameState -> [Natural.Natural]
shieldsLeft gs =
  let remaining re = case re of
        ReplacementEffect.DamageR (DamageR.MkDamageR _ (DamageRewrite.PreventNext n) _) -> Just n
        _ -> Nothing
   in Maybe.mapMaybe (remaining . ActiveReplacement.effect) (GameState.replacements gs)

-- Was CR 615.7's batch-order question raised at all? The elision half of every
-- group below asserts the negative of this, so the boards that ask nothing are
-- told apart from the boards that ask.
wasAskedToOrderDamage :: [Response.Response] -> Bool
wasAskedToOrderDamage =
  let isOrder r = case r of
        Response.OrderedDamage _ -> True
        _ -> False
   in any isOrder

countersOn :: CounterKind.CounterKind Keyword.Keyword -> ObjectId.ObjectId -> GameState.GameState -> Natural.Natural
countersOn kind oid gs =
  maybe 0 (Map.findWithDefault 0 kind . Object.counters) (Game.lookupObject oid gs)

castAndResolve :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> ObjectId.ObjectId -> GameState.GameState
castAndResolve answer gs spellId =
  S.runPure answer gs (S.cast S.alice spellId >> Stack.resolveTop)

-- castAndResolve over several of alice's spells in order. Top-level rather than
-- a `where` binding because the answer is rank-2 and GHC will not infer it.
castEach :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> [ObjectId.ObjectId] -> GameState.GameState
castEach answer = List.foldl' (castAndResolve answer)

-- Copy `wanted` when it is offered, decline otherwise.
copyOf :: ObjectId.ObjectId -> Prompt.Prompt r -> r
copyOf wanted p = case p of
  Prompt.ChooseCopyTarget _ _ _ legal -> if List.elem wanted legal then Just wanted else Nothing
  _ -> S.identityAnswer p

-- alice controls `n` untapped Islands in a main phase with priority, holding one
-- card of each printing in `hand`. Returns the state and the hand ids in order --
-- unlike S.handOne, which replaces the whole hand.
blueBoard :: Printing.Printing -> Int -> [Printing.Printing] -> (GameState.GameState, [ObjectId.ObjectId])
blueBoard island n hand =
  let base = S.landsInPlay island n
      addOne (ids, g) p = let (oid, g1) = S.addHandCard p S.alice g in (ids <> [oid], g1)
      (held, gs) = List.foldl' addOne ([], base) hand
   in ( gs
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          },
        held
      )

-- Pick entry option `which`, and copy the highest-id legal creature when offered.
enteringAs :: Natural.Natural -> Prompt.Prompt r -> r
enteringAs which p = case p of
  Prompt.ChooseEntryOption {} -> which
  Prompt.ChooseCopyTarget _ _ _ legal -> Maybe.listToMaybe (List.sortOn Ord.Down legal)
  _ -> S.identityAnswer p

-- The newest battlefield object whose printed card has this name.
newestNamed :: CardName.CardName -> GameState.GameState -> Maybe ObjectId.ObjectId
newestNamed wanted gs =
  let named oid = fmap Face.name (Game.faceOf oid gs) == Just wanted
   in Maybe.listToMaybe (List.sortOn Ord.Down (filter named (Set.toList (GameState.battlefield gs))))

-- Leyline of the Void's redirect, as a floating replacement: any card headed for
-- an OPPONENT's graveyard is exiled instead. CR 400.3 makes that graveyard the
-- card's OWNER's, which is what Replacement.matchesZoneOwner tests.
leylineShape :: ObjectId.ObjectId -> Timestamp.Timestamp -> ActiveReplacement.ActiveReplacement
leylineShape src ts =
  ActiveReplacement.MkActiveReplacement
    { ActiveReplacement.effect =
        ReplacementEffect.ZoneChangeR (ZoneChangeR.MkZoneChangeR (ZoneChangePattern.MkZoneChangePattern (Just Zone.Graveyard) ControllerRelation.Opponents (Filter.Type.And [])) Zone.Exile),
      ActiveReplacement.source = src,
      ActiveReplacement.controller = S.alice,
      ActiveReplacement.timestamp = ts,
      ActiveReplacement.expiry = Expiry.Never,
      ActiveReplacement.uses = Uses.Unlimited,
      ActiveReplacement.origin = ReplacementOrigin.Other,
      ActiveReplacement.condition = Nothing,
      ActiveReplacement.rider = Nothing,
      ActiveReplacement.slots = Map.empty
    }

-- Eon Hub {5} Artifact: "Players skip their upkeep steps."
--
-- CR 614.1b: "Effects that use the word 'skip' are replacement effects. These
-- replacement effects use the word 'skip' to indicate what events, steps,
-- phases, or turns will be replaced with nothing." So this is P5's carrier, not
-- a CR 613.11 rules-modifying continuous effect.
--
-- Sarcomancy is the discriminating observable. Its second ability is "at the
-- beginning of your upkeep, if there are no Zombies on the battlefield, this
-- enchantment deals 1 damage to you" (CR 603.2b), matched against the
-- GameEvent.StepBegan that Engine.runStep records as a step begins. A step
-- REPLACED WITH NOTHING records no such event, so the ability never triggers
-- (CR 614.6: "if an event is replaced, it never happens") -- as distinct from
-- triggering and resolving to nothing, which would still put an object on the
-- stack and still take the life. Each case below asserts on BOTH the event log
-- and the life total, so the two outcomes cannot be confused.
--
-- Sarcomancy is placed straight onto the battlefield, so its enters-trigger
-- never resolves and no Zombie token exists: CR 603.4's intervening "if" holds
-- and the upkeep ability really would fire.
stepSkipSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
stepSkipSpec s registry = Spec.describe s "Skip" $ do
  let untap = Phase.Beginning BeginningStep.Untap
      upkeep = Phase.Beginning BeginningStep.Upkeep
      drawStep = Phase.Beginning BeginningStep.DrawStep
      -- Alice's turn, positioned at her untap step with only the upkeep and draw
      -- steps left to schedule. Short on purpose: an empty schedule would hand
      -- the turn off and clear the event log these cases read, and the draw step
      -- is never entered, so nothing draws from the empty library.
      atUntap printings =
        let place g p = snd (S.addCreature p S.alice g)
            placed = List.foldl' place (Setup.emptyGame S.bothPlayers) printings
         in placed
              { GameState.phase = untap,
                GameState.activePlayer = S.alice,
                GameState.remaining = Seq.fromList [upkeep, drawStep]
              }
      -- The untap step, then whatever the schedule says comes next.
      twoSteps = do
        Engine.runStep
        Engine.runStep
      runTwo gs = snd (Engine.runGamePure S.identityAnswer gs twoSteps)
      began step gs = List.elem (GameEvent.StepBegan (StepBegan.MkStepBegan step S.alice)) (S.eventsOf gs)
  -- The control. Without Eon Hub the upkeep step begins normally, so the
  -- trigger fires and resolves.
  Spec.it s "CR 500.6 without a skip the upkeep step begins and its trigger fires" $ do
    sarcomancy <- S.printingOf s registry "Sarcomancy"
    let after = runTwo (atUntap [sarcomancy])
    Spec.assertBool s (began untap after) "the untap step began"
    Spec.assertBool s (began upkeep after) "the upkeep step began"
    Spec.assertEqWith s "alice took 1 from the trigger" (S.lifeOf S.alice after) (Just 19)
    Spec.assertEqWith s "and the draw step is next" (GameState.phase after) drawStep
  -- Eon Hub is BOB's, and it is ALICE's upkeep being skipped: "players
  -- skip THEIR upkeep steps" is symmetric, so the effect is not scoped to
  -- its controller.
  Spec.it s "CR 614.1b Eon Hub replaces the upkeep step with nothing" $ do
    sarcomancy <- S.printingOf s registry "Sarcomancy"
    eonHub <- S.printingOf s registry "Eon Hub"
    let base = atUntap [sarcomancy]
        armed = snd (S.addCreature eonHub S.bob base)
        after = runTwo armed
    Spec.assertBool s (began untap after) "the untap step still began"
    Spec.assertBool s (not (began upkeep after)) "the upkeep step never began"
    Spec.assertEqWith s "so nothing ever reached the stack" (GameState.stack after) []
    Spec.assertEqWith s "and alice took no damage" (S.lifeOf S.alice after) (Just 20)
    -- CR 500.11: "to skip a step, phase, or turn is to proceed past it as
    -- though it didn't exist" -- past it, not past the rest of the turn.
    Spec.assertEqWith s "the turn proceeded to the draw step" (GameState.phase after) drawStep
    Spec.assertEqWith s "having consumed exactly that one step" (GameState.remaining after) Seq.empty

-- Aim every target slot at one player. The player-side twin of `aimObject`
-- above; Fatigue's slot is Pool.Players, so a recipient tagged for any other
-- pool is not in its legal set at all.
aimPlayer :: PlayerId.PlayerId -> Prompt.Prompt r -> r
aimPlayer pid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToPlayer pid))) sets
  _ -> S.identityAnswer p

-- Fatigue {1}{U} Sorcery: "Target player skips their next draw step."
--
-- The three things Eon Hub above does not do, and this card does:
--
--   1. The skip is created by an EFFECT. Nothing on the battlefield carries it --
--      Fatigue is in a graveyard by the time the skip matters -- so it lives in
--      GameState.replacements, the floating store CR 614.3 describes as lasting
--      "until they're used up".
--   2. It is scoped to ONE player (PhasePattern.whosePhase), where Eon Hub's
--      "players skip their upkeep steps" is symmetric.
--   3. CR 614.10a: it is CONSUMED after one occurrence, and two of them
--      ACCUMULATE rather than coalescing -- "if two effects each cause a player
--      to skip their next occurrence, that player must skip the next two".
--
-- The observables are the same pair the Eon Hub cases use, read together so a
-- skipped step cannot be confused with a step that happened and drew nothing:
-- the CR 603.2b StepBegan record (CR 614.6, "if an event is replaced, it never
-- happens"), and alice's library, which a real draw step empties by one.
fatigueSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
fatigueSpec s registry = Spec.describe s "Fatigue" $ do
  let drawStep = Phase.Beginning BeginningStep.DrawStep
      -- alice's turn, positioned at her draw step with the precombat main phase
      -- scheduled after it. Not an empty schedule: that hands the turn off, which
      -- would clear the event log these cases count and move the active player.
      --
      -- CR 103.8a's first-turn skip is a TURN-BASED action (Engine.skipsDraw),
      -- not a replacement effect, and it would swallow the control case's draw;
      -- turn 2 puts it out of the way, which is also what makes this the SECOND
      -- turn's draw step -- exactly the "next" one Fatigue named.
      atDraw gs =
        gs
          { GameState.phase = drawStep,
            GameState.activePlayer = S.alice,
            GameState.remaining = Seq.singleton Phase.PrecombatMain,
            GameState.turnNumber = 2
          }
      -- One draw step. Applied repeatedly, each call is alice's NEXT draw step:
      -- the store under test is not turn-scoped (Expiry.Never, and no sweep ends
      -- it -- every Pawl.Engine.Expiry sweep keeps a Never), so what a real
      -- intervening turn would contribute is a longer log, not a different
      -- answer.
      runDraw gs = snd (Engine.runGamePure S.identityAnswer (atDraw gs) Engine.runStep)
      begun gs = length (filter (== GameEvent.StepBegan (StepBegan.MkStepBegan drawStep S.alice)) (S.eventsOf gs))
      libraryOf pid gs = length (Game.zoneMembers Zone.Library pid gs)
      armed gs = length (GameState.replacements gs)
      -- alice: two Islands per Fatigue to cast, a stocked library to draw from,
      -- and `n` Fatigues in hand. Only alice's draw step is ever run below, so
      -- only her library needs stocking.
      board island piker fatigue n =
        let (base, held) = blueBoard island (2 * n) (replicate n fatigue)
            stocked = List.foldl' (\g _ -> snd (S.addLibraryCard piker S.alice g)) base [1 .. (5 :: Int)]
         in (stocked, held)
  -- The control: the same board with the spell never cast. Without it
  -- alice's draw step begins and draws, which is what every case below is
  -- measured against.
  Spec.it s "CR 500.6 without a skip alice's draw step begins and draws" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    fatigue <- S.printingOf s registry "Fatigue"
    let (gs, _) = board island piker fatigue 1
        after = runDraw gs
    Spec.assertEqWith s "the draw step began" (begun after) 1
    Spec.assertEqWith s "and took a card off the library" (libraryOf S.alice after) 4
  -- CR 614.1b / 614.10a: one Fatigue takes exactly ONE draw step, and is
  -- gone afterwards. The second half is what distinguishes this from Eon
  -- Hub, whose skip is re-derived from the battlefield every time and so
  -- never runs out.
  Spec.it s "CR 614.10a one Fatigue skips one draw step, and the next one draws" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    fatigue <- S.printingOf s registry "Fatigue"
    let (gs, held) = board island piker fatigue 1
        cast = castEach (aimPlayer S.alice) gs held
    Spec.assertEqWith s "the resolution armed one floating skip" (armed cast) 1
    let first_ = runDraw cast
    Spec.assertEqWith s "the draw step never began" (begun first_) 0
    Spec.assertEqWith s "so nothing was drawn" (libraryOf S.alice first_) 5
    Spec.assertEqWith s "and the skip was used up (CR 614.3)" (armed first_) 0
    let second = runDraw first_
    Spec.assertEqWith s "the following draw step began" (begun second) 1
    Spec.assertEqWith s "and drew" (libraryOf S.alice second) 4
  -- THE PROVING CASE. CR 614.10a: "if two effects each cause a player to
  -- skip their next occurrence, that player must skip the next two; one
  -- effect will be satisfied in skipping the first occurrence, while the
  -- other will remain until another occurrence can be skipped."
  --
  -- Fails against any store that treats a skip as a fact about a player
  -- rather than as a countable instance -- a Set of patterns, a Boolean
  -- flag, or a single Maybe -- all of which coalesce the two into one and
  -- let the second draw step happen.
  Spec.it s "CR 614.10a two Fatigues skip two draw steps, not one" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    fatigue <- S.printingOf s registry "Fatigue"
    let (gs, held) = board island piker fatigue 2
        cast = castEach (aimPlayer S.alice) gs held
    Spec.assertEqWith s "two resolutions armed two floating skips" (armed cast) 2
    let first_ = runDraw cast
    Spec.assertEqWith s "the first draw step never began" (begun first_) 0
    Spec.assertEqWith s "and exactly one skip was spent" (armed first_) 1
    let second = runDraw first_
    Spec.assertEqWith s "nor did the second" (begun second) 0
    Spec.assertEqWith s "which spent the other" (armed second) 0
    let third = runDraw second
    Spec.assertEqWith s "the third began" (begun third) 1
    Spec.assertEqWith s "and it is the only card drawn across all three" (libraryOf S.alice third) 4
    -- CR 616.1's choice is elided, not made: the two candidates are EQUAL
    -- AS VALUES, so every order of applying them leaves the same board
    -- (one instance spent, one waiting). See Replacement.choose.
    Spec.assertBool
      s
      (not (wasAskedToReplace (answersFor S.identityAnswer (atDraw cast) Engine.runStep)))
      "and the engine chose nothing: two equal skips are indistinguishable"
  -- The "whose" dimension, read the discriminating way round: bob is
  -- named, so ALICE's draw step is untouched and the skip is still
  -- waiting afterwards. A skip that ignored PhasePattern.whosePhase --
  -- which is what every skip in the pool did before this card -- would
  -- take alice's step here and spend itself doing it.
  Spec.it s "CR 614.1b a Fatigue aimed at bob leaves alice's draw step alone" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    fatigue <- S.printingOf s registry "Fatigue"
    let (gs, held) = board island piker fatigue 1
        cast = castEach (aimPlayer S.bob) gs held
        after = runDraw cast
    Spec.assertEqWith s "alice's draw step began" (begun after) 1
    Spec.assertEqWith s "and drew" (libraryOf S.alice after) 4
    Spec.assertEqWith s "bob's skip is still armed, waiting for his own turn" (armed after) 1

-- The turn's schedule after the precombat main phase, so a board positioned in
-- that phase still runs its own combat.
afterPrecombatMain :: Seq.Seq Phase.Phase
afterPrecombatMain = Seq.drop 1 (Seq.dropWhileL (/= Phase.PrecombatMain) (Seq.fromList Turn.allPhases))

-- Run whole steps until `done` holds of the board, the game ends, or the bound
-- runs out. The bound is three turns' worth of steps, so a skip that dropped
-- more of the schedule than it should still terminates and fails an assertion
-- rather than hanging.
runUntil :: (GameState.GameState -> Bool) -> (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
runUntil done answer gs0 =
  let go n g =
        if n <= (0 :: Int) || done g || Maybe.isJust (GameState.result g)
          then g
          else go (n - 1) (snd (Engine.runGamePure answer g Engine.runStep))
   in go 40 gs0

-- Run whole steps until the board reaches its postcombat main phase. Top-level
-- rather than a `where` binding because the answer is rank-2 and GHC will not
-- infer it -- the same reason castEach above is.
atPostcombatMain :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
atPostcombatMain = runUntil ((== Phase.PostcombatMain) . GameState.phase)

-- Run whole steps until the turn hands off, leaving the board at the first step
-- of the next turn.
nextTurn :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
nextTurn answer gs = runUntil ((/= GameState.turnNumber gs) . GameState.turnNumber) answer gs

-- Attacks with everything, blocks with nothing, and aims every target slot at
-- `victim`. Blocks are declined so an attack's damage lands on the defending
-- PLAYER -- the observable a skipped combat phase removes. Never casts, which
-- is what makes it the control.
skirmishAnswer :: PlayerId.PlayerId -> Prompt.Prompt r -> r
skirmishAnswer victim p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToPlayer victim))) sets
  Prompt.DeclareAttackers _ _ ids -> ids
  Prompt.DeclareBlockers {} -> Map.empty
  _ -> S.identityAnswer p

-- skirmishAnswer, plus casting whatever is castable. alice's hand holds exactly
-- Stonehorn Dignitary and both libraries hold only lands, so this casts that one
-- card and nothing else.
castingSkirmishAnswer :: PlayerId.PlayerId -> Prompt.Prompt r -> r
castingSkirmishAnswer victim p = case p of
  Prompt.ChooseAction _ _ actions ->
    let isCast a = case a of
          Action.Cast {} -> True
          _ -> False
     in case filter isCast actions of
          h : _ -> h
          [] -> Action.Pass
  _ -> skirmishAnswer victim p

-- Cast whatever is offered, and otherwise pass. Read by the CR 614.1d case
-- below, where the one castable card in the game is the {B} creature whose
-- castability that case measures -- so "whatever is offered" is that card or
-- nothing at all.
castOrPassAnswer :: Prompt.Prompt r -> r
castOrPassAnswer p = case p of
  Prompt.ChooseAction _ _ actions ->
    let isCast a = case a of
          Action.Cast {} -> True
          _ -> False
     in case filter isCast actions of
          h : _ -> h
          [] -> Action.Pass
  _ -> S.identityAnswer p

-- castOrPassAnswer's sibling for the CR 614.1c pay-life-or-enter-tapped cases:
-- play the land, and answer its as-enters "you may pay N life" the given way.
-- Every other prompt falls through to S.playLandAnswer, so the two cases differ
-- in the OptionalDecision and in nothing else.
payLifeOnEntryAnswer :: OptionalDecision.OptionalDecision -> Prompt.Prompt r -> r
payLifeOnEntryAnswer decision p = case p of
  Prompt.ChoosePayLifeOnEntry {} -> decision
  _ -> S.playLandAnswer p

-- alice's precombat main phase with the stack empty, the modal double-faced card
-- and the {W} creature in hand, and NOTHING else in the game: the land face is
-- the only mana source there will be, which is what makes the creature's fate
-- read the land's tap state.
razorgrassBoard :: Printing.Printing -> Printing.Printing -> GameState.GameState
razorgrassBoard razorgrass warden =
  let (_, withField) = S.addHandCard razorgrass S.alice (Setup.emptyGame S.bothPlayers)
      (_, filled) = S.addHandCard warden S.alice withField
   in filled
        { GameState.phase = Phase.PrecombatMain,
          GameState.activePlayer = S.alice,
          GameState.priority = Just S.alice
        }

-- How many Soul Wardens made it to the battlefield.
wardenOut :: GameState.GameState -> Int
wardenOut gs =
  let wardenName = CardName.MkCardName (Text.pack "Soul Warden")
   in length [o | o <- Set.toList (GameState.battlefield gs), Projection.hasName wardenName o gs]

-- razorgrassBoard's sibling for Sea Gate, Reborn, parameterized by alice's life
-- total: the modal double-faced card and the {U} creature in hand, and NOTHING
-- else in the game. The life total is the ONLY axis, so a pair of boards built
-- here differ in it and in nothing else.
seaGateBoard :: Printing.Printing -> Printing.Printing -> Integer -> GameState.GameState
seaGateBoard seaGate warrior life =
  let (_, withGate) = S.addHandCard seaGate S.alice (Setup.emptyGame S.bothPlayers)
      (_, filled) = S.addHandCard warrior S.alice withGate
   in filled
        { GameState.phase = Phase.PrecombatMain,
          GameState.activePlayer = S.alice,
          GameState.priority = Just S.alice,
          GameState.players = Map.adjust (\p -> p {Player.life = life}) S.alice (GameState.players filled)
        }

-- How many Tidal Warriors made it to the battlefield.
warriorOut :: GameState.GameState -> Int
warriorOut gs =
  let warriorName = CardName.MkCardName (Text.pack "Tidal Warrior")
   in length [o | o <- Set.toList (GameState.battlefield gs), Projection.hasName warriorName o gs]

-- payLifeOnEntryAnswer's sibling for the CR 614.1c REVEAL cases: play the land,
-- and answer its "you may reveal a Kithkin card from your hand" with this card.
-- PINNED, never a search of the offered list: an answerer that picked whatever
-- the engine offered would find a legal card again after a mutation widened the
-- offer, and the case would stay green while the engine's own filter was broken.
revealOnEntryAnswer :: Maybe ObjectId.ObjectId -> Prompt.Prompt r -> r
revealOnEntryAnswer shown p = case p of
  Prompt.ChooseRevealOnEntry {} -> shown
  _ -> S.playLandAnswer p

-- How many times the as-enters reveal was put to a player.
revealAsks :: [Response.Response] -> Int
revealAsks responses =
  let isReveal r = case r of
        Response.ChoseRevealOnEntry _ -> True
        _ -> False
   in length (filter isReveal responses)

-- alice's precombat main phase with the stack empty, Rustic Clachan and ONE {W}
-- creature in hand, and NOTHING else in the game: the land's "{T}: Add {W}" is
-- the only mana there will be, so the creature's fate reads the land's tap state
-- as Soul Warden's does for razorgrassBoard above. The creature is the only axis
-- a pair of boards from here differ on -- Mosquito Guard and Benalish Hero are
-- both {W} 1/1 Soldiers, and one is a Kithkin.
clachanBoard :: Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, GameState.GameState)
clachanBoard clachan creature =
  let (_, withLand) = S.addHandCard clachan S.alice (Setup.emptyGame S.bothPlayers)
      (creatureId, filled) = S.addHandCard creature S.alice withLand
   in ( creatureId,
        filled
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- How many permanents with this name made it to the battlefield.
namedOut :: String -> GameState.GameState -> Int
namedOut name gs =
  let cardName = CardName.MkCardName (Text.pack name)
   in length [o | o <- Set.toList (GameState.battlefield gs), Projection.hasName cardName o gs]

-- Whether a life loss of exactly this size, by this player, was RECORDED -- the
-- channel a card that watches for life loss reads, and the half of CR 119.4 a
-- bare subtraction from the life total would not satisfy.
lostLife :: PlayerId.PlayerId -> Natural.Natural -> GameState.GameState -> Bool
lostLife pid n gs = GameEvent.LifeLost (LifeChange.MkLifeChange pid n) `elem` S.eventsOf gs

-- Stonehorn Dignitary {3}{W} Creature -- Rhino Soldier 1/4: "When this creature
-- enters, target opponent skips their next combat phase." (oracle checked on
-- Scryfall)
--
-- The pool's first skip of a phase that HAS steps. CR 500.1: "The beginning,
-- combat, and ending phases are further broken down into steps, which proceed in
-- order" -- so what this card names is not one entry of the turn's schedule, the
-- way Eon Hub's upkeep step and Fatigue's draw step are, but the whole of CR
-- 506.1's five.
--
-- CR 500.11: "to skip a step, phase, or turn is to proceed past it as though it
-- didn't exist" -- past the PHASE, so no step of it begins and the turn carries
-- on at the postcombat main phase, which is what CR 500.1's order puts next.
--
-- Everything Fatigue proved about a skip's LIFETIME rides along unchanged: the
-- skip is created by an effect, scoped to the player its resolution named, and
-- consumed by one occurrence (CR 614.10a).
stonehornSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
stonehornSpec s registry = Spec.describe s "Stonehorn Dignitary" $ do
  let -- alice in her precombat main phase on turn 2, holding Stonehorn Dignitary
      -- with four untapped Plains (exactly {3}{W}); bob has one Settled Goblin
      -- Piker, whose attack is what the skip must prevent. Both libraries hold
      -- five lands, so the draw steps this fixture runs through never reach CR
      -- 704.5b, and neither player can cast anything off the top.
      --
      -- Turn 2, so CR 103.8a's first-turn draw skip is out of the way.
      board plains stonehorn piker =
        let (_, gs1) = S.addCreature piker S.bob (S.landsInPlay plains 4)
            stock g pid = List.foldl' (\h _ -> snd (S.addLibraryCard plains pid h)) g [1 .. (5 :: Int)]
            (gs2, held) = S.handOne stonehorn (stock (stock gs1 S.alice) S.bob)
         in ( gs2
                { GameState.remaining = afterPrecombatMain,
                  GameState.turnNumber = 2
                },
              held
            )
      -- CR 603.2b: the steps of `pid`'s turn that actually BEGAN. A skipped step
      -- never appears, which is CR 614.6's "if an event is replaced, it never
      -- happens" -- and is why this is read at the postcombat main phase rather
      -- than after the turn, since Engine.handoffTurn clears the log.
      stepsBegunBy pid gs = [ph | GameEvent.StepBegan (StepBegan.MkStepBegan ph who) <- S.eventsOf gs, who == pid]
      combatStepsOf pid gs = [ph | ph@(Phase.Combat _) <- stepsBegunBy pid gs]
      armed gs = length (GameState.replacements gs)
  -- The control: the same board with the creature never cast. bob's combat
  -- phase runs all five of CR 506.1's steps and his Piker takes two off
  -- alice, which is what every case below is measured against.
  Spec.it s "CR 506.1 without a skip bob's combat phase runs and his Piker connects" $ do
    plains <- S.printingOf s registry "Plains"
    stonehorn <- S.printingOf s registry "Stonehorn Dignitary"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _) = board plains stonehorn piker
        bobsTurn = nextTurn (skirmishAnswer S.bob) gs
        mid = atPostcombatMain (skirmishAnswer S.bob) bobsTurn
    Spec.assertEqWith s "all five combat steps began" (length (combatStepsOf S.bob mid)) 5
    Spec.assertEqWith s "and the Piker's two damage landed" (S.lifeOf S.alice mid) (Just 18)
  -- THE PROVING CASE. CR 500.11 / 614.1b: the whole combat phase is
  -- replaced with nothing, so NO step of it begins -- not merely the
  -- beginning of combat step the boundary question is asked at. A
  -- pattern that named one step would leave the other four running, and
  -- bob's Piker would still be declared.
  Spec.it s "CR 500.11 the named opponent's whole combat phase is skipped, every step of it" $ do
    plains <- S.printingOf s registry "Plains"
    stonehorn <- S.printingOf s registry "Stonehorn Dignitary"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _) = board plains stonehorn piker
        bobsTurn = nextTurn (castingSkirmishAnswer S.bob) gs
        mid = atPostcombatMain (castingSkirmishAnswer S.bob) bobsTurn
    Spec.assertEqWith s "no combat step began at all" (combatStepsOf S.bob mid) []
    Spec.assertEqWith s "so the Piker never attacked" (S.attackerDeclarationsOf mid) []
    Spec.assertEqWith s "and alice took nothing" (S.lifeOf S.alice mid) (Just 20)
    -- CR 500.1 fixes the order of the five phases, so the postcombat
    -- main phase is what follows combat; CR 500.11's "proceed past it"
    -- is past the PHASE and no further.
    Spec.assertEqWith s "the turn proceeded to the postcombat main phase" (GameState.phase mid) Phase.PostcombatMain
    Spec.assertEqWith s "and the skip was used up (CR 614.3)" (armed mid) 0
  -- CR 614.10a: "anything scheduled for the 'next' occurrence of something
  -- waits for the first occurrence that isn't skipped" -- ONE occurrence,
  -- so bob's following combat phase is his own again.
  Spec.it s "CR 614.10a one Stonehorn skips one combat phase, and the next one happens" $ do
    plains <- S.printingOf s registry "Plains"
    stonehorn <- S.printingOf s registry "Stonehorn Dignitary"
    piker <- S.printingOf s registry "Goblin Piker"
    let answer = castingSkirmishAnswer S.bob
        (gs, _) = board plains stonehorn piker
        bobsTurn = nextTurn answer gs
        alicesTurn = nextTurn answer bobsTurn
        bobsSecondTurn = nextTurn answer alicesTurn
        mid = atPostcombatMain answer bobsSecondTurn
    Spec.assertEqWith s "bob is active again" (GameState.activePlayer bobsSecondTurn) S.bob
    Spec.assertEqWith s "all five combat steps began this time" (length (combatStepsOf S.bob mid)) 5
    Spec.assertEqWith s "and the Piker connected" (S.lifeOf S.alice mid) (Just 18)
  -- The "whose" dimension, read the discriminating way round. The skip is
  -- installed during ALICE's precombat main phase, one phase before her
  -- own combat phase -- so a whole-phase skip that ignored
  -- PhasePattern.whosePhase would eat alice's combat immediately, and
  -- spend itself doing it.
  Spec.it s "CR 614.1b a Stonehorn aimed at bob leaves alice's own combat phase alone" $ do
    plains <- S.printingOf s registry "Plains"
    stonehorn <- S.printingOf s registry "Stonehorn Dignitary"
    piker <- S.printingOf s registry "Goblin Piker"
    let answer = castingSkirmishAnswer S.bob
        (gs, _) = board plains stonehorn piker
        mid = atPostcombatMain answer gs
    Spec.assertBool s (not (null (combatStepsOf S.alice mid))) "alice's combat phase began"
    Spec.assertEqWith s "bob's skip is still armed, waiting for his own turn" (armed mid) 1

-- CR 615.7's prevention shield, whose plainest producer in data/cards/ is Mending
-- Hands ({W} Instant: "Prevent the next 4 damage that would be dealt to any
-- target this turn") -- the same countdown shield as Healing Grace below, minus
-- CR 609.7a's chosen source.
--
-- Three properties, and they are the three halves of the rule: the shield is
-- spent in DAMAGE rather than in events ("such effects count only the amount of
-- damage; the number of events or sources dealing it doesn't matter"), it is
-- scoped to the recipient it shields, and where two simultaneous sources contend
-- for it the shielded side chooses which damage it prevents rather than the
-- engine.
--
-- The group's last case is not about CR 615.7 at all: this card is the pool's
-- floating replacement that grants nobody CONTROL, which is what makes it the
-- discriminating twin for CR 800.4a's second clause
-- (Departure.givesControlOnEntryTo). It says why it is here.
--
-- The DAMAGE BATCHES below are hand-built and the SPELL is not: casting Mending
-- Hands for real is what proves the card, and reaching a real combat-damage batch
-- of two attackers with different powers would mean driving a whole combat phase
-- to produce a fixture these assertions read straight off. The same split, and
-- the same reason, as the Fog case in the group above.
mendingHandsSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
mendingHandsSpec s registry = Spec.describe s "Mending Hands (CR 615.7)" $ do
  let -- One noncombat damage event, from `src`, at `n`.
      hit src recipient n =
        DamageEvent.MkDamageEvent src recipient n False False False 0 Nothing DamageKind.Noncombat
      amounts gs = fmap DamageEvent.amount (S.damageEventsOf gs)
      -- Order a contested batch by preferring the event from `src`, by SOURCE id
      -- rather than by position, so the assertion does not depend on the order
      -- the batch was gathered in.
      shieldFirst :: ObjectId.ObjectId -> Prompt.Prompt r -> r
      shieldFirst src p = case p of
        Prompt.OrderDamage _ _ events ->
          let key e = (DamageEvent.source e /= src, DamageEvent.source e)
           in fmap fst (List.sortOn (key . snd) (zip [0 ..] events))
        _ -> S.identityAnswer p
  -- CR 615.7's arithmetic, one event at a time: the shield takes what it can of
  -- each event and reduces by exactly that much. Three 3-damage events against a
  -- shield of 4 -- so the first is prevented whole, the second only partly, and
  -- the third not at all.
  Spec.it s "CR 615.7 the shield counts DAMAGE, not events: 4 covers one 3 and part of the next" $ do
    plains <- S.printingOf s registry "Plains"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    mendingHands <- S.printingOf s registry "Mending Hands"
    let base = S.landsInPlay plains 1
        (victim, g1) = S.addCreature pikerPrinting S.alice base
        (attacker, g2) = S.addCreature pikerPrinting S.bob g1
        (g3, spellId) = S.handOne mendingHands g2
        shielded = castAndResolve (aimCreature victim) g3 spellId
        strike g = S.runPure S.identityAnswer g (Damage.applyDamage [hit attacker (Recipient.ToCreature victim) 3])
        once = strike shielded
        twice = strike once
        thrice = strike twice
    Spec.assertEqWith s "setup: the shield is a floating replacement" (length (GameState.replacements shielded)) 1
    -- CR 615.6: a fully prevented event never happens, so nothing is marked and
    -- nothing is recorded.
    Spec.assertEqWith s "the first 3 is prevented whole" (S.damageOf victim once) (Just 0)
    Spec.assertEqWith s "and no damage event happened at all" (amounts once) []
    Spec.assertEqWith s "the shield is still there, holding 1" (length (GameState.replacements once)) 1
    -- The partial case, which is what makes this a shield rather than a Fog: 1
    -- of the second 3 is prevented and the other 2 are dealt.
    Spec.assertEqWith s "1 of the second 3 is prevented, 2 are dealt" (S.damageOf victim twice) (Just 2)
    Spec.assertEqWith s "and the surviving event carries the reduced amount" (amounts twice) [2]
    -- CR 615.7: "once the shield has been reduced to 0, any remaining damage is
    -- dealt normally."
    Spec.assertEqWith s "the spent shield is gone" (GameState.replacements twice) []
    Spec.assertEqWith s "so the third 3 lands in full" (S.damageOf victim thrice) (Just 5)
  -- CR 615.7's "shielded permanent": a shield names ONE recipient, so a second
  -- creature is not covered by it and does not spend it. The discriminating twin
  -- of the case above -- a shield that ignored DamagePattern.whichRecipient,
  -- which is what every damage pattern in the pool did before this card, would
  -- prevent this damage and be spent doing it.
  Spec.it s "CR 615.7 a shield covers the recipient it names and no other" $ do
    plains <- S.printingOf s registry "Plains"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    mendingHands <- S.printingOf s registry "Mending Hands"
    let base = S.landsInPlay plains 1
        (shieldedOne, g1) = S.addCreature pikerPrinting S.alice base
        (bystander, g2) = S.addCreature pikerPrinting S.alice g1
        (attacker, g3) = S.addCreature pikerPrinting S.bob g2
        (g4, spellId) = S.handOne mendingHands g3
        shielded = castAndResolve (aimCreature shieldedOne) g4 spellId
        after = S.runPure S.identityAnswer shielded (Damage.applyDamage [hit attacker (Recipient.ToCreature bystander) 3])
    Spec.assertEqWith s "the unshielded creature takes all of it" (S.damageOf bystander after) (Just 3)
    Spec.assertEqWith s "and the shield was not spent on damage it does not cover" (length (GameState.replacements after)) 1
  -- THE CR 615.7 CHOICE. Two simultaneous sources, 5 and 3, against a shield of
  -- 4 on the player they are both aimed at: the shield can cover one of them or
  -- part of the other, never both, so which it prevents is a decision -- and CR
  -- 615.7 gives it to "the player or the controller of the permanent", never to
  -- the engine.
  --
  -- The total dealt is 4 either way, so LIFE cannot tell the two answers apart;
  -- what does is which events happened at all (CR 615.6). Prevent the 5 first and
  -- both events survive, at 1 and 3; prevent the 3 first and it never happens,
  -- leaving one event of 4.
  Spec.it s "CR 615.7 the shielded PLAYER chooses which of two simultaneous damages the shield prevents" $ do
    plains <- S.printingOf s registry "Plains"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    mendingHands <- S.printingOf s registry "Mending Hands"
    let base = S.landsInPlay plains 1
        (big, g1) = S.addCreature pikerPrinting S.alice base
        (small, g2) = S.addCreature pikerPrinting S.alice g1
        (g3, spellId) = S.handOne mendingHands g2
        shielded = castAndResolve (aimPlayer S.bob) g3 spellId
        batch = [hit big (Recipient.ToPlayer S.bob) 5, hit small (Recipient.ToPlayer S.bob) 3]
        tookTheBig = settleDamage (shieldFirst big) shielded batch
        tookTheSmall = settleDamage (shieldFirst small) shielded batch
    Spec.assertEqWith s "setup: bob is shielded, not alice" (length (GameState.replacements shielded)) 1
    Spec.assertBool
      s
      (wasAskedToOrderDamage (answersFor S.identityAnswer shielded (Damage.applyDamage batch)))
      "bob was asked which damage the shield prevents"
    Spec.assertEqWith s "bob spends the shield on the 5: 1 of it and all of the 3 get through" (amounts tookTheBig) [1, 3]
    Spec.assertEqWith s "bob spends it on the 3 instead: that event never happens, and 4 of the 5 land" (amounts tookTheSmall) [4]
    -- CR 615.7's last sentence again, from the other side: the shield prevents 4
    -- whichever order it is spent in, so the two boards differ in WHICH events
    -- happened and never in how much was prevented.
    Spec.assertEqWith s "either way the shield prevented exactly 4" (S.lifeOf S.bob tookTheBig) (Just 16)
    Spec.assertEqWith s "either way the shield prevented exactly 4" (S.lifeOf S.bob tookTheSmall) (Just 16)
    Spec.assertEqWith s "and either way it is spent" (GameState.replacements tookTheBig) []
    Spec.assertEqWith s "and either way it is spent" (GameState.replacements tookTheSmall) []
  -- The elision half, and the reason the prompt is gated rather than raised for
  -- every batch: a shield big enough to cover the whole batch prevents all of it
  -- in any order, so there is nothing to decide and nothing is asked.
  Spec.it s "CR 615.7 a shield that covers the whole batch asks nothing" $ do
    plains <- S.printingOf s registry "Plains"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    mendingHands <- S.printingOf s registry "Mending Hands"
    let base = S.landsInPlay plains 1
        (big, g1) = S.addCreature pikerPrinting S.alice base
        (small, g2) = S.addCreature pikerPrinting S.alice g1
        (g3, spellId) = S.handOne mendingHands g2
        shielded = castAndResolve (aimPlayer S.bob) g3 spellId
        batch = [hit big (Recipient.ToPlayer S.bob) 1, hit small (Recipient.ToPlayer S.bob) 2]
        after = S.runPure S.identityAnswer shielded (Damage.applyDamage batch)
    Spec.assertBool
      s
      (not (wasAskedToOrderDamage (answersFor S.identityAnswer shielded (Damage.applyDamage batch))))
      "no OrderDamage was raised: 4 covers 1 and 2 together"
    Spec.assertEqWith s "both events were prevented whole" (amounts after) []
    Spec.assertEqWith s "bob's life is untouched" (S.lifeOf S.bob after) (Just 20)
    Spec.assertEqWith s "and 3 of the shield's 4 were spent, so 1 remains" (length (GameState.replacements after)) 1
  -- CR 800.4a from the other side, and the case that proves the BUCKET half of
  -- Departure.givesControlOnEntryTo: the second clause ends only the effects that
  -- give the departing player control, and a prevention shield gives nobody
  -- control. So alice's shield on BOB's creature outlives her concession, exactly
  -- as her Giant Growth would.
  --
  -- It lives here rather than in Pawl.DepartureSpec because Mending Hands is a
  -- floating non-control row a test can install by casting a real card and then
  -- read off the board -- the plainest one in data/cards/, Healing Grace's
  -- differing only by CR 609.7a's chosen source; three seats because
  -- Departure.continuesAfterDeparture is `> 2`, and carol deals the damage so
  -- nothing about the strike depends on the seat that left.
  Spec.it s "CR 800.4a a departing player's shield is not a control effect, so it stays" $ do
    plains <- S.printingOf s registry "Plains"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    mendingHands <- S.printingOf s registry "Mending Hands"
    let base = Setup.emptyGame S.threePlayers
        (_, g1) = S.addCreature plains S.alice base
        (victim, g2) = S.addCreature pikerPrinting S.bob g1
        (attacker, g3) = S.addCreature pikerPrinting S.carol g2
        (spellId, g4) = S.addHandCard mendingHands S.alice g3
        shielded = S.runPure (aimCreature victim) g4 (S.cast S.alice spellId >> Stack.resolveTop)
        -- The one difference between the two runs.
        gone = S.runPure S.identityAnswer shielded (Departure.leaveGame Departure.Type.Conceded S.alice)
        strike g = S.runPure S.identityAnswer g (Damage.applyDamage [hit attacker (Recipient.ToCreature victim) 3])
    Spec.assertEqWith s "setup: alice's shield is floating before she leaves" (length (GameState.replacements shielded)) 1
    Spec.assertEqWith s "the shield still prevents carol's 3 after alice has left (CR 800.4a)" (S.damageOf victim (strike gone)) (Just 0)
    Spec.assertEqWith s "the same 3 with alice still seated is prevented too" (S.damageOf victim (strike shielded)) (Just 0)
    Spec.assertEqWith s "and her departure left the row standing" (length (GameState.replacements gone)) 1

-- Aim every target slot at `victim` and answer CR 609.7a's source choice with
-- `src`. FILTERED, not built: the id is taken from the offered set, so an answer
-- the prompt never offered cannot reach the engine -- and the group asserts on
-- the RECORDED response, so a `src` that was never offered fails the case rather
-- than quietly falling back.
aimAndChoose :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
aimAndChoose victim src p = case p of
  Prompt.ChooseDamageSource _ _ _ candidates ->
    Maybe.fromMaybe (NonEmpty.head candidates) (List.find (== src) (NonEmpty.toList candidates))
  _ -> aimCreature victim p

-- Which sources CR 609.7a's prompt was answered with, in order. The proxy half
-- of the group below: it says a choice was MADE, where the damage assertions say
-- the shield watches what was chosen.
chosenSourcesIn :: [Response.Response] -> [ObjectId.ObjectId]
chosenSourcesIn =
  let chosen r = case r of
        Response.ChoseDamageSource oid -> Just oid
        _ -> Nothing
   in Maybe.mapMaybe chosen

-- CR 609.7a's player-CHOSEN source, whose producer is Healing Grace ({W}
-- Instant: "Prevent the next 3 damage that would be dealt to any target this turn
-- by a source of your choice. You gain 3 life").
--
-- Mending Hands above with one clause added, which is exactly the difference the
-- rule makes: that shield watches every source ("the number of events or sources
-- dealing it doesn't matter", CR 615.7), this one watches the ONE object its
-- controller chose when the effect was created. The engine bakes the id into
-- DamagePattern.whichSource, never choosing it (CR 609.7a: "if an effect requires
-- a player to choose a source of damage").
--
-- The chosen source is deliberately NOT the first candidate the prompt offers:
-- CR 609.7a's pool here is alice's Plains, the two creatures, the shielded one
-- and Healing Grace itself on the stack, sorted ascending, so an engine that
-- ignored the answer and took the head would shield against the Plains and both
-- damage assertions would read the other way round.
healingGraceSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
healingGraceSpec s registry = Spec.describe s "Healing Grace (CR 609.7a)" $ do
  let hit src recipient n =
        DamageEvent.MkDamageEvent src recipient n False False False 0 Nothing DamageKind.Noncombat
  Spec.it s "CR 609.7a the shield watches the source its controller chose and no other" $ do
    plains <- S.printingOf s registry "Plains"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    healingGrace <- S.printingOf s registry "Healing Grace"
    let base = S.landsInPlay plains 1
        (victim, g1) = S.addCreature pikerPrinting S.alice base
        (alpha, g2) = S.addCreature pikerPrinting S.bob g1
        (omega, g3) = S.addCreature pikerPrinting S.bob g2
        (g4, spellId) = S.handOne healingGrace g3
        -- Rank-2, so the answerer is applied at each use rather than let-bound
        -- (castEach's reason above).
        shielded = castAndResolve (aimAndChoose victim omega) g4 spellId
        strike src n g = S.runPure S.identityAnswer g (Damage.applyDamage [hit src (Recipient.ToCreature victim) n])
    -- THE gameplay assertion, and the one the whole unit exists for: the source
    -- alice did NOT choose is not shielded against, however much the shield has
    -- left. Before DamagePattern.whichSource this 2 was prevented.
    Spec.assertEqWith s "the unchosen source's 2 lands in full" (S.damageOf victim (strike alpha 2 shielded)) (Just 2)
    -- Its twin, on the same board and differing in one thing: the chosen source's
    -- damage IS prevented, so the case cannot pass by installing no shield.
    Spec.assertEqWith s "the chosen source's 3 is prevented whole" (S.damageOf victim (strike omega 3 shielded)) (Just 0)
    -- CR 615.7 from the other side: a shield spends nothing on damage it does not
    -- cover, so alpha's 2 leaves the 3 intact for omega.
    Spec.assertEqWith s "and the shield survives the unchosen source untouched" (S.damageOf victim (strike omega 3 (strike alpha 2 shielded))) (Just 2)
    -- The proxies, after the behaviour: a choice was raised and answered with the
    -- source the assertions above read, and the card's second sentence ran.
    Spec.assertEqWith s "setup: the shield is a floating replacement" (length (GameState.replacements shielded)) 1
    Spec.assertEqWith s "alice was asked which source, and answered omega" (chosenSourcesIn (answersFor (aimAndChoose victim omega) g4 (S.cast S.alice spellId >> Stack.resolveTop))) [omega]
    Spec.assertEqWith s "and she gained the printed 3 life" (S.lifeOf S.alice shielded) (Just 23)
  -- The discriminating twin, differing from the case above in the CARD alone:
  -- Mending Hands prints no "of your choice", so its shield watches every source
  -- and no choice is raised at all. Without it the case above could pass on a
  -- board where nothing but the chosen source ever dealt damage.
  Spec.it s "CR 615.7 a shield naming NO source watches every source, and asks nothing" $ do
    plains <- S.printingOf s registry "Plains"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    mendingHands <- S.printingOf s registry "Mending Hands"
    let base = S.landsInPlay plains 1
        (victim, g1) = S.addCreature pikerPrinting S.alice base
        (alpha, g2) = S.addCreature pikerPrinting S.bob g1
        (_, g3) = S.addCreature pikerPrinting S.bob g2
        (g4, spellId) = S.handOne mendingHands g3
        shielded = castAndResolve (aimCreature victim) g4 spellId
        strike src n g = S.runPure S.identityAnswer g (Damage.applyDamage [hit src (Recipient.ToCreature victim) n])
    Spec.assertEqWith s "the same unchosen source's 2 is prevented here" (S.damageOf victim (strike alpha 2 shielded)) (Just 0)
    Spec.assertEqWith s "and nobody was asked to choose a source" (chosenSourcesIn (answersFor (aimCreature victim) g4 (S.cast S.alice spellId >> Stack.resolveTop))) []

-- CR 615.5's ADDITIONAL EFFECT, whose producer is Test of Faith ({1}{W} Instant:
-- "Prevent the next 3 damage that would be dealt to target creature this turn.
-- For each 1 damage prevented this way, put a +1/+1 counter on that creature").
--
-- Not a triggered ability, and that is the whole of what these cases
-- discriminate. Test of Faith's 2004-12-01 ruling: "The +1/+1 counters are put
-- onto the creature at the same time the damage is prevented. If a 1/1 creature
-- would be dealt 6 damage, 3 damage is prevented and three +1/+1 counters are
-- put on the creature." Under a triggered reading the counters would wait for
-- the stack and CR 704.5g would have destroyed the creature first.
--
-- Two seats is enough: no relational text is under test -- the rider names the
-- shielded creature, never "an opponent" or "the defending player" -- so the
-- three-seat trap does not apply.
--
-- The COMBAT case really runs the combat steps through Pawl.Engine.Engine, which
-- the other groups in this file avoid: the ordering under test is the one
-- between the rider and the step's state-based action check, and only the engine
-- has both.
testOfFaithSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
testOfFaithSpec s registry = Spec.describe s "Test of Faith (CR 615.5)" $ do
  let -- Put a board at declare attackers with alice active and bob defending,
      -- so S.runCombat drives the remaining steps -- S.combatBoardOf's shape,
      -- reached from a board that has already cast a spell.
      atCombat gs =
        gs
          { GameState.activePlayer = S.alice,
            GameState.phase = Phase.Combat CombatStep.DeclareAttackers,
            -- CR 703.4h has already happened on this board, so the defending
            -- player is stated rather than derived (S.combatBoardOf's posture).
            GameState.combat = Combat.emptyCombat {Combat.Type.defender = Just S.bob},
            GameState.remaining =
              Seq.fromList
                [ Phase.Combat CombatStep.DeclareBlockers,
                  Phase.Combat CombatStep.CombatDamage,
                  Phase.Combat CombatStep.EndOfCombat,
                  Phase.PostcombatMain
                ]
          }
  -- The ruling's own arithmetic, at combat scale. 5 damage meets a shield of 3:
  -- 3 is prevented and becomes 3 counters, 2 is marked, and the 2/1 that would
  -- have died is a 5/4 with 2 damage on it. Every number distinct -- shield 3,
  -- incoming 5, marked 2, final toughness 4 -- so no two readings of the rule
  -- land on the same board.
  Spec.it s "CR 615.5 the counters are on before CR 704.5g asks whether the attacker died" $ do
    plains <- S.printingOf s registry "Plains"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    jedit <- S.printingOf s registry "Jedit Ojanen"
    testOfFaith <- S.printingOf s registry "Test of Faith"
    let base = S.landsInPlay plains 2
        (attacker, g1) = S.addCreature pikerPrinting S.alice base
        (blocker, g2) = S.addCreature jedit S.bob g1
        (g3, spellId) = S.handOne testOfFaith g2
        shielded = castAndResolve (aimCreature attacker) g3 spellId
        after = S.runCombat S.aggressiveAnswer (atCombat shielded)
        -- The CONTROL is the same board, same two Plains, same card in hand,
        -- with the spell never cast -- so the only difference is the shield.
        control = S.runCombat S.aggressiveAnswer (atCombat g3)
    Spec.assertEqWith s "setup: the shield is a floating replacement holding 3" (shieldsLeft shielded) [3]
    Spec.assertBool s (S.onBattlefield attacker after) "the shielded 2/1 survived a 5-power blocker"
    Spec.assertEqWith s "with 3 +1/+1 counters, one per damage prevented" (countersOn CounterKind.PlusOnePlusOne attacker after) 3
    Spec.assertEqWith s "so it is a 5/4" (S.powerToughnessOf attacker after) (Just (5, 4))
    -- CR 615.6 / 120.3e: the prevented 3 never happened, and the other 2 are
    -- marked.
    Spec.assertEqWith s "and 2 of the 5 were marked on it" (S.damageOf attacker after) (Just 2)
    -- The anti-vacuity fence: the block really happened and the shield really
    -- ran out, so nothing above passes because no damage was dealt or because
    -- the row is still sitting there unspent.
    Spec.assertEqWith s "the blocker took the attacker's 2" (S.damageOf blocker after) (Just 2)
    Spec.assertEqWith s "and the shield is spent to 0 and dropped (CR 615.7)" (shieldsLeft after) []
    Spec.assertBool s (not (S.onBattlefield attacker control)) "without the shield that same 2/1 dies"
  -- The other funnel: damage dealt by a RESOLVING ability rather than by combat,
  -- which drains the rider inside Pawl.Engine.Resolve instead of inside the
  -- combat damage step. One ping against a shield of 3 -- so the counter count
  -- is 1, the amount THIS application prevented, and not the shield's printed 3.
  Spec.it s "CR 615.5 a shield spent by a resolving ability runs its rider too" $ do
    plains <- S.printingOf s registry "Plains"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    testOfFaith <- S.printingOf s registry "Test of Faith"
    let base = S.landsInPlay plains 2
        (victim, g1) = S.addCreature pikerPrinting S.alice base
        (pinger, g2) = S.addCreature sorcerer S.alice g1
        (g3, spellId) = S.handOne testOfFaith g2
        shielded = castAndResolve (aimCreature victim) g3 spellId
        ping g = S.runPure (aimCreature victim) g (Activate.activateAbility S.alice pinger (theAbility sorcerer) Monad.>> Stack.resolveTop)
        after = ping shielded
        control = ping g3
    Spec.assertEqWith s "setup: the shield holds 3" (shieldsLeft shielded) [3]
    Spec.assertEqWith s "the ping's 1 damage is prevented, so nothing is marked" (S.damageOf victim after) (Just 0)
    Spec.assertEqWith s "and exactly one counter goes on -- the amount prevented, not the shield" (countersOn CounterKind.PlusOnePlusOne victim after) 1
    Spec.assertEqWith s "so the 2/1 is a 3/2" (S.powerToughnessOf victim after) (Just (3, 2))
    Spec.assertEqWith s "with 2 of the shield left (CR 615.7)" (shieldsLeft after) [2]
    Spec.assertEqWith s "and unshielded the same ping marks 1 and puts no counter on" (S.damageOf victim control, countersOn CounterKind.PlusOnePlusOne victim control) (Just 1, 0)

-- How many UNBOUNDED shields (CR 615.1 / 615.3) are on the board. `shieldsLeft`
-- above is the wrong reader for these: it reports a countdown amount, and this
-- row has none, so it answers the empty list whether the shield is installed or
-- not.
preventAllRows :: GameState.GameState -> Int
preventAllRows gs =
  let isPreventAll re = case re of
        ReplacementEffect.DamageR (DamageR.MkDamageR _ DamageRewrite.PreventAll _) -> True
        _ -> False
   in length (filter (isPreventAll . ActiveReplacement.effect) (GameState.replacements gs))

-- Attacks with everything and blocks with NOTHING, so an attack's damage reaches
-- the defending player. skirmishAnswer's combat half without its targeting half.
attackNoBlock :: Prompt.Prompt r -> r
attackNoBlock p = case p of
  Prompt.DeclareAttackers _ _ ids -> ids
  Prompt.DeclareBlockers {} -> Map.empty
  _ -> S.identityAnswer p

-- Put a board at declare attackers with BOB active and alice defending -- the
-- mirror of testOfFaithSpec's `atCombat`, since a shield over "you" is over the
-- player who activated it and it takes combat damage only when that player is
-- the one being attacked.
bobAttacks :: GameState.GameState -> GameState.GameState
bobAttacks gs =
  gs
    { GameState.activePlayer = S.bob,
      GameState.phase = Phase.Combat CombatStep.DeclareAttackers,
      -- CR 703.4h has already happened on this board, so the defending player is
      -- stated rather than derived (S.combatBoardOf's posture).
      GameState.combat = Combat.emptyCombat {Combat.Type.defender = Just S.alice},
      GameState.remaining =
        Seq.fromList
          [ Phase.Combat CombatStep.DeclareBlockers,
            Phase.Combat CombatStep.CombatDamage,
            Phase.Combat CombatStep.EndOfCombat,
            Phase.PostcombatMain
          ]
    }

-- Aim every target slot at one creature by FILTERING the offered set rather than
-- building a recipient, so a candidate the card's filter excludes cannot be
-- smuggled back in: an illegal aim leaves the slot empty instead of silently
-- becoming a legal one.
onlyCreature :: ObjectId.ObjectId -> Prompt.Prompt r -> r
onlyCreature oid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter (== Recipient.ToCreature oid) . snd) sets
  _ -> S.identityAnswer p

-- CR 510.2 vs CR 608: a shield that names a KIND. Decorated Griffin ({4}{W}
-- Creature -- Griffin 2/3, flying) prints "{1}{W}: Prevent the next 1 combat
-- damage that would be dealt to you this turn" -- a counted shield (CR 615.7)
-- over a PLAYER, with no CR 615.5 clause, so the kind is the only thing under
-- test. Inkshield below is the same recipient WITH the CR 615.5 clause.
--
-- The discrimination needs both halves and a control each. A group using only
-- combat damage would pass identically on a shield that named no kind at all.
--
-- Numbers all distinct: the ping is 1 and the attack is 5, so alice ends on 19
-- where the kind is respected and 16 where the shield bites, against 20 and 15
-- for the two readings that are wrong.
decoratedGriffinSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
decoratedGriffinSpec s registry = Spec.describe s "Decorated Griffin (CR 510.2)" $ do
  Spec.it s "a combat-only shield leaves noncombat damage alone (CR 608)" $ do
    plains <- S.printingOf s registry "Plains"
    griffin <- S.printingOf s registry "Decorated Griffin"
    sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    let base = S.landsInPlay plains 2
        (bird, g1) = S.addCreature griffin S.alice base
        (pinger, g2) = S.addCreature sorcerer S.alice g1
        shielded = S.runPure S.identityAnswer g2 (Activate.activateAbility S.alice bird (theAbility griffin) Monad.>> Stack.resolveTop)
        ping g = S.runPure (aimPlayer S.alice) g (Activate.activateAbility S.alice pinger (theAbility sorcerer) Monad.>> Stack.resolveTop)
        after = ping shielded
        -- The CONTROL is the same board with the ability never activated, so
        -- the only difference is the shield.
        control = ping g2
    Spec.assertEqWith s "setup: the activation installed a shield of 1" (shieldsLeft shielded) [1]
    Spec.assertEqWith s "the Sorcerer's noncombat 1 is dealt anyway (CR 608)" (S.lifeOf S.alice after) (Just 19)
    Spec.assertEqWith s "and the combat-only shield is untouched" (shieldsLeft after) [1]
    Spec.assertEqWith s "which is what the unshielded board does too" (S.lifeOf S.alice control) (Just 19)
  Spec.it s "the same shield does prevent combat damage, 1 of it (CR 615.7)" $ do
    plains <- S.printingOf s registry "Plains"
    griffin <- S.printingOf s registry "Decorated Griffin"
    jedit <- S.printingOf s registry "Jedit Ojanen"
    let base = S.landsInPlay plains 2
        (bird, g1) = S.addCreature griffin S.alice base
        (_, g2) = S.addCreature jedit S.bob g1
        shielded = S.runPure S.identityAnswer g2 (Activate.activateAbility S.alice bird (theAbility griffin) Monad.>> Stack.resolveTop)
        after = S.runCombat attackNoBlock (bobAttacks shielded)
        control = S.runCombat attackNoBlock (bobAttacks g2)
    Spec.assertEqWith s "setup: the activation installed a shield of 1" (shieldsLeft shielded) [1]
    Spec.assertEqWith s "1 of the attacker's 5 is prevented" (S.lifeOf S.alice after) (Just 16)
    Spec.assertEqWith s "and the shield is spent to 0 and dropped (CR 615.7)" (shieldsLeft after) []
    Spec.assertEqWith s "where the unshielded board takes all 5" (S.lifeOf S.alice control) (Just 15)

-- CR 615.5's additional effect on the UNBOUNDED shield (CR 615.1 / 615.3), which
-- Test of Faith's countdown shield above cannot reach. Brace for Impact ({4}{W}
-- Instant) prints "Prevent all damage that would be dealt to target multicolored
-- creature this turn. For each 1 damage prevented this way, put a +1/+1 counter
-- on that creature."
--
-- "Multicolored" is CR 105.2b -- two or more of the five colors -- written as
-- the ten pairs of Filter.HasColor rather than as an atom of its own, since a
-- composition of existing atoms is not a second spelling of one relation.
--
-- The unbounded shield has no count, so CR 615.5's "the damage prevented this
-- way" is per APPLICATION rather than a running total; the second case is what
-- tells those two readings apart, and they answer 2 and 3.
braceForImpactSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
braceForImpactSpec s registry = Spec.describe s "Brace for Impact (CR 615.5)" $ do
  Spec.it s "an unbounded shield carries CR 615.5's rider" $ do
    plains <- S.printingOf s registry "Plains"
    jedit <- S.printingOf s registry "Jedit Ojanen"
    sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    brace <- S.printingOf s registry "Brace for Impact"
    let base = S.landsInPlay plains 5
        (victim, g1) = S.addCreature jedit S.alice base
        (pinger, g2) = S.addCreature sorcerer S.alice g1
        (g3, spellId) = S.handOne brace g2
        shielded = castAndResolve (aimCreature victim) g3 spellId
        ping g = S.runPure (aimCreature victim) g (Activate.activateAbility S.alice pinger (theAbility sorcerer) Monad.>> Stack.resolveTop)
        after = ping shielded
        control = ping g3
    Spec.assertEqWith s "setup: the unbounded shield is a floating replacement" (preventAllRows shielded) 1
    Spec.assertEqWith s "the ping's 1 is prevented, so nothing is marked" (S.damageOf victim after) (Just 0)
    Spec.assertEqWith s "and one +1/+1 counter goes on, per damage prevented" (countersOn CounterKind.PlusOnePlusOne victim after) 1
    Spec.assertEqWith s "so the 5/5 is a 6/6" (S.powerToughnessOf victim after) (Just (6, 6))
    Spec.assertEqWith s "and the shield stays: CR 615.7's terminator does not apply" (preventAllRows after) 1
    Spec.assertEqWith s "unshielded that same ping marks 1 and puts none on" (S.damageOf victim control, countersOn CounterKind.PlusOnePlusOne victim control) (Just 1, 0)
  Spec.it s "CR 615.5's amount is per application, not a running total" $ do
    plains <- S.printingOf s registry "Plains"
    jedit <- S.printingOf s registry "Jedit Ojanen"
    sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    brace <- S.printingOf s registry "Brace for Impact"
    let base = S.landsInPlay plains 5
        (victim, g1) = S.addCreature jedit S.alice base
        -- TWO Sorcerers, because one taps for its own ability and a running-total
        -- reading can only be told from a per-application one by a SECOND
        -- application.
        (first, g2) = S.addCreature sorcerer S.alice g1
        (second, g3) = S.addCreature sorcerer S.alice g2
        (g4, spellId) = S.handOne brace g3
        shielded = castAndResolve (aimCreature victim) g4 spellId
        ping oid g = S.runPure (aimCreature victim) g (Activate.activateAbility S.alice oid (theAbility sorcerer) Monad.>> Stack.resolveTop)
        after = ping second (ping first shielded)
    Spec.assertEqWith s "setup: the unbounded shield is a floating replacement" (preventAllRows shielded) 1
    -- A running-total reading would put 1 on and then 2 on, for 3.
    Spec.assertEqWith s "two applications of 1 put one counter on each" (countersOn CounterKind.PlusOnePlusOne victim after) 2
    Spec.assertEqWith s "with nothing ever marked" (S.damageOf victim after) (Just 0)
    Spec.assertEqWith s "and the shield still installed after both" (preventAllRows after) 1
  -- The POSITIVE half is the pin: the multicolored creature IS in the legal set
  -- on the same board the mono-coloured one is not, so a filter that excluded
  -- everything would fail here rather than pass the negative vacuously.
  Spec.it s "CR 105.2b: only a multicolored creature is a legal target" $ do
    plains <- S.printingOf s registry "Plains"
    jedit <- S.printingOf s registry "Jedit Ojanen"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    brace <- S.printingOf s registry "Brace for Impact"
    let base = S.landsInPlay plains 5
        (multi, g1) = S.addCreature jedit S.alice base
        (mono, g2) = S.addCreature pikerPrinting S.alice g1
        (g3, spellId) = S.handOne brace g2
        atMulti = castAndResolve (onlyCreature multi) g3 spellId
        atMono = castAndResolve (onlyCreature mono) g3 spellId
    Spec.assertEqWith s "the white-and-blue 5/5 is offered, so a shield goes up" (preventAllRows atMulti) 1
    Spec.assertEqWith s "the mono-red 2/1 is not, so none does" (preventAllRows atMono) 0

-- CR 615.5's additional effect over a PLAYER recipient, which neither Test of
-- Faith's nor Brace for Impact's nor Stormwild Capridor's permanent recipient can
-- reach: a player has no Object, so the prevented amount has nowhere to be bound
-- and CR 615.5's "the amount of damage that was prevented" travels on
-- GameState.ambientAmounts instead. Inkshield ({3}{W}{B} Instant) prints "Prevent
-- all combat damage that would be dealt to you this turn. For each 1 damage
-- prevented this way, create a 2/1 white and black Inkling creature token with
-- flying."
--
-- Numbers all distinct: the attacker is a 5/5 and the pinger deals 1, so the
-- readings answer 20 life and 5 tokens (right), 20 and 1 (one token per
-- application), 20 and 0 (the rider never runs), 15 and 0 (no shield at all) and
-- 19 and 0 (the kind refused it). No two coincide.
inkshieldSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
inkshieldSpec s registry = Spec.describe s "Inkshield (CR 615.5)" $ do
  Spec.it s "a shield over a PLAYER runs CR 615.5's rider, scaled by the amount" $ do
    plains <- S.printingOf s registry "Plains"
    swamp <- S.printingOf s registry "Swamp"
    jedit <- S.printingOf s registry "Jedit Ojanen"
    inkshield <- S.printingOf s registry "Inkshield"
    let base = S.landsFor swamp S.alice 3 (S.landsInPlay plains 2)
        (_, g1) = S.addCreature jedit S.bob base
        (g2, spellId) = S.handOne inkshield g1
        shielded = castAndResolve S.identityAnswer g2 spellId
        after = S.runCombat attackNoBlock (bobAttacks shielded)
        -- The CONTROL is the same board with Inkshield still in hand, so the one
        -- difference between the two is the shield.
        control = S.runCombat attackNoBlock (bobAttacks g2)
    Spec.assertEqWith s "setup: the unbounded shield is a floating replacement" (preventAllRows shielded) 1
    Spec.assertEqWith s "the 5/5's whole combat damage is prevented" (S.lifeOf S.alice after) (Just 20)
    Spec.assertEqWith s "and five Inklings arrive, one per damage prevented" (length (S.tokensOf after)) 5
    Monad.forM_ (S.tokensOf after) $ \oid -> do
      Spec.assertEqWith s "each a 2/1" (S.powerToughnessOf oid after) (Just (2, 1))
      Spec.assertEqWith s "white and black" (Projection.colorsOf oid after) (Set.fromList [Color.White, Color.Black])
      Spec.assertEqWith s "an Inkling" (Projection.subtypesOf oid after) (Set.singleton Subtype.Inkling)
      Spec.assertEqWith s "with flying" (Projection.hasKeyword Keyword.Flying oid after) True
      -- CR 111.4: the name is the subtypes plus the word "Token".
      Spec.assertEqWith s "named Inkling Token (CR 111.4)" (Projection.namesOf oid after) (Set.singleton (CardName.MkCardName (Text.pack "Inkling Token")))
    -- The channel the amount travelled on is put back, which the stamp it
    -- replaced had no test for: a value left behind would shadow every later
    -- reserved-slot read in the game.
    Spec.assertEqWith s "and CR 615.5's amount channel does not outlive the rider" (GameState.ambientAmounts after) Map.empty
    -- The VACUITY guard: unshielded, that same attack really is dealt, so the
    -- prevention above is a prevention rather than an attack that never happened.
    Spec.assertEqWith s "unshielded the same attack takes 5 and makes no token" (S.lifeOf S.alice control, length (S.tokensOf control)) (Just 15, 0)
  Spec.it s "the combat-only shield leaves noncombat damage alone, rider and all (CR 608)" $ do
    plains <- S.printingOf s registry "Plains"
    swamp <- S.printingOf s registry "Swamp"
    sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    inkshield <- S.printingOf s registry "Inkshield"
    let base = S.landsFor swamp S.alice 3 (S.landsInPlay plains 2)
        (pinger, g1) = S.addCreature sorcerer S.alice base
        (g2, spellId) = S.handOne inkshield g1
        shielded = castAndResolve S.identityAnswer g2 spellId
        ping g = S.runPure (aimPlayer S.alice) g (Activate.activateAbility S.alice pinger (theAbility sorcerer) Monad.>> Stack.resolveTop)
        after = ping shielded
    Spec.assertEqWith s "setup: the shield is installed" (preventAllRows shielded) 1
    Spec.assertEqWith s "the Sorcerer's noncombat 1 is dealt anyway" (S.lifeOf S.alice after) (Just 19)
    Spec.assertEqWith s "so nothing was prevented and no rider ran" (length (S.tokensOf after)) 0

-- CR 615.5's additional effect on a STATIC prevention ability, which is where
-- Test of Faith's floating shield above cannot reach: Stormwild Capridor ({2}{W}
-- Creature -- Bird Goat 1/3, flying) prints "If noncombat damage would be dealt
-- to this creature, prevent that damage. Put a +1/+1 counter on this creature
-- for each 1 damage prevented this way."
--
-- Three clauses, three cases, and each case is a PAIR of boards differing in one
-- thing:
--
--   * the rider itself -- 3 prevented becomes 3 counters -- against the same
--     Bolt aimed at the Goblin Piker beside it, which the ability does not cover;
--   * CR 615.1's printed recipient, which is why that second board's Piker dies;
--   * the printed KIND, combat damage passing where the same amount of
--     noncombat damage does not.
--
-- Numbers all distinct: the Bolt is 3, the combat hit is 2, the counters are 3,
-- and the shielded creature goes from 1/3 to 4/6. No two readings of the rule
-- meet on one of them -- a "one counter per event" reading would answer 1, an
-- unrun rider 0, and a Vigor-shaped "another creature you control" reading would
-- have saved the Piker instead.
stormwildCapridorSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
stormwildCapridorSpec s registry = Spec.describe s "Stormwild Capridor (CR 615.5)" $ do
  let hit kind src recipient n =
        DamageEvent.MkDamageEvent src recipient n False False False 0 Nothing kind
  -- The rider fires from the funnel a RESOLVING spell drains
  -- (Resolve.runPreventionRider), the same seam Test of Faith's shield uses --
  -- so what is new here is only where the rider came from: the permanent's
  -- printed ability rather than a row a resolution installed.
  Spec.it s "CR 615.5 prevented noncombat damage becomes that many +1/+1 counters" $ do
    mountain <- S.printingOf s registry "Mountain"
    capridorPrinting <- S.printingOf s registry "Stormwild Capridor"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let base = S.landsInPlay mountain 1
        (capridor, g1) = S.addCreature capridorPrinting S.alice base
        (piker, g2) = S.addCreature pikerPrinting S.alice g1
        (g3, spellId) = S.handOne bolt g2
        -- ONE board, two aims: the only difference between these is which
        -- creature the Bolt names.
        atCapridor = castAndResolve (aimCreature capridor) g3 spellId
        atPiker = castAndResolve (aimCreature piker) g3 spellId
    Spec.assertEqWith s "setup: the Capridor is a 1/3" (S.powerToughnessOf capridor g3) (Just (1, 3))
    -- CR 615.6: the prevented event never happened, so nothing is marked.
    Spec.assertEqWith s "no damage is marked on the Capridor" (S.damageOf capridor atCapridor) (Just 0)
    Spec.assertEqWith s "three +1/+1 counters, one per damage prevented" (countersOn CounterKind.PlusOnePlusOne capridor atCapridor) 3
    Spec.assertEqWith s "so it is a 4/6" (S.powerToughnessOf capridor atCapridor) (Just (4, 6))
    -- CR 615.1's printed recipient: the ability covers "this creature" and
    -- nothing else, so the same Bolt lands on the Piker in full. Marked rather
    -- than dead, since nothing has taken priority to run CR 704.3's check.
    Spec.assertEqWith s "the same Bolt marks its whole 3 on the Piker beside it" (S.damageOf piker atPiker) (Just 3)
    Spec.assertEqWith s "and puts no counter on the Capridor" (countersOn CounterKind.PlusOnePlusOne capridor atPiker) 0
  -- The printed KIND, asked through the damage funnel alone: one field of the
  -- event differs between these two boards and nothing else does. Only the
  -- prevention is read here -- Damage.applyDamage queues the rider for a caller
  -- to drain, and the case below is what proves the queue stays empty on the
  -- combat side.
  Spec.it s "CR 615.1 the printed kind admits noncombat damage and refuses combat damage" $ do
    mountain <- S.printingOf s registry "Mountain"
    capridorPrinting <- S.printingOf s registry "Stormwild Capridor"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    let base = S.landsInPlay mountain 1
        (capridor, g1) = S.addCreature capridorPrinting S.alice base
        (attacker, g2) = S.addCreature pikerPrinting S.bob g1
        settle kind = settleDamage S.identityAnswer g2 [hit kind attacker (Recipient.ToCreature capridor) 2]
    Spec.assertEqWith s "noncombat: the 2 is prevented" (S.damageOf capridor (settle DamageKind.Noncombat)) (Just 0)
    Spec.assertEqWith s "combat: the same 2 is marked" (S.damageOf capridor (settle DamageKind.Combat)) (Just 2)
  -- The same refusal driven through a REAL combat phase, which is the funnel
  -- that would run a rider if one fired (Engine's combat damage step drains the
  -- queue): the Capridor blocks, takes 2, and gains nothing.
  Spec.it s "CR 615.5 combat damage puts no counter on, because none of it was prevented" $ do
    plains <- S.printingOf s registry "Plains"
    capridorPrinting <- S.printingOf s registry "Stormwild Capridor"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    let base = S.landsInPlay plains 1
        (_attacker, g1) = S.addCreature pikerPrinting S.alice base
        (capridor, g2) = S.addCreature capridorPrinting S.bob g1
        after =
          S.runCombat S.aggressiveAnswer $
            g2
              { GameState.activePlayer = S.alice,
                GameState.phase = Phase.Combat CombatStep.DeclareAttackers,
                GameState.combat = Combat.emptyCombat {Combat.Type.defender = Just S.bob},
                GameState.remaining =
                  Seq.fromList
                    [ Phase.Combat CombatStep.DeclareBlockers,
                      Phase.Combat CombatStep.CombatDamage,
                      Phase.Combat CombatStep.EndOfCombat,
                      Phase.PostcombatMain
                    ]
              }
    Spec.assertBool s (S.onBattlefield capridor after) "the 1/3 blocker survived a 2-power attacker"
    Spec.assertEqWith s "with the attacker's 2 marked on it" (S.damageOf capridor after) (Just 2)
    Spec.assertEqWith s "and no counters, since nothing was prevented" (countersOn CounterKind.PlusOnePlusOne capridor after) 0
    Spec.assertEqWith s "so it is still a 1/3" (S.powerToughnessOf capridor after) (Just (1, 3))

-- CR 604.2's "as long as" clause on a PRINTED replacement ability, whose producer
-- is Jared Carthalion, True Heir ({R}{G}{W} Legendary Creature -- Human Warrior
-- 3/3): "If damage would be dealt to Jared Carthalion while you're the monarch,
-- prevent that damage and put that many +1/+1 counters on it."
--
-- THREE SEATS, because CR 725.3 makes the monarch a designation exactly one
-- player holds -- on a two-seat board "the monarch" and "your opponent" are the
-- same player, and a gate reading either would pass. Jared is BOB's, the Firebolt
-- is ALICE's, and the seat that is monarch on the negative board is CAROL's: a
-- gate reading the damage's controller, or reading merely that a monarch exists,
-- answers the same on both boards and so fails one of them.
--
-- Numbers distinct: the Firebolt is 2, the counters are 2 because CR 615.5's
-- "that many" says so, and Jared's 3/3 becomes 5/5. A "one counter per event"
-- reading would answer 1 and an unrun rider 0.
jaredCarthalionSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
jaredCarthalionSpec s registry = Spec.describe s "Jared Carthalion, True Heir (CR 604.2)" $ do
  -- alice holds `n` Firebolts over one Mountain apiece, bob's Jared is on the
  -- battlefield, and carol is the third seat. Jared is placed rather than cast,
  -- so his own CR 725.1 enters trigger never fires and each case names the
  -- monarch itself.
  let board n = do
        mountain <- S.printingOf s registry "Mountain"
        jaredPrinting <- S.printingOf s registry "Jared Carthalion, True Heir"
        firebolt <- S.printingOf s registry "Firebolt"
        let withLands = S.landsFor mountain S.alice n S.threePlayerGame
            (jared, g1) = S.addCreature jaredPrinting S.bob withLands
            addOne (ids, g) _ = let (oid, g') = S.addHandCard firebolt S.alice g in (ids <> [oid], g')
            (bolts, g2) = List.foldl' addOne ([], g1) [1 .. n]
        pure
          ( jared,
            bolts,
            g2
              { GameState.phase = Phase.PrecombatMain,
                GameState.activePlayer = S.alice,
                GameState.priority = Just S.alice
              }
          )
  -- THE PROVING TEST. One board, two monarchs, and nothing else differs.
  Spec.it s "CR 604.2 the clause is asked of the ability's controller, not of whoever the monarch is" $ do
    (jared, bolts, g) <- board 1
    let cast gs = case bolts of
          [bolt] -> castAndResolve (aimCreature jared) gs bolt
          _ -> gs
        heldByBob = cast (S.withMonarch S.bob g)
        heldByCarol = cast (S.withMonarch S.carol g)
    Spec.assertEqWith s "setup: Jared is a 3/3" (S.powerToughnessOf jared g) (Just (3, 3))
    -- CR 615.6: the prevented event never happened, so nothing is marked.
    Spec.assertEqWith s "bob is the monarch, so no damage is marked" (S.damageOf jared heldByBob) (Just 0)
    Spec.assertEqWith s "and CR 615.5's rider puts that many +1/+1 counters on" (countersOn CounterKind.PlusOnePlusOne jared heldByBob) 2
    Spec.assertEqWith s "so Jared is a 5/5" (S.powerToughnessOf jared heldByBob) (Just (5, 5))
    -- carol holds the crown on the other board: bob is no more Jared's monarch
    -- than alice is, so the ability does not apply at all.
    Spec.assertEqWith s "carol is the monarch, so the same 2 is marked in full" (S.damageOf jared heldByCarol) (Just 2)
    Spec.assertEqWith s "and no counter is put on" (countersOn CounterKind.PlusOnePlusOne jared heldByCarol) 0
    Spec.assertEqWith s "so Jared is still a 3/3" (S.powerToughnessOf jared heldByCarol) (Just (3, 3))
  -- CR 604.1's "simply true", which is what makes the clause a live read rather
  -- than a latch: the crown changes hands with no trigger and no resolution in
  -- between, and the SAME permanent's ability stops applying. A gate snapshotted
  -- when the ability was gathered -- or when the permanent entered -- would
  -- prevent the second Firebolt too.
  Spec.it s "CR 604.1 the clause is re-asked, so losing the crown turns the ability off" $ do
    (jared, bolts, g) <- board 2
    case bolts of
      [first, second] -> do
        let shielded = castAndResolve (aimCreature jared) (S.withMonarch S.bob g) first
            dethroned = castAndResolve (aimCreature jared) (S.withMonarch S.carol shielded) second
        Spec.assertEqWith s "the first Firebolt is prevented while bob wears the crown" (S.damageOf jared shielded) (Just 0)
        Spec.assertEqWith s "leaving two +1/+1 counters" (countersOn CounterKind.PlusOnePlusOne jared shielded) 2
        Spec.assertEqWith s "the second lands in full once carol has it" (S.damageOf jared dethroned) (Just 2)
        Spec.assertEqWith s "and adds no third counter" (countersOn CounterKind.PlusOnePlusOne jared dethroned) 2
      _ -> Spec.assertFailure s "fixture should hold two Firebolts"

-- CR 615.12's damage that "can't be prevented", whose one producer in the pool
-- is Spider-Punk ({1}{R} Legendary Creature -- Spider Human Hero 2/1, Marvel's
-- Spider-Man 92), set against a COUNTDOWN shield, Mending Hands ("Prevent the
-- next 4 damage that would be dealt to any target this turn"). Fog and Selfless
-- Squire install prevention rows too, but CR 615.7's remaining amount is what
-- clause 3 is about, and the countdown producers in data/cards/ are Mending
-- Hands, Healing Grace, Test of Faith and Decorated Griffin -- Mending Hands
-- being the one that narrows nothing else.
--
-- The rule's first and third clauses: the damage is dealt in full though an
-- applicable shield is there, and "existing damage prevention shields won't be
-- reduced by damage that can't be prevented" -- the shield read here is the
-- countdown amount that sentence names, and every case below asserts it
-- untouched, which is what keeps the middle clause's application from
-- over-reaching into it. The MIDDLE clause is shieldCounterSpec's, where CR
-- 122.1c's amount-independent counter removal makes it observable.
--
-- Not implemented: CR 615.5's authored rider, the other thing that clause would
-- carry (testOfFaithSpec above). No rider in the pool is amount-independent, so
-- nothing here can see it (#1695).
--
-- EVERY case here has a CONTROL on a board that differs in Spider-Punk and in
-- nothing else, so no assertion can pass because the damage would have got
-- through anyway: the control board's shield genuinely prevents it. The first
-- two cases are the two halves of one comparison, one board each; the third runs
-- literally the same script over both.
--
-- The DAMAGE BATCHES are hand-built and the SPELL is not, for mendingHandsSpec's
-- reason: casting Mending Hands for real is what proves the card, while reaching
-- a real two-attacker combat batch would mean driving a whole combat phase to
-- produce a fixture these assertions read straight off.
spiderPunkSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spiderPunkSpec s registry = Spec.describe s "Spider-Punk (CR 615.12)" $ do
  let hit src recipient n =
        DamageEvent.MkDamageEvent src recipient n False False False 0 Nothing DamageKind.Noncombat
      amounts gs = fmap DamageEvent.amount (S.damageEventsOf gs)
      -- alice's Piker is shielded by a Mending Hands she really casts on it;
      -- bob's TWO Pikers are the sources of the hand-built events. Two of them
      -- rather than one because CR 615.7's choice clause is conditioned on
      -- "damage ... by two or more applicable sources at the same time", which
      -- one source repeated does not satisfy on the rule's letter.
      --
      -- The Spider-Punks ride out as a LIST -- singleton or empty -- so both
      -- boards can be driven by one script, and destroying "the Punks" is a
      -- no-op on the control.
      withBoard act = do
        plains <- S.printingOf s registry "Plains"
        pikerPrinting <- S.printingOf s registry "Goblin Piker"
        mendingHands <- S.printingOf s registry "Mending Hands"
        punkPrinting <- S.printingOf s registry "Spider-Punk"
        let build withPunk =
              let base = S.landsInPlay plains 1
                  (victim, g1) = S.addCreature pikerPrinting S.alice base
                  (attacker, g2) = S.addCreature pikerPrinting S.bob g1
                  (other, g3) = S.addCreature pikerPrinting S.bob g2
                  (punk, g4) = S.addCreature punkPrinting S.alice g3
                  (punks, g5) = if withPunk then ([punk], g4) else ([], g3)
                  (g6, spellId) = S.handOne mendingHands g5
               in (victim, attacker, other, punks, castAndResolve (aimCreature victim) g6 spellId)
        act build
  -- THE CONTROL. Without Spider-Punk the shield does its ordinary CR 615.7 job:
  -- the whole 3 is prevented, the event never happens (CR 615.6), and 1 of the
  -- shield's 4 is left. Every refusal below would be true of a board whose
  -- shield had never been there at all if this case did not pass.
  Spec.it s "CR 615.7 without Spider-Punk the shield prevents the whole 3"
    . withBoard
    $ \build -> do
      let (victim, attacker, _, _, shielded) = build False
          after = settleDamage S.identityAnswer shielded [hit attacker (Recipient.ToCreature victim) 3]
      Spec.assertEqWith s "setup: the shield is a floating replacement" (shieldsLeft shielded) [4]
      Spec.assertEqWith s "nothing is marked on the shielded creature" (S.damageOf victim after) (Just 0)
      Spec.assertEqWith s "and no damage event happened at all" (amounts after) []
      Spec.assertEqWith s "3 of the shield's 4 were spent, so 1 remains" (shieldsLeft after) [1]
  -- CR 615.12's first and third clauses on one board. The shield is applicable
  -- and is still applied -- CR 615.12a gives it exactly one application, which
  -- is why this terminates -- but it prevents none of the damage, and it is not
  -- reduced by damage it could not prevent.
  Spec.it s "CR 615.12 with Spider-Punk the same 3 lands in full, and the shield is not reduced"
    . withBoard
    $ \build -> do
      let (victim, attacker, _, _, shielded) = build True
          after = settleDamage S.identityAnswer shielded [hit attacker (Recipient.ToCreature victim) 3]
      Spec.assertEqWith s "setup: the same shield is on the same creature" (shieldsLeft shielded) [4]
      Spec.assertEqWith s "the whole 3 is marked on the shielded creature" (S.damageOf victim after) (Just 3)
      Spec.assertEqWith s "and the event happened, at its full amount" (amounts after) [3]
      Spec.assertEqWith s "the shield still holds all 4" (shieldsLeft after) [4]
  -- The third clause made GAMEPLAY-observable, which the row read above is not:
  -- one script over both boards -- take 3, lose the Punks (CR 604.2), take 2 --
  -- and the shield that was never reduced still covers the 2, where the control's
  -- spent shield cannot.
  Spec.it s "CR 615.12 the unreduced shield still covers the next 2 once Spider-Punk is gone"
    . withBoard
    $ \build -> do
      let script (victim, attacker, _, punks, shielded) =
            let first_ = settleDamage S.identityAnswer shielded [hit attacker (Recipient.ToCreature victim) 3]
                gone = S.runPure S.identityAnswer first_ (Event.destroy Regenerability.Regenerable punks)
             in settleDamage S.identityAnswer gone [hit attacker (Recipient.ToCreature victim) 2]
          punkBoard@(punkVictim, _, _, _, _) = build True
          control@(controlVictim, _, _, _, _) = build False
      Spec.assertEqWith s "with Spider-Punk only the first 3 is marked: the 2 is prevented whole" (S.damageOf punkVictim (script punkBoard)) (Just 3)
      Spec.assertEqWith s "and 2 of the shield's untouched 4 are left" (shieldsLeft (script punkBoard)) [2]
      Spec.assertEqWith s "without it the first 3 was prevented, so only 1 of the 2 is" (S.damageOf controlVictim (script control)) (Just 1)
      Spec.assertEqWith s "and that shield is spent to 0 and gone" (shieldsLeft (script control)) []
  -- The ELISION half, and the CR 615.7 prompt's other gate: two applicable
  -- sources deal damage to the shielded creature at the same time, and the rule
  -- gives its controller the choice of which the shield prevents -- but only
  -- when that choice can change the board. It cannot here, since an
  -- unpreventable batch costs THIS shield nothing in any order (CR 615.12's last
  -- sentence), so nothing is asked and the whole 8 lands either way. A CR 122.1c
  -- shield counter is the opposite and is asked about, since the rule's middle
  -- clause spends it either way -- shieldCounterSpec's CR 101.4c pair.
  Spec.it s "CR 615.12 / 615.7 an unpreventable batch asks the shielded creature's controller nothing"
    . withBoard
    $ \build -> do
      let (victim, attacker, other, _, shielded) = build True
          (controlVictim, controlAttacker, controlOther, _, controlShielded) = build False
          batch = [hit attacker (Recipient.ToCreature victim) 5, hit other (Recipient.ToCreature victim) 3]
          controlBatch = [hit controlAttacker (Recipient.ToCreature controlVictim) 5, hit controlOther (Recipient.ToCreature controlVictim) 3]
      Spec.assertBool
        s
        (wasAskedToOrderDamage (answersFor S.identityAnswer controlShielded (Damage.applyDamage controlBatch)))
        "setup: without Spider-Punk 4 cannot cover 5 and 3, so alice is asked"
      Spec.assertBool
        s
        (not (wasAskedToOrderDamage (answersFor S.identityAnswer shielded (Damage.applyDamage batch))))
        "no OrderDamage was raised: no order of unpreventable damage spends the shield"
      let after = settleDamage S.identityAnswer shielded batch
      Spec.assertEqWith s "both events happened in full" (amounts after) [5, 3]
      Spec.assertEqWith s "the whole 8 is marked" (S.damageOf victim after) (Just 8)
      Spec.assertEqWith s "and the shield is untouched" (shieldsLeft after) [4]

-- The players asked to decide something while a damage batch settles, in the
-- order they were asked. Both batch-level questions count: CR 616.1's "which
-- effect applies next" and CR 615.7's "which damage does the shield prevent".
--
-- The prompt STREAM rather than the board, because there is no board that can
-- tell these two apart: the two players choose about DIFFERENT objects -- each
-- orders the effects hitting their own creature -- so neither answer constrains
-- the other and the same permanents end up in the same state whoever was asked
-- first. What CR 616.1's last sentence governs is which player is asked, and
-- under CR 101.4b that is information the later chooser gets to have.
--
-- Not a claim about Magic: an effect reachable from two players' events at once
-- and spendable only once WOULD make this board-visible, and pawl has no such
-- effect to build with today. Every replacement the pool can produce is either
-- unlimited (Furnace of Rath) or names one recipient (Mending Hands).
choosersAsked :: GameState.GameState -> [DamageEvent.DamageEvent] -> [PlayerId.PlayerId]
choosersAsked gs batch =
  let step :: Prompt.Prompt r -> State.State [PlayerId.PlayerId] r
      step p = case p of
        Prompt.ChooseReplacement _ pid _ -> do
          State.modify' (<> [pid])
          pure 0
        Prompt.OrderDamage _ pid events -> do
          State.modify' (<> [pid])
          pure (zipWith const [0 ..] events)
        _ -> pure (S.identityAnswer p)
   in State.execState (Engine.runGame step gs (Damage.applyDamage batch)) []

-- A Furnace of Rath under alice, one Goblin Piker each side, and a Mending Hands
-- shield on both creatures -- so every event addressed to either creature has
-- two applicable effects that differ, and both controllers owe a CR 616.1
-- choice. Answers the state, alice's creature and bob's.
doubledAndShielded :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId)
doubledAndShielded plains piker furnace mendingHands =
  let base = S.landsInPlay plains 2
      (_, g1) = S.addCreature furnace S.alice base
      (hers, g2) = S.addCreature piker S.alice g1
      (his, g3) = S.addCreature piker S.bob g2
      (g4, firstShield) = S.handOne mendingHands g3
      (g5, secondShield) = S.handOne mendingHands g4
   in (castAndResolve (aimCreature his) (castAndResolve (aimCreature hers) g5 firstShield) secondShield, hers, his)

-- Spend a prevention shield on `src`'s hit first (CR 615.7), and take the shield
-- over `furnace` whenever both are offered (CR 616.1). The second half is named
-- as "not the Furnace" because a floating row's `source` is the spell that
-- installed it -- a CR 608.2n object no fixture holds an id for -- while the
-- permanent's row is the Furnace itself. Pinning it is what makes the amounts
-- the rule's rather than an artefact of which candidate is canonical.
--
-- Top-level rather than a `where` binding, for settleDamage's reason and one
-- more: MonoLocalBinds (implied by GADTs, on at the top of this module) declines
-- to generalize a local binding that closes over a local, which `furnace` is.
allocateShield :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
allocateShield furnace src p = case p of
  Prompt.OrderDamage _ _ events ->
    let key e = (DamageEvent.source e /= src, DamageEvent.source e)
     in fmap fst (List.sortOn (key . snd) (zip [0 ..] events))
  Prompt.ChooseReplacement _ _ entries ->
    maybe 0 Int.toNaturalSaturating (List.findIndex ((/= furnace) . ReplacementEntry.source) entries)
  _ -> S.identityAnswer p

-- CR 616.1's last sentence: "If two or more players have to make these choices
-- at the same time, choices are made in APNAP order (see rule 101.4)."
--
-- The board that reaches it needs one batch whose events are addressed to two
-- players' objects, and TWO DISTINGUISHABLE applicable effects per event --
-- otherwise `choose` elides the prompt and nobody is asked anything. Furnace of
-- Rath ({1}{R}{R}{R} Enchantment, "If a source would deal damage to a permanent
-- or player, it deals double that damage to that permanent or player instead")
-- is symmetric, so it supplies one candidate to every event in the batch; a
-- Mending Hands on each creature supplies the second, and a doubler and a shield
-- differ in `effect`, so neither pair is elided.
--
-- Two Furnaces would NOT do it, which is the trap this fixture avoids: two
-- copies carry the same ReplacementEffect.DamageR, `distinguishing` finds them
-- interchangeable and the prompt is correctly elided. The rule needs candidates
-- that differ, not merely candidates that are several.
--
-- The DAMAGE BATCH is hand-built and the SPELLS are not, for mendingHandsSpec's
-- reason -- and here the batch's ORDER is the input under test, which only a
-- hand-built batch can state.
apnapSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
apnapSpec s registry = Spec.describe s "APNAP (CR 616.1)" $ do
  let hit src recipient n =
        DamageEvent.MkDamageEvent src recipient n False False False 0 Nothing DamageKind.Noncombat
  -- Both batch orders, because only the PAIR discriminates: settling the batch
  -- in gather order already answers [alice, bob] when alice's event happens to
  -- come first, and would answer [bob, alice] when it does not.
  Spec.it s "CR 616.1 two players choosing for one batch are asked in APNAP order" $ do
    plains <- S.printingOf s registry "Plains"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    furnace <- S.printingOf s registry "Furnace of Rath"
    mendingHands <- S.printingOf s registry "Mending Hands"
    let (shielded, hers, his) = doubledAndShielded plains pikerPrinting furnace mendingHands
        toHers = hit his (Recipient.ToCreature hers) 1
        toHis = hit hers (Recipient.ToCreature his) 1
    Spec.assertEqWith s "setup: alice is the active player" (Game.apnapOrder shielded) [S.alice, S.bob]
    Spec.assertEqWith s "setup: both creatures are shielded" (length (GameState.replacements shielded)) 2
    Spec.assertEqWith
      s
      "alice chooses before bob when her event is gathered first"
      (choosersAsked shielded [toHers, toHis])
      [S.alice, S.bob]
    Spec.assertEqWith
      s
      "and still before bob when his event is gathered first"
      (choosersAsked shielded [toHis, toHers])
      [S.alice, S.bob]
  -- The reason CR 615.7 and CR 616.1's APNAP clause do not contend for the same
  -- ordering, stated as a board. They order DIFFERENT LEVELS: APNAP orders the
  -- choosers, CR 615.7 orders one chooser's own events among themselves, and CR
  -- 101.4c is what licenses the second ("If a player would make more than one
  -- choice at the same time, the player makes the choices in the order
  -- specified"). Nothing forces a pick between them.
  --
  -- What makes that structural rather than lucky: a shield names ONE recipient,
  -- so every event a shield contests is addressed to one player's object -- and
  -- that is the same player CR 616.1 asks about those events, since `contested`
  -- and `choose` read the chooser off the recipient through one `chooserOf`. A
  -- CR 615.7 group can therefore never straddle two CR 616.1 choosers, which is
  -- exactly the shape a genuine collision would need.
  --
  -- Two events at alice's creature and one at bob's, with alice's shield too
  -- small for both of hers: she is asked to allocate it (CR 615.7) and asked
  -- twice which effect applies (CR 616.1), and all three of her questions come
  -- before bob's. That her allocation is then HONOURED is mendingHandsSpec's
  -- "the shielded PLAYER chooses which of two simultaneous damages the shield
  -- prevents", which this fixture does not restate.
  Spec.it s "CR 615.7's order sits INSIDE one chooser's APNAP turn, not across choosers" $ do
    plains <- S.printingOf s registry "Plains"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    furnace <- S.printingOf s registry "Furnace of Rath"
    mendingHands <- S.printingOf s registry "Mending Hands"
    let (shielded, hers, his) = doubledAndShielded plains pikerPrinting furnace mendingHands
        -- 1 and 4 against a shield of 4: their total exceeds it, so CR 615.7 has
        -- something to ask. The small one first is load-bearing -- doubled by the
        -- Furnace it still costs the shield at most 2, so the shield is still
        -- standing for the 4 and alice's SECOND CR 616.1 choice is a real
        -- question rather than a lone candidate `choose` would elide.
        batch = [hit hers (Recipient.ToCreature his) 1, hit his (Recipient.ToCreature hers) 1, hit his (Recipient.ToCreature hers) 4]
    Spec.assertEqWith
      s
      "alice allocates her shield and settles both her events before bob is asked anything"
      (choosersAsked shielded batch)
      [S.alice, S.alice, S.alice, S.bob]
  -- The other half of "they compose": alice's CR 615.7 answer must still land on
  -- the events she was asked about. `contested` reports BATCH POSITIONS and
  -- `askOne` splices by position, so the sort has to happen before the positions
  -- are computed -- sorting afterwards would leave alice permuting whatever now
  -- sits at her old indices, which here includes bob's event.
  --
  -- The assertion is which event SURVIVED rather than how much damage landed,
  -- because the total cannot tell the two answers apart: the shield prevents 4
  -- either way and the Furnace doubles what is left of 5, so alice takes 2
  -- whichever event she spends it on. What differs is WHICH source dealt it (CR
  -- 615.6: a fully prevented event never happens), which is why the two hits at
  -- her creature come from two different creatures of bob's.
  Spec.it s "CR 615.7's allocation lands on the events it was asked about, after the sort" $ do
    plains <- S.printingOf s registry "Plains"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    furnace <- S.printingOf s registry "Furnace of Rath"
    mendingHands <- S.printingOf s registry "Mending Hands"
    let base = S.landsInPlay plains 1
        (theFurnace, g1) = S.addCreature furnace S.alice base
        (hers, g2) = S.addCreature pikerPrinting S.alice g1
        (small, g3) = S.addCreature pikerPrinting S.bob g2
        (big, g4) = S.addCreature pikerPrinting S.bob g3
        (g5, shieldSpell) = S.handOne mendingHands g4
        shielded = castAndResolve (aimCreature hers) g5 shieldSpell
        -- Bob's event FIRST, so alice's two sit at positions 1 and 2 before the
        -- sort and at 0 and 1 after it.
        batch =
          [ hit hers (Recipient.ToCreature small) 1,
            hit small (Recipient.ToCreature hers) 1,
            hit big (Recipient.ToCreature hers) 4
          ]
        survivors gs = fmap DamageEvent.source (S.damageEventsOf gs)
    Spec.assertEqWith s "setup: alice's creature is the shielded one" (length (GameState.replacements shielded)) 1
    -- Shield on the 1: it is prevented whole and never happens, and the 4 keeps
    -- the remaining 3 off, leaving 1 for the Furnace to double.
    Spec.assertEqWith
      s
      "alice spends the shield on the small hit: the big one is what gets through"
      (survivors (settleDamage (allocateShield theFurnace small) shielded batch))
      [big, hers]
    -- Shield on the 4: 4 covers it whole, and the 1 is then unshielded.
    Spec.assertEqWith
      s
      "alice spends it on the big hit instead: the small one gets through"
      (survivors (settleDamage (allocateShield theFurnace big) shielded batch))
      [small, hers]

-- CR 615.12 NARROWED, whose producer is Excruciator ({6}{R}{R} Creature --
-- Avatar 7/7, Ravnica: City of Guilds 121, "Damage that would be dealt by this
-- creature can't be prevented"). Spider-Punk's sentence names no quality of the
-- damage and this one names its SOURCE -- CR 120.1's "an object that deals
-- damage is the source of that damage", which is Pawl.Types.DamagePattern's
-- `whatSource`.
--
-- ONE board carries both directions, and that is the whole point of the group:
-- alice's Goblin Piker is shielded by a Mending Hands she really casts on it,
-- and bob controls Excruciator AND a Goblin Piker. The first two cases send the
-- same 3 from each of them into that one shield -- the Excruciator's lands whole
-- and costs the shield nothing, the Piker's is prevented whole and spends 3 of
-- the shield's 4 -- so nothing but the damage's SOURCE differs between them, and
-- neither can pass because the damage would have got through anyway. The third
-- puts both in one batch.
--
-- The DAMAGE BATCHES are hand-built and the SPELL is not, for spiderPunkSpec's
-- reason.
excruciatorSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
excruciatorSpec s registry = Spec.describe s "Excruciator (CR 615.12)" $ do
  let hit src recipient n =
        DamageEvent.MkDamageEvent src recipient n False False False 0 Nothing DamageKind.Noncombat
      amounts gs = fmap DamageEvent.amount (S.damageEventsOf gs)
      withBoard act = do
        plains <- S.printingOf s registry "Plains"
        pikerPrinting <- S.printingOf s registry "Goblin Piker"
        mendingHands <- S.printingOf s registry "Mending Hands"
        excruciator <- S.printingOf s registry "Excruciator"
        let base = S.landsInPlay plains 1
            (victim, g1) = S.addCreature pikerPrinting S.alice base
            (piker, g2) = S.addCreature pikerPrinting S.bob g1
            (avatar, g3) = S.addCreature excruciator S.bob g2
            (g4, spellId) = S.handOne mendingHands g3
        act victim piker avatar (castAndResolve (aimCreature victim) g4 spellId)
  -- THE CONTROL, and it shares its board with the case below rather than
  -- standing on a second one: the shield is applicable to the Piker's damage and
  -- prevents the whole of it, though an Excruciator is on the battlefield the
  -- entire time. A pattern that admitted every source would fail here.
  Spec.it s "CR 615.7 the shield still prevents the Goblin Piker's 3 whole"
    . withBoard
    $ \victim piker _ shielded -> do
      let after = settleDamage S.identityAnswer shielded [hit piker (Recipient.ToCreature victim) 3]
      Spec.assertEqWith s "setup: the shield is a floating replacement" (shieldsLeft shielded) [4]
      Spec.assertEqWith s "nothing is marked on the shielded creature" (S.damageOf victim after) (Just 0)
      Spec.assertEqWith s "and no damage event happened at all" (amounts after) []
      Spec.assertEqWith s "3 of the shield's 4 were spent, so 1 remains" (shieldsLeft after) [1]
  -- CR 615.12 for the source the clause names: the same shield, on the same
  -- creature, on the same board, prevents none of the Excruciator's 3 and is not
  -- reduced by it.
  Spec.it s "CR 615.12 the Excruciator's 3 lands in full, and the shield is not reduced"
    . withBoard
    $ \victim _ avatar shielded -> do
      let after = settleDamage S.identityAnswer shielded [hit avatar (Recipient.ToCreature victim) 3]
      Spec.assertEqWith s "setup: the same shield is on the same creature" (shieldsLeft shielded) [4]
      Spec.assertEqWith s "the whole 3 is marked on the shielded creature" (S.damageOf victim after) (Just 3)
      Spec.assertEqWith s "and the event happened, at its full amount" (amounts after) [3]
      Spec.assertEqWith s "the shield still holds all 4" (shieldsLeft after) [4]
  -- Both directions in ONE batch, which is what makes the narrowing a per-EVENT
  -- fact rather than a per-board one: CR 615.12's clause reaches the
  -- Excruciator's event and leaves the Piker's alone, in a batch the engine
  -- settles together.
  --
  -- The two amounts DIFFER, and that is what makes the case discriminate: with
  -- 3 and 3 an engine that had the two events exactly backwards would leave the
  -- same board, and every assertion here would pass on it.
  Spec.it s "CR 615.12 one batch: the Excruciator's 3 lands and the Piker's 2 is prevented"
    . withBoard
    $ \victim piker avatar shielded -> do
      let after = settleDamage S.identityAnswer shielded [hit avatar (Recipient.ToCreature victim) 3, hit piker (Recipient.ToCreature victim) 2]
      Spec.assertEqWith s "only the Excruciator's event happened" (amounts after) [3]
      Spec.assertEqWith s "so only its 3 is marked" (S.damageOf victim after) (Just 3)
      Spec.assertEqWith s "and only the Piker's 2 came off the shield" (shieldsLeft after) [2]

-- CR 615.12 narrowed on TWO axes at once, whose producer is Questing Beast
-- ({2}{G}{G} Legendary Creature -- Beast 4/4, Throne of Eldraine 171, "Combat
-- damage that would be dealt by creatures you control can't be prevented").
-- Excruciator's clause above names ONE object (Filter.IsSource) and says nothing
-- about the kind; this one names a SET by characteristic and pins CR 120.1's
-- source to CR 510.2's combat damage, so it is the pool's one statement of CR
-- 615.12 where both `whichKind` and a characteristic `whatSource` have to be
-- read -- Excruciator's and Malignus' write IsSource and Spider-Punk's writes
-- neither field.
--
-- CR 109.5: the "you" inside whatSource is the ABILITY'S SOURCE's controller,
-- which is the Maybe ObjectId Pawl.Engine.PlayerEffect.unpreventable threads out
-- beside each pattern -- bob's, and not the shielded creature's controller.
--
-- ONE board carries all four cases, excruciatorSpec's design and for its reason:
-- alice's Goblin Piker is shielded by a Mending Hands she really casts on it,
-- alice controls a SECOND Piker, and bob controls a Piker and the Beast. Nothing
-- but the axis under test differs between the cases -- the source's controller
-- in the first two, the damage's KIND in the third -- so neither control can
-- pass because the damage would have got through anyway.
--
-- The DAMAGE BATCHES are hand-built and the SPELL is not, for spiderPunkSpec's
-- reason: that is what lets a case name DamageKind.Combat without driving a
-- whole combat phase to produce a fixture these assertions read straight off.
questingBeastSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
questingBeastSpec s registry = Spec.describe s "Questing Beast (CR 615.12)" $ do
  let hit kind src recipient n =
        DamageEvent.MkDamageEvent src recipient n False False False 0 Nothing kind
      amounts gs = fmap DamageEvent.amount (S.damageEventsOf gs)
      withBoard act = do
        plains <- S.printingOf s registry "Plains"
        pikerPrinting <- S.printingOf s registry "Goblin Piker"
        mendingHands <- S.printingOf s registry "Mending Hands"
        questingBeast <- S.printingOf s registry "Questing Beast"
        let base = S.landsInPlay plains 1
            (victim, g1) = S.addCreature pikerPrinting S.alice base
            (hers, g2) = S.addCreature pikerPrinting S.alice g1
            (his, g3) = S.addCreature pikerPrinting S.bob g2
            (_, g4) = S.addCreature questingBeast S.bob g3
            (g5, spellId) = S.handOne mendingHands g4
        act victim hers his (castAndResolve (aimCreature victim) g5 spellId)
  -- THE whatSource CONTROL: the same shield, on the same board, with the Beast on
  -- the battlefield the whole time, prevents the combat damage of a creature
  -- ALICE controls whole. "Creatures you control" is bob's set, so a pattern that
  -- dropped the ControlledBy atom would fail here.
  Spec.it s "CR 615.7 combat damage from a creature the Beast's controller does NOT control is prevented"
    . withBoard
    $ \victim hers _ shielded -> do
      let after = settleDamage S.identityAnswer shielded [hit DamageKind.Combat hers (Recipient.ToCreature victim) 3]
      Spec.assertEqWith s "setup: the shield is a floating replacement" (shieldsLeft shielded) [4]
      Spec.assertEqWith s "nothing is marked on the shielded creature" (S.damageOf victim after) (Just 0)
      Spec.assertEqWith s "and no damage event happened at all" (amounts after) []
      Spec.assertEqWith s "3 of the shield's 4 were spent, so 1 remains" (shieldsLeft after) [1]
  -- THE CARD: the same shield, the same creature, the same 3, and the source is
  -- now a creature BOB controls. It lands whole and the shield is not reduced
  -- (CR 615.12's last sentence).
  --
  -- Bob's GOBLIN PIKER and not the Beast itself, which is what separates this
  -- clause from Excruciator's: an engine reading whatSource as Filter.IsSource
  -- would prevent this damage.
  Spec.it s "CR 615.12 combat damage from a creature the Beast's controller DOES control lands in full"
    . withBoard
    $ \victim _ his shielded -> do
      let after = settleDamage S.identityAnswer shielded [hit DamageKind.Combat his (Recipient.ToCreature victim) 3]
      Spec.assertEqWith s "setup: the same shield is on the same creature" (shieldsLeft shielded) [4]
      Spec.assertEqWith s "the whole 3 is marked on the shielded creature" (S.damageOf victim after) (Just 3)
      Spec.assertEqWith s "and the event happened, at its full amount" (amounts after) [3]
      Spec.assertEqWith s "the shield still holds all 4" (shieldsLeft after) [4]
  -- THE whichKind CONTROL: the SAME source, the SAME shield, the same 3 -- only
  -- CR 510.2's kind differs, and the damage is prevented. This is the assertion
  -- that keeps whichKind from being decoration.
  Spec.it s "CR 615.7 NONcombat damage from that same creature is prevented"
    . withBoard
    $ \victim _ his shielded -> do
      let after = settleDamage S.identityAnswer shielded [hit DamageKind.Noncombat his (Recipient.ToCreature victim) 3]
      Spec.assertEqWith s "setup: the same shield is on the same creature" (shieldsLeft shielded) [4]
      Spec.assertEqWith s "nothing is marked on the shielded creature" (S.damageOf victim after) (Just 0)
      Spec.assertEqWith s "and no damage event happened at all" (amounts after) []
      Spec.assertEqWith s "3 of the shield's 4 came off it" (shieldsLeft after) [1]
  -- Both directions in ONE batch, which is what makes the narrowing a per-EVENT
  -- fact rather than a per-board one, exactly as excruciatorSpec's third case is.
  --
  -- The two amounts DIFFER, and that is what makes the case discriminate: with 3
  -- and 3 an engine that had the two events exactly backwards would leave the
  -- same board and every assertion here would pass on it.
  Spec.it s "CR 615.12 one batch: bob's Piker's combat 3 lands and alice's Piker's combat 2 is prevented"
    . withBoard
    $ \victim hers his shielded -> do
      let after = settleDamage S.identityAnswer shielded [hit DamageKind.Combat his (Recipient.ToCreature victim) 3, hit DamageKind.Combat hers (Recipient.ToCreature victim) 2]
      Spec.assertEqWith s "only bob's Piker's event happened" (amounts after) [3]
      Spec.assertEqWith s "so only its 3 is marked" (S.damageOf victim after) (Just 3)
      Spec.assertEqWith s "and only alice's Piker's 2 came off the shield" (shieldsLeft after) [2]

-- CR 615.1 / 609.7b: a shield that names its source by CHARACTERISTIC rather than
-- by identity, whose producer is Luminesce ({W} Instant, Tenth Edition 28,
-- "Prevent all damage that black sources and red sources would deal this turn").
-- Fog with a colour filter on the source and nothing else: no recipient, no
-- amount, no choice made at creation, so it isolates that one axis.
--
-- THE VACUITY TRAP is that "prevented" and "prevents everything" leave the same
-- board when every source on it matches, so bob's three creatures are a Bog
-- Wraith (black), a Goblin Piker (red) and a War Mammoth (green), and all three
-- deal damage in ONE batch. The amounts are 4, 2 and 3 -- distinct, so every
-- reading of the card lands alice on a different life total: 20 - 3 = 17 is the
-- card, 11 prevents nothing, 13 and 15 each drop one of the two disjuncts, and 20
-- ignores the filter altogether.
--
-- The DAMAGE BATCH is hand-built and the SPELL is not, for mendingHandsSpec's
-- reason.
luminesceSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
luminesceSpec s registry = Spec.describe s "Luminesce (CR 615.1, CR 609.7b)" $ do
  let hit src n = DamageEvent.MkDamageEvent src (Recipient.ToPlayer S.alice) n False False False 0 Nothing DamageKind.Noncombat
      amounts gs = fmap DamageEvent.amount (S.damageEventsOf gs)
      withBoard act = do
        plains <- S.printingOf s registry "Plains"
        wraithPrinting <- S.printingOf s registry "Bog Wraith"
        pikerPrinting <- S.printingOf s registry "Goblin Piker"
        mammothPrinting <- S.printingOf s registry "War Mammoth"
        luminesce <- S.printingOf s registry "Luminesce"
        let base = S.landsInPlay plains 1
            (wraith, g1) = S.addCreature wraithPrinting S.bob base
            (piker, g2) = S.addCreature pikerPrinting S.bob g1
            (mammoth, g3) = S.addCreature mammothPrinting S.bob g2
            (g4, spellId) = S.handOne luminesce g3
        act wraith piker mammoth (castAndResolve S.identityAnswer g4 spellId)
  Spec.it s "CR 615.1 the green source's 3 lands and the black and red 4 and 2 are prevented"
    . withBoard
    $ \wraith piker mammoth shielded -> do
      let after = S.runPure S.identityAnswer shielded (Damage.applyDamage [hit mammoth 3, hit wraith 4, hit piker 2])
      Spec.assertEqWith s "setup: the shield is a floating replacement" (length (GameState.replacements shielded)) 1
      Spec.assertEqWith s "setup: alice starts on 20" (S.lifeOf S.alice shielded) (Just 20)
      Spec.assertEqWith s "only the War Mammoth's event happened" (amounts after) [3]
      Spec.assertEqWith s "so alice loses 3 and no more" (S.lifeOf S.alice after) (Just 17)
      Spec.assertEqWith s "and it lasts the turn rather than being used up (CR 615.3)" (length (GameState.replacements after)) 1
  -- CR 609.7b's RECHECK, which is what makes this a filter rather than a list of
  -- objects captured when the shield was made: the Piker's damage is prevented
  -- while it is red and dealt in full once it is not. One board, one shield, and
  -- the only thing that differs between the two readings is the source's colour.
  Spec.it s "CR 609.7b the shield rechecks the source: a Piker made green deals its 2"
    . withBoard
    $ \_ piker _ shielded -> do
      let bleached = S.withEffect piker (Modification.SetColor (Set.singleton Color.Green))
          after = S.runPure S.identityAnswer (bleached shielded) (Damage.applyDamage [hit piker 2])
          asPrinted = S.runPure S.identityAnswer shielded (Damage.applyDamage [hit piker 2])
      Spec.assertEqWith s "while red, the Piker's 2 is prevented whole" (S.lifeOf S.alice asPrinted) (Just 20)
      Spec.assertEqWith s "once green, the same 2 is dealt" (S.lifeOf S.alice after) (Just 18)
  -- CR 608.2h's DEPARTED source, the half neither case above reaches: both of
  -- them deal damage from a permanent still on the battlefield, so the recheck
  -- reads a live projection either way. Ghitu Fire-Eater ({2}{R} Creature --
  -- Human Nomad 2/2, "{T}, Sacrifice this creature: It deals damage equal to its
  -- power to any target") pays the source's own departure as a COST, so the id
  -- the damage event carries names nothing by the time the ability resolves --
  -- no response and no prompt stands between the departure and the read.
  --
  -- THE VACUITY TRAP here is the AMOUNT collapsing to 0: were Quantity.Power to
  -- read the dead id's blank view, both readings would leave bob on 20 and the
  -- case would pass whatever the shield did. Pawl.ActivateSpec's "CR 113.7a whole
  -- card" case pins the amount at 2 with the source already gone, which is what
  -- keeps 20 and 18 apart below. That trap is not hypothetical: emptying
  -- GameState.lastKnown outright is NOT a usable mutation here, because the same
  -- store feeds Quantity.Power, the amount collapses to nothing, and the red board
  -- below lands on 20 under both readings. Only the green board catches it.
  --
  -- TWO boards differing in exactly one thing, because one cannot separate the
  -- readings on its own. Pairing (red, green) the shield gives (20, 18) reading
  -- the filed record, (20, 20) reading the printed card -- the print is red --
  -- (20, 20) admitting any departed source unconditionally, and (18, 18) reading
  -- the live projection of a dead id, which is what the engine did before #1844.
  -- The green board rests on the record holding the PROJECTED characteristics
  -- rather than the printed face; Pawl.ActivateSpec's pumped Fire-Eater is the
  -- direct proof of that, and this is the same fact read on the colour axis.
  --
  -- bob is the recipient because a PLAYER puts no toughness, no state-based
  -- action and no marked damage between the divergence and the read. Two seats
  -- and not three: nothing in CR 608.2h, CR 609.7b or Luminesce's text says
  -- "opponent", "that player" or "defending player" -- the shield is
  -- source-scoped and the recipient is whatever the ability targeted.
  let ghituBoard tint act = do
        plains <- S.printingOf s registry "Plains"
        ghitu <- S.printingOf s registry "Ghitu Fire-Eater"
        luminesce <- S.printingOf s registry "Luminesce"
        let base = S.landsInPlay plains 1
            (fireEater, g1) = S.addCreature ghitu S.alice base
            (g2, spellId) = S.handOne luminesce g1
            shielded = tint fireEater (castAndResolve S.identityAnswer g2 spellId)
        act fireEater shielded (S.runPure (aimPlayer S.bob) shielded (Activate.activateAbility S.alice fireEater (theAbility ghitu) Monad.>> Stack.resolveTop))
  Spec.it s "CR 608.2h a sacrificed red source's damage is still prevented: the shield reads last known information"
    . ghituBoard (\_ gs -> gs)
    $ \fireEater shielded after -> do
      Spec.assertEqWith s "bob takes nothing: the departed source is read as the red creature it last was" (S.lifeOf S.bob after) (Just 20)
      Spec.assertBool s (not (Set.member fireEater (GameState.battlefield after))) "setup: the cost really did remove the source"
      Spec.assertBool s (Maybe.isNothing (Game.lookupObject fireEater after)) "setup: and the id it left behind names nothing"
      Spec.assertEqWith s "setup: the shield was a floating replacement before the activation" (length (GameState.replacements shielded)) 1
      Spec.assertEqWith s "supporting: and it lasts the turn rather than being used up (CR 615.3)" (length (GameState.replacements after)) 1
  Spec.it s "CR 609.7b the recheck reads the RECORD, not the print: a Fire-Eater made green before it left deals its 2"
    . ghituBoard (\oid -> S.withEffect oid (Modification.SetColor (Set.singleton Color.Green)))
    $ \fireEater _ after -> do
      Spec.assertEqWith s "bob takes 2: neither disjunct matches the green source the record filed" (S.lifeOf S.bob after) (Just 18)
      Spec.assertBool s (Maybe.isNothing (Game.lookupObject fireEater after)) "setup: the source departed here too, so the two boards differ only in colour"

-- CR 615.1 / 609.7b again, on two axes at once: a shield that names its source by
-- CHARACTERISTIC and narrows by DAMAGE KIND. Its producer is Moonmist ({1}{G}
-- Instant, "Transform all Humans. Prevent all combat damage that would be dealt
-- this turn by creatures other than Werewolves and Wolves"). Luminesce with a
-- subtype EXCLUSION where Luminesce has a colour list, plus the kind narrowing
-- Luminesce's card has not.
--
-- THE VACUITY TRAP is that "prevented" and "prevents everything" leave the same
-- board when every source matches or none does, so the batch carries THREE
-- sources whose readings all differ: a Wolf (Russet Wolves), a Werewolf (Tovolar,
-- Dire Overlord) and neither (Goblin Piker). The discriminating assertion is on
-- the SURVIVING EVENTS' SOURCE IDS rather than on alice's life, because the Wolf
-- and the Werewolf deal the same 3: a life total cannot tell "Werewolf dropped
-- from the Or" from "Wolf dropped from the Or", and would pass under either.
--
-- Tovolar's front face is a Human Werewolf, so Moonmist's OWN first sentence
-- names him -- and CR 702.145b's third static ability refuses it (Pawl.DaytimeSpec's
-- restrictionSpec is the proof). Nothing here rests on which way that goes: the
-- damage amounts are hand-built rather than read off power, and both of his faces
-- are Werewolves.
--
-- The DAMAGE BATCH is hand-built and the SPELL is not, for mendingHandsSpec's
-- reason.
--
-- The card's `HasCardType Creature` conjunct ("by CREATURES other than ...") is a
-- REGRESSION FENCE here rather than a proved behaviour: deleting it leaves the
-- whole suite green, because CR 510.1a makes attacking and blocking creatures the
-- only assigners of combat damage, so no rules-legal board separates the two
-- readings. It is written because a hand-built batch like this one can name a
-- Forest as a Combat source, and the card file should say what the card says.
moonmistSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
moonmistSpec s registry = Spec.describe s "Moonmist (CR 615.1, CR 609.7b)" $ do
  let hit kind src n = DamageEvent.MkDamageEvent src (Recipient.ToPlayer S.alice) n False False False 0 Nothing kind
      sources gs = fmap DamageEvent.source (S.damageEventsOf gs)
      withBoard act = do
        forest <- S.printingOf s registry "Forest"
        wolfPrinting <- S.printingOf s registry "Russet Wolves"
        werewolfPrinting <- S.printingOf s registry "Tovolar, Dire Overlord"
        pikerPrinting <- S.printingOf s registry "Goblin Piker"
        moonmist <- S.printingOf s registry "Moonmist"
        let base = S.landsInPlay forest 2
            (wolf, g1) = S.addCreature wolfPrinting S.bob base
            (werewolf, g2) = S.addCreature werewolfPrinting S.bob g1
            (piker, g3) = S.addCreature pikerPrinting S.bob g2
            (g4, spellId) = S.handOne moonmist g3
        act wolf werewolf piker (castAndResolve S.castAnswer g4 spellId)
  Spec.it s "CR 615.1 the Piker's combat 2 is prevented and the Wolf's and the Werewolf's 3s land"
    . withBoard
    $ \wolf werewolf piker shielded -> do
      let batch = [hit DamageKind.Combat wolf 3, hit DamageKind.Combat werewolf 3, hit DamageKind.Combat piker 2]
          after = S.runPure S.identityAnswer shielded (Damage.applyDamage batch)
      -- The behavioural assertion leads, so no proxy below it can absorb a
      -- mutation to the card's filter and report itself instead.
      Spec.assertEqWith s "the Wolf's and the Werewolf's events happened, the Piker's did not" (sources after) [wolf, werewolf]
      Spec.assertEqWith s "so alice loses 3 and 3 and no more" (S.lifeOf S.alice after) (Just 14)
      Spec.assertEqWith s "supporting: the shield is a floating replacement" (length (GameState.replacements shielded)) 1
      Spec.assertEqWith s "supporting: alice started on 20" (S.lifeOf S.alice shielded) (Just 20)
      Spec.assertEqWith s "and it lasts the turn rather than being used up (CR 615.3)" (length (GameState.replacements after)) 1
  -- The KIND half, which luminesceSpec's card cannot reach: the very same Piker
  -- dealing the very same 2 as NONCOMBAT damage is not prevented. A card file that
  -- dropped whichKind passes the case above and fails this one.
  Spec.it s "CR 615.1 the same source's NONCOMBAT 2 is not prevented"
    . withBoard
    $ \_ _ piker shielded -> do
      let after = S.runPure S.identityAnswer shielded (Damage.applyDamage [hit DamageKind.Noncombat piker 2])
      Spec.assertEqWith s "the event happened" (sources after) [piker]
      Spec.assertEqWith s "and alice took it" (S.lifeOf S.alice after) (Just 18)
  -- CR 609.7b's RECHECK, which is what makes this a filter rather than a list of
  -- objects captured when the shield was made: the Piker's damage is prevented
  -- while it is not a Wolf, and dealt in full once it is one. One board, one
  -- shield, and the only thing that differs between the two readings is a subtype
  -- the projection adds after the shield already exists.
  Spec.it s "CR 609.7b the shield rechecks the source: a Piker made a Wolf deals its 2"
    . withBoard
    $ \_ _ piker shielded -> do
      let lupine = S.withEffect piker (Modification.AddCreatureSubtype Subtype.Wolf)
          after = S.runPure S.identityAnswer (lupine shielded) (Damage.applyDamage [hit DamageKind.Combat piker 2])
          asPrinted = S.runPure S.identityAnswer shielded (Damage.applyDamage [hit DamageKind.Combat piker 2])
      Spec.assertEqWith s "while a Goblin Warrior, the 2 is prevented whole" (S.lifeOf S.alice asPrinted) (Just 20)
      Spec.assertEqWith s "once a Wolf, the same 2 is dealt" (S.lifeOf S.alice after) (Just 18)

-- CR 615.13's trigger, whose one producer in the pool is Selfless Squire ({3}{W}
-- Creature -- Human Soldier 1/1, Flash, "When this creature enters, prevent all
-- damage that would be dealt to you this turn. Whenever damage that would be
-- dealt to you is prevented, put that many +1/+1 counters on this creature").
--
-- Four properties, and between them they are the rule: a prevention that
-- prevents something fires the ability, the ability is handed HOW MUCH was
-- prevented, one prevention effect across several SIMULTANEOUS events fires it
-- ONCE, and damage prevented to a permanent is not damage prevented to a player.
-- The card's own 2016-11-08 ruling supplies a fifth -- "any effect that uses the
-- word 'prevent' will cause it to trigger" -- which is the Mending Hands case.
--
-- The DAMAGE BATCHES are hand-built and the SPELL is not, for mendingHandsSpec's
-- reason: casting the Squire for real is what proves the card, while reaching a
-- two-attacker combat batch would mean driving a whole combat phase to produce a
-- fixture these assertions read straight off.
selflessSquireSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
selflessSquireSpec s registry = Spec.describe s "Selfless Squire (CR 615.13)" $ do
  let hit src recipient n =
        DamageEvent.MkDamageEvent src recipient n False False False 0 Nothing DamageKind.Noncombat
      amounts gs = fmap DamageEvent.amount (S.damageEventsOf gs)
      -- Cast the Squire, let its CR 603.6a enters trigger go on the stack, and
      -- resolve it -- which is what installs the CR 615.1 shield.
      castSquire gs spellId =
        let entered = castAndResolve S.identityAnswer gs spellId
            triggered = S.runPure S.identityAnswer entered Engine.settleForPriority
         in S.runPure S.identityAnswer triggered Stack.resolveTop
      -- Settle one damage batch, then let whatever it triggered go on the stack
      -- and resolve. One trigger per pass, which is all any case here makes.
      strikeAndSettle gs batch =
        let dealt = S.runPure S.identityAnswer gs (Damage.applyDamage batch >> Engine.settleForPriority)
         in (dealt, S.runPure S.identityAnswer dealt Stack.resolveTop)
      squireOf gs = case Set.toList (Set.filter (\oid -> fmap S.nameOf (Game.cardOf oid gs) == Just (CardName.MkCardName (Text.pack "Selfless Squire"))) (GameState.battlefield gs)) of
        oid : _ -> Just oid
        [] -> Nothing
  -- THE WHOLE CARD, and the whole of CR 615.13: a prevention effect is applied,
  -- it prevents some damage, and an ability that watches for exactly that fires
  -- carrying the amount. 3 damage aimed at alice is prevented whole (CR 615.6),
  -- and the 1/1 that shielded her becomes a 4/4.
  Spec.it s "CR 615.13 whole card: damage prevented to alice puts that many +1/+1 counters on the Squire" $ do
    plains <- S.printingOf s registry "Plains"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    squirePrinting <- S.printingOf s registry "Selfless Squire"
    let base = S.landsInPlay plains 4
        (attacker, g1) = S.addCreature pikerPrinting S.bob base
        (g2, spellId) = S.handOne squirePrinting g1
        shielded = castSquire g2 spellId
        squire = squireOf shielded
        (dealt, after) = strikeAndSettle shielded [hit attacker (Recipient.ToPlayer S.alice) 3]
    Spec.assertEqWith s "setup: the shield is a floating replacement" (length (GameState.replacements shielded)) 1
    Spec.assertEqWith s "setup: the Squire is on the battlefield as a 1/1" (squire >>= \oid -> S.powerToughnessOf oid shielded) (Just (1, 1))
    -- CR 615.6: a fully prevented event never happens, so alice loses nothing and
    -- no damage event is recorded.
    Spec.assertEqWith s "alice's life is untouched" (S.lifeOf S.alice after) (Just 20)
    Spec.assertEqWith s "and no damage event happened at all" (amounts after) []
    -- The discriminating half: the prevention alone would leave the Squire a
    -- 1/1. What makes it a 4/4 is CR 615.13's trigger reading the amount.
    Spec.assertEqWith s "exactly one trigger was gathered" (length (GameState.stack dealt)) 1
    Spec.assertEqWith s "and it put 3 +1/+1 counters on the Squire" (fmap (\oid -> countersOn CounterKind.PlusOnePlusOne oid after) squire) (Just 3)
    Spec.assertEqWith s "so the 1/1 is now a 4/4" (squire >>= \oid -> S.powerToughnessOf oid after) (Just (4, 4))
  -- The BASELINE that makes the case above discriminate: the same board with the
  -- shield never installed. The Squire is put onto the battlefield directly, so
  -- its CR 603.6a enters trigger never fires and nothing prevents anything.
  --
  -- Both halves move: alice takes the damage AND the Squire stays a 1/1. An
  -- implementation that fired the counters off the DAMAGE rather than off the
  -- prevention would pass the case above and fail this one.
  Spec.it s "CR 615.13 no prevention, no trigger: the same 3 damage lands and the Squire stays a 1/1" $ do
    plains <- S.printingOf s registry "Plains"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    squirePrinting <- S.printingOf s registry "Selfless Squire"
    let base = S.landsInPlay plains 4
        (attacker, g1) = S.addCreature pikerPrinting S.bob base
        (squire, g2) = S.addCreature squirePrinting S.alice g1
        (dealt, after) = strikeAndSettle g2 [hit attacker (Recipient.ToPlayer S.alice) 3]
    Spec.assertEqWith s "setup: no shield was installed" (length (GameState.replacements g2)) 0
    Spec.assertEqWith s "alice takes all 3" (S.lifeOf S.alice after) (Just 17)
    Spec.assertEqWith s "and the damage event happened" (amounts after) [3]
    Spec.assertEqWith s "nothing triggered" (length (GameState.stack dealt)) 0
    Spec.assertEqWith s "so the Squire is still a 1/1" (S.powerToughnessOf squire after) (Just (1, 1))
  -- CR 615.13's own arithmetic: "each time a prevention effect is applied to ONE
  -- OR MORE SIMULTANEOUS damage events". One shield across a batch of two is one
  -- application, so the ability fires ONCE with the total -- not once per event.
  --
  -- The counter count cannot tell the two readings apart (3 + 2 either way), so
  -- what discriminates is the number of triggers gathered.
  Spec.it s "CR 615.13 one prevention effect across two simultaneous events fires the ability ONCE" $ do
    plains <- S.printingOf s registry "Plains"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    squirePrinting <- S.printingOf s registry "Selfless Squire"
    let base = S.landsInPlay plains 4
        (first, g1) = S.addCreature pikerPrinting S.bob base
        (second, g2) = S.addCreature pikerPrinting S.bob g1
        (g3, spellId) = S.handOne squirePrinting g2
        shielded = castSquire g3 spellId
        squire = squireOf shielded
        batch = [hit first (Recipient.ToPlayer S.alice) 3, hit second (Recipient.ToPlayer S.alice) 2]
        (dealt, after) = strikeAndSettle shielded batch
    Spec.assertEqWith s "both events were prevented whole" (amounts after) []
    Spec.assertEqWith s "ONE trigger, not one per event" (length (GameState.stack dealt)) 1
    Spec.assertEqWith s "carrying the TOTAL prevented" (fmap (\oid -> countersOn CounterKind.PlusOnePlusOne oid after) squire) (Just 5)
  -- The recipient half of the condition. Selfless Squire says "damage that would
  -- be dealt to YOU", so a prevention that covers a CREATURE is silence -- and
  -- the 2016-11-08 ruling's half is here too: the prevention doing the work is
  -- MENDING HANDS, a card the Squire has nothing to do with, and the Squire is
  -- put onto the battlefield directly so its own shield is not in play to
  -- confuse the two.
  Spec.it s "CR 615.13 someone else's prevention fires it, but only for damage to a PLAYER" $ do
    plains <- S.printingOf s registry "Plains"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    squirePrinting <- S.printingOf s registry "Selfless Squire"
    mendingHands <- S.printingOf s registry "Mending Hands"
    let base = S.landsInPlay plains 1
        (victim, g1) = S.addCreature pikerPrinting S.alice base
        (attacker, g2) = S.addCreature pikerPrinting S.bob g1
        (squire, g3) = S.addCreature squirePrinting S.alice g2
        (g4, spellId) = S.handOne mendingHands g3
        onCreature = castAndResolve (aimCreature victim) g4 spellId
        onAlice = castAndResolve (aimPlayer S.alice) g4 spellId
        (creatureDealt, creatureAfter) = strikeAndSettle onCreature [hit attacker (Recipient.ToCreature victim) 3]
        (playerDealt, playerAfter) = strikeAndSettle onAlice [hit attacker (Recipient.ToPlayer S.alice) 3]
    Spec.assertEqWith s "the creature's 3 was prevented" (S.damageOf victim creatureAfter) (Just 0)
    Spec.assertEqWith s "but nothing triggered" (length (GameState.stack creatureDealt)) 0
    Spec.assertEqWith s "so the Squire stays a 1/1" (S.powerToughnessOf squire creatureAfter) (Just (1, 1))
    -- The same shield, aimed one recipient over, is the discriminating twin.
    Spec.assertEqWith s "alice's 3 was prevented too" (S.lifeOf S.alice playerAfter) (Just 20)
    Spec.assertEqWith s "and THAT fired the Squire" (length (GameState.stack playerDealt)) 1
    Spec.assertEqWith s "for 3 counters, off a prevention its own ability had nothing to do with" (S.powerToughnessOf squire playerAfter) (Just (4, 4))

-- alice is mid-combat attacking with `mine`; bob defends holding `spells` and
-- `lands` untapped Plains that pay for them. Sits at the declare attackers step
-- like every combatBoardOf board, so the ENGINE declares the attack and the
-- combat damage this group observes is CR 510.2's own, never hand-written.
-- CombatSpec.killShotBoard is the same shape one seat over.
tablesBoard :: Printing.Printing -> Int -> [Printing.Printing] -> [Printing.Printing] -> (GameState.GameState, [ObjectId.ObjectId])
tablesBoard plains lands mine spells =
  let (gs0, ours, _) = S.combatBoardOf mine []
      withLands = List.foldl' (\g _ -> snd (S.addCreature plains S.bob g)) gs0 [1 .. lands]
      withCards = List.foldl' (\g p -> snd (S.addHandCard p S.bob g)) withLands spells
   in (withCards, ours)

-- Cast the named cards in the ORDER GIVEN, aiming every target at `victim`, and
-- attack with everything.
--
-- Order matters in the Kill Shot case, and one answerer cannot express it: both
-- spells declare the same Pool.Creatures/IsAttacking slot, and casting both in
-- one priority round puts Kill Shot on TOP of the stack, so it resolves first
-- and Turn the Tables fizzles under CR 608.2b with no row installed. That case
-- therefore runs one step per spell, naming one card each time.
castInOrder :: [String] -> ObjectId.ObjectId -> Prompt.Prompt r -> r
castInOrder names victim p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToCreature victim))) sets
  Prompt.ChooseAction _ _ actions ->
    let isNamed n a = case a of
          Action.Cast _ cardName _ -> cardName == CardName.MkCardName (Text.pack n)
          _ -> False
     in case Maybe.mapMaybe (\n -> List.find (isNamed n) actions) names of
          h : _ -> h
          [] -> Action.Pass
  -- Blocks are DECLINED, so a case that gives bob a creature of his own still
  -- has the attacker's damage aimed at bob for the redirect to move.
  Prompt.DeclareBlockers {} -> Map.empty
  _ -> S.aggressiveAnswer p

-- Every floating redirection row, as (kind, source side, destination). The three
-- things Resolve bakes, read back off the store: a case that asserted only on
-- where the damage landed could not tell a redirect aimed at the right creature
-- from one that always aims at the first attacker.
redirectRows :: GameState.GameState -> [(Maybe DamageKind.DamageKind, Maybe Recipient.Recipient, Recipient.Recipient)]
redirectRows gs =
  [ (DamagePattern.whichKind pat, DamagePattern.whichRecipient pat, dest)
  | active <- GameState.replacements gs,
    ReplacementEffect.DamageR (DamageR.MkDamageR pat (DamageRewrite.Redirect dest) _) <- [ActiveReplacement.effect active]
  ]

-- CR 614.9: "Some effects replace damage dealt to one battle, creature,
-- planeswalker, or player with the same damage dealt to another ...; such
-- effects are called redirection effects."
--
-- Turn the Tables ({3}{W}{W}, Instant, Darksteel): "All combat damage that would
-- be dealt to you this turn is dealt to target attacking creature instead." bob
-- is the caster, because "you" is the redirect's controller and only the
-- DEFENDING player is being dealt combat damage worth moving.
turnTheTablesSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
turnTheTablesSpec s registry = Spec.describe s "Turn the Tables (CR 614.9)" $ do
  let atCombatDamage = S.runToStep (Phase.Combat CombatStep.CombatDamage)
      hit src recipient n = DamageEvent.MkDamageEvent src recipient n False False False 0 Nothing DamageKind.Noncombat
      amounts gs = fmap DamageEvent.amount (S.damageEventsOf gs)
      targets gs = fmap DamageEvent.target (S.damageEventsOf gs)
  -- THE WHOLE CARD. alice attacks with a lone Jedit Ojanen (5/5); bob redirects
  -- its combat damage onto Jedit itself, which is lethal to a 5/5 (CR 704.5g).
  Spec.it s "CR 614.9 whole card: the attacker's combat damage is dealt to the attacker instead of to bob" $ do
    plains <- S.printingOf s registry "Plains"
    jedit <- S.printingOf s registry "Jedit Ojanen"
    tables <- S.printingOf s registry "Turn the Tables"
    case tablesBoard plains 5 [jedit] [tables] of
      (gs, [attacker]) -> do
        let atDamage = atCombatDamage (castInOrder ["Turn the Tables"] attacker) gs
            after = S.runCombat (castInOrder ["Turn the Tables"] attacker) atDamage
        -- Combat-timing vacuity: a fixture that skipped combat would pass the
        -- life assertions below without dealing anything.
        Spec.assertEqWith s "setup: the spell resolved and combat damage has NOT been dealt yet" (GameState.phase atDamage) (Phase.Combat CombatStep.CombatDamage)
        -- The "did the target stick" trap: all three baked fields, not merely
        -- that some redirect exists.
        Spec.assertEqWith s "setup: one row, keyed to COMBAT damage to bob, aimed at the creature bob targeted" (redirectRows atDamage) [(Just DamageKind.Combat, Just (Recipient.ToPlayer S.bob), Recipient.ToCreature attacker)]
        Spec.assertEqWith s "the damage never reached bob" (S.lifeOf S.bob after) (Just 20)
        Spec.assertEqWith s "it landed on the attacker instead" (targets after) [Recipient.ToCreature attacker]
        -- The assertion that separates a redirect from a prevention: CR 614.9
        -- replaces the RECIPIENT and nothing else, so one event of the same size.
        Spec.assertEqWith s "one event, of the same amount" (amounts after) [5]
        Spec.assertEqWith s "5 marked on a 5/5 is lethal" (S.creaturesInPlay S.alice after) 0
        Spec.assertEqWith s "and nothing splashed onto the attacker's controller" (S.lifeOf S.alice after) (Just 20)
      _ -> Spec.assertFailure s "fixture should have one attacker"
  -- The BASELINE that makes the case above discriminate: the same board, the
  -- spell never cast. Every assertion moves.
  Spec.it s "CR 614.9 no redirect, no move: the same 5 lands on bob and the attacker survives" $ do
    plains <- S.printingOf s registry "Plains"
    jedit <- S.printingOf s registry "Jedit Ojanen"
    tables <- S.printingOf s registry "Turn the Tables"
    case tablesBoard plains 5 [jedit] [tables] of
      (gs, [attacker]) -> do
        let after = S.runCombat S.aggressiveAnswer gs
        Spec.assertEqWith s "setup: no row was installed" (redirectRows after) []
        Spec.assertEqWith s "bob takes all 5" (S.lifeOf S.bob after) (Just 15)
        Spec.assertEqWith s "addressed to bob" (targets after) [Recipient.ToPlayer S.bob]
        Spec.assertEqWith s "and the attacker is unharmed" (S.damageOf attacker after) (Just 0)
      _ -> Spec.assertFailure s "fixture should have one attacker"
  -- THE KIND NARROWING. "All COMBAT damage", so a noncombat event aimed at bob
  -- is none of the card's business. Without this case the whichKind thread is
  -- unproven, and dropping it would run WEAKER than printed in bob's favour.
  Spec.it s "CR 614.9 the printed narrowing: NONCOMBAT damage to bob is not redirected" $ do
    plains <- S.printingOf s registry "Plains"
    jedit <- S.printingOf s registry "Jedit Ojanen"
    tables <- S.printingOf s registry "Turn the Tables"
    case tablesBoard plains 5 [jedit] [tables] of
      (gs, [attacker]) -> do
        let atDamage = atCombatDamage (castInOrder ["Turn the Tables"] attacker) gs
            after = settleDamage S.identityAnswer atDamage [hit attacker (Recipient.ToPlayer S.bob) 4]
        Spec.assertEqWith s "setup: the redirect really is installed" (length (redirectRows atDamage)) 1
        Spec.assertEqWith s "bob takes the noncombat 4" (S.lifeOf S.bob after) (Just 16)
        Spec.assertEqWith s "addressed to bob, not moved" (targets after) [Recipient.ToPlayer S.bob]
        Spec.assertEqWith s "and the attacker is untouched" (S.damageOf attacker after) (Just 0)
      _ -> Spec.assertFailure s "fixture should have one attacker"
  -- CR 614.9's GUARD. "If one of those permanents is no longer on the
  -- battlefield when the damage would be redirected ... the effect does
  -- nothing." bob redirects onto the Hill Giant and then kills it with Kill
  -- Shot, so the surviving attacker's damage lands on bob exactly as if the
  -- redirect were not there.
  --
  -- "Does nothing" is not "prevents": the load-bearing assertion is that the
  -- event still HAPPENED. Dropping it instead would run weaker than printed.
  Spec.it s "CR 614.9 guard: a destination that left the battlefield makes the effect do nothing" $ do
    plains <- S.printingOf s registry "Plains"
    jedit <- S.printingOf s registry "Jedit Ojanen"
    hillGiant <- S.printingOf s registry "Hill Giant"
    tables <- S.printingOf s registry "Turn the Tables"
    killShot <- S.printingOf s registry "Kill Shot"
    case tablesBoard plains 8 [jedit, hillGiant] [tables, killShot] of
      (gs, [big, doomed]) -> do
        let atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers) (castInOrder ["Turn the Tables"] doomed) gs
            atDamage = atCombatDamage (castInOrder ["Kill Shot"] doomed) atBlockers
            after = S.runCombat S.aggressiveAnswer atDamage
        -- The negative-cast trap: bob is at 15 either way if the spell was never
        -- cast at all, so the row's existence is asserted outright.
        Spec.assertEqWith s "setup: the redirect was installed, aimed at the doomed creature" (redirectRows atDamage) [(Just DamageKind.Combat, Just (Recipient.ToPlayer S.bob), Recipient.ToCreature doomed)]
        Spec.assertEqWith s "setup: Kill Shot really destroyed it" (S.creaturesInPlay S.alice atDamage) 1
        Spec.assertEqWith s "so the survivor's 5 lands on bob" (S.lifeOf S.bob after) (Just 15)
        Spec.assertEqWith s "and the event still HAPPENED -- 'does nothing' is not 'prevents'" (amounts after) [5]
        Spec.assertEqWith s "addressed to bob, its original recipient" (targets after) [Recipient.ToPlayer S.bob]
        Spec.assertEqWith s "the big attacker took nothing" (S.damageOf big after) (Just 0)
      _ -> Spec.assertFailure s "fixture should have two attackers"
  -- CR 615.12 is about PREVENTION effects, and CR 614.9's redirection is not one
  -- (CR 615.1a: it never says "prevent"). Spider-Punk's "damage can't be
  -- prevented" therefore has nothing to say to it, and the damage still moves.
  --
  -- This is the case that gives Replacement.prevents' Redirect arm an observer:
  -- classify a redirect as a prevention and `inertPrevention` makes it do
  -- nothing here, though the CR 615.13 trigger route cannot tell the two apart
  -- (a redirect shrinks no event, so preventionBy reports nothing either way).
  Spec.it s "CR 615.12 a redirect is not a prevention: unpreventable damage is still moved" $ do
    plains <- S.printingOf s registry "Plains"
    jedit <- S.printingOf s registry "Jedit Ojanen"
    spiderPunk <- S.printingOf s registry "Spider-Punk"
    tables <- S.printingOf s registry "Turn the Tables"
    case tablesBoard plains 5 [jedit] [tables] of
      (gs, [attacker]) -> do
        let (punk, withPunk) = S.addCreature spiderPunk S.bob gs
            after = S.runCombat (castInOrder ["Turn the Tables"] attacker) withPunk
        Spec.assertBool s (Set.member punk (GameState.battlefield withPunk)) "setup: Spider-Punk is out, so no damage can be prevented"
        Spec.assertEqWith s "the damage still left bob" (S.lifeOf S.bob after) (Just 20)
        Spec.assertEqWith s "and landed on the attacker" (targets after) [Recipient.ToCreature attacker]
        Spec.assertEqWith s "at its full size" (amounts after) [5]
      _ -> Spec.assertFailure s "fixture should have one attacker"

-- Apply one damage batch under a given interpreter. Top-level rather than a
-- `where` binding for castEach's reason: the answer is rank-2 and GHC will not
-- infer it.
settleDamage :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> [DamageEvent.DamageEvent] -> GameState.GameState
settleDamage answer gs batch = S.runPure answer gs (Damage.applyDamage batch)

-- Aim every target slot at one creature. Mending Hands' slot is Pool.AnyTarget
-- (CR 115.4), whose creature members are tagged ToCreature.
aimCreature :: ObjectId.ObjectId -> Prompt.Prompt r -> r
aimCreature oid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToCreature oid))) sets
  _ -> S.identityAnswer p

-- CR 614.5's applied set is what makes the CR 616.1 loop TERMINATE, not merely
-- correct: a regression there (an effect invoking itself repeatedly, e.g. two
-- Hardened Scales re-triggering each other forever) manifests as this group
-- hanging, not failing. "CR 614.5 two Hardened Scales are two instances" below
-- is the case that asserts the CORRECTNESS half (each gets exactly one
-- opportunity); this timeout is the safety net for the TERMINATION half -- it
-- asserts nothing on a green run, and guards a hang rather than a slowdown.
-- Measured 2026-08-09 on GHC 9.14.1 / aarch64-darwin: the
-- slowest case in this group runs 0.02s and all 105 run in 0.08s, so five
-- seconds is 250x the worst case -- more than a loaded shared runner can eat.
-- CI sets the same figure suite-wide through flake.nix's testFlags, so this
-- option is what holds when a local run passes something tighter. The timeout
-- is not applied here -- Pawl.Spec cannot express one -- but where this spec
-- is wired into the tasty runner.
spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Replacement" $ do
  -- P9: a pattern's permanent match runs through the lower Pawl.Engine.Filter over
  -- the PROJECTED view, the same evaluator Pawl.Engine.Cost narrows sacrifices with
  -- (#111 retired). CR 205.2b/300.2/613.1d: creature-ness is projected; the
  -- trivial filter And [] matches every permanent (what AnyPermanent was).
  Spec.it s "CR 614.1 matchesPermanent narrows a permanent through Filter.matches" $ do
    swamp <- S.printingOf s registry "Swamp"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    let base = S.landsInPlay swamp 1
        (piker, g1) = S.addCreature pikerPrinting S.alice base
        land = case Set.toList (GameState.battlefield base) of
          oid : _ -> Just oid
          [] -> Nothing
    case land of
      Nothing -> Spec.assertFailure s "fixture did not build a land"
      Just landId -> do
        Spec.assertBool s (Replacement.matchesPermanent g1 Nothing (Filter.Type.HasCardType CardType.Creature) piker) "the creature matches HasCardType Creature"
        Spec.assertBool s (not (Replacement.matchesPermanent g1 Nothing (Filter.Type.HasCardType CardType.Creature) landId)) "the land does not match HasCardType Creature"
        Spec.assertBool s (Replacement.matchesPermanent g1 Nothing (Filter.Type.And []) landId) "the trivial filter matches the land too"
  -- NOT a CR 614.5 test: this does not exercise the applied set at all. After
  -- the first Rest in Peace redirects the event to Exile, the SECOND Rest in
  -- Peace's pattern (whenDestination = Graveyard) no longer matches the
  -- rewritten event, so `applies` alone -- not CR 614.5's applied-set --
  -- is what stops the second application. Deleting the applied-set logic
  -- from `loop` entirely leaves this test passing. What it actually proves:
  -- a redirect whose output no longer matches its own `whenDestination`
  -- cannot re-fire. See "CR 614.5 the applied set ..." below for the real
  -- 614.5 coverage.
  Spec.it s "CR 614.1a a redirect that no longer matches its own pattern cannot re-fire" $ do
    restInPeace <- S.printingOf s registry "Rest in Peace"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    let (_, g0) = S.addCreature restInPeace S.alice (Setup.emptyGame S.bothPlayers)
        (_, g1) = S.addCreature restInPeace S.alice g0
        (piker, g2) = S.addCreature pikerPrinting S.bob g1
        after = S.runPure S.identityAnswer g2 (Event.changeZone piker Zone.Graveyard)
    Spec.assertEqWith s "not in a graveyard" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 0
    Spec.assertEqWith s "exactly one object in exile" (Set.size (GameState.exile after)) 1
  Spec.it s "CR 616.1 value-equal candidates elide the prompt (nothing to choose)" $ do
    restInPeace <- S.printingOf s registry "Rest in Peace"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    let (_, g0) = S.addCreature restInPeace S.alice (Setup.emptyGame S.bothPlayers)
        (_, g1) = S.addCreature restInPeace S.alice g0
        (piker, g2) = S.addCreature pikerPrinting S.bob g1
        asked = answersFor S.identityAnswer g2 (Event.changeZone piker Zone.Graveyard)
    Spec.assertBool s (not (wasAskedToReplace asked)) "no ChooseReplacement was raised"
  -- The other side of Replacement.readsApplier, and the reason it exists rather
  -- than a blanket "compare the controller too". Rest in Peace's pattern is
  -- the trivial Filter under ControllerRelation.Anyones, so alice's copy
  -- and bob's are both applicable to bob's dying Piker at once, equal in `effect`
  -- and differing only in who controls the row. Applying either exiles the same
  -- card, so there is nothing to decide and nothing to ask -- where comparing
  -- `(effect, controller)` unconditionally would have started prompting.
  Spec.it s "CR 616.1 value-equal candidates under DIFFERENT controllers still elide" $ do
    restInPeace <- S.printingOf s registry "Rest in Peace"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    let (_, g0) = S.addCreature restInPeace S.alice (Setup.emptyGame S.bothPlayers)
        (_, g1) = S.addCreature restInPeace S.bob g0
        (piker, g2) = S.addCreature pikerPrinting S.bob g1
        after = S.runPure S.identityAnswer g2 (Event.changeZone piker Zone.Graveyard)
        asked = answersFor S.identityAnswer g2 (Event.changeZone piker Zone.Graveyard)
    Spec.assertEqWith s "the Piker was exiled, not buried" (Set.size (GameState.exile after)) 1
    Spec.assertBool s (not (wasAskedToReplace asked)) "no ChooseReplacement was raised"
  -- CR 704.3: "the game checks for any of the listed conditions for
  -- state-based actions, then performs all applicable state-based actions
  -- simultaneously as a single event." So the put-into-graveyard batch one
  -- pass performs is ONE event, and the replacement effects in force for it
  -- are the ones on the battlefield when the pass began -- including one
  -- belonging to a permanent the pass is itself burying.
  --
  -- Opalescence makes Rest in Peace a 2/2 (its mana value); two -1/-1
  -- counters take it to 0/0 and one takes the 2/1 Piker to 1/0, so CR
  -- 704.5f names both in the same pass. Rest in Peace is added FIRST on
  -- purpose: Sba walks the battlefield in ascending ObjectId order, so it
  -- is buried first and an implementation that re-collected the Piker's
  -- candidates from the live board would find it gone.
  Spec.it s "CR 704.3 a Rest in Peace buried by an SBA pass still exiles that pass's other victim" $ do
    opalescence <- S.printingOf s registry "Opalescence"
    restInPeace <- S.printingOf s registry "Rest in Peace"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    let (_, g0) = S.addCreature opalescence S.alice (Setup.emptyGame S.bothPlayers)
        (rip, g1) = S.addCreature restInPeace S.alice g0
        (piker, g2) = S.addCreature pikerPrinting S.bob g1
        board = S.addCounter CounterKind.MinusOneMinusOne 1 piker (S.addCounter CounterKind.MinusOneMinusOne 2 rip g2)
        after = S.settleSba board
    Spec.assertBool s (rip < piker) "setup: Rest in Peace is buried before the Piker"
    Spec.assertEqWith s "setup: Opalescence's 2/2 is a 0/0" (S.powerToughnessOf rip board) (Just (0, 0))
    Spec.assertEqWith s "setup: the Piker is a 1/0" (S.powerToughnessOf piker board) (Just (1, 0))
    Spec.assertEqWith s "the Piker was exiled, not buried" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 0
    Spec.assertEqWith s "the Piker's card is in exile" (length (Game.zoneMembers Zone.Exile S.bob after)) 1
    Spec.assertEqWith s "and Rest in Peace exiled its own card too" (length (Game.zoneMembers Zone.Exile S.alice after)) 1
  -- The same CR 704.3 event, across the pass's OTHER seam. The case above
  -- keeps both victims inside the pass's put-into-graveyard batch; this one
  -- puts the second victim in the DESTRUCTION batch (CR 704.5g's lethal
  -- marked damage), which Pawl.Engine.Sba performs after the buries. CR 704.3 makes
  -- the two one event, so the destruction's graveyard move must see the same
  -- board the buries did -- with Rest in Peace still on it.
  --
  -- Rest in Peace is again a 2/2 by Opalescence taken to 0/0 by two -1/-1
  -- counters (CR 704.5f). The Piker keeps its printed 2/1 and takes 1
  -- marked damage instead, so it is lethally damaged rather than
  -- zero-toughness and CR 704.5g claims it.
  Spec.it s "CR 704.3 a Rest in Peace the pass buries still exiles what the pass DESTROYS" $ do
    opalescence <- S.printingOf s registry "Opalescence"
    restInPeace <- S.printingOf s registry "Rest in Peace"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    let (_, g0) = S.addCreature opalescence S.alice (Setup.emptyGame S.bothPlayers)
        (rip, g1) = S.addCreature restInPeace S.alice g0
        (piker, g2) = S.addCreature pikerPrinting S.bob g1
        board = S.markDamage piker 1 (S.addCounter CounterKind.MinusOneMinusOne 2 rip g2)
        after = S.settleSba board
    Spec.assertEqWith s "setup: Opalescence's 2/2 is a 0/0, so CR 704.5f buries it" (S.powerToughnessOf rip board) (Just (0, 0))
    Spec.assertEqWith s "setup: the Piker is still a 2/1" (S.powerToughnessOf piker board) (Just (2, 1))
    Spec.assertEqWith s "setup: with lethal damage marked, so CR 704.5g destroys it" (S.damageOf piker board) (Just 1)
    Spec.assertEqWith s "the Piker was exiled, not buried" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 0
    Spec.assertEqWith s "the Piker's card is in exile" (length (Game.zoneMembers Zone.Exile S.bob after)) 1
    Spec.assertEqWith s "and Rest in Peace exiled its own card too" (length (Game.zoneMembers Zone.Exile S.alice after)) 1
  -- The other side of the coin above. Sharing the pass's board is what CR
  -- 704.3 asks of the two halves' REPLACEMENT collection; it is not what it
  -- asks of the destroy funnel's existence filter, which stays live. CR
  -- 614.7 is why: "If a replacement effect would replace an event, but that
  -- event never happens, the replacement effect simply doesn't do anything."
  -- A permanent the pass's put-into-graveyard half has already moved is not
  -- on the battlefield to be destroyed, so the destruction never happens and
  -- a regeneration shield on it must be neither applied nor spent.
  --
  -- The one shape in the pool that reaches it: a permanent named by both
  -- halves of one pass. CR 704.5f's victims can never also be CR 704.5g's
  -- (Pawl.Engine.Sba's classify gives 704.5f priority) and Pawl.Engine.Sba already
  -- excludes CR 704.5j's and CR 704.5k's by name, so an Aura -- named by CR
  -- 704.5m in the first half and CR 704.5g in the second -- is all that is
  -- left. Getting one takes Liquimetal Coating plus Skilled Animator, since
  -- every printed enchantment animator excludes Auras: the Aura is made an
  -- artifact first, then animated as one. See Pawl.AuraSpec's CR 303.4d case
  -- for the same fixture proving the detach-then-bury order this builds on.
  --
  -- The shield is seeded rather than activated because CR 701.19a's shield
  -- "protects the permanent" its effect names, and the only two producers in
  -- the pool -- Drudge Skeletons and Uthden Troll -- name themselves. No Aura
  -- prints one, so there is no gameplay route to a shield on this Aura.
  Spec.it s "CR 614.7 an Aura the same pass buries is never offered to a regeneration shield" $ do
    island <- S.printingOf s registry "Island"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    unholyStrength <- S.printingOf s registry "Unholy Strength"
    coating <- S.printingOf s registry "Liquimetal Coating"
    animator <- S.printingOf s registry "Skilled Animator"
    let base = S.landsInPlay island 3 -- {2}{U} for the Animator
        (creature, g1) = S.addCreature pikerPrinting S.alice base
        (aura, g2) = S.addCreature unholyStrength S.alice g1
        (coatingId, g3) = S.addCreature coating S.alice (S.attach aura creature g2)
        ready = g3 {GameState.priority = Just S.alice}
        activated = S.runPure (aimObject aura) ready (Activate.activateAbility S.alice coatingId (theAbility coating))
        coated = S.runPure (aimObject aura) activated Stack.resolveTop
        (withSpell, spellId) = S.handOne animator coated
        entered = S.runPure (aimObject aura) withSpell (S.cast S.alice spellId >> Stack.resolveTop)
        triggered = S.runPure (aimObject aura) entered Engine.settleForPriority
        animated = S.runPure (aimObject aura) triggered Stack.resolveTop
        -- One pass, so the two state-based actions stay separately
        -- observable: CR 704.5p unattaches the animated Aura here and CR
        -- 704.5m buries it on the pass below.
        unattachedNow = S.settleSba animated
        -- Lethal damage on the 5/5 makes CR 704.5g name it too, so the next
        -- pass names it in BOTH halves.
        armed = S.addRegenShield aura (S.markDamage aura 5 unattachedNow)
        after = S.settleSba armed
    Spec.assertEqWith s "setup: the Aura is an unattached 5/5" (S.powerToughnessOf aura armed) (Just (5, 5))
    Spec.assertEqWith s "setup: attached to nothing, so CR 704.5m names it" (fmap Object.attachedTo (Game.lookupObject aura armed)) (Just Nothing)
    Spec.assertEqWith s "setup: with lethal damage, so CR 704.5g names it as well" (S.damageOf aura armed) (Just 5)
    Spec.assertEqWith s "setup: exactly one floating replacement, the shield" (length (GameState.replacements armed)) 1
    Spec.assertBool s (not (Set.member aura (GameState.battlefield after))) "CR 704.5m buried it"
    Spec.assertEqWith s "in its owner's graveyard, not regenerated" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
    Spec.assertEqWith s "and the shield was never spent on a destruction that did not happen" (length (GameState.replacements after)) 1
  Spec.it s "CR 614.1a a move whose destination the pattern misses is untouched" $ do
    restInPeace <- S.printingOf s registry "Rest in Peace"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    let (_, g0) = S.addCreature restInPeace S.alice (Setup.emptyGame S.bothPlayers)
        (piker, g1) = S.addCreature pikerPrinting S.bob g0
        -- Rest in Peace watches graveyard-bound moves only; a bounce to hand
        -- is not one, so the loop finds no candidate and the move stands.
        after = S.runPure S.identityAnswer g1 (Event.changeZone piker Zone.Hand)
    Spec.assertEqWith s "in bob's hand" (length (Game.zoneMembers Zone.Hand S.bob after)) 1
    Spec.assertEqWith s "nothing was exiled" (Set.size (GameState.exile after)) 0
  Spec.it s "CR 615.10 Fog prevents both attackers' damage in one batch" $ do
    forest <- S.printingOf s registry "Forest"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    fog <- S.printingOf s registry "Fog"
    let base = S.landsInPlay forest 1
        (victimA, g1) = S.addCreature pikerPrinting S.bob base
        (victimB, g2) = S.addCreature pikerPrinting S.bob g1
        (g3, fogId) = S.handOne fog g2
        resolved = S.runPure S.identityAnswer g3 (S.cast S.alice fogId >> Stack.resolveTop)
        -- Hand-built rather than driven through real combat: reaching a real
        -- combat-damage batch would mean driving an entire combat phase, which
        -- this assertion (Fog prevents a whole batch, not just one event) does
        -- not need.
        batch =
          [ DamageEvent.MkDamageEvent victimA (Recipient.ToCreature victimA) 2 False False False 0 Nothing DamageKind.Combat,
            DamageEvent.MkDamageEvent victimB (Recipient.ToCreature victimB) 2 False False False 0 Nothing DamageKind.Combat
          ]
        after = S.runPure S.identityAnswer resolved (Damage.applyDamage batch)
    Spec.assertEqWith s "the first attacker's damage was prevented" (S.damageOf victimA after) (Just 0)
    Spec.assertEqWith s "and so was the second's, independently" (S.damageOf victimB after) (Just 0)
    Spec.assertEqWith s "no damage event was recorded at all" (S.damageEventsOf after) []
  Spec.it s "CR 701.19a Uses=Once: the first destruction is replaced, the second is not" $ do
    swamp <- S.printingOf s registry "Swamp"
    drudgeSkeletons <- S.printingOf s registry "Drudge Skeletons"
    let base = S.landsInPlay swamp 1
        (skel, g1) = S.addCreature drudgeSkeletons S.alice base
        -- Activate {B}: regenerate this creature, and resolve it.
        armed = S.runPure S.identityAnswer g1 (Activate.activateAbility S.alice skel (theAbility drudgeSkeletons) >> Stack.resolveTop)
        -- CR 701.19a's "remove it from combat" half needs the creature
        -- actually attacking. Driving a full combat phase to reach a legal
        -- attack is disproportionate to what this asserts, so seed
        -- GameState.combat's attacker map directly -- the same shortcut
        -- Support.addRegenShield takes for the shield itself.
        attacking = armed {GameState.combat = (GameState.combat armed) {Combat.Type.attackers = Map.singleton skel (AttackTarget.OfPlayer S.bob)}}
        once = S.runPure S.identityAnswer attacking (Event.destroy Regenerability.Regenerable [skel])
        twice = S.runPure S.identityAnswer once (Event.destroy Regenerability.Regenerable [skel])
    Spec.assertBool s (Map.null (Combat.Type.attackers (GameState.combat armed))) "combat started with no attackers"
    Spec.assertBool s (Set.member skel (GameState.battlefield once)) "survived the first destruction"
    Spec.assertEqWith s "the shield was spent" (GameState.replacements once) []
    Spec.assertBool s (not (Map.member skel (Combat.Type.attackers (GameState.combat once)))) "removed from combat by the regeneration (CR 701.19a)"
    Spec.assertBool s (not (Set.member skel (GameState.battlefield twice))) "the second destruction kills it"
  -- CR 701.19c: "Effects that say that a permanent can't be regenerated
  -- don't preclude such abilities from being activated or such spells from
  -- being cast; rather, they cause regeneration shields to not be applied."
  -- So the shield still exists -- it simply does not fire.
  Spec.it s "CR 701.19c a shield does not save a creature from a destruction that forbids regeneration" $ do
    swamp <- S.printingOf s registry "Swamp"
    drudgeSkeletons <- S.printingOf s registry "Drudge Skeletons"
    let (skel, g1) = S.addCreature drudgeSkeletons S.alice (S.landsInPlay swamp 1)
        shielded = S.addRegenShield skel g1
        after = S.runPure S.identityAnswer shielded (Event.destroy Regenerability.CantBeRegenerated [skel])
    Spec.assertBool s (not (Set.member skel (GameState.battlefield after))) "it died anyway"
    -- CR 701.19c again, and the sharp half: an unapplied shield is not a
    -- spent one. Nothing consumed it, because it was never chosen.
    Spec.assertEqWith s "and the shield was not consumed" (length (GameState.replacements after)) (length (GameState.replacements shielded))
  -- The discriminating twin: identical creature, identical shield, and the
  -- only difference is whether the destruction forbids regeneration. This
  -- fails if the gate is ignored, and equally if it is applied to every
  -- destruction.
  Spec.it s "CR 701.19a the same shield DOES save it from an ordinary destruction" $ do
    swamp <- S.printingOf s registry "Swamp"
    drudgeSkeletons <- S.printingOf s registry "Drudge Skeletons"
    let (skel, g1) = S.addCreature drudgeSkeletons S.alice (S.landsInPlay swamp 1)
        shielded = S.addRegenShield skel g1
        after = S.runPure S.identityAnswer shielded (Event.destroy Regenerability.Regenerable [skel])
    Spec.assertBool s (Set.member skel (GameState.battlefield after)) "it survived"
    Spec.assertEqWith s "and this time the shield was spent" (GameState.replacements after) []
  -- The gameplay-level proof (design.md section 4): real cards, cast and
  -- resolved. Uthden Troll rather than Drudge Skeletons because Terror
  -- cannot target a black creature -- the Troll is red.
  Spec.it s "CR 701.19c whole cards: Terror kills an Uthden Troll that just regenerated" $ do
    mountain <- S.printingOf s registry "Mountain"
    swamp <- S.printingOf s registry "Swamp"
    uthdenTroll <- S.printingOf s registry "Uthden Troll"
    terror <- S.printingOf s registry "Terror"
    let base = foldl (\gs p -> snd (S.addCreature p S.alice gs)) (Setup.emptyGame S.bothPlayers) [mountain, swamp, swamp]
        (troll, g1) = S.addCreature uthdenTroll S.alice base
        -- {R}: Regenerate this creature -- the shield is really activated.
        armed = S.runPure S.identityAnswer g1 (Activate.activateAbility S.alice troll (theAbility uthdenTroll) >> Stack.resolveTop)
        (withTerror, spell) = S.handOne terror armed
        afterCast = S.runPure S.identityAnswer withTerror (S.cast S.alice spell)
        resolved = S.runPure S.identityAnswer afterCast Stack.resolveTop
    Spec.assertBool s (not (null (GameState.replacements armed))) "the shield really was created"
    Spec.assertBool s (not (Set.member troll (GameState.battlefield resolved))) "and Terror killed the Troll through it"
  -- The twin of the whole-card test: the SAME creature and the SAME shield,
  -- destroyed by the CR 704.5g state-based action instead, which carries no
  -- such clause. Regeneration is exactly what it is for.
  Spec.it s "CR 701.19a an Uthden Troll's shield still saves it from lethal damage" $ do
    mountain <- S.printingOf s registry "Mountain"
    uthdenTroll <- S.printingOf s registry "Uthden Troll"
    let base = S.landsInPlay mountain 1
        (troll, g1) = S.addCreature uthdenTroll S.alice base
        armed = S.runPure S.identityAnswer g1 (Activate.activateAbility S.alice troll (theAbility uthdenTroll) >> Stack.resolveTop)
        -- 2 damage is lethal to a 2/2.
        hurt = S.runPure S.identityAnswer armed (Damage.applyDamage [DamageEvent.MkDamageEvent troll (Recipient.ToCreature troll) 2 False False False 0 Nothing DamageKind.Combat])
        settled = S.settleSba hurt
    Spec.assertBool s (Set.member troll (GameState.battlefield settled)) "the shield saved it"
  Spec.it s "CR 614.8 regeneration replaces the destruction, so Rest in Peace never sees it" $ do
    swamp <- S.printingOf s registry "Swamp"
    restInPeace <- S.printingOf s registry "Rest in Peace"
    drudgeSkeletons <- S.printingOf s registry "Drudge Skeletons"
    let base = S.landsInPlay swamp 1
        (_, g1) = S.addCreature restInPeace S.bob base
        (skel, g2) = S.addCreature drudgeSkeletons S.alice g1
        shielded = S.addRegenShield skel g2
        after = S.runPure S.identityAnswer shielded (Event.destroy Regenerability.Regenerable [skel])
    Spec.assertBool s (Set.member skel (GameState.battlefield after)) "still on the battlefield"
    Spec.assertEqWith s "nothing was exiled -- the put-into-graveyard never happened" (Set.size (GameState.exile after)) 0
    Spec.assertEqWith s "and nothing reached a graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 0
  Spec.it s "CR 614.7 an event that never happens does not consume a shield" $ do
    darksteelMyr <- S.printingOf s registry "Darksteel Myr"
    let base = Setup.emptyGame S.bothPlayers
        (myr, g1) = S.addCreature darksteelMyr S.alice base
        shielded = S.addRegenShield myr g1
        after = S.runPure S.identityAnswer shielded (Event.destroy Regenerability.Regenerable [myr])
    Spec.assertBool s (Set.member myr (GameState.battlefield after)) "the indestructible creature survives"
    Spec.assertEqWith s "the shield is intact" (length (GameState.replacements after)) 1
  Spec.it s "CR 616.1 Scales first, then Corpsejack: 1 -> 2 -> 4" $ do
    forest <- S.printingOf s registry "Forest"
    battlegrowth <- S.printingOf s registry "Battlegrowth"
    hardenedScales <- S.printingOf s registry "Hardened Scales"
    corpsejackMenace <- S.printingOf s registry "Corpsejack Menace"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    let (gs, spellId, mine, _) = counterBoard forest battlegrowth [hardenedScales, corpsejackMenace, pikerPrinting] []
    case mine of
      scales : _ : piker : _ ->
        let after = castAndResolve (raceAnswer scales piker) gs spellId
         in Spec.assertEqWith s "(1 + 1) * 2" (countersOn CounterKind.PlusOnePlusOne piker after) 4
      _ -> Spec.assertFailure s "fixture did not build three permanents"
  Spec.it s "CR 616.1 Corpsejack first, then Scales: 1 -> 2 -> 3 (same input, different board)" $ do
    forest <- S.printingOf s registry "Forest"
    battlegrowth <- S.printingOf s registry "Battlegrowth"
    hardenedScales <- S.printingOf s registry "Hardened Scales"
    corpsejackMenace <- S.printingOf s registry "Corpsejack Menace"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    let (gs, spellId, mine, _) = counterBoard forest battlegrowth [hardenedScales, corpsejackMenace, pikerPrinting] []
    case mine of
      _ : corpsejack : piker : _ ->
        let after = castAndResolve (raceAnswer corpsejack piker) gs spellId
         in Spec.assertEqWith s "(1 * 2) + 1" (countersOn CounterKind.PlusOnePlusOne piker after) 3
      _ -> Spec.assertFailure s "fixture did not build three permanents"
  -- CR 305.7: a land whose subtype is set to a basic type "loses all
  -- abilities generated from its rules text", and a replacement effect is
  -- one of them. Ashaya makes the Menace a Forest land, Blood Moon sets
  -- that to Mountain, and the doubling goes with the rest of its text --
  -- so Battlegrowth's one counter stays one. The Piker is animated too and
  -- is still a creature (CR 305.7 removes no card types), so it is still a
  -- legal target for "target creature".
  Spec.it s "CR 305.7 an Ashaya-animated, Blood Moon'd Corpsejack Menace doubles nothing" $ do
    forest <- S.printingOf s registry "Forest"
    battlegrowth <- S.printingOf s registry "Battlegrowth"
    corpsejackMenace <- S.printingOf s registry "Corpsejack Menace"
    ashaya <- S.printingOf s registry "Ashaya, Soul of the Wild"
    bloodMoon <- S.printingOf s registry "Blood Moon"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    let (gs, spellId, mine, _) = counterBoard forest battlegrowth [corpsejackMenace, ashaya, bloodMoon, pikerPrinting] []
    case mine of
      corpsejack : _ : _ : piker : _ ->
        let after = castAndResolve (raceAnswer corpsejack piker) gs spellId
         in Spec.assertEqWith s "one counter, not two" (countersOn CounterKind.PlusOnePlusOne piker after) 1
      _ -> Spec.assertFailure s "fixture did not build four permanents"
  Spec.it s "CR 616.1 the engine ASKS -- it does not proceed on list order" $ do
    forest <- S.printingOf s registry "Forest"
    battlegrowth <- S.printingOf s registry "Battlegrowth"
    hardenedScales <- S.printingOf s registry "Hardened Scales"
    corpsejackMenace <- S.printingOf s registry "Corpsejack Menace"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    let (gs, spellId, mine, _) = counterBoard forest battlegrowth [hardenedScales, corpsejackMenace, pikerPrinting] []
    case mine of
      scales : _ : piker : _ ->
        let asked = answersFor (raceAnswer scales piker) gs (S.cast S.alice spellId >> Stack.resolveTop)
         in Spec.assertBool s (wasAskedToReplace asked) "a ChooseReplacement was raised"
      _ -> Spec.assertFailure s "fixture did not build three permanents"
  Spec.it s "CR 616.1 one Hardened Scales alone is not asked about (nothing to choose)" $ do
    forest <- S.printingOf s registry "Forest"
    battlegrowth <- S.printingOf s registry "Battlegrowth"
    hardenedScales <- S.printingOf s registry "Hardened Scales"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    let (gs, spellId, mine, _) = counterBoard forest battlegrowth [hardenedScales, pikerPrinting] []
    case mine of
      scales : piker : _ ->
        let after = castAndResolve (raceAnswer scales piker) gs spellId
            asked = answersFor (raceAnswer scales piker) gs (S.cast S.alice spellId >> Stack.resolveTop)
         in do
              Spec.assertEqWith s "1 + 1" (countersOn CounterKind.PlusOnePlusOne piker after) 2
              Spec.assertBool s (not (wasAskedToReplace asked)) "no ChooseReplacement was raised"
      _ -> Spec.assertFailure s "fixture did not build two permanents"
  Spec.it s "CR 614.5 two Hardened Scales are two instances: 1 -> 2 -> 3, unprompted" $ do
    forest <- S.printingOf s registry "Forest"
    battlegrowth <- S.printingOf s registry "Battlegrowth"
    hardenedScales <- S.printingOf s registry "Hardened Scales"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    let (gs, spellId, mine, _) = counterBoard forest battlegrowth [hardenedScales, hardenedScales, pikerPrinting] []
    case mine of
      scales : _ : piker : _ ->
        let after = castAndResolve (raceAnswer scales piker) gs spellId
            asked = answersFor (raceAnswer scales piker) gs (S.cast S.alice spellId >> Stack.resolveTop)
         in do
              Spec.assertEqWith s "each gets its own opportunity" (countersOn CounterKind.PlusOnePlusOne piker after) 3
              Spec.assertBool s (not (wasAskedToReplace asked)) "value-equal candidates elide the prompt"
      _ -> Spec.assertFailure s "fixture did not build three permanents"
  Spec.it s "CR 614.1 Hardened Scales ignores a -1/-1 counter (whichKind)" $ do
    swamp <- S.printingOf s registry "Swamp"
    hardenedScales <- S.printingOf s registry "Hardened Scales"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    instillInfection <- S.printingOf s registry "Instill Infection"
    let base = S.landsInPlay swamp 4
        (scales, g1) = S.addCreature hardenedScales S.alice base
        (piker, g2) = S.addCreature pikerPrinting S.alice g1
        (g3, spellId) = S.handOne instillInfection g2
        after = castAndResolve (raceAnswer scales piker) g3 spellId
    Spec.assertEqWith s "one -1/-1 counter, unscaled" (countersOn CounterKind.MinusOneMinusOne piker after) 1
  Spec.it s "CR 109.5 Corpsejack Menace does not double an opponent's counters" $ do
    forest <- S.printingOf s registry "Forest"
    battlegrowth <- S.printingOf s registry "Battlegrowth"
    corpsejackMenace <- S.printingOf s registry "Corpsejack Menace"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    let (gs, spellId, mine, theirs) = counterBoard forest battlegrowth [corpsejackMenace] [pikerPrinting]
    case (mine, theirs) of
      (corpsejack : _, piker : _) ->
        let after = castAndResolve (raceAnswer corpsejack piker) gs spellId
         in Spec.assertEqWith s "not doubled -- ControllerRelation is Yours" (countersOn CounterKind.PlusOnePlusOne piker after) 1
      _ -> Spec.assertFailure s "fixture did not build both sides"
  -- THE PROVING TEST for #78's candidate-collection channel. CR 614.12 settles
  -- which replacement effects modify how a permanent enters by taking into
  -- account "continuous effects that already exist and would apply to the
  -- permanent" -- and a permanent arriving in the SAME batch has none yet, since
  -- its static abilities begin to apply only once it is on the battlefield, which
  -- is the moment this one arrives too. Corpsejack Menace's own ruling states the
  -- effect this rule denies it here: "if a creature you control would enter the
  -- battlefield with a number of +1/+1 counters on it, it enters with twice that
  -- many instead."
  --
  -- Rise of the Dark Realms returns every creature card from every graveyard as
  -- ONE CR 608.2f event, so the Menace and the Worker enter simultaneously.
  -- Arcbound Worker is a printed 0/0 with modular 1 (CR 702.43a), which
  -- Pawl.Engine.Keyword mints as the CR 614.1c entry replacement "enters with one
  -- +1/+1 counter" -- so the counter is placed inside the Worker's entry loop,
  -- exactly where the rule is asked.
  --
  -- The Menace is buried FIRST so it takes the lower ObjectId and moves first
  -- (Resolve.graveyardCardsOf sorts ascending, S.addGraveyardCard mints in call
  -- order), which is the only order in which a live-board reading has anything to
  -- double; the mirrored leg pins that the answer does not depend on it, which is
  -- CR 608.2f's point -- the batch is one event and nobody gets to order it.
  --
  -- Power and toughness ride along with the counter count because they are what a
  -- player sees: 1 counter is a 1/1 Worker, 2 is a 2/2, and 0 would be a 0/0 that
  -- CR 704.5f buries -- three boards no pair of readings can confuse.
  Spec.it s "CR 614.12 a Corpsejack Menace reanimated beside a modular creature doubles nothing (#78)" $ do
    swamp <- S.printingOf s registry "Swamp"
    rise <- S.printingOf s registry "Rise of the Dark Realms"
    corpsejackMenace <- S.printingOf s registry "Corpsejack Menace"
    arcboundWorker <- S.printingOf s registry "Arcbound Worker"
    let outcome buried =
          let graves = List.foldl' (\g printing -> snd (S.addGraveyardCard printing S.alice g)) (S.landsInPlay swamp 9) buried
              (gs, spellId) = S.handOne rise graves
              after = castAndResolve S.identityAnswer gs spellId
              workers =
                [ oid
                | oid <- Set.toList (GameState.battlefield after),
                  Projection.namesOf oid after == Set.singleton (CardName.MkCardName (Text.pack "Arcbound Worker"))
                ]
           in [(countersOn CounterKind.PlusOnePlusOne oid after, S.powerToughnessOf oid after) | oid <- workers]
        menaceFirst = outcome [corpsejackMenace, arcboundWorker]
        workerFirst = outcome [arcboundWorker, corpsejackMenace]
    Spec.assertEqWith s "modular 1's one counter, undoubled -- a 1/1 Worker" menaceFirst [(1, Just (1, 1))]
    Spec.assertEqWith s "and the batch's processing order changes nothing (CR 608.2f)" workerFirst menaceFirst
  -- THE PROVING TEST for #78's PROJECTION channel, the other half of the same
  -- rule. A permanent's static abilities function only while it is on the
  -- battlefield (CR 113.6), and CR 614.12a puts an as-enters choice BEFORE the
  -- permanent enters -- so at the moment a batch member makes its choice, a
  -- sibling arriving in the same batch has no continuous effect yet, which is the
  -- same thing CR 614.12 says by admitting only "continuous effects that already
  -- exist". This engine materializes every batch member up front, so the sibling
  -- IS sitting on the battlefield when the projection reads run, and its static
  -- has to be suppressed rather than merely not looked at.
  --
  -- Ashaya, Soul of the Wild ("nontoken creatures you control are Forest lands in
  -- addition to their other types") and Wood Elemental ("as this creature enters,
  -- sacrifice any number of untapped Forests") come back from one graveyard as ONE
  -- CR 608.2f event. The victim is a THIRD permanent -- a Goblin Piker already on
  -- the battlefield -- because CR 614.13a already bars the batch's own members from
  -- the offer, so a sibling of the batch could not tell the two rules apart.
  --
  -- The answer is greedy (sacrificesAll), so the offer is not merely observed but
  -- SPENT: with Ashaya's static visible the Piker projects as an untapped Forest,
  -- is offered, and dies.
  --
  -- Ashaya is buried FIRST so it takes the lower ObjectId and arrives first
  -- (Resolve.graveyardCardsOf sorts ascending, S.addGraveyardCard mints in call
  -- order) -- the only order in which a live-board reading has a Forest to offer.
  -- The mirrored leg pins that the answer does not depend on it, which is CR
  -- 608.2f's point. The nine lands are Swamps, not Forests, so the only Forest
  -- anywhere on the board is one Ashaya would have made.
  --
  -- Wood Elemental's power and toughness ride along, read after one SBA sweep:
  -- its CDA reads the count it sacrificed (CR 208.2a), so 0 is a 0/0 that CR
  -- 704.5f buries and 1 is a 1/1 still standing -- a second reading of the same
  -- divergence, on the same board, that the Piker count cannot be confused with.
  Spec.it s "CR 614.12 a Wood Elemental reanimated beside Ashaya sacrifices nothing (#78)" $ do
    swamp <- S.printingOf s registry "Swamp"
    rise <- S.printingOf s registry "Rise of the Dark Realms"
    ashaya <- S.printingOf s registry "Ashaya, Soul of the Wild"
    woodElemental <- S.printingOf s registry "Wood Elemental"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    let pikerName = CardName.MkCardName (Text.pack "Goblin Piker")
        outcome buried =
          let (_, withPiker) = S.addCreature pikerPrinting S.alice (S.landsInPlay swamp 9)
              graves = List.foldl' (\g printing -> snd (S.addGraveyardCard printing S.alice g)) withPiker buried
              (gs, spellId) = S.handOne rise graves
              after = S.settleSba (castAndResolve sacrificesAll gs spellId)
              pikers = length [oid | oid <- Set.toList (GameState.battlefield after), Projection.hasName pikerName oid after]
           in (pikers, fmap (`S.powerToughnessOf` after) (newestNamed (CardName.MkCardName (Text.pack "Wood Elemental")) after))
        ashayaFirst = outcome [ashaya, woodElemental]
        elementalFirst = outcome [woodElemental, ashaya]
    Spec.assertEqWith s "the Piker was never a Forest, so it was not offered and it lives -- and the Wood Elemental that sacrificed nothing is a 0/0 CR 704.5f buried" ashayaFirst (1, Nothing)
    Spec.assertEqWith s "and the batch's processing order changes nothing (CR 608.2f)" elementalFirst ashayaFirst
  -- CR 614.1d: "Continuous effects that read '[This permanent] enters . . .' or
  -- '[Objects] enter [the battlefield] . . .' are replacement effects." Zof
  -- Bloodbog prints one sentence of exactly that shape -- "This land enters
  -- tapped" -- and no effect is involved anywhere on the path: CR 305.1's special
  -- action simply puts the land onto the battlefield, so the rewrite has to be
  -- read off the permanent's own text through the CR 616.1 loop.
  --
  -- Played through the real priority loop rather than through the entry funnel,
  -- so what is asserted is the whole path a player actually takes to CR 305.1.
  --
  -- The tap state on its own is a field this same code just wrote, so what the
  -- case measures is what a PLAYER loses by it: Typhoid Rats, a {B} creature, is
  -- in the hand beside the land, and the tapped land's "{T}: Add {B}" is the only
  -- mana in the game. So the Rats stays uncast here and enters on the SAME board
  -- with that one land forced untapped -- the only difference entering tapped
  -- makes. Without the second half the first would pass on a board where the Rats
  -- was uncastable for some other reason.
  --
  -- CR 302.6 has nothing to say either way: Zof Bloodbog is a land, so summoning
  -- sickness is not what is being measured.
  Spec.it s "CR 614.1d Zof Bloodbog's own text makes it enter TAPPED" $ do
    zof <- S.printingOf s registry "Zof Consumption"
    rats <- S.printingOf s registry "Typhoid Rats"
    let (_, withZof) = S.addHandCard zof S.alice (Setup.emptyGame S.bothPlayers)
        (_, filled) = S.addHandCard rats S.alice withZof
        board =
          filled
            { GameState.phase = Phase.PrecombatMain,
              GameState.activePlayer = S.alice,
              GameState.priority = Just S.alice
            }
        played = S.runPure S.playLandAnswer board Engine.priorityLoop
        untap oid gs = gs {GameState.objects = Map.adjust (\o -> o {Object.tapped = TapState.Untapped}) oid (GameState.objects gs)}
        ratsName = CardName.MkCardName (Text.pack "Typhoid Rats")
        ratsOut gs = length [o | o <- Set.toList (GameState.battlefield gs), Projection.hasName ratsName o gs]
    case Set.toList (GameState.battlefield played) of
      [permId] -> do
        Spec.assertEqWith s "the land entered tapped" (fmap Object.tapped (Game.lookupObject permId played)) (Just TapState.Tapped)
        Spec.assertEqWith
          s
          "so the {B} creature in hand stays there -- no mana to cast it with"
          (ratsOut (S.runPure castOrPassAnswer played Engine.priorityLoop))
          0
        Spec.assertEqWith
          s
          "and the same land untapped pays for it"
          (ratsOut (S.runPure castOrPassAnswer (untap permId played) Engine.priorityLoop))
          1
      other -> Spec.assertFailure s ("expected one permanent, got " <> show (length other))
  -- CR 614.1c: "Effects that read ... 'As [this permanent] enters . . .' ... are
  -- replacement effects." Razorgrass Field -- the land face of the modal
  -- double-faced Razorgrass Ambush // Razorgrass Field -- prints one of exactly
  -- that shape with a PRICE in it: "As this land enters, you may pay 3 life. If
  -- you don't, it enters tapped."
  --
  -- The pair of cases below is one fixture answered two ways, so nothing but the
  -- answer differs between them. Played through the real priority loop, the whole
  -- path a player takes to CR 305.1, exactly as the Zof Bloodbog case above is.
  --
  -- What each case measures is not the tap-state field this same code just wrote
  -- but what a PLAYER gets for the 3 life: Soul Warden, a {W} creature, sits in
  -- the hand beside the land, and the land's "{T}: Add {W}" is the only mana in
  -- the game. So declining leaves the Warden uncast and paying gets it onto the
  -- battlefield -- Activate.activatable is deliberately NOT asked, since CR
  -- 605.3b keeps a mana ability off the stack and it answers False for one on
  -- every board.
  --
  -- The life is asserted twice over: the total, and CR 119.4's "in other words,
  -- the player loses that much life" as a recorded GameEvent.LifeLost. The second
  -- is the channel every life-loss trigger in the pool reads -- Mindcrank's and
  -- Exquisite Blood's "whenever an opponent loses life" watch this same
  -- GameEvent -- so a payment that quietly subtracted from the total instead of
  -- going through the CR 119.4 door would pass the first assertion and fail this
  -- one.
  --
  -- Soul Warden's own "whenever ANOTHER creature enters" cannot move the total:
  -- it is the only creature in the fixture, and both life assertions are read off
  -- the board the land play left, before it is ever cast.
  Spec.it s "CR 614.1c Razorgrass Field DECLINED enters tapped and costs no life" $ do
    razorgrass <- S.printingOf s registry "Razorgrass Ambush"
    warden <- S.printingOf s registry "Soul Warden"
    let played = S.runPure (payLifeOnEntryAnswer OptionalDecision.Declines) (razorgrassBoard razorgrass warden) Engine.priorityLoop
    case Set.toList (GameState.battlefield played) of
      [permId] -> do
        Spec.assertEqWith s "the land entered tapped" (fmap Object.tapped (Game.lookupObject permId played)) (Just TapState.Tapped)
        Spec.assertEqWith s "and cost nothing" (S.lifeOf S.alice played) (Just 20)
        Spec.assertBool s (not (lostLife S.alice 3 played)) "no life loss was recorded"
        Spec.assertEqWith
          s
          "so the {W} creature in hand stays there -- no mana to cast it with"
          (wardenOut (S.runPure castOrPassAnswer played Engine.priorityLoop))
          0
      other -> Spec.assertFailure s ("expected one permanent, got " <> show (length other))
  Spec.it s "CR 614.1c Razorgrass Field PAID FOR enters untapped, for exactly 3 life" $ do
    razorgrass <- S.printingOf s registry "Razorgrass Ambush"
    warden <- S.printingOf s registry "Soul Warden"
    let played = S.runPure (payLifeOnEntryAnswer OptionalDecision.Exercises) (razorgrassBoard razorgrass warden) Engine.priorityLoop
    case Set.toList (GameState.battlefield played) of
      [permId] -> do
        Spec.assertEqWith s "the land entered untapped" (fmap Object.tapped (Game.lookupObject permId played)) (Just TapState.Untapped)
        Spec.assertEqWith s "20 - 3" (S.lifeOf S.alice played) (Just 17)
        Spec.assertBool s (lostLife S.alice 3 played) "the payment was recorded as a life loss (CR 119.4)"
        Spec.assertEqWith
          s
          "and the untapped land pays for the {W} creature"
          (wardenOut (S.runPure castOrPassAnswer played Engine.priorityLoop))
          1
      other -> Spec.assertFailure s ("expected one permanent, got " <> show (length other))
  -- The pool's other printing of the same CR 614.1c sentence: Sea Gate, Reborn,
  -- the land face of Sea Gate Restoration // Sea Gate, Reborn ("As this land
  -- enters, you may pay 3 life. If you don't, it enters tapped." -- oracle checked
  -- on Scryfall). Razorgrass Field's pair above proves the rewrite; this pair
  -- proves the CARD reaches it, which it did not while the face was transcribed as
  -- a bare EntryRewrite.Tapped.
  --
  -- The PAID case is the one that carries that, and it is the only one that can:
  -- a bare Tapped leaves exactly the board declining leaves, so the DECLINED case
  -- below passes either way and is a regression fence rather than a proof.
  --
  -- Tidal Warrior, a {U} creature with no enters trigger, plays Soul Warden's part
  -- above: the land's "{T}: Add {U}" is the only mana in the game, so what the
  -- 3 life buys is read off whether the Warrior gets cast.
  Spec.it s "CR 614.1c Sea Gate, Reborn DECLINED enters tapped and costs no life" $ do
    seaGate <- S.printingOf s registry "Sea Gate Restoration"
    warrior <- S.printingOf s registry "Tidal Warrior"
    let played = S.runPure (payLifeOnEntryAnswer OptionalDecision.Declines) (seaGateBoard seaGate warrior 20) Engine.priorityLoop
    case Set.toList (GameState.battlefield played) of
      [permId] -> do
        Spec.assertEqWith s "the land entered tapped" (fmap Object.tapped (Game.lookupObject permId played)) (Just TapState.Tapped)
        Spec.assertEqWith s "and cost nothing" (S.lifeOf S.alice played) (Just 20)
        Spec.assertBool s (not (lostLife S.alice 3 played)) "no life loss was recorded"
        Spec.assertEqWith
          s
          "so the {U} creature in hand stays there -- no mana to cast it with"
          (warriorOut (S.runPure castOrPassAnswer played Engine.priorityLoop))
          0
      other -> Spec.assertFailure s ("expected one permanent, got " <> show (length other))
  Spec.it s "CR 614.1c Sea Gate, Reborn PAID FOR enters untapped, for exactly 3 life" $ do
    seaGate <- S.printingOf s registry "Sea Gate Restoration"
    warrior <- S.printingOf s registry "Tidal Warrior"
    let played = S.runPure (payLifeOnEntryAnswer OptionalDecision.Exercises) (seaGateBoard seaGate warrior 20) Engine.priorityLoop
    case Set.toList (GameState.battlefield played) of
      [permId] -> do
        Spec.assertEqWith s "the land entered untapped" (fmap Object.tapped (Game.lookupObject permId played)) (Just TapState.Untapped)
        Spec.assertEqWith s "20 - 3" (S.lifeOf S.alice played) (Just 17)
        Spec.assertBool s (lostLife S.alice 3 played) "the payment was recorded as a life loss (CR 119.4)"
        Spec.assertEqWith
          s
          "and the untapped land pays for the {U} creature"
          (warriorOut (S.runPure castOrPassAnswer played Engine.priorityLoop))
          1
      other -> Spec.assertFailure s ("expected one permanent, got " <> show (length other))
  -- CR 119.4: a player may pay an amount of life greater than 0 only if their
  -- life total is at least that amount. So a tapped Sea Gate, Reborn has TWO
  -- causes -- the controller declined, or the controller could not pay -- and the
  -- pair below is what tells them apart.
  --
  -- The answerer is pinned to Exercises in BOTH cases, so the ANSWER is held
  -- fixed and the only difference between the two boards is alice's life total:
  -- 4, where paying 3 is legal, against 2, where it is not. The engine's own
  -- CR 119.4 gate is therefore the only thing that can move the outcome. Were
  -- the gate dropped, the 2-life board would pay anyway and enter untapped; were
  -- the prompt never raised at all, the 4-life board would enter tapped.
  --
  -- 4 and 2 rather than 3 and 2, because paying 3 at 3 life leaves 0 and CR
  -- 704.5a ends the game before the assertions run.
  Spec.it s "CR 119.4 at 4 life the payment is legal, so Sea Gate, Reborn enters untapped" $ do
    seaGate <- S.printingOf s registry "Sea Gate Restoration"
    warrior <- S.printingOf s registry "Tidal Warrior"
    let played = S.runPure (payLifeOnEntryAnswer OptionalDecision.Exercises) (seaGateBoard seaGate warrior 4) Engine.priorityLoop
    case Set.toList (GameState.battlefield played) of
      [permId] -> do
        Spec.assertEqWith s "untapped" (fmap Object.tapped (Game.lookupObject permId played)) (Just TapState.Untapped)
        Spec.assertEqWith s "4 - 3" (S.lifeOf S.alice played) (Just 1)
        Spec.assertEqWith
          s
          "and the untapped land pays for the {U} creature"
          (warriorOut (S.runPure castOrPassAnswer played Engine.priorityLoop))
          1
      other -> Spec.assertFailure s ("expected one permanent, got " <> show (length other))
  Spec.it s "CR 119.4 at 2 life the payment is ILLEGAL, so it enters tapped with no life paid" $ do
    seaGate <- S.printingOf s registry "Sea Gate Restoration"
    warrior <- S.printingOf s registry "Tidal Warrior"
    let played = S.runPure (payLifeOnEntryAnswer OptionalDecision.Exercises) (seaGateBoard seaGate warrior 2) Engine.priorityLoop
    case Set.toList (GameState.battlefield played) of
      [permId] -> do
        Spec.assertEqWith s "tapped, though the answerer said pay" (fmap Object.tapped (Game.lookupObject permId played)) (Just TapState.Tapped)
        Spec.assertEqWith s "and the 2 life is untouched" (S.lifeOf S.alice played) (Just 2)
        Spec.assertBool s (not (lostLife S.alice 3 played)) "no life loss was recorded"
        Spec.assertEqWith
          s
          "so the {U} creature in hand stays there"
          (warriorOut (S.runPure castOrPassAnswer played Engine.priorityLoop))
          0
      other -> Spec.assertFailure s ("expected one permanent, got " <> show (length other))
  -- CR 614.1c's other price: Rustic Clachan, "As this land enters, you may reveal
  -- a Kithkin card from your hand. If you don't, this land enters tapped" (oracle
  -- checked on Scryfall). The three cases below are one fixture in three states,
  -- and they differ pairwise in exactly one thing each: the first two share a
  -- board and differ only in the ANSWER, the first and third share an answer that
  -- names the held creature and differ only in whether that creature is a Kithkin.
  --
  -- The land enters in all three, so "entered untapped" is told apart from
  -- "entered" -- each case reads the tap state of the one permanent on the board.
  --
  -- Mosquito Guard ({W} 1/1 Kithkin Soldier) and Benalish Hero ({W} 1/1 Human
  -- Soldier) are the pair. Neither has an enters trigger, and the Clachan's
  -- "{T}: Add {W}" is the only mana in the game, so what the reveal buys is read
  -- off whether the creature gets cast -- Activate.activatable is deliberately not
  -- asked, since CR 605.3b keeps a mana ability off the stack.
  --
  -- The reveal is asserted through S.revealsOf, CR 701.20a's public log, and not
  -- through the tap state alone: showing a card is the whole of what the player
  -- did, and a rewrite that left the land untapped without revealing anything
  -- would pass the tap assertion.
  Spec.it s "CR 614.1c Rustic Clachan REVEALING a Kithkin card enters untapped" $ do
    clachan <- S.printingOf s registry "Rustic Clachan"
    guard_ <- S.printingOf s registry "Mosquito Guard"
    let (guardId, board) = clachanBoard clachan guard_
        played = S.runPure (revealOnEntryAnswer (Just guardId)) board Engine.priorityLoop
    case Set.toList (GameState.battlefield played) of
      [permId] -> do
        Spec.assertEqWith s "the land entered untapped" (fmap Object.tapped (Game.lookupObject permId played)) (Just TapState.Untapped)
        Spec.assertEqWith
          s
          "and alice showed the Kithkin card to do it (CR 701.20a)"
          (S.revealsOf played)
          [(S.alice, Set.singleton (CardName.MkCardName (Text.pack "Mosquito Guard")))]
        Spec.assertEqWith s "one ChooseRevealOnEntry was raised" (revealAsks (answersFor (revealOnEntryAnswer (Just guardId)) board Engine.priorityLoop)) 1
        Spec.assertEqWith
          s
          "and the untapped land pays for the {W} creature"
          (namedOut "Mosquito Guard" (S.runPure castOrPassAnswer played Engine.priorityLoop))
          1
      other -> Spec.assertFailure s ("expected one permanent, got " <> show (length other))
  -- The "may" half. CR 614.1c states the reveal as optional, so holding the
  -- Kithkin card is not being made to show it -- and this is the case that proves
  -- the answer is the player's rather than the engine's, since the board is the
  -- one above's exactly.
  Spec.it s "CR 614.1c DECLINING with a Kithkin card in hand still enters tapped" $ do
    clachan <- S.printingOf s registry "Rustic Clachan"
    guard_ <- S.printingOf s registry "Mosquito Guard"
    let (_, board) = clachanBoard clachan guard_
        played = S.runPure (revealOnEntryAnswer Nothing) board Engine.priorityLoop
    case Set.toList (GameState.battlefield played) of
      [permId] -> do
        Spec.assertEqWith s "the land entered tapped" (fmap Object.tapped (Game.lookupObject permId played)) (Just TapState.Tapped)
        Spec.assertEqWith s "and nothing was shown" (S.revealsOf played) []
        Spec.assertEqWith
          s
          "so the {W} creature in hand stays there -- no mana to cast it with"
          (namedOut "Mosquito Guard" (S.runPure castOrPassAnswer played Engine.priorityLoop))
          0
      other -> Spec.assertFailure s ("expected one permanent, got " <> show (length other))
  -- The negative, and the case the printed filter is for. The answerer is pinned
  -- to the held creature in BOTH this case and the first, so the only difference
  -- between the two boards is whether that creature is a Kithkin: the engine's own
  -- filter is the only thing that can move the outcome. Were the offer unfiltered,
  -- the Hero would be shown and the land would enter untapped.
  Spec.it s "CR 614.1c with NO Kithkin card in hand it enters tapped, unasked" $ do
    clachan <- S.printingOf s registry "Rustic Clachan"
    hero <- S.printingOf s registry "Benalish Hero"
    let (heroId, board) = clachanBoard clachan hero
        played = S.runPure (revealOnEntryAnswer (Just heroId)) board Engine.priorityLoop
    case Set.toList (GameState.battlefield played) of
      [permId] -> do
        Spec.assertEqWith s "the land entered tapped" (fmap Object.tapped (Game.lookupObject permId played)) (Just TapState.Tapped)
        Spec.assertEqWith s "and the non-Kithkin card was not shown" (S.revealsOf played) []
        Spec.assertEqWith s "and no ChooseRevealOnEntry was raised -- nothing in hand to show" (revealAsks (answersFor (revealOnEntryAnswer (Just heroId)) board Engine.priorityLoop)) 0
        Spec.assertEqWith
          s
          "so the {W} creature in hand stays there"
          (namedOut "Benalish Hero" (S.runPure castOrPassAnswer played Engine.priorityLoop))
          0
      other -> Spec.assertFailure s ("expected one permanent, got " <> show (length other))
  Spec.it s "CR 707.5 declining the copy leaves a 0/0 that dies (CR 704.5f)" $ do
    island <- S.printingOf s registry "Island"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    clone <- S.printingOf s registry "Clone"
    let base = S.landsInPlay island 4
        (_, withPiker) = S.addCreature pikerPrinting S.alice base
        (gs, cloneId) = S.handOne clone withPiker
        -- S.identityAnswer declines ChooseCopyTarget (Clone's own "may").
        resolved = S.runPure S.identityAnswer gs (S.cast S.alice cloneId >> Stack.resolveTop >> Engine.settleForPriority)
        named = filter (\oid -> fmap Face.name (Game.faceOf oid resolved) == Just (CardName.MkCardName $ Text.pack "Clone")) (Set.toList (GameState.battlefield resolved))
    Spec.assertEqWith s "the 0/0 Clone is gone" named []
  Spec.it s "CR 614.12a the copy choice is locked in BEFORE the enters event exists" $ do
    island <- S.printingOf s registry "Island"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    clonePrinting <- S.printingOf s registry "Clone"
    let base = S.landsInPlay island 4
        (piker, withPiker) = S.addCreature pikerPrinting S.alice base
        (gs, cloneId) = S.handOne clonePrinting withPiker
        -- No settle: the choice must already be made when resolveTop returns.
        resolved = S.runPure (copyOf piker) gs (S.cast S.alice cloneId >> Stack.resolveTop)
        named = filter (\oid -> fmap Face.name (Game.faceOf oid resolved) == Just (CardName.MkCardName $ Text.pack "Clone")) (Set.toList (GameState.battlefield resolved))
    case named of
      [] -> Spec.assertFailure s "Clone did not reach the battlefield"
      clone : _ -> Spec.assertEqWith s "already a 2/1, with no settle run" (Projection.powerOf clone resolved) (Just 2)
  Spec.it s "CR 208.2b Primal Plasma enters as the 2/2 with flying its controller picked" $ do
    island <- S.printingOf s registry "Island"
    primalPlasma <- S.printingOf s registry "Primal Plasma"
    let (gs, held) = blueBoard island 4 [primalPlasma]
    case held of
      plasmaCard : _ ->
        let after = S.runPure (enteringAs 1) gs (S.cast S.alice plasmaCard >> Stack.resolveTop)
         in case newestNamed (CardName.MkCardName $ Text.pack "Primal Plasma") after of
              Nothing -> Spec.assertFailure s "Primal Plasma did not reach the battlefield"
              Just plasma -> do
                Spec.assertEqWith s "power" (Projection.powerOf plasma after) (Just 2)
                Spec.assertEqWith s "toughness" (Projection.toughnessOf plasma after) (Just 2)
                Spec.assertBool s (Projection.hasKeyword Keyword.Flying plasma after) "flying"
      _ -> Spec.assertFailure s "fixture did not deal a card"
  Spec.it s "CR 616.2 a Clone of a 2/2-flying Plasma that picks 1/6 is 1/6 with flying AND defender" $ do
    -- THE CENTERPIECE, and the Gatherer ruling verbatim: "it copies the values
    -- determined by its enters-the-battlefield replacement effect, but its
    -- power and toughness are determined by the copy's own
    -- enters-the-battlefield replacement effect."
    island <- S.printingOf s registry "Island"
    primalPlasma <- S.printingOf s registry "Primal Plasma"
    clonePrinting <- S.printingOf s registry "Clone"
    let (gs, held) = blueBoard island 8 [primalPlasma, clonePrinting]
    case held of
      plasmaCard : cloneCard : _ ->
        let withPlasma = S.runPure (enteringAs 1) gs (S.cast S.alice plasmaCard >> Stack.resolveTop)
            after = S.runPure (enteringAs 2) withPlasma (S.cast S.alice cloneCard >> Stack.resolveTop)
         in case newestNamed (CardName.MkCardName $ Text.pack "Clone") after of
              Nothing -> Spec.assertFailure s "Clone did not reach the battlefield"
              Just clone -> do
                Spec.assertEqWith s "power is the CLONE's own choice" (Projection.powerOf clone after) (Just 1)
                Spec.assertEqWith s "toughness is the CLONE's own choice" (Projection.toughnessOf clone after) (Just 6)
                Spec.assertBool s (Projection.hasKeyword Keyword.Flying clone after) "flying came from the COPY"
                Spec.assertBool s (Projection.hasKeyword Keyword.Defender clone after) "defender came from the CHOICE"
      _ -> Spec.assertFailure s "fixture did not deal two cards"
  Spec.it s "CR 616.2 the same Clone picking 3/3 is a 3/3 with flying" $ do
    island <- S.printingOf s registry "Island"
    primalPlasma <- S.printingOf s registry "Primal Plasma"
    clonePrinting <- S.printingOf s registry "Clone"
    let (gs, held) = blueBoard island 8 [primalPlasma, clonePrinting]
    case held of
      plasmaCard : cloneCard : _ ->
        let withPlasma = S.runPure (enteringAs 1) gs (S.cast S.alice plasmaCard >> Stack.resolveTop)
            after = S.runPure (enteringAs 0) withPlasma (S.cast S.alice cloneCard >> Stack.resolveTop)
         in case newestNamed (CardName.MkCardName $ Text.pack "Clone") after of
              Nothing -> Spec.assertFailure s "Clone did not reach the battlefield"
              Just clone -> do
                Spec.assertEqWith s "3/3" (Projection.powerOf clone after) (Just 3)
                Spec.assertEqWith s "3/3" (Projection.toughnessOf clone after) (Just 3)
                Spec.assertBool s (Projection.hasKeyword Keyword.Flying clone after) "still flying (keywords UNION, never assign)"
                Spec.assertBool s (not (Projection.hasKeyword Keyword.Defender clone after)) "no defender"
      _ -> Spec.assertFailure s "fixture did not deal two cards"
  Spec.it s "CR 707.2 a Clone of that Clone copies 1/6-flying-defender and then chooses again" $ do
    island <- S.printingOf s registry "Island"
    primalPlasma <- S.printingOf s registry "Primal Plasma"
    clonePrinting <- S.printingOf s registry "Clone"
    let (gs, held) = blueBoard island 12 [primalPlasma, clonePrinting, clonePrinting]
    case held of
      plasmaCard : cloneA : cloneB : _ ->
        let s1 = S.runPure (enteringAs 1) gs (S.cast S.alice plasmaCard >> Stack.resolveTop)
            s2 = S.runPure (enteringAs 2) s1 (S.cast S.alice cloneA >> Stack.resolveTop)
            s3 = S.runPure (enteringAs 0) s2 (S.cast S.alice cloneB >> Stack.resolveTop)
         in case newestNamed (CardName.MkCardName $ Text.pack "Clone") s3 of
              Nothing -> Spec.assertFailure s "the second Clone did not reach the battlefield"
              Just clone -> do
                Spec.assertEqWith s "its OWN choice wins on P/T" (Projection.powerOf clone s3) (Just 3)
                Spec.assertEqWith s "its OWN choice wins on P/T" (Projection.toughnessOf clone s3) (Just 3)
                Spec.assertBool s (Projection.hasKeyword Keyword.Flying clone s3 && Projection.hasKeyword Keyword.Defender clone s3) "flying and defender rode the copy chain"
      _ -> Spec.assertFailure s "fixture did not deal three cards"
  -- CR 208.2b's own elision, at the ChoiceOf boundary: such an ability
  -- "lists two or more specific power and toughness values", so a
  -- single-option as-enters choice is not a 208.2b choice at all and
  -- must apply with no ChooseEntryOption prompt. NOT the same shape as
  -- the CR 616.1 "one Hardened Scales alone is not asked about" case
  -- above -- that is one CANDIDATE in a race between several sources;
  -- this is one OPTION inside a single candidate's own payload, which
  -- 616.1 (choosing which replacement effect applies) never reaches.
  -- Built as rules-level data (a floating EntryR ChoiceOf with one
  -- option, seeded via S.addReplacement) rather than a synthetic card
  -- file: no printed card in the pool has a single-option choice.
  Spec.it s "CR 400.3 an Opponents zone-change redirect exiles an opponent's card, not your own" $ do
    -- Leyline of the Void's shape without the Leyline: a floating redirect
    -- whose source alice controls. Bob's card is exiled on the way to his
    -- graveyard; alice's own reaches hers untouched.
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    let (src, g1) = S.addCreature pikerPrinting S.alice (Setup.emptyGame S.bothPlayers)
        (mine, g2) = S.addCreature pikerPrinting S.alice g1
        (theirs, g3) = S.addCreature pikerPrinting S.bob g2
        g4 = S.addReplacement (leylineShape src (fst (Game.freshTimestamp g3))) g3
        after = S.runPure S.identityAnswer g4 (Event.changeZone mine Zone.Graveyard >> Event.changeZone theirs Zone.Graveyard)
    Spec.assertEqWith s "alice's own card reaches her graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
    Spec.assertEqWith s "bob's does not" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 0
    Spec.assertEqWith s "it was exiled instead" (length (Game.zoneMembers Zone.Exile S.bob after)) 1
  Spec.it s "CR 400.3 the zone-change subject is the card's OWNER, not its controller" $ do
    -- A card alice OWNS but bob CONTROLS still dies to alice's graveyard
    -- (CR 400.3), so alice's own redirect must not exile it. A
    -- controller-based test would, which is the case this pins.
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    let slot = SlotName.MkSlotName (Text.pack "target")
        (src, g1) = S.addCreature pikerPrinting S.alice (Setup.emptyGame S.bothPlayers)
        (oid, g2) = S.addCreature pikerPrinting S.alice g1
        g3 = S.addReplacement (leylineShape src (fst (Game.freshTimestamp g2))) g2
        stolen =
          S.runPure S.identityAnswer g3 $
            Resolve.applyEffect S.noSource S.noSource S.bob (Map.singleton slot (Set.singleton (Recipient.ToObject oid))) (Map.singleton slot (Set.singleton (Recipient.ToObject oid))) (Effect.GainControl (DurationRef.MkDurationRef Duration.Indefinite (ObjectRef.InSlot slot)))
        after = S.runPure S.identityAnswer stolen (Event.changeZone oid Zone.Graveyard)
    Spec.assertEqWith s "bob really did take control of it" (Projection.controllerOf oid stolen) (Just S.bob)
    Spec.assertEqWith s "it reaches its OWNER's graveyard, unexiled" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
    Spec.assertEqWith s "and nothing was exiled" (length (Game.zoneMembers Zone.Exile S.alice after)) 0
  Spec.it s "CR 208.2b a single-option ChoiceOf is not a choice and must not prompt" $ do
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    let (piker, g1) = S.addCreature pikerPrinting S.alice (Setup.emptyGame S.bothPlayers)
        (ts, g2) = Game.freshTimestamp g1
        onlyOption = EntryOption.MkEntryOption {EntryOption.power = 3, EntryOption.toughness = 3, EntryOption.keywords = Set.empty}
        active =
          ActiveReplacement.MkActiveReplacement
            { ActiveReplacement.effect = ReplacementEffect.EntryR (EntryR.MkEntryR Filter.Type.IsSource (EntryRewrite.ChoiceOf [onlyOption])),
              ActiveReplacement.source = piker,
              ActiveReplacement.controller = S.alice,
              ActiveReplacement.timestamp = ts,
              ActiveReplacement.expiry = Expiry.AtCleanup,
              ActiveReplacement.uses = Uses.Once,
              ActiveReplacement.origin = ReplacementOrigin.Other,
              ActiveReplacement.condition = Nothing,
              ActiveReplacement.rider = Nothing,
              ActiveReplacement.slots = Map.empty
            }
        g3 = S.addReplacement active g2
        asked = answersFor S.identityAnswer g3 (Event.runEntry Set.empty piker)
        after = S.runPure S.identityAnswer g3 (Event.runEntry Set.empty piker)
    Spec.assertBool s (not (wasAskedForEntryOption asked)) "no ChooseEntryOption was raised"
    Spec.assertEqWith s "the sole option applied anyway" (Projection.powerOf piker after) (Just 3)
  Spec.it s "CR 614.16 Doubling Season turns Dragon Fodder's two Goblins into four" $ do
    mountain <- S.printingOf s registry "Mountain"
    doublingSeason <- S.printingOf s registry "Doubling Season"
    dragonFodder <- S.printingOf s registry "Dragon Fodder"
    let base = S.landsInPlay mountain 2
        (_, g1) = S.addCreature doublingSeason S.alice base
        (g2, spellId) = S.handOne dragonFodder g1
        after = castAndResolve S.identityAnswer g2 spellId
    Spec.assertEqWith s "twice that many" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Goblin Token") S.alice after) 4
  Spec.it s "CR 614.5 two Doubling Seasons are two instances: eight Goblins" $ do
    mountain <- S.printingOf s registry "Mountain"
    doublingSeason <- S.printingOf s registry "Doubling Season"
    dragonFodder <- S.printingOf s registry "Dragon Fodder"
    let base = S.landsInPlay mountain 2
        (_, g1) = S.addCreature doublingSeason S.alice base
        (_, g2) = S.addCreature doublingSeason S.alice g1
        (g3, spellId) = S.handOne dragonFodder g2
        after = castAndResolve S.identityAnswer g3 spellId
    Spec.assertEqWith s "2 -> 4 -> 8" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Goblin Token") S.alice after) 8
  Spec.it s "CR 614.1 Doubling Season's OTHER clause doubles counters, not tokens" $ do
    forest <- S.printingOf s registry "Forest"
    battlegrowth <- S.printingOf s registry "Battlegrowth"
    doublingSeason <- S.printingOf s registry "Doubling Season"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    let (gs, spellId, mine, _) = counterBoard forest battlegrowth [doublingSeason, pikerPrinting] []
    case mine of
      season : piker : _ ->
        let after = castAndResolve (raceAnswer season piker) gs spellId
         in Spec.assertEqWith s "1 * 2" (countersOn CounterKind.PlusOnePlusOne piker after) 2
      _ -> Spec.assertFailure s "fixture did not build two permanents"
  Spec.it s "CR 616.1 Doubling Season racing Hardened Scales: 4 or 3, by the prompt" $ do
    forest <- S.printingOf s registry "Forest"
    battlegrowth <- S.printingOf s registry "Battlegrowth"
    doublingSeason <- S.printingOf s registry "Doubling Season"
    hardenedScales <- S.printingOf s registry "Hardened Scales"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    let (gs, spellId, mine, _) = counterBoard forest battlegrowth [doublingSeason, hardenedScales, pikerPrinting] []
    case mine of
      season : scales : piker : _ ->
        let seasonFirst = castAndResolve (raceAnswer season piker) gs spellId
            scalesFirst = castAndResolve (raceAnswer scales piker) gs spellId
         in do
              Spec.assertEqWith s "(1 * 2) + 1" (countersOn CounterKind.PlusOnePlusOne piker seasonFirst) 3
              Spec.assertEqWith s "(1 + 1) * 2" (countersOn CounterKind.PlusOnePlusOne piker scalesFirst) 4
      _ -> Spec.assertFailure s "fixture did not build three permanents"
  -- #79: resolveDestruction answers with the SETTLED object, not a Bool. The
  -- identity of what the CR 616.1 loop hands back is what Event.destroy must
  -- put into the graveyard; collapsing it to a predicate is what made a
  -- redirecting DestructionRewrite silently unimplementable.
  Spec.it s "CR 701.8 an unreplaced destruction settles on the object itself" $ do
    swamp <- S.printingOf s registry "Swamp"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    let base = S.landsInPlay swamp 1
        (piker, g1) = S.addCreature pikerPrinting S.alice base
        (settled, _) = S.runPureWith S.identityAnswer g1 (Event.resolveDestruction Nothing DestructionCause.ByEffect Regenerability.Regenerable piker)
    Spec.assertEqWith s "the object it was asked about" settled (Just piker)
  Spec.it s "CR 701.19a a regenerated destruction settles on nothing" $ do
    swamp <- S.printingOf s registry "Swamp"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    let base = S.landsInPlay swamp 1
        (piker, g1) = S.addCreature pikerPrinting S.alice base
        (settled, _) = S.runPureWith S.identityAnswer (S.addRegenShield piker g1) (Event.resolveDestruction Nothing DestructionCause.ByEffect Regenerability.Regenerable piker)
    Spec.assertEqWith s "consumed by the shield" settled Nothing
  stepSkipSpec s registry
  fatigueSpec s registry
  stonehornSpec s registry
  galvanicBlastSpec s registry
  voltaicSurgeSpec s registry
  mendingHandsSpec s registry
  healingGraceSpec s registry
  testOfFaithSpec s registry
  decoratedGriffinSpec s registry
  braceForImpactSpec s registry
  inkshieldSpec s registry
  stormwildCapridorSpec s registry
  jaredCarthalionSpec s registry
  spiderPunkSpec s registry
  apnapSpec s registry
  excruciatorSpec s registry
  questingBeastSpec s registry
  luminesceSpec s registry
  moonmistSpec s registry
  selflessSquireSpec s registry
  turnTheTablesSpec s registry
  gatherSpecimensSpec s registry
  kismetSpec s registry
  shimatsuSpec s registry
  entryBudgetSpec s registry
  riotSpec s registry
  unleashSpec s registry
  bloodthirstSpec s registry
  brineElementalSpec s registry
  coldsteelHeartSpec s registry
  stuffyDollSpec s registry
  vorinclexSpec s registry
  damageCountersSpec s registry
  entryCountersSpec s registry
  shieldCounterSpec s registry
  warLeechSpec s registry

-- Monstrous War-Leech {3}{B} Creature -- Leech Horror \*/*, whole text: "Kicker
-- {U}. As this creature enters, if it was kicked, mill four cards. Monstrous
-- War-Leech's power and toughness are each equal to the greatest mana value
-- among cards in your graveyard." (oracle checked on Scryfall)
--
-- CR 614.1c's shape that RUNS AN EFFECT, gated on a condition (see #1416) --
-- EntryRewrite.RunEffects, with "if it was kicked" on CR 604.2's clause.
--
-- THE BOARD, one fixture the cases below take in three states: five lands (four
-- Swamps and an Island, so {3}{B} is payable with or without the kicker {U}), the
-- Leech in hand, SIX Lairwatch Giants in the library, and whatever `buried` names
-- already in the graveyard.
--
-- ONE Lightning Bolt is what the first two pass, and it is what makes both halves
-- observable at once. Without it the unkicked Leech is a 0/0 that CR 704.5f
-- buries, and "no mill" would be told from "mill" by a permanent that is not
-- there -- the confusion that issue's own bar rules out. Lightning Bolt's mana value is
-- 1 and Lairwatch Giant's is 6 (CR 202.3), so the Leech ENTERS AND SURVIVES on
-- both boards and the two are told apart by what it is: a 1/1 unkicked, a 6/6
-- kicked. The third case passes NONE, which is how it sees the ordering the other
-- two cannot.
--
-- Every number distinct: one Bolt, four milled, six in the library before and two
-- after, five lands, and the two power/toughness readings 1 and 6.
warLeechBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> [Printing.Printing] -> (GameState.GameState, ObjectId.ObjectId)
warLeechBoard swamp island leech giant buried =
  let lands = S.landsFor island S.alice 1 (S.landsInPlay swamp 4)
      withBuried = List.foldl' (\g p -> snd (S.addGraveyardCard p S.alice g)) lands buried
      stocked = List.foldl' (\g _ -> snd (S.addLibraryCard giant S.alice g)) withBuried [1 :: Int .. 6]
   in S.handOne leech stocked

-- Answers CR 702.33a's kicker question with `decision` and defers everything else,
-- so the two boards below differ in this one answer and nothing else.
kicks :: KickerDecision.KickerDecision -> Prompt.Prompt r -> r
kicks decision p = case p of
  Prompt.ChooseKicker {} -> decision
  _ -> S.identityAnswer p

-- How many cards are in alice's library, and in her graveyard.
zoneSizes :: GameState.GameState -> (Int, Int)
zoneSizes gs =
  ( Seq.length (Map.findWithDefault Seq.empty S.alice (GameState.library gs)),
    Seq.length (Map.findWithDefault Seq.empty S.alice (GameState.graveyard gs))
  )

-- The battlefield's Monstrous War-Leech, by name: CR 400.7 gives the permanent a
-- new id, so the one the cast was handed names nothing here.
leechOut :: GameState.GameState -> [ObjectId.ObjectId]
leechOut gs = [o | o <- Set.toList (GameState.battlefield gs), Projection.hasName (CardName.MkCardName (Text.pack "Monstrous War-Leech")) o gs]

-- Cast the Leech with this kicker answer and settle: the entry rewrite's effects
-- are queued by Pawl.Engine.Event and drained by performSettle, so the mill has
-- happened by the time the state-based action pass reads the Leech's toughness.
castLeech :: KickerDecision.KickerDecision -> GameState.GameState -> ObjectId.ObjectId -> GameState.GameState
castLeech decision gs leechId =
  let cast = snd (Engine.runGamePure (kicks decision) gs (S.cast S.alice leechId))
   in snd (Engine.runGamePure (kicks decision) cast (Stack.resolveTop >> Engine.settleForPriority))

warLeechSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
warLeechSpec s registry = Spec.describe s "Monstrous War-Leech" $ do
  -- CR 702.33d's designation survives the resolution (CR 400.7d), the CR 604.2
  -- clause reads it off the entering permanent, and CR 614.1c's rewrite runs the
  -- mill.
  Spec.it s "CR 614.1c kicked, the as-enters mill runs: four cards leave the library and the Leech is a 6/6" $ do
    swamp <- S.printingOf s registry "Swamp"
    island <- S.printingOf s registry "Island"
    leech <- S.printingOf s registry "Monstrous War-Leech"
    giant <- S.printingOf s registry "Lairwatch Giant"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let (board, leechId) = warLeechBoard swamp island leech giant [bolt]
        settled = castLeech KickerDecision.Kicks board leechId
    Spec.assertEqWith s "four cards were milled: six in the library became two, and the graveyard's one Bolt became five cards" (zoneSizes settled) (2, 5)
    case leechOut settled of
      [permId] -> Spec.assertEqWith s "the greatest mana value among them is Lairwatch Giant's 6" (S.powerToughnessOf permId settled) (Just (6, 6))
      other -> Spec.assertFailure s ("expected one Leech, got " <> show (length other))
  -- The same board and the same answerer but for the one answer. The Leech enters
  -- HERE TOO, so what the two cases tell apart is whether the replacement ran and
  -- not whether the permanent arrived.
  Spec.it s "CR 614.1c unkicked, the rewrite does not apply: nothing is milled and the Leech is a 1/1" $ do
    swamp <- S.printingOf s registry "Swamp"
    island <- S.printingOf s registry "Island"
    leech <- S.printingOf s registry "Monstrous War-Leech"
    giant <- S.printingOf s registry "Lairwatch Giant"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let (board, leechId) = warLeechBoard swamp island leech giant [bolt]
        settled = castLeech KickerDecision.Declines board leechId
    Spec.assertEqWith s "the library is untouched and the graveyard still holds only the Bolt" (zoneSizes settled) (6, 1)
    case leechOut settled of
      [permId] -> Spec.assertEqWith s "so the greatest mana value is Lightning Bolt's 1" (S.powerToughnessOf permId settled) (Just (1, 1))
      other -> Spec.assertFailure s ("expected one Leech, got " <> show (length other))
  -- WHEN the effects run, which the two cases above cannot see: the Bolt keeps the
  -- Leech alive whatever the mill does. With an EMPTY graveyard the Leech's CDA
  -- determines nothing and CR 208.2a makes it 0, so the mill is the only thing
  -- between it and CR 704.5f -- and it survives, which says the drain happens
  -- before the state-based action pass rather than after it.
  --
  -- Pawl.PowerToughnessSpec's "CR 704.5f the 0/0 Leech dies" is this board's other
  -- half: no blue mana there, so no kicker is offered, nothing is milled and the
  -- Leech is buried.
  Spec.it s "CR 704.5f kicked with an EMPTY graveyard, the mill beats the SBA pass: the Leech lives as a 6/6" $ do
    swamp <- S.printingOf s registry "Swamp"
    island <- S.printingOf s registry "Island"
    leech <- S.printingOf s registry "Monstrous War-Leech"
    giant <- S.printingOf s registry "Lairwatch Giant"
    let (board, leechId) = warLeechBoard swamp island leech giant []
        settled = castLeech KickerDecision.Kicks board leechId
    Spec.assertEqWith s "four cards were milled into an empty graveyard" (zoneSizes settled) (2, 4)
    case leechOut settled of
      [permId] -> Spec.assertEqWith s "and the Leech is a 6/6 rather than a buried 0/0" (S.powerToughnessOf permId settled) (Just (6, 6))
      other -> Spec.assertFailure s ("expected one Leech, got " <> show (length other))

-- alice controls one Mountain plus `artifacts` Darksteel Myr, and holds a
-- Galvanic Blast; `others` are her further permanents, added after the Myr.
-- Returns the state and the Blast's hand id.
--
-- Darksteel Myr because it is an artifact with nothing else going on -- no
-- static ability, no mana ability of its own to be tapped for, and CR 702.12b's
-- indestructibility never comes up because nothing here destroys anything. Three
-- copies of one card, since "three or more artifacts" counts artifacts and not
-- distinct names.
metalcraftBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Int -> [Printing.Printing] -> (GameState.GameState, ObjectId.ObjectId)
metalcraftBoard mountain myr galvanicBlast artifacts others =
  let addAll ps gs = List.foldl' (\g p -> snd (S.addCreature p S.alice g)) gs ps
      gs1 = addAll (replicate artifacts myr <> others) (S.landsInPlay mountain 1)
      (gs2, spellId) = S.handOne galvanicBlast gs1
   in (gs2, spellId)

-- Aim every target slot at bob himself. CR 115.4's "any target" admits a player,
-- and a life total is the cleanest readout of an amount: 2, 4 and 8 are three
-- distinct answers with no toughness or state-based action in the way.
atBob :: Prompt.Prompt r -> r
atBob p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToPlayer S.bob))) sets
  _ -> S.identityAnswer p

-- CR 614.15's self-replacement effects and CR 616.1a's bucket, through the one
-- card in the pool that prints one.
--
-- Galvanic Blast is CR 614.15's own description almost word for word -- "the text
-- creating a self-replacement effect is usually part of the ability whose effect
-- is being replaced, but the text can be a separate ability, particularly when
-- preceded by an ability word." Metalcraft is the ability word -- CR 207.2c lists
-- it by name and says ability words "have no special rules meaning" -- the clause
-- replaces the damage the spell's own first line deals, and the whole thing is
-- one instant.
--
-- The card's two lines resolve as two effects in the ISA, and in the opposite
-- order from the printing: the Replace comes first so the replacement exists
-- before the DealDamage proposes the event it replaces (CR 614.4, "replacement
-- effects must exist before the appropriate event occurs"). Nothing observes the
-- gap: CR 117.3b gives the active player priority only AFTER a spell resolves,
-- and CR 608.2g's last sentence forbids casting or activating anything during one.
-- So the printed reading and this one agree on every board.
galvanicBlastSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
galvanicBlastSpec s registry =
  Spec.describe s "Galvanic Blast (CR 614.15)" $ do
    Spec.it s "CR 614.15 with two artifacts metalcraft is off, so the Blast deals its printed 2" $ do
      mountain <- S.printingOf s registry "Mountain"
      myr <- S.printingOf s registry "Darksteel Myr"
      galvanicBlast <- S.printingOf s registry "Galvanic Blast"
      let (gs, spellId) = metalcraftBoard mountain myr galvanicBlast 2 []
          after = castAndResolve atBob gs spellId
      Spec.assertEqWith s "bob takes 2" (S.lifeOf S.bob after) (Just 18)
      -- CR 614.1: the row IS installed and simply does not apply, its clause
      -- being false when the damage would happen (voltaicSurgeSpec below is
      -- where that separation is observable). Unspent, since it never applied.
      Spec.assertEqWith s "the row is installed but unapplied" (length (GameState.replacements after)) 1
    -- The discriminating twin: one more artifact, everything else identical.
    --
    -- The FOUR-artifact leg is what makes this a Comparison.AtLeast test rather
    -- than an Exactly one -- the card says "three or MORE", and at exactly three
    -- the two comparisons agree.
    Spec.it s "CR 614.15 with three or more artifacts the self-replacement applies: 4 instead of 2" $ do
      mountain <- S.printingOf s registry "Mountain"
      myr <- S.printingOf s registry "Darksteel Myr"
      galvanicBlast <- S.printingOf s registry "Galvanic Blast"
      let (three, threeId) = metalcraftBoard mountain myr galvanicBlast 3 []
          (four, fourId) = metalcraftBoard mountain myr galvanicBlast 4 []
          after = castAndResolve atBob three threeId
      Spec.assertEqWith s "at three, bob takes 4" (S.lifeOf S.bob after) (Just 16)
      Spec.assertEqWith s "at four, still 4 -- not back down to the printed 2" (S.lifeOf S.bob (castAndResolve atBob four fourId)) (Just 16)
      -- CR 614.3's "until they're used up": the row applied, so Uses.Once spent
      -- it. Nothing is left to replace a later damage event this turn.
      Spec.assertEqWith s "and the one-shot was consumed" (GameState.replacements after) []
    Spec.it s "CR 614.15 the metalcraft count is artifacts YOU control, not everyone's" $ do
      mountain <- S.printingOf s registry "Mountain"
      myr <- S.printingOf s registry "Darksteel Myr"
      galvanicBlast <- S.printingOf s registry "Galvanic Blast"
      -- alice has two; bob has three. CR 109.5's "you" is the spell's
      -- controller, so hers is the count that matters and metalcraft is off.
      let (gs0, spellId) = metalcraftBoard mountain myr galvanicBlast 2 []
          gs = List.foldl' (\g _ -> snd (S.addCreature myr S.bob g)) gs0 [1 :: Int, 2, 3]
          after = castAndResolve atBob gs spellId
      Spec.assertEqWith s "bob takes 2, not 4" (S.lifeOf S.bob after) (Just 18)
    -- CR 616.1a, and the reason the SelfReplacement bucket exists: "if any of
    -- the replacement and/or prevention effects are self-replacement effects
    -- (see rule 614.15), one of them must be chosen."
    --
    -- Furnace of Rath is the other applicable damage replacement, and the two
    -- ORDERS DISAGREE, which is what makes this an assertion rather than a
    -- coincidence:
    --
    --   * CR 616.1a's order -- metalcraft first (2 becomes 4), then the Furnace
    --     doubles it: 8.
    --   * the other order -- the Furnace first (2 becomes 4), then metalcraft
    --     sets it to 4: 4.
    --
    -- Furnace of Rath's own ruling states the general rule this is an instance
    -- of: "if multiple effects modify how damage will be dealt, the player who
    -- would be dealt damage ... chooses the order to apply the effects." CR
    -- 616.1a is the exception that takes that choice away here.
    Spec.it s "CR 616.1a the self-replacement is applied BEFORE Furnace of Rath: 2 -> 4 -> 8" $ do
      mountain <- S.printingOf s registry "Mountain"
      myr <- S.printingOf s registry "Darksteel Myr"
      furnaceOfRath <- S.printingOf s registry "Furnace of Rath"
      galvanicBlast <- S.printingOf s registry "Galvanic Blast"
      let (gs, spellId) = metalcraftBoard mountain myr galvanicBlast 3 [furnaceOfRath]
          after = castAndResolve atBob gs spellId
          asked = answersFor atBob gs (S.cast S.alice spellId >> Stack.resolveTop)
      Spec.assertEqWith s "bob takes 8, not the 4 the other order gives" (S.lifeOf S.bob after) (Just 12)
      -- The second half of CR 616.1a: because the self-replacement is alone in
      -- the highest non-empty bucket, there is nothing to choose and the engine
      -- must not ask. The Hardened-Scales-versus-Corpsejack race above, whose
      -- two candidates share CR 616.1e's bucket, DOES ask -- so this is the
      -- bucket ordering being observed, not prompts being suppressed in general.
      Spec.assertBool s (not (wasAskedToReplace asked)) "no ChooseReplacement was raised"
    -- The control leg for the Furnace: with metalcraft OFF there is no
    -- self-replacement at all, so the Furnace doubles the printed 2. Without
    -- this, an engine that ignored the metalcraft clause entirely and simply
    -- doubled twice would also reach 8.
    Spec.it s "CR 614.1a Furnace of Rath alone doubles the printed 2, not 4" $ do
      mountain <- S.printingOf s registry "Mountain"
      myr <- S.printingOf s registry "Darksteel Myr"
      furnaceOfRath <- S.printingOf s registry "Furnace of Rath"
      galvanicBlast <- S.printingOf s registry "Galvanic Blast"
      -- Two Myr, so the Furnace is the ONLY artifact short of metalcraft's three
      -- -- an enchantment, so it cannot make up the count itself.
      let (gs, spellId) = metalcraftBoard mountain myr galvanicBlast 2 [furnaceOfRath]
          after = castAndResolve atBob gs spellId
      Spec.assertEqWith s "bob takes 4" (S.lifeOf S.bob after) (Just 16)
    -- CR 614.15's "this way": the clause replaces the damage ITS OWN SOURCE is
    -- dealing and nothing else.
    --
    -- The row is seeded rather than cast, and its use count is widened to
    -- Unlimited, because Galvanic Blast cannot show this on any board: the
    -- Blast's own damage event is the first one the row is ever offered, and
    -- Uses.Once spends it there (CR 614.3), so a second event would be untouched
    -- whatever the pattern said. Widening the count is what lets both events
    -- reach the same row, which is what isolates the PATTERN. Everything else --
    -- the funnel, the CR 616.1 loop, the rewrite -- is the real machinery, and
    -- the shape seeded is the one data/cards/galvanic-blast.json carries.
    Spec.it s "CR 614.15 a source-scoped rewrite takes its own source's damage and no one else's" $ do
      pikerPrinting <- S.printingOf s registry "Goblin Piker"
      let base = Setup.emptyGame S.bothPlayers
          (mine, g1) = S.addCreature pikerPrinting S.alice base
          (theirs, g2) = S.addCreature pikerPrinting S.bob g1
          (victim, g3) = S.addCreature pikerPrinting S.bob g2
          (ts, g4) = Game.freshTimestamp g3
          armed = S.addReplacement (blastShape mine ts) g4
          hit src = S.runPure S.identityAnswer armed (Damage.applyDamage [DamageEvent.MkDamageEvent src (Recipient.ToCreature victim) 2 False False False 0 Nothing DamageKind.Noncombat])
      Spec.assertEqWith s "its own source's 2 becomes 4" (S.damageOf victim (hit mine)) (Just 4)
      Spec.assertEqWith s "another source's 2 stays 2" (S.damageOf victim (hit theirs)) (Just 2)

-- Synthetic Voltaic Surge {1}{R} Instant: "Until end of turn, if a source you
-- control would deal damage to a permanent or player and you control three or
-- more artifacts, it deals double that damage to that permanent or player
-- instead." A floating (CR 614.3) row with a stated duration whose printed "if"
-- is separated from the resolution that installed it, which is what makes CR
-- 614.1's "they aren't locked in ahead of time" observable: the clause is asked
-- as the damage would happen, not when the row was created.
--
-- SYNTHETIC because the shape has no printing. The clause and the rewrite are
-- both taken from cards that do print them -- Anthem of Rakdos ("if a source you
-- control would deal damage to a permanent or player, it deals double that
-- damage . . . instead") and Galvanic Blast's metalcraft count -- and what no
-- card puts together is that pair with a DURATION. Scryfall, 2026-08-21:
-- `(t:instant or t:sorcery) o:"this turn" o:"instead" o:"if you"`,
-- `o:"this turn" o:"would" o:"instead" o:"as long as"`,
-- `o:"this turn" o:"would" o:"instead" (o:"only while" or o:"only as long as" or
-- o:"only if you")` and `o:"the next time" o:"would" o:"instead" o:"if"` return
-- only two families: one-shot spells whose "if" is settled inside their own
-- resolution (Galvanic Blast, Cackling Flames, Twinstrike, Winds of Qal Sisma)
-- and permanents whose "as long as" rides a static ability (Anthem of Rakdos,
-- Aether Revolt, Jared Carthalion). A printing of either family with a stated
-- duration would refute this and replace the synthetic.
--
-- Bonesplitter is the artifact, NOT Darksteel Myr: the case below destroys one to
-- turn the clause off, and rule 702.12b would refuse. Unattached it modifies
-- nothing, so the board reads only its count.
--
-- Firebolt deals 2, so bob's life tells the two readings apart at every step: 20,
-- then 18 (printed) or 16 (doubled), then 14 either way is the coincidence this
-- avoids by asserting the intermediate.
surgeBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Int -> (GameState.GameState, ObjectId.ObjectId, [ObjectId.ObjectId], [ObjectId.ObjectId])
surgeBoard mountain splitter surge firebolt artifacts =
  let base = S.landsFor mountain S.alice 5 (Setup.emptyGame S.bothPlayers)
      addArtifact (ids, g) _ = let (oid, g') = S.addCreature splitter S.alice g in (ids <> [oid], g')
      (splitters, g1) = List.foldl' addArtifact ([], base) [1 .. artifacts]
      (surgeId, g2) = S.addHandCard surge S.alice g1
      addBolt (ids, g) _ = let (oid, g') = S.addHandCard firebolt S.alice g in (ids <> [oid], g')
      (bolts, g3) = List.foldl' addBolt ([], g2) [1 :: Int, 2]
   in ( g3
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          },
        surgeId,
        bolts,
        splitters
      )

voltaicSurgeSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
voltaicSurgeSpec s registry =
  Spec.describe s "Synthetic Voltaic Surge (CR 614.1)" $ do
    let board artifacts = do
          mountain <- S.printingOf s registry "Mountain"
          splitter <- S.printingOf s registry "Bonesplitter"
          surge <- S.printingOf s registry "Synthetic Voltaic Surge"
          firebolt <- S.printingOf s registry "Firebolt"
          pure (splitter, surgeBoard mountain splitter surge firebolt artifacts)
    -- THE PROVING TEST. The clause is true when the row is installed and false
    -- when the second Firebolt would be doubled, and NOTHING removed the row: a
    -- gate read at installation doubles both, and a row that was swept would
    -- leave GameState.replacements empty.
    Spec.it s "CR 614.1 the clause is re-asked, so losing an artifact turns the row off without removing it" $ do
      (_, (gs, surgeId, bolts, splitters)) <- board 3
      case (bolts, splitters) of
        ([first_, second], doomed : _) -> do
          let armed = castAndResolve atBob gs surgeId
              doubled = castAndResolve atBob armed first_
              shrunk = S.runPure S.identityAnswer doubled (Event.destroy Regenerability.Regenerable [doomed])
              after = castAndResolve atBob shrunk second
          Spec.assertEqWith s "the first Firebolt is doubled while alice controls three artifacts" (S.lifeOf S.bob doubled) (Just 16)
          Spec.assertEqWith s "the second lands at its printed 2 once one artifact is gone" (S.lifeOf S.bob after) (Just 14)
          -- By NAME rather than by a controls-count: alice owns every artifact
          -- on this board, so S.countOnBattlefieldByName's owner index answers
          -- the control question too (see Pawl.Support).
          Spec.assertEqWith s "setup: alice is down to two artifacts" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Bonesplitter")) S.alice shrunk) 2
          Spec.assertEqWith s "and the row is still installed -- it stopped applying, it was not removed" (length (GameState.replacements after)) 1
        _ -> Spec.assertFailure s "fixture should hold two Firebolts and three artifacts"
    -- The other direction, and the one a gate read at installation cannot reach
    -- at all: the clause is FALSE as the spell resolves, so the old reading
    -- installed nothing and no later board could turn it on.
    Spec.it s "CR 614.1 a row installed while its clause was false applies once the clause turns true" $ do
      (splitter, (gs, surgeId, bolts, _)) <- board 2
      case bolts of
        [first_, second] -> do
          let armed = castAndResolve atBob gs surgeId
              printed = castAndResolve atBob armed first_
              grown = snd (S.addCreature splitter S.alice printed)
              after = castAndResolve atBob grown second
          Spec.assertEqWith s "the first Firebolt lands at its printed 2" (S.lifeOf S.bob printed) (Just 18)
          Spec.assertEqWith s "the third artifact turns the clause on, so the second is doubled" (S.lifeOf S.bob after) (Just 14)
          Spec.assertEqWith s "setup: the row was installed though its clause was false" (length (GameState.replacements armed)) 1
        _ -> Spec.assertFailure s "fixture should hold two Firebolts"

-- How many battlefield permanents `pid` CONTROLS are printed with this name. NOT
-- S.countOnBattlefieldByName, which counts by OWNER (Game.zoneMembers filters the
-- shared battlefield by Object.owner) -- the whole point of a CR 616.1b rewrite
-- is that the owner and the controller have come apart.
controlledNamed :: CardName.CardName -> PlayerId.PlayerId -> GameState.GameState -> Int
controlledNamed wanted pid gs =
  length (filter (\oid -> fmap Face.name (Game.faceOf oid gs) == Just wanted) (Projection.controls pid gs))

-- alice controls six untapped Islands (Gather Specimens is {3}{U}{U}{U}) and one
-- Goblin Piker for a Clone to copy; bob controls ten, enough for a Gather
-- Specimens of his own plus a Clone at {3}{U}, or for two Clones, with no untap
-- step in between. alice holds one Gather Specimens, bob holds one of each
-- printing in `bobsHand`. It is alice's precombat main phase, and she has
-- priority. Returns the state, alice's spell id, bob's hand ids in order, and
-- the Piker.
specimenBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> [Printing.Printing] -> (GameState.GameState, ObjectId.ObjectId, [ObjectId.ObjectId], ObjectId.ObjectId)
specimenBoard island pikerPrinting gatherSpecimens bobsHand =
  let addLands pid n g = List.foldl' (\acc _ -> snd (S.addCreature island pid acc)) g [1 .. n :: Int]
      base = addLands S.bob 10 (addLands S.alice 6 (Setup.emptyGame S.bothPlayers))
      (piker, g1) = S.addCreature pikerPrinting S.alice base
      (gatherId, g2) = S.addHandCard gatherSpecimens S.alice g1
      addOne (ids, g) p = let (oid, g3) = S.addHandCard p S.bob g in (ids <> [oid], g3)
      (bobs, g4) = List.foldl' addOne ([], g2) bobsHand
   in ( g4
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          },
        gatherId,
        bobs,
        piker
      )

-- CR 800.1: specimenBoard's three-seat twin, and the smallest board on which two
-- control-on-entry replacements can race for one creature. alice and bob each
-- control six untapped Islands (Gather Specimens is {3}{U}{U}{U}) and hold one
-- Gather Specimens; carol controls two and holds one card of `creature`. It is
-- alice's precombat main phase with priority. Returns the state, alice's spell
-- id, bob's, and carol's card.
threeSeatSpecimenBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId)
threeSeatSpecimenBoard island gatherSpecimens creature =
  let addLands pid n g = List.foldl' (\acc _ -> snd (S.addCreature island pid acc)) g [1 .. n :: Int]
      base = addLands S.carol 2 (addLands S.bob 6 (addLands S.alice 6 (Setup.emptyGame S.threePlayers)))
      (aliceGather, g1) = S.addHandCard gatherSpecimens S.alice base
      (bobGather, g2) = S.addHandCard gatherSpecimens S.bob g1
      (carols, g3) = S.addHandCard creature S.carol g2
   in ( g3
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          },
        aliceGather,
        bobGather,
        carols
      )

-- The SOURCE of the floating row `who` installed, so an answer can name a CR
-- 616.1 candidate by whose row it is rather than by list position -- the same
-- reason raceAnswer takes an ObjectId.
rowSourceOf :: PlayerId.PlayerId -> GameState.GameState -> Maybe ObjectId.ObjectId
rowSourceOf who gs =
  Maybe.listToMaybe
    [ ActiveReplacement.source active
    | active <- GameState.replacements gs,
      ActiveReplacement.controller active == who
    ]

-- Name the candidate whose source is `preferred`, but only when CR 616.1's race
-- is put to `who`; every other prompt takes the default. The readout for WHICH
-- player the choice was handed to: an engine that asked anyone else would take
-- the canonical first for both preferences, and the two runs would converge on
-- one board instead of disagreeing.
replaceIfAskedOf :: PlayerId.PlayerId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
replaceIfAskedOf who preferred p = case p of
  Prompt.ChooseReplacement _ asked entries
    | asked == who ->
        maybe 0 Int.toNaturalSaturating (List.findIndex ((== preferred) . ReplacementEntry.source) entries)
  _ -> S.identityAnswer p

-- Copy `wanted` if and only if the copy choice is offered to `who`, and decline
-- otherwise. The readout for WHICH player held Clone's own CR 109.5 "you" when
-- the copy choice was made: the copy lands (a 2/1) only when the engine asked
-- the named player, and the Clone stays a 0/0 when it asked anyone else.
copyIfAskedOf :: PlayerId.PlayerId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
copyIfAskedOf who wanted p = case p of
  Prompt.ChooseCopyTarget _ asked _ legal ->
    if asked == who && List.elem wanted legal then Just wanted else Nothing
  _ -> S.identityAnswer p

-- CR 616.1b's bucket, through the one card in the pool that produces one.
--
-- Gather Specimens ({3}{U}{U}{U} instant, Shards of Alara): "If a creature would
-- enter the battlefield under an opponent's control this turn, it enters under
-- your control instead." It is CR 614.1d's other-objects form ("[Objects] enter
-- [the battlefield] . . ."), which is why EntryR carries a Filter at all, and its
-- whole content is modifying under whose control an object enters -- CR 616.1b's
-- description word for word.
--
-- Clone is the competing entry replacement. CR 616.1c's copy bucket sits one step
-- BELOW 616.1b's, so on the entering Clone's first iteration the control rewrite
-- is the only candidate in the highest non-empty bucket -- and the copy choice
-- that follows goes to whoever controls the object THEN, which the control
-- rewrite has just changed. CR 109.5 is the rule for that second question:
-- Clone's "YOU may have this enter as a copy" is its own controller's choice,
-- made at CR 614.12a's moment (before the permanent enters). CR 616.1's chooser
-- is a different question -- WHICH replacement to apply -- and this board never
-- raises it, since each bucket holds one candidate.
gatherSpecimensSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
gatherSpecimensSpec s registry =
  Spec.describe s "Gather Specimens (CR 616.1b)" $ do
    Spec.it s "CR 616.1b an opponent's entering creature enters under YOUR control instead" $ do
      island <- S.printingOf s registry "Island"
      pikerPrinting <- S.printingOf s registry "Goblin Piker"
      gatherSpecimens <- S.printingOf s registry "Gather Specimens"
      clonePrinting <- S.printingOf s registry "Clone"
      let (gs, gatherId, bobs, piker) = specimenBoard island pikerPrinting gatherSpecimens [clonePrinting]
      case bobs of
        cloneId : _ ->
          let armed = S.runPure S.identityAnswer gs (S.cast S.alice gatherId >> Stack.resolveTop)
              after = S.runPure (copyIfAskedOf S.alice piker) armed (S.cast S.bob cloneId >> Stack.resolveTop)
              -- The DISCRIMINATING TWIN: the same cast on the same board with no
              -- Gather Specimens resolved first.
              alone = S.runPure (copyIfAskedOf S.alice piker) gs (S.cast S.bob cloneId >> Stack.resolveTop)
           in case (newestNamed (CardName.MkCardName $ Text.pack "Clone") after, newestNamed (CardName.MkCardName $ Text.pack "Clone") alone) of
                (Just taken, Just untaken) -> do
                  Spec.assertEqWith s "bob's Clone entered under alice's control" (Projection.controllerOf taken after) (Just S.alice)
                  Spec.assertEqWith s "without the Gather Specimens it is bob's" (Projection.controllerOf untaken alone) (Just S.bob)
                _ -> Spec.assertFailure s "a Clone did not reach the battlefield"
        _ -> Spec.assertFailure s "fixture did not deal bob a card"
    -- CR 616.1b BEFORE CR 616.1c, and the two orders disagree about WHO IS ASKED
    -- -- which is what makes this an assertion rather than a coincidence:
    --
    --   * CR 616.1b's order -- the control rewrite first, so the object is
    --     alice's by the time Clone's own choice is offered, and ALICE picks
    --     the copy target.
    --   * the other order -- the copy first, while the object is still bob's,
    --     so BOB picks.
    --
    -- CR 109.5 makes Clone's "you" the entering object's CONTROLLER, and CR
    -- 614.12a fixes when that is read (before the permanent enters). For an
    -- opponent's entering creature that controller is not the Gather Specimens
    -- controller until CR 616.1b's rewrite has been applied -- which is the whole
    -- point: the bucket ordering decides who the second question goes to.
    Spec.it s "CR 616.1b before CR 616.1c: the NEW controller chooses the copy" $ do
      island <- S.printingOf s registry "Island"
      pikerPrinting <- S.printingOf s registry "Goblin Piker"
      gatherSpecimens <- S.printingOf s registry "Gather Specimens"
      clonePrinting <- S.printingOf s registry "Clone"
      let (gs, gatherId, bobs, piker) = specimenBoard island pikerPrinting gatherSpecimens [clonePrinting]
      case bobs of
        cloneId : _ ->
          let armed = S.runPure S.identityAnswer gs (S.cast S.alice gatherId >> Stack.resolveTop)
              askedAlice = S.runPure (copyIfAskedOf S.alice piker) armed (S.cast S.bob cloneId >> Stack.resolveTop)
              askedBob = S.runPure (copyIfAskedOf S.bob piker) armed (S.cast S.bob cloneId >> Stack.resolveTop)
              asked = answersFor (copyIfAskedOf S.alice piker) armed (S.cast S.bob cloneId >> Stack.resolveTop)
           in case (newestNamed (CardName.MkCardName $ Text.pack "Clone") askedAlice, newestNamed (CardName.MkCardName $ Text.pack "Clone") askedBob) of
                (Just toAlice, Just toBob) -> do
                  Spec.assertEqWith s "alice was offered the copy, and took it" (Projection.powerOf toAlice askedAlice) (Just 2)
                  Spec.assertEqWith s "bob was never offered it, so the Clone is still a 0/0" (Projection.powerOf toBob askedBob) (Just 0)
                  -- CR 616.1b's "one of them must be chosen" with one member:
                  -- the control rewrite is alone in the highest non-empty
                  -- bucket, so there is nothing to choose and the engine must
                  -- not ask. Were both candidates in CR 616.1e's bucket, this
                  -- is the race that would be prompted.
                  Spec.assertBool s (not (wasAskedToReplace asked)) "no ChooseReplacement was raised"
                _ -> Spec.assertFailure s "a Clone did not reach the battlefield"
        _ -> Spec.assertFailure s "fixture did not deal bob a card"
    -- CR 614.1d's filter is the card's own "a creature", and this is the leg that
    -- holds it to that word. Its other half -- "under an OPPONENT's control" --
    -- is held by the duelling-Gather-Specimens leg below, which needs a second
    -- copy of the card to see it at all: with one on the board the relation is
    -- invisible, because rewriting alice's own entering creature to alice's
    -- control is a no-op and adding seats adds no discrimination.
    Spec.it s "CR 614.1d an opponent's entering NONcreature is not a specimen" $ do
      island <- S.printingOf s registry "Island"
      pikerPrinting <- S.printingOf s registry "Goblin Piker"
      gatherSpecimens <- S.printingOf s registry "Gather Specimens"
      coating <- S.printingOf s registry "Liquimetal Coating"
      let (gs, gatherId, bobs, _) = specimenBoard island pikerPrinting gatherSpecimens [coating]
      case bobs of
        coatingId : _ ->
          let armed = S.runPure S.identityAnswer gs (S.cast S.alice gatherId >> Stack.resolveTop)
              after = S.runPure S.identityAnswer armed (S.cast S.bob coatingId >> Stack.resolveTop)
           in case newestNamed (CardName.MkCardName $ Text.pack "Liquimetal Coating") after of
                Nothing -> Spec.assertFailure s "the Coating did not reach the battlefield"
                Just coatingObj -> Spec.assertEqWith s "an artifact is not a creature" (Projection.controllerOf coatingObj after) (Just S.bob)
        _ -> Spec.assertFailure s "fixture did not deal bob a card"
    -- CR 614.3's "until they're used up": Gather Specimens states no count, so
    -- its row is Uses.Unlimited and every creature an opponent plays for the rest
    -- of the turn comes over. A Uses.Once row would take the first and leave the
    -- second, which is the difference this pins.
    Spec.it s "CR 614.3 the effect lasts the turn: bob's SECOND creature comes over too" $ do
      island <- S.printingOf s registry "Island"
      pikerPrinting <- S.printingOf s registry "Goblin Piker"
      gatherSpecimens <- S.printingOf s registry "Gather Specimens"
      clonePrinting <- S.printingOf s registry "Clone"
      let (gs, gatherId, bobs, piker) = specimenBoard island pikerPrinting gatherSpecimens [clonePrinting, clonePrinting]
      case bobs of
        firstClone : secondClone : _ ->
          let armed = S.runPure S.identityAnswer gs (S.cast S.alice gatherId >> Stack.resolveTop)
              one = S.runPure (copyIfAskedOf S.alice piker) armed (S.cast S.bob firstClone >> Stack.resolveTop)
              two = S.runPure (copyIfAskedOf S.alice piker) one (S.cast S.bob secondClone >> Stack.resolveTop)
           in Spec.assertEqWith s "both of bob's Clones are alice's" (controlledNamed (CardName.MkCardName $ Text.pack "Clone") S.alice two) 2
        _ -> Spec.assertFailure s "fixture did not deal bob two cards"
    -- DUELLING GATHER SPECIMENS, and the only board where the filter's
    -- "under an OPPONENT's control" is observable at all.
    --
    -- alice resolves one, bob resolves one, and then BOB's Clone enters. The
    -- entering side is what makes this discriminate; alice's own creature does
    -- not, for the reason the two orders below converge on it.
    --
    -- With the relation, CR 616.1f drives a forced two-step -- "this process is
    -- repeated (taking into account only replacement or prevention effects that
    -- would now be applicable) until there are no more left to apply":
    --
    --   1. bob's creature would enter under bob's control. alice's row applies
    --      (bob is her opponent); bob's does not (bob is not his own opponent).
    --      One candidate in CR 616.1b's bucket, so nothing is chosen and nothing
    --      is asked. It enters under alice's control.
    --   2. Re-collected, bob's row is NOW applicable -- alice is his opponent --
    --      and alice's is spent by CR 614.5's "only one opportunity". One
    --      candidate again. It enters under BOB's control.
    --   3. Nothing is left in that bucket, so CR 616.1c's copy choice follows,
    --      offered to bob.
    --
    -- Without the relation both rows are applicable at step 1, and CR 616.1b's
    -- "one of them must be chosen" would have something to choose between: they
    -- are equal in `effect` (one card, one filter) but differ in the baked CR
    -- 109.5 controller, which Replacement.readsApplier makes a distinguishing
    -- field for this rewrite. So bob would be ASKED, take the canonical first --
    -- his own, the newest floating row -- as a no-op, and alice's would apply at
    -- step 2, leaving the creature HERS. The assertion is therefore
    -- bob-not-alice, and the prompt assertion below discriminates too: with the
    -- relation each step has one candidate and nothing is asked.
    Spec.it s "CR 614.1d/616.1f duelling Gather Specimens: alice takes it, then bob takes it back" $ do
      island <- S.printingOf s registry "Island"
      pikerPrinting <- S.printingOf s registry "Goblin Piker"
      gatherSpecimens <- S.printingOf s registry "Gather Specimens"
      clonePrinting <- S.printingOf s registry "Clone"
      let (gs, aliceGather, bobs, piker) = specimenBoard island pikerPrinting gatherSpecimens [gatherSpecimens, clonePrinting]
      case bobs of
        bobGather : cloneId : _ ->
          let armed = S.runPure S.identityAnswer gs (S.cast S.alice aliceGather >> Stack.resolveTop)
              duelling = S.runPure S.identityAnswer armed (S.cast S.bob bobGather >> Stack.resolveTop)
              after = S.runPure (copyIfAskedOf S.bob piker) duelling (S.cast S.bob cloneId >> Stack.resolveTop)
              asked = answersFor (copyIfAskedOf S.bob piker) duelling (S.cast S.bob cloneId >> Stack.resolveTop)
           in case newestNamed (CardName.MkCardName $ Text.pack "Clone") after of
                Nothing -> Spec.assertFailure s "the Clone did not reach the battlefield"
                Just clone -> do
                  -- Both rows really are on the board: without bob's, this is
                  -- the first case in this group and the answer is alice.
                  Spec.assertEqWith s "two floating replacements are live" (length (GameState.replacements duelling)) 2
                  Spec.assertEqWith s "alice took it, and bob took it back" (Projection.controllerOf clone after) (Just S.bob)
                  -- And the copy choice landed on bob, the controller CR 616.1b
                  -- left the object with.
                  Spec.assertEqWith s "bob was offered the copy, and took it" (Projection.powerOf clone after) (Just 2)
                  -- Each step of CR 616.1f had ONE applicable control rewrite,
                  -- so there was never anything for CR 616.1b's "one of them
                  -- must be chosen" to choose between.
                  Spec.assertBool s (not (wasAskedToReplace asked)) "no ChooseReplacement was raised"
        _ -> Spec.assertFailure s "fixture did not deal bob two cards"
    -- THREE SEATS, where CR 616.1b's "one of them must be chosen" finally has
    -- something to choose between -- the case the duelling leg above cannot
    -- reach. alice and bob each resolve a Gather Specimens and CAROL casts a
    -- creature: it would enter under carol's control, carol is an opponent of
    -- both, so BOTH rows are applicable in the SAME iteration of CR 616.1f. Two
    -- seats cannot produce that (a permanent has one controller, so at most one
    -- such row can see it as an opponent's), which is why the leg above sees a
    -- forced order instead.
    --
    -- The two rows are equal in `effect` -- one card, one filter -- and differ
    -- only in the CR 109.5 controller each baked, which rides the CANDIDATE.
    -- That is precisely what Replacement.readsApplier answers True for, so the
    -- pair is not indistinguishable and CR 616.1 owes the question to the
    -- affected object's controller: carol.
    --
    -- Her answer decides the board, and INVERTS it. The row she names applies
    -- now; CR 616.1f then re-collects, where the other row is newly applicable
    -- (from its controller's side the creature is an opponent's again) and the
    -- named one is spent by CR 614.5. So the creature settles with the player
    -- she did NOT name, and the two runs disagree -- which is the whole point:
    -- before this was fixed both rows were elided as value-equal and the
    -- floating store's newest-first order decided the board with nobody asked.
    Spec.it s "CR 616.1b three seats: carol is asked WHICH Gather Specimens takes her creature" $ do
      island <- S.printingOf s registry "Island"
      gatherSpecimens <- S.printingOf s registry "Gather Specimens"
      narcomoeba <- S.printingOf s registry "Narcomoeba"
      let (gs, aliceGather, bobGather, moeba) = threeSeatSpecimenBoard island gatherSpecimens narcomoeba
          resolveFor pid oid g = S.runPure S.identityAnswer g (S.cast pid oid >> Stack.resolveTop)
          armed = resolveFor S.bob bobGather (resolveFor S.alice aliceGather gs)
          entry = S.cast S.carol moeba >> Stack.resolveTop
          asked = answersFor S.identityAnswer armed entry
          moebaName = CardName.MkCardName $ Text.pack "Narcomoeba"
      Spec.assertEqWith s "two floating replacements are live" (length (GameState.replacements armed)) 2
      Spec.assertBool s (wasAskedToReplace asked) "a ChooseReplacement was raised"
      case (rowSourceOf S.alice armed, rowSourceOf S.bob armed) of
        (Just aliceRow, Just bobRow) ->
          let namedAlice = S.runPure (replaceIfAskedOf S.carol aliceRow) armed entry
              namedBob = S.runPure (replaceIfAskedOf S.carol bobRow) armed entry
           in case (newestNamed moebaName namedAlice, newestNamed moebaName namedBob) of
                (Just afterAlice, Just afterBob) -> do
                  Spec.assertEqWith s "carol named alice's row, so bob's applies second and keeps it" (Projection.controllerOf afterAlice namedAlice) (Just S.bob)
                  Spec.assertEqWith s "carol named bob's row, so alice's applies second and keeps it" (Projection.controllerOf afterBob namedBob) (Just S.alice)
                _ -> Spec.assertFailure s "the creature did not reach the battlefield"
        _ -> Spec.assertFailure s "both Gather Specimens rows should be floating"
    -- CR 800.4a's SECOND clause, at three seats: alice resolves a Gather
    -- Specimens and then concedes. A floating control-on-entry row is an effect
    -- whose whole content is giving its controller control of objects, so it is
    -- one of the "effects which give that player control of any objects" that
    -- end when she leaves -- and carol's creature, entering afterwards, stays
    -- carol's.
    --
    -- Three seats are required twice over: Departure.continuesAfterDeparture is
    -- `> 2`, so at two seats CR 104.2a ends the game and none of CR 800.4a runs,
    -- and a creature entering under bob's control is not "an opponent's" from
    -- bob's own side.
    Spec.it s "CR 800.4a a control-on-entry row ends when its controller leaves the game" $ do
      island <- S.printingOf s registry "Island"
      gatherSpecimens <- S.printingOf s registry "Gather Specimens"
      narcomoeba <- S.printingOf s registry "Narcomoeba"
      let (gs, aliceGather, _, moeba) = threeSeatSpecimenBoard island gatherSpecimens narcomoeba
          armed = S.runPure S.identityAnswer gs (S.cast S.alice aliceGather >> Stack.resolveTop)
          -- The one difference between the two runs.
          gone = S.runPure S.identityAnswer armed (Departure.leaveGame Departure.Type.Conceded S.alice)
          entry = S.cast S.carol moeba >> Stack.resolveTop
          after = S.runPure S.identityAnswer gone entry
          stayed = S.runPure S.identityAnswer armed entry
          moebaName = CardName.MkCardName $ Text.pack "Narcomoeba"
      -- Both read boards taken BEFORE the departure filter runs, so neither can
      -- absorb a mutation of it.
      Spec.assertEqWith s "alice's row was floating before she left" (length (GameState.replacements armed)) 1
      Spec.assertBool s (List.notElem S.alice (Game.stillPlaying gone)) "alice really has left"
      case (newestNamed moebaName after, newestNamed moebaName stayed) of
        (Just departed, Just present) -> do
          Spec.assertEqWith s "carol's creature stays carol's (CR 800.4a)" (Projection.controllerOf departed after) (Just S.carol)
          -- The discriminating twin, on the identical board with alice seated:
          -- the fix ended the row, it did not disable the rewrite.
          Spec.assertEqWith s "with alice seated the same creature is hers (CR 616.1b)" (Projection.controllerOf present stayed) (Just S.alice)
          -- CR 110.2a's entry controller, written by the effect that put the
          -- permanent there and left alone by an entry loop with no candidate:
          -- carol, and so not the departed player CR 800.4c draws its line at.
          Spec.assertEqWith s "the recorded entry controller is carol, not alice (CR 110.2a)" (fmap Object.enteredUnder (Game.lookupObject departed after)) (Just (Just S.carol))
          Spec.assertEqWith s "the row itself is gone" (length (GameState.replacements gone)) 0
        _ -> Spec.assertFailure s "the creature did not reach the battlefield"
    -- WHY CR 800.4a ends the row rather than Event's UnderSourceControl arm
    -- refusing to name a departed player. A guard inside that arm would leave
    -- alice's row a CR 616.1 candidate, and Replacement.readsApplier answers True
    -- for that rewrite, so the pair stays distinguishable and carol is asked
    -- which row takes her creature -- a choice one of whose options does nothing.
    -- With the row ended there is one candidate, and CR 616.1b's one-candidate
    -- elision means no prompt at all. This is the board the two fixes disagree on.
    Spec.it s "CR 616.1b a departed player's row is not a candidate, so carol is not asked" $ do
      island <- S.printingOf s registry "Island"
      gatherSpecimens <- S.printingOf s registry "Gather Specimens"
      narcomoeba <- S.printingOf s registry "Narcomoeba"
      let (gs, aliceGather, bobGather, moeba) = threeSeatSpecimenBoard island gatherSpecimens narcomoeba
          resolveFor pid oid g = S.runPure S.identityAnswer g (S.cast pid oid >> Stack.resolveTop)
          armed = resolveFor S.bob bobGather (resolveFor S.alice aliceGather gs)
          gone = S.runPure S.identityAnswer armed (Departure.leaveGame Departure.Type.Conceded S.alice)
          entry = S.cast S.carol moeba >> Stack.resolveTop
          asked = answersFor S.identityAnswer gone entry
          after = S.runPure S.identityAnswer gone entry
          moebaName = CardName.MkCardName $ Text.pack "Narcomoeba"
      Spec.assertEqWith s "both rows were floating before alice left" (length (GameState.replacements armed)) 2
      case newestNamed moebaName after of
        Just moebaObj -> do
          Spec.assertEqWith s "bob's row is the only one left, so the creature is bob's" (Projection.controllerOf moebaObj after) (Just S.bob)
          Spec.assertBool s (not (wasAskedToReplace asked)) "no ChooseReplacement was raised"
          Spec.assertEqWith s "only bob's row survived alice's departure" (length (GameState.replacements gone)) 1
        Nothing -> Spec.assertFailure s "the creature did not reach the battlefield"

-- Kismet ({3}{W} Enchantment, "Artifacts, creatures, and lands your opponents
-- control enter tapped") -- CR 614.1d's other-objects form, bucketing to CR
-- 616.1e. bob controls it, so alice's entering permanent is the opponent's one
-- it rewrites.
--
-- alice controls a Goblin Piker to copy and six of `land` to pay with, and holds
-- one card of `spell`. It is her precombat main phase with priority. Returns the
-- state, the Piker and the held card.
kismetBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId)
kismetBoard land pikerPrinting kismet spell =
  let addLands pid n g = List.foldl' (\acc _ -> snd (S.addCreature land pid acc)) g [1 .. n :: Int]
      base = addLands S.alice 6 (Setup.emptyGame S.bothPlayers)
      (pikerId, g1) = S.addCreature pikerPrinting S.alice base
      (_, g2) = S.addCreature kismet S.bob g1
      (spellId, g3) = S.addHandCard spell S.alice g2
   in ( g3
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          },
        pikerId,
        spellId
      )

-- CR 616.1c's bucket, through the first pair in the pool that races it against
-- CR 616.1e: an entering Clone (AsCopy, CR 616.1c) under an opponent's Kismet
-- (Tapped, CR 616.1e). Both rows are applicable to the same entering permanent on
-- the same iteration of CR 616.1f's loop, and the copy is alone in the highest
-- non-empty bucket -- so there is nothing to choose and the engine must not ask.
--
-- The two orders CONVERGE on one board: CR 616.1f re-collects, so the Clone ends
-- up both a copy and tapped whichever is applied first (Kismet's row is not on
-- the copied Piker, so unlike CR 616.1f's Essence of the Wild example the copy
-- does not take the tap clause away). The absence of the prompt is therefore the
-- only observable the split has, which is why it is what the first case asserts;
-- the second case is the discriminating twin that shows the recorder can see one.
kismetSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
kismetSpec s registry =
  Spec.describe s "Kismet (CR 616.1c/616.1d)" $ do
    -- THE PROVING CASE. Collapse CopyOnEntry into Other and the same board raises
    -- a CR 616.1e race the rules do not have.
    --
    -- The two board assertions beside it are the non-vacuity check, not the
    -- proof: they show Kismet really was a second candidate, so "no prompt" is
    -- not "no second effect".
    Spec.it s "CR 616.1c the copy bucket outranks Kismet's, so no order is asked" $ do
      island <- S.printingOf s registry "Island"
      pikerPrinting <- S.printingOf s registry "Goblin Piker"
      kismet <- S.printingOf s registry "Kismet"
      clonePrinting <- S.printingOf s registry "Clone"
      let (gs, pikerId, cloneId) = kismetBoard island pikerPrinting kismet clonePrinting
          cast = S.cast S.alice cloneId >> Stack.resolveTop
          after = S.runPure (copyIfAskedOf S.alice pikerId) gs cast
          asked = answersFor (copyIfAskedOf S.alice pikerId) gs cast
      case newestNamed (CardName.MkCardName $ Text.pack "Clone") after of
        Nothing -> Spec.assertFailure s "the Clone did not reach the battlefield"
        Just cloneOid -> do
          Spec.assertEqWith s "CR 616.1c the Clone copied the Piker" (Projection.powerOf cloneOid after) (Just 2)
          Spec.assertBool s (Game.isTapped cloneOid after) "CR 614.1d and Kismet tapped it too"
          Spec.assertBool s (not (wasAskedToReplace asked)) "no ChooseReplacement was raised"
    -- The DISCRIMINATING TWIN: the same fixture and the same recorder, a spell
    -- whose own entry rewrites are both CR 616.1e's. Coldsteel Heart is an
    -- artifact, so Kismet's row joins its two in one bucket and the race really
    -- is raised. Without this, "no prompt" above would pass under a recorder that
    -- never sees a ChooseReplacement on any board.
    Spec.it s "CR 616.1e rewrites sharing one bucket ARE raced" $ do
      island <- S.printingOf s registry "Island"
      pikerPrinting <- S.printingOf s registry "Goblin Piker"
      kismet <- S.printingOf s registry "Kismet"
      coldsteel <- S.printingOf s registry "Coldsteel Heart"
      let (gs, _, heartId) = kismetBoard island pikerPrinting kismet coldsteel
          asked = answersFor S.identityAnswer gs (S.cast S.alice heartId >> Stack.resolveTop)
      Spec.assertBool s (wasAskedToReplace asked) "a ChooseReplacement was raised"
    -- The sibling bucket, one step DOWN: CR 616.1d's back-face-up rewrite against
    -- the same CR 616.1e row. CR 702.145b's daybound mints EntersTransformed, and
    -- at night it and Kismet's row are both applicable to the entering werewolf in
    -- one iteration -- so CR 616.1d's bucket is alone at the top and, again, there
    -- is nothing to ask. Same convergence as the copy case: the werewolf ends up
    -- transformed AND tapped either way, so the prompt is the observable.
    --
    -- Forests, not Islands: Infestation Expert is {4}{G}, and its faces are 3/4
    -- and 4/5. The power reading 4 says the werewolf is back face up; it does NOT
    -- say the entry rewrite is what put it there, since CR 702.145c would
    -- transform it a moment later anyway. Like the tap, it is a non-vacuity
    -- guard. The prompt is the assertion.
    Spec.it s "CR 616.1d the back-face bucket outranks Kismet's, so no order is asked" $ do
      forest <- S.printingOf s registry "Forest"
      pikerPrinting <- S.printingOf s registry "Goblin Piker"
      kismet <- S.printingOf s registry "Kismet"
      werewolf <- S.printingOf s registry "Infestation Expert"
      let (day, _, wolfId) = kismetBoard forest pikerPrinting kismet werewolf
          gs = day {GameState.daytime = Just Daytime.Night}
          cast = S.cast S.alice wolfId >> Stack.resolveTop
          after = S.runPure S.identityAnswer gs cast
          asked = answersFor S.identityAnswer gs cast
      -- Named by the BACK face, which newestNamed reads off the current face: an
      -- Infestation Expert that stayed front face up is not found at all.
      case newestNamed (CardName.MkCardName $ Text.pack "Infested Werewolf") after of
        Nothing -> Spec.assertFailure s "the werewolf did not reach the battlefield transformed"
        Just wolfOid -> do
          Spec.assertEqWith s "CR 702.145b it entered on its back face, a 4/5" (Projection.powerOf wolfOid after) (Just 4)
          Spec.assertBool s (Game.isTapped wolfOid after) "CR 614.1d and Kismet tapped it too"
          Spec.assertBool s (not (wasAskedToReplace asked)) "no ChooseReplacement was raised"

-- Galvanic Blast's metalcraft clause as a floating row: the damage THIS source is
-- dealing, whatever its kind, becomes 4 (CR 614.15 / 614.1a). Uses.Unlimited
-- rather than the card's Once, for the reason its one caller gives.
blastShape :: ObjectId.ObjectId -> Timestamp.Timestamp -> ActiveReplacement.ActiveReplacement
blastShape src ts =
  ActiveReplacement.MkActiveReplacement
    { ActiveReplacement.effect =
        ReplacementEffect.DamageR (DamageR.MkDamageR (DamagePattern.MkDamagePattern Nothing Filter.Type.IsSource Nothing Nothing Nothing) (DamageRewrite.SetAmount 4) Seq.empty),
      ActiveReplacement.source = src,
      ActiveReplacement.controller = S.alice,
      ActiveReplacement.timestamp = ts,
      ActiveReplacement.expiry = Expiry.Never,
      ActiveReplacement.uses = Uses.Unlimited,
      ActiveReplacement.origin = ReplacementOrigin.SelfReplacement,
      ActiveReplacement.condition = Nothing,
      ActiveReplacement.rider = Nothing,
      ActiveReplacement.slots = Map.empty
    }

-- alice controls `n` Mountains and `extra` further permanents, and holds a
-- Shimatsu the Bloodcloaked. Returns the state, the Shimatsu's hand id, and the
-- ids of the extra permanents in the order they were added.
--
-- Goblin Piker for the extras: a vanilla creature, so nothing it carries can
-- reach the entry loop and the only thing that changes when one is sacrificed is
-- the count.
shimatsuBoard :: Printing.Printing -> Int -> Printing.Printing -> Int -> Printing.Printing -> (GameState.GameState, ObjectId.ObjectId, [ObjectId.ObjectId])
shimatsuBoard mountain n pikerPrinting extra shimatsu =
  let base = S.landsInPlay mountain n
      addOne (ids, g) _ = let (oid, g1) = S.addCreature pikerPrinting S.alice g in (ids <> [oid], g1)
      (pikers, withPikers) = List.foldl' addOne ([], base) (replicate extra ())
      (gs, held) = S.handOne shimatsu withPikers
   in ( gs
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          },
        held,
        pikers
      )

-- Sacrifice exactly `wanted` when the as-enters choice is offered, and nothing
-- else about the game.
sacrificesExactly :: [ObjectId.ObjectId] -> Prompt.Prompt r -> r
sacrificesExactly wanted p = case p of
  Prompt.ChooseAnyNumberToSacrifice _ _ _ candidates -> Set.fromList (filter (`elem` candidates) wanted)
  _ -> S.identityAnswer p

-- Sacrifice EVERYTHING the engine offers. What makes the CR 614.12a exclusion
-- testable: if the entering Shimatsu were among its own candidates, a greedy
-- answer would sacrifice it.
sacrificesAll :: Prompt.Prompt r -> r
sacrificesAll p = case p of
  Prompt.ChooseAnyNumberToSacrifice _ _ _ candidates -> Set.fromList candidates
  _ -> S.identityAnswer p

shimatsuSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
shimatsuSpec s registry =
  Spec.describe s "Shimatsu the Bloodcloaked (CR 614.1c)" $ do
    Spec.it s "CR 614.1c sacrificing two permanents enters a 2/2 with two +1/+1 counters" $ do
      mountain <- S.printingOf s registry "Mountain"
      pikerPrinting <- S.printingOf s registry "Goblin Piker"
      shimatsu <- S.printingOf s registry "Shimatsu the Bloodcloaked"
      let (gs, held, pikers) = shimatsuBoard mountain 4 pikerPrinting 3 shimatsu
      case pikers of
        first : second : _ ->
          let after = S.runPure (sacrificesExactly [first, second]) gs (S.cast S.alice held >> Stack.resolveTop)
           in case newestNamed (CardName.MkCardName $ Text.pack "Shimatsu the Bloodcloaked") after of
                Nothing -> Spec.assertFailure s "Shimatsu did not reach the battlefield"
                Just shimatsuId -> do
                  Spec.assertEqWith s "two +1/+1 counters" (countersOn CounterKind.PlusOnePlusOne shimatsuId after) 2
                  -- Printed 0/0, so the counters are the whole of its body.
                  Spec.assertEqWith s "power" (Projection.powerOf shimatsuId after) (Just 2)
                  Spec.assertEqWith s "toughness" (Projection.toughnessOf shimatsuId after) (Just 2)
                  -- CR 701.21a: to the OWNER's graveyard, and only the two named.
                  Spec.assertEqWith s "the two chosen Pikers left the battlefield" (filter (\oid -> Set.member oid (GameState.battlefield after)) [first, second]) []
        _ -> Spec.assertFailure s "fixture did not build two Pikers"
    Spec.it s "CR 704.5f sacrificing nothing enters a 0/0 that dies" $ do
      mountain <- S.printingOf s registry "Mountain"
      pikerPrinting <- S.printingOf s registry "Goblin Piker"
      shimatsu <- S.printingOf s registry "Shimatsu the Bloodcloaked"
      let (gs, held, _) = shimatsuBoard mountain 4 pikerPrinting 2 shimatsu
          -- S.identityAnswer answers the empty set: sacrifice nothing.
          after = S.runPure S.identityAnswer gs (S.cast S.alice held >> Stack.resolveTop >> Engine.settleForPriority)
      Spec.assertEqWith s "the 0/0 Shimatsu is gone" (newestNamed (CardName.MkCardName $ Text.pack "Shimatsu the Bloodcloaked") after) Nothing
    Spec.it s "CR 614.12a/701.21a Shimatsu is not among the permanents it may sacrifice" $ do
      mountain <- S.printingOf s registry "Mountain"
      pikerPrinting <- S.printingOf s registry "Goblin Piker"
      shimatsu <- S.printingOf s registry "Shimatsu the Bloodcloaked"
      -- Four Mountains and two Pikers, all of them alice's permanents, so a
      -- greedy answer sacrifices six -- and would sacrifice seven if the
      -- entering Shimatsu were offered to itself.
      let (gs, held, _) = shimatsuBoard mountain 4 pikerPrinting 2 shimatsu
          after = S.runPure sacrificesAll gs (S.cast S.alice held >> Stack.resolveTop)
      case newestNamed (CardName.MkCardName $ Text.pack "Shimatsu the Bloodcloaked") after of
        Nothing -> Spec.assertFailure s "Shimatsu sacrificed itself"
        Just shimatsuId -> do
          Spec.assertEqWith s "six counters, one per OTHER permanent" (countersOn CounterKind.PlusOnePlusOne shimatsuId after) 6
          Spec.assertEqWith s "nothing else of alice's is left" (Set.toList (GameState.battlefield after)) [shimatsuId]

-- CR 614.12b's board: alice controls nine untapped Islands, two untapped
-- Forests and an untapped Bayou, a TAPPED Forest, a Mountain and a Wood
-- Elemental, and holds a Rite of Replication. Returns the state, the Rite's
-- hand id, the Wood Elemental, the three sacrificeable lands in the order they
-- were added, the tapped Forest and the Mountain.
--
-- Wood Elemental {3}{G} Creature -- Elemental */*: "As this creature enters,
-- sacrifice any number of untapped Forests. Wood Elemental's power and
-- toughness are each equal to the number of Forests sacrificed as it entered."
-- Kicked, the Rite mints FIVE token copies of it at one moment, and each token
-- carries the copied face's own as-enters sacrifice -- so five entry costs
-- compete for one supply of three Forests.
--
-- The Islands are added FIRST, so they are the lowest-id mana sources and
-- Replay.defaultAnswer's head-of-list ChooseManaSource answer pays the whole
-- kicked cost ({2}{U}{U} plus {5}) with them. A Forest tapped to pay for the
-- spell would leave the assertions measuring the payment rather than the rule.
--
-- Three sacrificeable lands against five entering permanents is the scarcity
-- the rule is about, and 3 and 5 are chosen so no two readings of it agree: a
-- shared supply leaves ONE 3/3 token, a per-permanent supply leaves five, and
-- one-each leaves three 1/1s.
--
-- Bayou (Land -- Swamp Forest) among the two Forests, and a tapped Forest and a
-- Mountain beside them, so WHICH permanents went is observable rather than
-- inferred from a count: the criterion is "untapped Forests", which the Bayou
-- satisfies, and which the tapped Forest and the Mountain each fail on one
-- half.
--
-- The Wood Elemental on the battlefield carries a +1/+1 counter, the trick
-- Pawl.CopySpec's Clone board uses: its P/T is a sacrifice count it never made,
-- so a 0/0 would die to CR 704.5f before the Rite could target it. Counters are
-- not copiable (CR 707.2), so the tokens are minted off the printed */*.
woodElementalBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId, [ObjectId.ObjectId], ObjectId.ObjectId, ObjectId.ObjectId)
woodElementalBoard island forest bayou mountain woodElemental rite =
  let base = S.landsInPlay island 9
      (firstForest, board1) = S.addCreature forest S.alice base
      (secondForest, board2) = S.addCreature forest S.alice board1
      (bayouId, board3) = S.addCreature bayou S.alice board2
      (tappedForest, board4) = S.addCreature forest S.alice board3
      (mountainId, board5) = S.addCreature mountain S.alice board4
      (elementalId, board6) = S.addCreature woodElemental S.alice board5
      (held, board7) = S.addHandCard rite S.alice (S.addCounter CounterKind.PlusOnePlusOne 1 elementalId (S.tapObject tappedForest board6))
   in ( board7
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          },
        held,
        elementalId,
        [firstForest, secondForest, bayouId],
        tappedForest,
        mountainId
      )

-- Kick the Rite of Replication and aim it at `victim` -- PINNED to that id
-- rather than searched for, so a mutation cannot be repaired by an answerer
-- that finds another legal target -- answering every as-enters sacrifice with
-- `sacrificeAnswer`.
riteOn :: ObjectId.ObjectId -> (forall r. Prompt.Prompt r -> r) -> Prompt.Prompt a -> a
riteOn victim sacrificeAnswer p = case p of
  Prompt.ChooseKicker {} -> KickerDecision.Kicks
  Prompt.ChooseTargets _ _ _ sets -> Map.map (const (Set.singleton (Recipient.ToCreature victim))) sets
  _ -> sacrificeAnswer p

-- Sacrifice the FIRST permanent of `order` that is still being offered, and
-- nothing else. Pinned by id rather than by position in the offer: an engine
-- that let a later entry choice see what an earlier one already spent would
-- hand every token the same first id, where this answerer walks down the list
-- exactly as the supply is consumed.
sacrificesOneOf :: [ObjectId.ObjectId] -> Prompt.Prompt r -> r
sacrificesOneOf order p = case p of
  Prompt.ChooseAnyNumberToSacrifice _ _ _ candidates ->
    Set.fromList (take 1 (filter (`elem` candidates) order))
  _ -> S.identityAnswer p

-- riteOn with sacrificesOneOf, as one rank-1 function: a `let` binding of the
-- composition monomorphizes, and both S.runPure and answersFor want it
-- polymorphic.
riteSplitting :: ObjectId.ObjectId -> [ObjectId.ObjectId] -> Prompt.Prompt r -> r
riteSplitting victim order = riteOn victim (sacrificesOneOf order)

-- How many times a player was asked to make an as-enters sacrifice. One per
-- entering permanent that had something to choose from -- the arm elides the
-- prompt when nothing is offered, so this counts the entry costs that found a
-- budget left rather than the permanents that entered.
sacrificeAsks :: [Response.Response] -> Int
sacrificeAsks responses =
  let isSacrifice r = case r of
        Response.ChoseSacrifices _ -> True
        _ -> False
   in length (filter isSacrifice responses)

-- The names of the cards in a player's graveyard, sorted.
graveyardNames :: PlayerId.PlayerId -> GameState.GameState -> [CardName.CardName]
graveyardNames pid gs = List.sort (Maybe.mapMaybe (\oid -> fmap Face.name (Game.faceOf oid gs)) (Game.zoneMembers Zone.Graveyard pid gs))

-- The tokens on the battlefield, newest first.
tokensOnBattlefield :: GameState.GameState -> [ObjectId.ObjectId]
tokensOnBattlefield gs = List.sortOn Ord.Down (filter (`Game.isToken` gs) (Set.toList (GameState.battlefield gs)))

entryBudgetSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
entryBudgetSpec s registry =
  Spec.describe s "One budget for simultaneous entry costs (CR 614.12b)" $ do
    -- THE PROVING BOARD for CR 614.12b and CR 614.13b, with a greedy answerer:
    -- the first token to be asked takes every Forest there is, and the four
    -- entering beside it are left with nothing to sacrifice and no prompt at
    -- all. Five permanents, three Forests, one 3/3.
    --
    -- CR 614.13b ("the same object can't be chosen to change zones more than
    -- once when applying replacement effects that modify how one or more
    -- permanents enter the battlefield") is the sharper half of the citation,
    -- and the P/T is what observes it: the arm counts what was CHOSEN, so an
    -- engine that offered a spent Forest to the next token would stamp the same
    -- three on all five and leave five 3/3s, even though the sacrifice funnel
    -- would move nothing the second time.
    Spec.it s "a greedy first choice leaves the four entering beside it nothing (CR 614.12b, CR 614.13b)" $ do
      island <- S.printingOf s registry "Island"
      forest <- S.printingOf s registry "Forest"
      bayou <- S.printingOf s registry "Bayou"
      mountain <- S.printingOf s registry "Mountain"
      woodElemental <- S.printingOf s registry "Wood Elemental"
      rite <- S.printingOf s registry "Rite of Replication"
      let (gs, held, elementalId, sacrificeable, tappedForest, mountainId) = woodElementalBoard island forest bayou mountain woodElemental rite
          play = S.cast S.alice held >> Stack.resolveTop >> Engine.settleForPriority
          after = S.runPure (riteOn elementalId sacrificesAll) gs play
          asks = sacrificeAsks (answersFor (riteOn elementalId sacrificesAll) gs play)
      Spec.assertEqWith s "one token survived, and it is the 3/3 that got all three" (fmap (\oid -> S.powerToughnessOf oid after) (tokensOnBattlefield after)) [Just (3, 3)]
      Spec.assertEqWith s "only ONE of the five entry costs found anything to spend" asks 1
      Spec.assertEqWith s "the two Forests and the Bayou all left the battlefield" (filter (\oid -> Set.member oid (GameState.battlefield after)) sacrificeable) []
      -- BY NAME, not by id: CR 400.7's new object gets a fresh id on the way to
      -- the graveyard, so the ids the fixture holds name only the battlefield
      -- incarnations. Three cards and no more is the other half of the
      -- assertion above -- nothing was sacrificed twice.
      Spec.assertEqWith s "the two Forests and the Bayou are alice's whole graveyard, beside the spent Rite" (graveyardNames S.alice after) (fmap (CardName.MkCardName . Text.pack) ["Bayou", "Forest", "Forest", "Rite of Replication"])
      Spec.assertBool s (Set.member tappedForest (GameState.battlefield after)) "the TAPPED Forest was never offered"
      Spec.assertBool s (Set.member mountainId (GameState.battlefield after)) "nor was the Mountain"
      Spec.assertEqWith s "the copied Wood Elemental is untouched" (S.powerToughnessOf elementalId after) (Just (1, 1))
    -- The same board, the same five entering permanents and the same supply,
    -- with the one answer changed: each entry cost spends ONE named Forest
    -- rather than all of them. The budget NARROWS the later choices rather than
    -- only emptying them -- three tokens are asked and each gets a different
    -- land, the fourth and fifth are asked nothing.
    --
    -- The paired control for the case above: the two boards differ in exactly
    -- the answer, and they disagree about how many tokens survive and at what
    -- size, so neither can pass for the other's reason.
    Spec.it s "each later choice sees only what the earlier ones left (CR 614.12b)" $ do
      island <- S.printingOf s registry "Island"
      forest <- S.printingOf s registry "Forest"
      bayou <- S.printingOf s registry "Bayou"
      mountain <- S.printingOf s registry "Mountain"
      woodElemental <- S.printingOf s registry "Wood Elemental"
      rite <- S.printingOf s registry "Rite of Replication"
      let (gs, held, elementalId, sacrificeable, _, _) = woodElementalBoard island forest bayou mountain woodElemental rite
          play = S.cast S.alice held >> Stack.resolveTop >> Engine.settleForPriority
          after = S.runPure (riteSplitting elementalId sacrificeable) gs play
          asks = sacrificeAsks (answersFor (riteSplitting elementalId sacrificeable) gs play)
      Spec.assertEqWith s "three tokens survived, each a 1/1 off one Forest" (fmap (\oid -> S.powerToughnessOf oid after) (tokensOnBattlefield after)) [Just (1, 1), Just (1, 1), Just (1, 1)]
      Spec.assertEqWith s "three of the five entry costs found something to spend" asks 3
      Spec.assertEqWith s "all three lands went, one apiece" (filter (\oid -> Set.member oid (GameState.battlefield after)) sacrificeable) []

-- alice controls `mountains` untapped Mountains and `forests` untapped Forests
-- in a precombat main phase with priority, holding one card per printing in
-- `hand`. Returns the state and the hand ids in the order given.
--
-- Two land printings rather than blueBoard's one, because riot's producers are
-- Gruul: Zhur-Taa Goblin is {R}{G}.
riotBoard :: Printing.Printing -> Int -> Printing.Printing -> Int -> [Printing.Printing] -> (GameState.GameState, [ObjectId.ObjectId])
riotBoard mountain mountains forest forests hand =
  let base = S.landsInPlay mountain mountains
      -- S.addCreature puts one permanent of a printing onto the battlefield,
      -- settled; nothing in it is creature-specific, which is what lets a second
      -- land printing join a board S.landsInPlay built from one.
      addLand g _ = snd (S.addCreature forest S.alice g)
      withForests = List.foldl' addLand base (replicate forests ())
      addOne (ids, g) p = let (oid, g1) = S.addHandCard p S.alice g in (ids <> [oid], g1)
      (held, gs) = List.foldl' addOne ([], withForests) hand
   in ( gs
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          },
        held
      )

-- Answer riot's "may" one way, and everything else the way S.aggressiveAnswer
-- does -- which declares every attacker it is offered, so one answerer carries
-- both halves of a case that casts a creature and then attacks with it.
riotChoosing :: OptionalDecision.OptionalDecision -> Prompt.Prompt r -> r
riotChoosing choice p = case p of
  Prompt.ChooseRiot {} -> choice
  _ -> S.aggressiveAnswer p

wasAskedForRiot :: [Response.Response] -> Bool
wasAskedForRiot responses = riotAsks responses > 0

-- How many times riot's "may" was put to a player. CR 702.136b turns this into
-- an assertion rather than a diagnostic: one instance, one ask.
riotAsks :: [Response.Response] -> Int
riotAsks responses =
  let isRiot r = case r of
        Response.ChoseRiot _ -> True
        _ -> False
   in length (filter isRiot responses)

-- Turn the LAST riot answer in a transcript into a decline, leaving every other
-- answer alone.
--
-- A transcript rewrite because a `Prompt r -> r` answerer cannot do it: CR
-- 702.136b's two prompts name the same decider, the same player and the same
-- permanent, so nothing in the prompt tells them apart, while a positional
-- transcript does. Pawl.Engine.Replay.replay is the same machinery MulliganSpec
-- replays an opening hand with.
declineLastRiot :: [Response.Response] -> [Response.Response]
declineLastRiot responses =
  let flipFirst rs = case rs of
        [] -> []
        Response.ChoseRiot _ : rest -> Response.ChoseRiot OptionalDecision.Declines : rest
        r : rest -> r : flipFirst rest
   in reverse (flipFirst (reverse responses))

-- The board moved to alice's declare-attackers step, with bob defending. Stated
-- rather than played out, exactly as S.combatBoardOf states it: a direct-call
-- test never runs the turn-based action that would settle CR 506.2's defending
-- player.
--
-- Nothing else is touched, so a creature cast in the main phase is still as new
-- to the battlefield as CR 302.6 finds it.
atDeclareAttackers :: GameState.GameState -> GameState.GameState
atDeclareAttackers gs =
  gs
    { GameState.phase = Phase.Combat CombatStep.DeclareAttackers,
      GameState.combat = Combat.emptyCombat {Combat.Type.defender = Just S.bob}
    }

attackersIn :: GameState.GameState -> [ObjectId.ObjectId]
attackersIn gs = Map.keys (Combat.Type.attackers (GameState.combat gs))

riotSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
riotSpec s registry = Spec.describe s "Riot (CR 702.136)" $ do
  -- Zhur-Taa Goblin and not Spider-Punk, whose file also carries "spells and
  -- abilities can't be countered" and a riot-granting static ability: the
  -- keyword is what is under test, and this printing is nothing but the keyword.
  Spec.it s "CR 702.136a taking the counter enters a 3/3 with no haste" $ do
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    zhurTaa <- S.printingOf s registry "Zhur-Taa Goblin"
    let (gs, held) = riotBoard mountain 1 forest 1 [zhurTaa]
    case held of
      goblinCard : _ ->
        let after = S.runPure (riotChoosing OptionalDecision.Exercises) gs (S.cast S.alice goblinCard >> Stack.resolveTop)
         in case newestNamed (CardName.MkCardName $ Text.pack "Zhur-Taa Goblin") after of
              Nothing -> Spec.assertFailure s "Zhur-Taa Goblin did not reach the battlefield"
              Just goblin -> do
                Spec.assertEqWith s "one +1/+1 counter" (countersOn CounterKind.PlusOnePlusOne goblin after) 1
                -- Printed 2/2, so the counter is visible in the projection (CR
                -- 613.4c, layer 7c).
                Spec.assertEqWith s "power" (Projection.powerOf goblin after) (Just 3)
                Spec.assertEqWith s "toughness" (Projection.toughnessOf goblin after) (Just 3)
                -- CR 702.136a's "if you don't" is what grants haste, so taking
                -- the counter must not.
                Spec.assertBool s (not (Projection.hasKeyword Keyword.Haste goblin after)) "no haste"
      _ -> Spec.assertFailure s "fixture did not deal a card"
  Spec.it s "CR 702.136a declining the counter grants haste instead" $ do
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    zhurTaa <- S.printingOf s registry "Zhur-Taa Goblin"
    let (gs, held) = riotBoard mountain 1 forest 1 [zhurTaa]
    case held of
      goblinCard : _ ->
        let after = S.runPure (riotChoosing OptionalDecision.Declines) gs (S.cast S.alice goblinCard >> Stack.resolveTop)
         in case newestNamed (CardName.MkCardName $ Text.pack "Zhur-Taa Goblin") after of
              Nothing -> Spec.assertFailure s "Zhur-Taa Goblin did not reach the battlefield"
              Just goblin -> do
                Spec.assertEqWith s "no counters" (countersOn CounterKind.PlusOnePlusOne goblin after) 0
                Spec.assertEqWith s "power" (Projection.powerOf goblin after) (Just 2)
                Spec.assertBool s (Projection.hasKeyword Keyword.Haste goblin after) "haste"
      _ -> Spec.assertFailure s "fixture did not deal a card"
  -- THE PAIR THAT MAKES THE HASTE REAL. CR 302.6 keeps a creature that entered
  -- this turn from attacking, and CR 702.10b is the exception riot buys.
  Spec.it s "CR 702.10b the goblin that took haste attacks the turn it entered" $ do
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    zhurTaa <- S.printingOf s registry "Zhur-Taa Goblin"
    let (gs, held) = riotBoard mountain 1 forest 1 [zhurTaa]
        answer = riotChoosing OptionalDecision.Declines
    case held of
      goblinCard : _ ->
        let entered = S.runPure answer gs (S.cast S.alice goblinCard >> Stack.resolveTop)
            after = S.runPure answer (atDeclareAttackers entered) (Combat.declareAttackers S.alice)
         in case newestNamed (CardName.MkCardName $ Text.pack "Zhur-Taa Goblin") after of
              Nothing -> Spec.assertFailure s "Zhur-Taa Goblin did not reach the battlefield"
              Just goblin -> Spec.assertEqWith s "attacks" (attackersIn after) [goblin]
      _ -> Spec.assertFailure s "fixture did not deal a card"
  Spec.it s "CR 302.6 the goblin that took the counter cannot attack that turn" $ do
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    zhurTaa <- S.printingOf s registry "Zhur-Taa Goblin"
    let (gs, held) = riotBoard mountain 1 forest 1 [zhurTaa]
        answer = riotChoosing OptionalDecision.Exercises
    case held of
      goblinCard : _ ->
        let entered = S.runPure answer gs (S.cast S.alice goblinCard >> Stack.resolveTop)
            after = S.runPure answer (atDeclareAttackers entered) (Combat.declareAttackers S.alice)
         in Spec.assertEqWith s "no attackers" (attackersIn after) []
      _ -> Spec.assertFailure s "fixture did not deal a card"
  -- THE CHOICE IS THE ANSWERER'S. Both outcomes above are reachable only through
  -- a prompt, and the prompt is never elided: CR 702.136a's two halves are
  -- distinguishable on every board.
  Spec.it s "CR 702.136a the controller is asked, and the engine chooses nothing" $ do
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    zhurTaa <- S.printingOf s registry "Zhur-Taa Goblin"
    let (gs, held) = riotBoard mountain 1 forest 1 [zhurTaa]
    case held of
      goblinCard : _ ->
        let asked = answersFor S.identityAnswer gs (S.cast S.alice goblinCard >> Stack.resolveTop)
         in Spec.assertBool s (wasAskedForRiot asked) "a ChooseRiot was raised"
      _ -> Spec.assertFailure s "fixture did not deal a card"
  -- The control that keeps the case above from passing for the wrong reason: a
  -- creature WITHOUT riot enters through the same funnel and is asked nothing.
  Spec.it s "CR 702.136a a creature without riot raises no riot prompt" $ do
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    let (gs, held) = riotBoard mountain 3 forest 0 [pikerPrinting]
    case held of
      pikerCard : _ ->
        let asked = answersFor S.identityAnswer gs (S.cast S.alice pikerCard >> Stack.resolveTop)
         in Spec.assertBool s (not (wasAskedForRiot asked)) "no ChooseRiot was raised"
      _ -> Spec.assertFailure s "fixture did not deal a card"
  -- CR 702.136a through a GRANT rather than a printing: Spider-Punk's "other
  -- Spiders you control have riot". The keyword reaches the entering Giant
  -- Spider through layer 6 (CR 613.1f), and the replacement is minted off that
  -- post-layer projection -- which is the whole reason the mint lives there.
  Spec.it s "CR 702.136a Spider-Punk gives another Spider riot as it enters" $ do
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    spiderPunk <- S.printingOf s registry "Spider-Punk"
    giantSpider <- S.printingOf s registry "Giant Spider"
    let (gs, held) = riotBoard mountain 3 forest 1 [giantSpider]
        (_, board) = S.addCreature spiderPunk S.alice gs
        answer = riotChoosing OptionalDecision.Declines
    case held of
      spiderCard : _ ->
        let asked = answersFor answer board (S.cast S.alice spiderCard >> Stack.resolveTop)
            after = S.runPure answer board (S.cast S.alice spiderCard >> Stack.resolveTop)
         in case newestNamed (CardName.MkCardName $ Text.pack "Giant Spider") after of
              Nothing -> Spec.assertFailure s "Giant Spider did not reach the battlefield"
              Just spider -> do
                Spec.assertBool s (wasAskedForRiot asked) "a ChooseRiot was raised for the granted riot"
                Spec.assertBool s (Projection.hasKeyword Keyword.Haste spider after) "haste"
      _ -> Spec.assertFailure s "fixture did not deal a card"
  -- The grant from a permanent that has no riot of its own: Rhythm of the Wild
  -- is an enchantment whose whole riot contribution is "nontoken creatures you
  -- control have riot", so it is the case Spider-Punk cannot make -- the
  -- entering creature's base face prints no riot AND the granting permanent's
  -- prints none either, which is what Projection.grantsKeywordWhere is for.
  Spec.it s "CR 702.136a Rhythm of the Wild gives an entering creature riot" $ do
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    rhythm <- S.printingOf s registry "Rhythm of the Wild"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    let (gs, held) = riotBoard mountain 2 forest 0 [pikerPrinting]
        (_, board) = S.addCreature rhythm S.alice gs
        answer = riotChoosing OptionalDecision.Declines
    case held of
      pikerCard : _ ->
        let asked = answersFor answer board (S.cast S.alice pikerCard >> Stack.resolveTop)
            after = S.runPure answer board (S.cast S.alice pikerCard >> Stack.resolveTop)
         in case newestNamed (CardName.MkCardName $ Text.pack "Goblin Piker") after of
              Nothing -> Spec.assertFailure s "Goblin Piker did not reach the battlefield"
              Just piker -> do
                Spec.assertBool s (wasAskedForRiot asked) "a ChooseRiot was raised for the granted riot"
                Spec.assertBool s (Projection.hasKeyword Keyword.Haste piker after) "haste"
      _ -> Spec.assertFailure s "fixture did not deal a card"
  -- The control for the grant: the same Spider on the same board with no
  -- Spider-Punk is asked nothing and gains nothing.
  Spec.it s "CR 702.136a without Spider-Punk that Spider has no riot" $ do
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    giantSpider <- S.printingOf s registry "Giant Spider"
    let (gs, held) = riotBoard mountain 3 forest 1 [giantSpider]
        answer = riotChoosing OptionalDecision.Declines
    case held of
      spiderCard : _ ->
        let asked = answersFor answer gs (S.cast S.alice spiderCard >> Stack.resolveTop)
            after = S.runPure answer gs (S.cast S.alice spiderCard >> Stack.resolveTop)
         in case newestNamed (CardName.MkCardName $ Text.pack "Giant Spider") after of
              Nothing -> Spec.assertFailure s "Giant Spider did not reach the battlefield"
              Just spider -> do
                Spec.assertBool s (not (wasAskedForRiot asked)) "no ChooseRiot was raised"
                Spec.assertBool s (not (Projection.hasKeyword Keyword.Haste spider after)) "no haste"
      _ -> Spec.assertFailure s "fixture did not deal a card"
  -- CR 702.136b: "If a permanent has multiple instances of riot, each works
  -- separately." Zhur-Taa Goblin PRINTS riot and Rhythm of the Wild GRANTS it to
  -- her nontoken creatures, so the goblin enters holding riot twice -- two
  -- textually identical replacement abilities on ONE source, which is the shape
  -- CR 614.5's identity had to learn to tell apart.
  --
  -- The single-riot leg in the same case is what keeps the counter count from
  -- being read as a doubling: one instance still asks once and still yields one
  -- counter, on a board differing only in the enchantment.
  Spec.it s "CR 702.136b riot twice is asked twice, and both counters land" $ do
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    zhurTaa <- S.printingOf s registry "Zhur-Taa Goblin"
    rhythm <- S.printingOf s registry "Rhythm of the Wild"
    let (gs, held) = riotBoard mountain 1 forest 1 [zhurTaa]
        (_, board) = S.addCreature rhythm S.alice gs
        answer = riotChoosing OptionalDecision.Exercises
    case held of
      goblinCard : _ ->
        let play = S.cast S.alice goblinCard >> Stack.resolveTop
            after = S.runPure answer board play
         in case newestNamed (CardName.MkCardName $ Text.pack "Zhur-Taa Goblin") after of
              Nothing -> Spec.assertFailure s "Zhur-Taa Goblin did not reach the battlefield"
              Just goblin -> do
                Spec.assertEqWith s "two ChooseRiot were raised" (riotAsks (answersFor answer board play)) 2
                Spec.assertEqWith s "one ChooseRiot without the grant" (riotAsks (answersFor answer gs play)) 1
                Spec.assertEqWith s "two +1/+1 counters" (countersOn CounterKind.PlusOnePlusOne goblin after) 2
                -- Printed 2/2 (CR 613.4c, layer 7c).
                Spec.assertEqWith s "power" (Projection.powerOf goblin after) (Just 4)
                Spec.assertEqWith s "toughness" (Projection.toughnessOf goblin after) (Just 4)
                Spec.assertBool s (not (Projection.hasKeyword Keyword.Haste goblin after)) "no haste"
      _ -> Spec.assertFailure s "fixture did not deal a card"
  -- CR 702.136b's "each works separately" read at its sharpest: the rule's own
  -- consequence is that one instance may take the counter while the other takes
  -- haste, which no single application of riot can produce -- CR 702.136a's two
  -- halves are exclusive.
  Spec.it s "CR 702.136b one instance takes the counter and the other takes haste" $ do
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    zhurTaa <- S.printingOf s registry "Zhur-Taa Goblin"
    rhythm <- S.printingOf s registry "Rhythm of the Wild"
    let (gs, held) = riotBoard mountain 1 forest 1 [zhurTaa]
        (_, board) = S.addCreature rhythm S.alice gs
        answer = riotChoosing OptionalDecision.Exercises
    case held of
      goblinCard : _ ->
        let play = S.cast S.alice goblinCard >> Stack.resolveTop
            script = declineLastRiot (answersFor answer board play)
            ((_, after), desync) = Replay.replay script board play
         in case newestNamed (CardName.MkCardName $ Text.pack "Zhur-Taa Goblin") after of
              Nothing -> Spec.assertFailure s "Zhur-Taa Goblin did not reach the battlefield"
              Just goblin -> do
                -- An exhausted or mismatched transcript would fall back to
                -- Replay.defaultAnswer, which would decide riot itself.
                Spec.assertBool s (Maybe.isNothing desync) "the transcript answered every prompt"
                Spec.assertEqWith s "the exercised instance's counter" (countersOn CounterKind.PlusOnePlusOne goblin after) 1
                Spec.assertBool s (Projection.hasKeyword Keyword.Haste goblin after) "and the declined instance's haste"
      _ -> Spec.assertFailure s "fixture did not deal a card"

-- Answer unleash's "may" one way, and everything else the way S.aggressiveAnswer
-- does -- riotChoosing's shape, one keyword over.
unleashChoosing :: OptionalDecision.OptionalDecision -> Prompt.Prompt r -> r
unleashChoosing choice p = case p of
  Prompt.ChooseUnleash {} -> choice
  _ -> S.aggressiveAnswer p

-- How many times unleash's "may" was put to a player.
unleashAsks :: [Response.Response] -> Int
unleashAsks responses =
  let isUnleash r = case r of
        Response.ChoseUnleash _ -> True
        _ -> False
   in length (filter isUnleash responses)

-- CR 702.98a's two static abilities, on Gore-House Chainwalker ({1}{R} 2/1 with
-- unleash) -- the first keyword in pawl to mint a COMBAT RESTRICTION, where riot
-- above mints only a replacement effect.
--
-- Gameplay-level throughout: every case casts the card and answers the prompt, so
-- the counter and the restriction are both reached the way a game reaches them.
-- The two answers are asserted against each other on boards differing in nothing
-- but the answer, which is what keeps `canBlock` answering False for the reason
-- under test rather than for a tapped or missing creature.
unleashSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
unleashSpec s registry = Spec.describe s "Unleash (CR 702.98)" $ do
  Spec.it s "CR 702.98a taking the counter makes it bigger and stops it blocking" $ do
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    chainwalker <- S.printingOf s registry "Gore-House Chainwalker"
    spider <- S.printingOf s registry "Giant Spider"
    let (gs0, held) = riotBoard mountain 2 forest 0 [chainwalker]
        -- A second creature on the SAME board, so a blanket "nobody may block"
        -- bug cannot pass: the minted restriction has to be narrow to the
        -- permanent holding the keyword. Giant Spider rather than a second
        -- Chainwalker, and 2/1 against 2/5, so no reading of the board confuses
        -- the two.
        (bystander, gs) = S.addCreature spider S.alice gs0
        answer = unleashChoosing OptionalDecision.Exercises
    case held of
      card : _ ->
        let play = S.cast S.alice card >> Stack.resolveTop
            after = S.runPure answer gs play
         in case newestNamed (CardName.MkCardName $ Text.pack "Gore-House Chainwalker") after of
              Nothing -> Spec.assertFailure s "Gore-House Chainwalker did not reach the battlefield"
              Just walker -> do
                Spec.assertEqWith s "one ChooseUnleash was raised" (unleashAsks (answersFor answer gs play)) 1
                Spec.assertEqWith s "one +1/+1 counter" (countersOn CounterKind.PlusOnePlusOne walker after) 1
                -- Printed 2/1, so the counter shows in the projection (CR 613.4c,
                -- layer 7c).
                Spec.assertEqWith s "power" (Projection.powerOf walker after) (Just 3)
                Spec.assertEqWith s "toughness" (Projection.toughnessOf walker after) (Just 2)
                -- CR 509.1b through rule 702.98a's second static ability.
                Spec.assertBool s (not (Combat.canBlock S.alice walker after)) "it cannot block"
                Spec.assertBool s (Combat.canBlock S.alice bystander after) "the Spider beside it can"
                Spec.assertEqWith s "and only the Spider is offered" (Combat.legalBlockers S.alice after) [bystander]
      _ -> Spec.assertFailure s "fixture did not deal a card"
  -- THE PAIR THAT MAKES THE RESTRICTION REAL. Same board, same fixture, opposite
  -- answer: rule 702.98a's second ability keys on the counter, so declining puts
  -- the creature back among the legal blockers.
  Spec.it s "CR 702.98a declining leaves a 2/1 that can block" $ do
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    chainwalker <- S.printingOf s registry "Gore-House Chainwalker"
    spider <- S.printingOf s registry "Giant Spider"
    let (gs0, held) = riotBoard mountain 2 forest 0 [chainwalker]
        (bystander, gs) = S.addCreature spider S.alice gs0
        answer = unleashChoosing OptionalDecision.Declines
    case held of
      card : _ ->
        let after = S.runPure answer gs (S.cast S.alice card >> Stack.resolveTop)
         in case newestNamed (CardName.MkCardName $ Text.pack "Gore-House Chainwalker") after of
              Nothing -> Spec.assertFailure s "Gore-House Chainwalker did not reach the battlefield"
              Just walker -> do
                Spec.assertEqWith s "no counters" (countersOn CounterKind.PlusOnePlusOne walker after) 0
                Spec.assertEqWith s "power" (Projection.powerOf walker after) (Just 2)
                Spec.assertEqWith s "toughness" (Projection.toughnessOf walker after) (Just 1)
                -- Rule 702.98a states no consequence for declining, where riot
                -- grants haste, so the board holds nothing but a 2/1.
                Spec.assertBool s (not (Projection.hasKeyword Keyword.Haste walker after)) "no haste"
                Spec.assertBool s (Combat.canBlock S.alice walker after) "it can block"
                Spec.assertEqWith s "and both creatures are offered" (List.sort (Combat.legalBlockers S.alice after)) (List.sort [bystander, walker])
      _ -> Spec.assertFailure s "fixture did not deal a card"
  -- CR 702.98a says "a +1/+1 counter", not "that counter", so the restriction is
  -- not a fact about how the permanent entered. A counter arriving later shuts
  -- blocking off on a board where the controller DECLINED, which no reading tied
  -- to the entry replacement can produce.
  Spec.it s "CR 702.98a a +1/+1 counter arriving later shuts blocking off too" $ do
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    chainwalker <- S.printingOf s registry "Gore-House Chainwalker"
    spider <- S.printingOf s registry "Giant Spider"
    let (gs0, held) = riotBoard mountain 2 forest 0 [chainwalker]
        (bystander, gs) = S.addCreature spider S.alice gs0
        answer = unleashChoosing OptionalDecision.Declines
    case held of
      card : _ ->
        let after = S.runPure answer gs (S.cast S.alice card >> Stack.resolveTop)
         in case newestNamed (CardName.MkCardName $ Text.pack "Gore-House Chainwalker") after of
              Nothing -> Spec.assertFailure s "Gore-House Chainwalker did not reach the battlefield"
              Just walker -> do
                -- A STATE fixture for the counters, S.addCounter's documented use:
                -- nothing in the pool puts a +1/+1 counter on each of two
                -- creatures at a moment this board can reach.
                let counted = S.addCounter CounterKind.PlusOnePlusOne 1 bystander (S.addCounter CounterKind.PlusOnePlusOne 1 walker after)
                Spec.assertBool s (Combat.canBlock S.alice walker after) "without the counter it blocks"
                Spec.assertBool s (not (Combat.canBlock S.alice walker counted)) "with one it cannot"
                -- THE COUNTER IS NOT THE WHOLE CONDITION. The Spider carries the
                -- same counter and no unleash, so the restriction has to name its
                -- own source (CR 109.5) rather than every counter-bearing
                -- creature on the board.
                Spec.assertBool s (Combat.canBlock S.alice bystander counted) "the Spider with the same counter still blocks"
      _ -> Spec.assertFailure s "fixture did not deal a card"

-- alice controls three untapped Swamps on a THREE-SEAT board and holds one
-- Bloodrage Vampire, in her precombat main phase with priority; bob controls one
-- Ogre Sentry. Returns the state, the card in hand and the Sentry.
--
-- Three seats rather than riotBoard's two, and for rule 702.54a's word "an
-- opponent": a two-player board collapses "an opponent" onto the only other
-- player, so a reading that admitted the entry whenever ANY player was dealt
-- damage could not be told from one that admitted it only for an opponent's.
bloodthirstBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId)
bloodthirstBoard swamp vampire sentry =
  let lands = S.landsFor swamp S.alice 3 S.threePlayerGame
      (bobsSentry, withSentry) = S.addCreature sentry S.bob lands
      (held, gs) = S.addHandCard vampire S.alice withSentry
   in (readyForAlice gs, held, bobsSentry)

-- alice, in her precombat main phase, with priority. Applied by the fixture above
-- and again after a turn handoff, which leaves the game in CR 502's untap step.
readyForAlice :: GameState.GameState -> GameState.GameState
readyForAlice gs =
  gs
    { GameState.phase = Phase.PrecombatMain,
      GameState.activePlayer = S.alice,
      GameState.priority = Just S.alice
    }

-- CR 702.54: bloodthirst, on Bloodrage Vampire ({2}{B} 3/1 Vampire, "Bloodthirst
-- 1" and nothing else). The second minted entry replacement whose own rule states
-- a condition, after rule 702.145b's daybound, which is why
-- Pawl.Engine.Replacement.admitsEntry now has two arms that are not `True`.
--
-- ONE BOARD throughout, and every case differs from the others in nothing but
-- what happened before the cast: the same three seats, the same three Swamps, the
-- same card cast the same way. The Vampire ENTERS in every case, so what the
-- assertions tell apart is "entered with counters" from "entered", never "entered"
-- from "did not".
--
-- Distinct numbers everywhere, so no two readings coincide: bloodthirst 1 on a
-- printed 3/1 shows as a 4/2, and the damage amounts are 4 at bob, 5 at alice, 2
-- at bob's Sentry (which a 3/3 survives) and 6 of life paid.
--
-- The damage goes in through Damage.applyDamage, the funnel that records the
-- event, exactly as Pawl.TriggerSpec's Furious Spinesplitter group does -- and
-- everything downstream of the record is the card's own, driven through a real
-- cast and a real resolution.
bloodthirstSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
bloodthirstSpec s registry =
  let hit src target amount gs =
        S.runPure
          S.identityAnswer
          gs
          (Damage.applyDamage [DamageEvent.MkDamageEvent src target amount False False False 0 Nothing DamageKind.Noncombat])
      enters = castAndResolve S.aggressiveAnswer
      vampireIn = newestNamed (CardName.MkCardName $ Text.pack "Bloodrage Vampire")
   in Spec.describe s "Bloodthirst (CR 702.54)" $ do
        Spec.it s "CR 702.54a nobody was dealt damage, so it enters a 3/1" $ do
          swamp <- S.printingOf s registry "Swamp"
          vampire <- S.printingOf s registry "Bloodrage Vampire"
          sentry <- S.printingOf s registry "Ogre Sentry"
          let (gs, held, _) = bloodthirstBoard swamp vampire sentry
              after = enters gs held
          case vampireIn after of
            Nothing -> Spec.assertFailure s "Bloodrage Vampire did not reach the battlefield"
            Just vamp -> do
              Spec.assertEqWith s "no counters" (countersOn CounterKind.PlusOnePlusOne vamp after) 0
              Spec.assertEqWith s "power" (Projection.powerOf vamp after) (Just 3)
              Spec.assertEqWith s "toughness" (Projection.toughnessOf vamp after) (Just 1)
        -- THE PAIR THAT MAKES THE CONDITION REAL. The board above with one damage
        -- event added and nothing else changed.
        Spec.it s "CR 702.54a an opponent was dealt damage, so it enters a 4/2" $ do
          swamp <- S.printingOf s registry "Swamp"
          vampire <- S.printingOf s registry "Bloodrage Vampire"
          sentry <- S.printingOf s registry "Ogre Sentry"
          let (gs0, held, bobsSentry) = bloodthirstBoard swamp vampire sentry
              after = enters (hit bobsSentry (Recipient.ToPlayer S.bob) 4 gs0) held
          case vampireIn after of
            Nothing -> Spec.assertFailure s "Bloodrage Vampire did not reach the battlefield"
            Just vamp -> do
              Spec.assertEqWith s "one +1/+1 counter" (countersOn CounterKind.PlusOnePlusOne vamp after) 1
              -- Printed 3/1, so the counter shows in the projection (CR 613.4c,
              -- layer 7c).
              Spec.assertEqWith s "power" (Projection.powerOf vamp after) (Just 4)
              Spec.assertEqWith s "toughness" (Projection.toughnessOf vamp after) (Just 2)
        -- CR 102.2 / 109.5: "an opponent" is a player other than the entering
        -- permanent's controller, so alice's own damage is not an opponent's.
        -- Unreachable on a two-seat board, which is why this group takes three.
        Spec.it s "CR 102.2 damage to the controller herself does not turn it on" $ do
          swamp <- S.printingOf s registry "Swamp"
          vampire <- S.printingOf s registry "Bloodrage Vampire"
          sentry <- S.printingOf s registry "Ogre Sentry"
          let (gs0, held, bobsSentry) = bloodthirstBoard swamp vampire sentry
              after = enters (hit bobsSentry (Recipient.ToPlayer S.alice) 5 gs0) held
          case vampireIn after of
            Nothing -> Spec.assertFailure s "Bloodrage Vampire did not reach the battlefield"
            Just vamp -> Spec.assertEqWith s "no counters" (countersOn CounterKind.PlusOnePlusOne vamp after) 0
        -- CR 120.3a names the PLAYER recipient; a creature bob controls is not bob.
        Spec.it s "CR 120.3a damage to an opponent's creature does not either" $ do
          swamp <- S.printingOf s registry "Swamp"
          vampire <- S.printingOf s registry "Bloodrage Vampire"
          sentry <- S.printingOf s registry "Ogre Sentry"
          let (gs0, held, bobsSentry) = bloodthirstBoard swamp vampire sentry
              damaged = hit bobsSentry (Recipient.ToCreature bobsSentry) 2 gs0
              after = enters damaged held
          case vampireIn after of
            Nothing -> Spec.assertFailure s "Bloodrage Vampire did not reach the battlefield"
            Just vamp -> do
              Spec.assertEqWith s "no counters" (countersOn CounterKind.PlusOnePlusOne vamp after) 0
              Spec.assertBool s (Set.member bobsSentry (GameState.battlefield after)) "and the 3/3 Sentry survived the 2, so the board is otherwise the same"
        -- CR 119.4's life loss is not CR 120.1's damage, and the log records the
        -- two separately. Without this the condition could be reading
        -- GameEvent.LifeLost and pass every case above, since damage to a player
        -- files one of those too.
        Spec.it s "CR 119.4 life lost without damage is not damage" $ do
          swamp <- S.printingOf s registry "Swamp"
          vampire <- S.printingOf s registry "Bloodrage Vampire"
          sentry <- S.printingOf s registry "Ogre Sentry"
          let (gs0, held, _) = bloodthirstBoard swamp vampire sentry
              after = enters (Event.payLife S.bob 6 gs0) held
          case vampireIn after of
            Nothing -> Spec.assertFailure s "Bloodrage Vampire did not reach the battlefield"
            Just vamp -> Spec.assertEqWith s "no counters" (countersOn CounterKind.PlusOnePlusOne vamp after) 0
        -- CR 608.2i: the window is THIS turn. Without this a lifetime tally passes
        -- every case above. The turn goes all the way round to alice again, so the
        -- only difference from the positive case is which turn it is.
        Spec.it s "CR 608.2i the damage is THIS turn's: the handoff clears it" $ do
          swamp <- S.printingOf s registry "Swamp"
          vampire <- S.printingOf s registry "Bloodrage Vampire"
          sentry <- S.printingOf s registry "Ogre Sentry"
          let (gs0, held, bobsSentry) = bloodthirstBoard swamp vampire sentry
              damaged = hit bobsSentry (Recipient.ToPlayer S.bob) 4 gs0
              handoff gs = S.runPure S.identityAnswer gs Engine.handoffTurn
              roundAgain = readyForAlice (handoff (handoff (handoff damaged)))
              after = enters roundAgain held
          Spec.assertEqWith s "the turn came back to alice" (GameState.activePlayer roundAgain) S.alice
          case vampireIn after of
            Nothing -> Spec.assertFailure s "Bloodrage Vampire did not reach the battlefield"
            Just vamp -> Spec.assertEqWith s "no counters a turn cycle later" (countersOn CounterKind.PlusOnePlusOne vamp after) 0
        -- CR 702.54c: "if an object has multiple instances of bloodthirst, each
        -- applies separately." Asserted at the mint rather than at gameplay level,
        -- because nothing in the pool prints or grants a second instance -- the
        -- same footing Pawl.TriggerSpec's vanishing group states its rule 702.63c
        -- claim on.
        Spec.it s "CR 702.54c each instance is its own row" $
          Spec.assertEqWith
            s
            "bloodthirst 1 held twice mints two rows"
            (Keyword.Engine.mintedReplacementsFor (Keyword.Bloodthirst 1) 2)
            (replicate 2 (ReplacementEffect.EntryR (EntryR.MkEntryR Filter.Type.IsSource (EntryRewrite.Bloodthirst 1))))

-- The tap state of a permanent, which is what CR 502.3's untap step writes -- and
-- so what a skipped untap step leaves alone.
tapStateOf :: ObjectId.ObjectId -> GameState.GameState -> Maybe TapState.TapState
tapStateOf oid = fmap Object.tapped . Game.lookupObject oid

-- The one permanent a move added to the battlefield, or Nothing when it added
-- none or several. `newestNamed` above cannot answer this one: a face-down
-- permanent has no name at all (CR 708.2a).
arrivedOne :: GameState.GameState -> GameState.GameState -> Maybe ObjectId.ObjectId
arrivedOne before after =
  case Set.toList (Set.difference (GameState.battlefield after) (GameState.battlefield before)) of
    [oid] -> Just oid
    _ -> Nothing

-- The board at the START of each of the next `n` turns, oldest first -- so the
-- board a turn LEFT is the next element, and a turn's own first step is still
-- ahead of the element that names it. Top-level rather than a `where` binding
-- because the answer is rank-2 and GHC will not infer it, the same reason
-- castEach above is.
turnStarts :: (forall r. Prompt.Prompt r -> r) -> Int -> GameState.GameState -> [GameState.GameState]
turnStarts answer n gs =
  if n <= 0
    then []
    else let next = nextTurn answer gs in next : turnStarts answer (n - 1) next

-- Answer CR 616.1's choice between two untap-step skips by the row's SOURCE:
-- True takes Brine Elemental's row, False the other one, which on that board is
-- Savor the Moment's. By source and not by index, because Replacement.collect
-- emits the floating store newest-first while installTurnSkips prepends -- so a
-- change in either order must not silently swap what an answer means.
--
-- Savor's row is named by exclusion rather than by its own id: CR 400.7 minted a
-- new object as the card moved to the stack, and the id the fixture holds is the
-- hand card's.
skipAnswer :: Bool -> ObjectId.ObjectId -> Prompt.Prompt r -> r
skipAnswer wantBrine brine p = case p of
  Prompt.ChooseReplacement _ _ entries ->
    let wanted = if wantBrine then (== brine) else (/= brine)
     in maybe 0 Int.toNaturalSaturating (List.findIndex (wanted . ReplacementEntry.source) entries)
  _ -> S.identityAnswer p

-- Three seats (CR 800.1), alice active in her precombat main phase:
--
--   * alice holds Savor the Moment and has three untapped Islands for its
--     {1}{U}{U}, plus one TAPPED Goblin Piker as her observable;
--   * bob holds Brine Elemental and has ten untapped Islands -- CR 702.37a's {3}
--     for the face-down cast, plus CR 702.37e's {5}{U}{U} to turn it back up;
--   * carol has a TAPPED Goblin Piker of her own and nothing else.
--
-- Three seats and not two: at two, "each opponent" and "each player" name the
-- same set, and carol is what shows the trigger reached bob's opponents rather
-- than everybody.
--
-- The Pikers are the observable, for the reason Pawl.TurnSpec's savorBoard has
-- one: CR 502.3 untaps the ACTIVE player's permanents, so a turn whose untap step
-- happened leaves its player's Piker untapped and a turn whose untap step was
-- skipped leaves it tapped -- and nothing else in these cases taps or untaps
-- either. Lands go in through S.addCreature too, which puts any printing on the
-- battlefield untapped; only the mana matters here, never the card type.
--
-- Libraries are stocked because these cases run seven whole turns, and CR 104.3c
-- decks a player who draws from an empty one.
brineBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId)
brineBoard island savor brine piker =
  let addLands pid n g = List.foldl' (\acc _ -> snd (S.addCreature island pid acc)) g [1 .. (n :: Int)]
      withLands = addLands S.bob 10 (addLands S.alice 3 S.threePlayerGame)
      (alicePiker, g1) = S.addCreature piker S.alice withLands
      (carolPiker, g2) = S.addCreature piker S.carol g1
      (savorId, g3) = S.addHandCard savor S.alice g2
      (brineId, g4) = S.addHandCard brine S.bob g3
      stock g pid = List.foldl' (\g' _ -> snd (S.addLibraryCard piker pid g')) g [1 .. (15 :: Int)]
      stocked = List.foldl' stock g4 [S.alice, S.bob, S.carol]
   in ( (S.tapObject carolPiker (S.tapObject alicePiker stocked))
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice,
            GameState.turnNumber = 1
          },
        savorId,
        brineId,
        alicePiker,
        carolPiker
      )

-- Arm both effects on brineBoard: alice casts Savor the Moment, then bob casts
-- Brine Elemental face down for CR 702.37a's {3} and turns it face up for CR
-- 702.37e's {5}{U}{U}. Nothing when the face-down cast did not land. Hands back
-- the board and the Brine Elemental permanent, whose id `skipAnswer` names its
-- row by.
--
-- The turn-face-up special action is taken through Pawl.Engine.FaceDown directly,
-- as Pawl.FaceDownSpec's cases do: CR 702.37e's "any time you have priority" is
-- satisfied by the engine offering the action to the priority holder alone, and
-- what this fixture needs is bob taking it during ALICE's turn. The card's
-- "when this creature is turned face up" ability triggers on the special action
-- and resolves in the priority loop like any other (CR 603.2).
brineArmed ::
  Printing.Printing ->
  GameState.GameState ->
  ObjectId.ObjectId ->
  ObjectId.ObjectId ->
  Maybe (GameState.GameState, ObjectId.ObjectId)
brineArmed brine gs savorId brineId =
  let withSavor = S.runPure S.identityAnswer gs (S.cast S.alice savorId >> Stack.resolveTop)
      down =
        S.runPure
          S.identityAnswer
          withSavor
          (Cast.castSpell S.bob brineId (S.printingName brine) (Facing.faceDown FaceDownReason.Morphed) >> Stack.resolveTop)
   in do
        permanent <- arrivedOne withSavor down
        pure (S.runPure S.identityAnswer down (FaceDown.turnFaceUp S.bob TurnUpProcedure.Morph permanent >> Engine.priorityLoop), permanent)

-- The seven-turn timeline the two answering cases below share, plus carol's
-- negative control alongside it: alice's own turn (1), Savor's extra turn (2),
-- bob's (3), carol's (4), alice's again (5), bob's (6), carol's again (7).
--
-- `aliceAfter` is what the answer decided -- alice's Piker once her turn 5 has run
-- -- and `left` is how many floating rows the extra turn's cleanup left behind.
--
-- Top-level rather than a `where` binding because the answer is rank-2 and GHC
-- will not infer it, the same reason castEach above is.
assertBrineRun ::
  (Monad m) =>
  Spec.Spec m n ->
  (forall r. Prompt.Prompt r -> r) ->
  GameState.GameState ->
  ObjectId.ObjectId ->
  ObjectId.ObjectId ->
  Maybe TapState.TapState ->
  Int ->
  m ()
assertBrineRun s answer gs alicePiker carolPiker aliceAfter left =
  case turnStarts answer 7 gs of
    [startExtra, startBob, startCarol, startAlice, startBobAgain, startCarolAgain, startLast] -> do
      -- The schedule the rest is read against, pinned first so a mis-ordered turn
      -- cannot be mistaken for a mis-applied skip.
      Spec.assertEqWith
        s
        "the turns run alice (extra), bob, carol, alice, bob, carol, alice"
        (fmap GameState.activePlayer [startExtra, startBob, startCarol, startAlice, startBobAgain, startCarolAgain, startLast])
        [S.alice, S.bob, S.carol, S.alice, S.bob, S.carol, S.alice]
      -- CR 500.11: the extra turn's untap step is skipped either way -- one of the
      -- two rows takes it, and WHICH one is exactly what is not observable yet.
      Spec.assertEqWith s "the extra turn untapped nothing of alice's" (tapStateOf alicePiker startBob) (Just TapState.Tapped)
      -- carol's negative control: ONE row applies to her untap step, so CR 616.1
      -- has nothing to choose and the prompt is correctly elided -- and that one
      -- row takes exactly one untap step of hers.
      Spec.assertBool
        s
        (not (wasAskedToReplace (answersFor answer startCarol Engine.runStep)))
        "carol's lone skip is not a choice, so she is asked nothing"
      Spec.assertEqWith s "carol's own untap step was skipped" (tapStateOf carolPiker startAlice) (Just TapState.Tapped)
      Spec.assertEqWith s "and her NEXT one happened" (tapStateOf carolPiker startLast) (Just TapState.Untapped)
      -- THE OBSERVABLE, and read off the BOARD rather than off a row index:
      -- `collect` and installTurnSkips disagree about which end of the store they
      -- work from, so an ordering change must be unable to flip this. alice's turn
      -- 5 has run by startBobAgain.
      Spec.assertEqWith s "alice's following untap step, as the answer decided it" (tapStateOf alicePiker startBobAgain) aliceAfter
      -- The store behind it, asserted last so the board is what fails first: the
      -- extra turn's cleanup swept Savor's Expiry.AtCleanup row if it was still
      -- there, and kept every Expiry.Never one.
      Spec.assertEqWith s "and the extra turn's cleanup left this many rows" (length (GameState.replacements startBob)) left
    _ -> Spec.assertFailure s "the seven turns did not run"

-- Brine Elemental {4}{U}{U} Creature -- Elemental 5/4: "Morph {5}{U}{U} / When
-- this creature is turned face up, each opponent skips their next untap step",
-- alongside Savor the Moment ({1}{U}{U} Sorcery: "Take an extra turn after this
-- one. Skip the untap step of that turn.").
--
-- CR 614.10a states the outcome outright: "if two effects each cause a player to
-- skip their next occurrence, that player must skip the next two; one effect will
-- be satisfied in skipping the first occurrence, while the other will remain until
-- another occurrence can be skipped."
--
-- What makes this pair the discriminating one is that the two rows are equal in
-- their `effect` -- each a PhaseR naming alice's untap step -- and unequal in
-- LIFETIME. Brine's is Expiry.Never, so it waits however many turns it must;
-- Savor's is Expiry.AtCleanup, because "the untap step of THAT turn" dies with the
-- turn it named. Which of the two CR 616.1 spends is therefore observable a turn
-- of alice's later, and the choice is hers to make -- CR 616.1's "affected
-- player", which for a step beginning is whose step it is.
brineElementalSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
brineElementalSpec s registry = Spec.describe s "BrineElemental" $ do
  let boardOf = do
        island <- S.printingOf s registry "Island"
        savor <- S.printingOf s registry "Savor the Moment"
        brine <- S.printingOf s registry "Brine Elemental"
        piker <- S.printingOf s registry "Goblin Piker"
        let (gs, savorId, brineId, alicePiker, carolPiker) = brineBoard island savor brine piker
        pure (brineArmed brine gs savorId brineId, alicePiker, carolPiker)
  -- The setup control: the two spells armed three rows, and not four. bob is the
  -- ability's controller, so CR 806.1's free-for-all opponents are alice and carol
  -- while bob himself gets nothing; Savor's own row is added as its extra turn
  -- BEGINS (CR 500.11), which is why it is missing until the first handoff ran.
  Spec.it s "CR 614.1b Brine Elemental arms one skip per opponent, and Savor's turn one more" $ do
    (armed, _, _) <- boardOf
    case armed of
      Nothing -> Spec.assertFailure s "the face-down cast did not reach the battlefield"
      Just (gs, _) -> do
        Spec.assertEqWith s "the trigger armed one row per opponent, so two" (length (GameState.replacements gs)) 2
        case turnStarts S.identityAnswer 1 gs of
          [atExtra] -> do
            Spec.assertEqWith
              s
              "and the extra turn is alice's second"
              (GameState.turnNumber atExtra, GameState.activePlayer atExtra)
              (2, S.alice)
            Spec.assertEqWith s "whose beginning added Savor's own, for three" (length (GameState.replacements atExtra)) 3
          _ -> Spec.assertFailure s "the extra turn did not begin"
  -- THE ELISION GOING AWAY. Two rows apply to one untap step and they are not
  -- interchangeable, so CR 616.1's choice is a real one and the affected player
  -- has to be asked for it.
  --
  -- Fails against a `distinguishing` that compares `effect` alone: the two rows
  -- are equal in `effect`, so the prompt is elided and the engine spends whichever
  -- row it happened to collect first.
  Spec.it s "CR 616.1 two skips alike in effect but not in lifetime raise a choice" $ do
    (armed, _, _) <- boardOf
    case armed of
      Nothing -> Spec.assertFailure s "the face-down cast did not reach the battlefield"
      Just (gs, brine) ->
        case turnStarts (skipAnswer True brine) 1 gs of
          [atExtra] ->
            Spec.assertBool
              s
              (wasAskedToReplace (answersFor (skipAnswer True brine) atExtra Engine.runStep))
              "a ChooseReplacement was raised for the extra turn's untap step"
          _ -> Spec.assertFailure s "the extra turn did not begin"
  -- Answering SAVOR's row: it is the one consumed, and Brine's Expiry.Never row
  -- remains to take alice's NEXT untap step -- CR 614.10a's "the other will remain
  -- until another occurrence can be skipped". So her Piker is still tapped after
  -- the following turn of hers. Two rows survive the extra turn's cleanup, both of
  -- them Brine's.
  Spec.it s "CR 614.10a spending Savor's row leaves Brine's to take the following untap step" $ do
    (armed, alicePiker, carolPiker) <- boardOf
    case armed of
      Nothing -> Spec.assertFailure s "the face-down cast did not reach the battlefield"
      Just (gs, brine) ->
        assertBrineRun s (skipAnswer False brine) gs alicePiker carolPiker (Just TapState.Tapped) 2
  -- Answering BRINE's row: it is consumed, and Savor's row survives the untap step
  -- only to be swept at that same turn's cleanup, since "the untap step of THAT
  -- turn" is scoped to the turn it named (Expiry.AtCleanup; CR 514.2). Nothing is
  -- left to take alice's following untap step, so it happens and her Piker untaps.
  -- One row survives the cleanup, carol's.
  --
  -- This is the half that reads LIFETIME rather than merely which row was picked:
  -- were Savor's row armed Expiry.Never like Brine's, it would survive the cleanup,
  -- take the following untap step too, and leave this case reading Tapped.
  Spec.it s "CR 614.10a spending Brine's row lets Savor's expire with its own turn" $ do
    (armed, alicePiker, carolPiker) <- boardOf
    case armed of
      Nothing -> Spec.assertFailure s "the face-down cast did not reach the battlefield"
      Just (gs, brine) ->
        assertBrineRun s (skipAnswer True brine) gs alicePiker carolPiker (Just TapState.Untapped) 1

-- alice casts a Coldsteel Heart off two Mountains and resolves it, answering CR
-- 616.1's race with `pick` and CR 614.1c's colour with blue. Returns every
-- ChooseReplacement payload raised, the finished board, and the permanent that
-- entered.
--
-- BLUE and not the default: Replay.defaultAnswer falls back to white, so a
-- colour assertion against white would pass on a game that asked nothing.
--
-- The payloads are recorded through State the way choosersAsked records player
-- ids -- mid-game, because the discriminating observable here is what reached
-- the wire, not what the board settled on (see the group below).
castColdsteel ::
  Printing.Printing ->
  Printing.Printing ->
  ([ReplacementEntry.ReplacementEntry] -> Natural.Natural) ->
  ([[ReplacementEntry.ReplacementEntry]], GameState.GameState, Maybe ObjectId.ObjectId)
castColdsteel mountain coldsteel pick =
  let (withCard, oid) = S.handOne coldsteel (S.landsInPlay mountain 2)
      step :: Prompt.Prompt r -> State.State [[ReplacementEntry.ReplacementEntry]] r
      step p = case p of
        Prompt.ChooseReplacement _ _ entries -> do
          State.modify' (<> [entries])
          pure (pick entries)
        Prompt.ChooseColor {} -> pure Color.Blue
        _ -> pure (S.identityAnswer p)
      ((_, after), payloads) = State.runState (Engine.runGame step withCard (S.cast S.alice oid >> Stack.resolveTop)) []
      entered = case Set.toList (Set.difference (GameState.battlefield after) (GameState.battlefield withCard)) of
        o : _ -> Just o
        [] -> Nothing
   in (payloads, after, entered)

-- Take the candidate carrying `rewrite`. Total, falling back on the canonical
-- first the way the engine's own out-of-range handling does.
pickRewrite :: EntryRewrite.EntryRewrite (Effect.Effect Card.Card) -> [ReplacementEntry.ReplacementEntry] -> Natural.Natural
pickRewrite rewrite entries =
  let wanted e = ReplacementEntry.effect e == ReplacementEffect.EntryR (EntryR.MkEntryR Filter.Type.IsSource rewrite)
   in maybe 0 Int.toNaturalSaturating (List.findIndex wanted entries)

-- CR 616.1 with CR 614.1c and CR 614.1d. Coldsteel Heart ({2} Snow Artifact,
-- "This artifact enters tapped." / "As this artifact enters, choose a color." /
-- "{T}: Add one mana of the chosen color.") is one source with TWO applicable
-- replacement effects for one entry event -- both ReplacementBucket.Other, both
-- ReplacementOrigin.Other -- so both reach `choose` in a single iteration and
-- the payload must say which is which (#74).
coldsteelHeartSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
coldsteelHeartSpec s registry = Spec.describe s "Coldsteel Heart (CR 616.1)" $ do
  -- THE PROVING CASE. Asserted on the PROMPT PAYLOAD and not on the board, and
  -- that is not a shortcut: CR 616.1f re-collects and CR 614.5 gives each effect
  -- one opportunity, so the artifact ends up tapped with the chosen colour
  -- whichever candidate is picked. A board-level assertion here would be
  -- over-determined and would pass under the [ObjectId] payload this replaces.
  --
  -- The payload LIST is asserted to have exactly one element before anything is
  -- said about its contents: a card file that lost one of the two replacements
  -- would leave one candidate, elide the prompt, and make a "every payload had
  -- two distinct entries" assertion pass over zero payloads.
  Spec.it s "CR 616.1 two entry replacements of ONE source are distinct entries" $ do
    mountain <- S.printingOf s registry "Mountain"
    coldsteel <- S.printingOf s registry "Coldsteel Heart"
    case castColdsteel mountain coldsteel (const 0) of
      ([entries], _, _) -> do
        Spec.assertEqWith s "CR 616.1e offered both candidates" (length entries) 2
        Spec.assertEqWith s "and the player can tell them apart" (length (List.nub entries)) 2
        Spec.assertEqWith s "though both come from the same permanent" (length (List.nub (fmap ReplacementEntry.source entries))) 1
      (payloads, _, _) -> Spec.assertFailure s ("expected exactly one ChooseReplacement, got " <> show (length payloads))
  -- The card-data control: independent of any payload assertion, so a JSON typo
  -- cannot hide behind a green one. Both rewrites ran.
  Spec.it s "CR 614.1c/614.1d both replacements applied, whichever was chosen" $ do
    mountain <- S.printingOf s registry "Mountain"
    coldsteel <- S.printingOf s registry "Coldsteel Heart"
    let assertBoth label pick = case castColdsteel mountain coldsteel pick of
          (_, after, Just oid) -> case Game.lookupObject oid after of
            Nothing -> Spec.assertFailure s (label <> ": the artifact left the battlefield")
            Just obj -> do
              Spec.assertEqWith s (label <> ": CR 614.1d it entered tapped") (Object.tapped obj) TapState.Tapped
              Spec.assertEqWith s (label <> ": CR 614.1c it chose blue") (Object.chosenColor obj) (Just Color.Blue)
          _ -> Spec.assertFailure s (label <> ": the artifact did not reach the battlefield")
    -- OVER-DETERMINED BY DESIGN, and named as such: this passes under the broken
    -- payload too. Its job is to catch a mis-indexing regression in the answerers
    -- migrated to ReplacementEntry, not to prove anything about #74.
    assertBoth "tapped first" (pickRewrite EntryRewrite.Tapped)
    assertBoth "colour first" (pickRewrite EntryRewrite.ChooseColor)

-- CR 614.1c with CR 120.3a. Stuffy Doll, {5} Artifact Creature -- Construct 0/1,
-- whole text: "Indestructible / As this creature enters, choose a player. /
-- Whenever this creature is dealt damage, it deals that much damage to the chosen
-- player. / {T}: This creature deals 1 damage to itself." (oracle checked on
-- Scryfall)
--
-- The first card whose as-enters choice is a PLAYER, and the first whose payload
-- reads one back. Both halves are proved by ONE observable -- whose life total
-- moved -- so a stamp with no reader, and a reader with no stamp, both fail.
--
-- THREE SEATS, and the pair of boards differs in exactly one thing: WHOM the
-- ChoosePlayer answer names. Two seats would collapse "the chosen player" onto
-- the only opponent, and the assertion would pass under a hard-coded "an
-- opponent" as happily as under the field.
--
-- The amount is 3, which is neither the Doll's power (0) nor its toughness (1)
-- nor the {T} ability's 1, so a payload reading a characteristic instead of CR
-- 603.2's captured binding fails rather than passing by luck.
stuffyDollSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
stuffyDollSpec s registry =
  let resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
      settleAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.settleForPriority)
      -- A noncombat event's own path, enrageSpec's: applyDamage records the
      -- DamageDealt entries, the settle puts what they triggered on the stack
      -- (CR 603.3), and the priority loop resolves it.
      dealing events gs = resolveAll (settleAll (S.runPure S.identityAnswer gs (Damage.applyDamage events)))
      noncombat src target amount = DamageEvent.MkDamageEvent src (Recipient.ToCreature target) amount False False False 0 Nothing DamageKind.Noncombat
      lives g = (S.lifeOf S.alice g, S.lifeOf S.bob g, S.lifeOf S.carol g)
      chosenOn oid g = Game.lookupObject oid g >>= Object.chosenPlayer
      -- alice casts the Doll off five Mountains on a three-seat board and answers
      -- CR 614.12a's choice with `who`. The Doll must be CAST: S.addCreature puts
      -- an object straight onto the battlefield without running the entry loop,
      -- so it would choose nobody.
      --
      -- The answer is pinned to a PlayerId by identity rather than by an index
      -- into the offer: an answerer that searched the candidate list for a legal
      -- seat would find a different one after a mutation and repair the assertion.
      castDoll :: Printing.Printing -> Printing.Printing -> PlayerId.PlayerId -> (GameState.GameState, Maybe ObjectId.ObjectId, [Response.Response])
      castDoll doll mountain who =
        let (withCard, oid) = S.handOne doll (S.landsFor mountain S.alice 5 S.threePlayerGame)
            step :: Prompt.Prompt r -> r
            step p = case p of
              Prompt.ChoosePlayer {} -> who
              _ -> S.identityAnswer p
            ((_, after), answers) = Replay.record step withCard (S.cast S.alice oid >> Stack.resolveTop)
            entered = case Set.toList (Set.difference (GameState.battlefield after) (GameState.battlefield withCard)) of
              o : _ -> Just o
              [] -> Nothing
         in (after, entered, answers)
   in Spec.describe s "Stuffy Doll (CR 614.1c)" $ do
        -- THE PROVING CASE, and a pair of boards differing only in the answer.
        Spec.it s "CR 614.1c the chosen player, and only they, take the damage" $ do
          doll <- S.printingOf s registry "Stuffy Doll"
          mountain <- S.printingOf s registry "Mountain"
          piker <- S.printingOf s registry "Goblin Piker"
          let hit who = case castDoll doll mountain who of
                (gs, Just dollId, _) ->
                  let (pikerId, board) = S.addCreature piker S.bob gs
                   in Just (board, dollId, dealing [noncombat pikerId dollId 3] board)
                _ -> Nothing
          case (hit S.bob, hit S.carol) of
            (Just (before, dollId, chosenBob), Just (_, _, chosenCarol)) -> do
              Spec.assertEqWith s "all three seats start at 20" (lives before) (Just 20, Just 20, Just 20)
              Spec.assertEqWith s "CR 120.3a bob was chosen, so bob loses 3" (lives chosenBob) (Just 20, Just 17, Just 20)
              -- The same board and the same amount, one different answer.
              Spec.assertEqWith s "carol chosen instead, so carol loses 3" (lives chosenCarol) (Just 20, Just 20, Just 17)
              -- CR 120.3e: the damage really landed on the Doll, so the two
              -- assertions above are this trigger and not bookkeeping.
              Spec.assertEqWith s "CR 120.3e and the 3 is marked on the Doll" (fmap Object.damage (Game.lookupObject dollId chosenBob)) (Just 3)
              -- CR 702.12b: 3 over a toughness of 1 is lethal, and indestructible
              -- keeps it on the battlefield anyway.
              Spec.assertBool s (Set.member dollId (GameState.battlefield chosenBob)) "CR 702.12b indestructible: still on the battlefield"
            _ -> Spec.assertFailure s "the Doll did not reach the battlefield"
        -- The STAMP, asserted independently of the payload, so a JSON typo in the
        -- trigger cannot hide behind a green read-back -- and the other way round.
        Spec.it s "CR 614.12a the choice is made before the permanent enters" $ do
          doll <- S.printingOf s registry "Stuffy Doll"
          mountain <- S.printingOf s registry "Mountain"
          case castDoll doll mountain S.carol of
            (gs, Just dollId, answers) -> do
              Spec.assertEqWith s "CR 614.1c the Doll remembers carol" (chosenOn dollId gs) (Just S.carol)
              -- The SECOND INVARIANT, asserted directly: the engine did not pick a
              -- seat, it asked. Three seats make CR 102.1's offer three wide, so
              -- the prompt is a real question rather than an elided one.
              Spec.assertBool s (List.elem (Response.ChosePlayer S.carol) answers) "CR 614.12a the engine raised a ChoosePlayer prompt"
            _ -> Spec.assertFailure s "the Doll did not reach the battlefield"
        -- CR 400.7: a NEW object forgets the choice. Object.newIncarnation is a
        -- record UPDATE, so omitting the field there compiles and silently carries
        -- a chosen player across a zone change; -Werror cannot name that site, and
        -- this is what stands in its place. Mirrors Pawl.GameSpec's Painter's
        -- Servant chosenColor case.
        Spec.it s "CR 400.7 a new incarnation has chosen nobody" $ do
          doll <- S.printingOf s registry "Stuffy Doll"
          mountain <- S.printingOf s registry "Mountain"
          case castDoll doll mountain S.bob of
            (gs, Just dollId, _) -> do
              -- The discriminator: without it an assertion over an object that
              -- never chose anybody would pass whatever newIncarnation does.
              Spec.assertEqWith s "the object going in genuinely carried a chosen player" (chosenOn dollId gs) (Just S.bob)
              Spec.assertEqWith
                s
                "CR 400.7 the rebuilt object forgot the player it chose"
                (fmap (Object.chosenPlayer . Object.newIncarnation) (Game.lookupObject dollId gs))
                (Just Nothing)
            _ -> Spec.assertFailure s "the Doll did not reach the battlefield"
        -- The fourth clause, and the one that makes the card self-contained: {T}
        -- deals 1 to itself, which re-enters the trigger with a DIFFERENT number.
        -- 1 against the case above's 3 separates "reads the event" from "reads a
        -- constant".
        Spec.it s "CR 120.1 the Doll's own {T} ability feeds its own trigger" $ do
          doll <- S.printingOf s registry "Stuffy Doll"
          mountain <- S.printingOf s registry "Mountain"
          case castDoll doll mountain S.bob of
            (gs, Just dollId, _) -> do
              -- CR 302.6: the Doll was cast this turn, so its {T} ability is
              -- unactivatable until its controller's next turn begins. Settled by
              -- hand rather than by driving a turn cycle, which would add a draw
              -- step and CR 104.3c to a case about one printed clause.
              let unsick g = g {GameState.objects = Map.adjust (\o -> o {Object.sickness = Sickness.Settled S.alice}) dollId (GameState.objects g)}
                  activated = S.runPure S.identityAnswer (unsick gs) (Activate.activateAbility S.alice dollId (theAbility doll))
                  after = resolveAll (settleAll activated)
              Spec.assertEqWith s "CR 120.3a bob loses exactly 1, not 3" (lives after) (Just 20, Just 19, Just 20)
              Spec.assertEqWith s "CR 120.3e and 1 is marked on the Doll" (fmap Object.damage (Game.lookupObject dollId after)) (Just 1)
            _ -> Spec.assertFailure s "the Doll did not reach the battlefield"

-- CR 122.1 / 122.6 with CR 614.1: Vorinclex, Monstrous Raider ({4}{G}{G}
-- Legendary Creature -- Phyrexian Praetor 6/6, "Trample, haste. If you would put
-- one or more counters on a permanent or player, put twice that many . . .
-- instead. If an opponent would put one or more counters on a permanent or
-- player, they put half that many . . . instead, rounded down.").
--
-- The card the player-counter funnel needed: its own ruling says it "cares deeply
-- about who is putting the counters", so a board that reads the RECIPIENT instead
-- answers differently on every case below but the first.
--
-- Every count is chosen so the three readings differ: three energy is six
-- doubled, one halved and three unreplaced, and one poison counter is two, zero
-- and one.
vorinclexSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
vorinclexSpec s registry = Spec.describe s "Vorinclex, Monstrous Raider (CR 122.1)" $ do
  let -- THREE seats: at two, "an opponent" and "the other player" are the same
      -- seat, and carol is what tells CR 122.6a's putter apart from a reading
      -- that asks who is affected.
      --
      -- `withVorinclex` is the only difference between a case and its control --
      -- same printing, same seat, same trigger.
      board withVorinclex printing pid = do
        vorinclex <- S.printingOf s registry "Vorinclex, Monstrous Raider"
        let seated = if withVorinclex then snd (S.addCreature vorinclex S.alice S.threePlayerGame) else S.threePlayerGame
            (oid, entered) = S.entersWithTrigger printing pid seated
        pure (oid, S.runPure S.identityAnswer entered (Engine.settleForPriority >> Stack.resolveTop >> Engine.settleForPriority))
      energyIn = S.playerCounterOf PlayerCounterKind.Energy S.alice
      poisonIn g = (S.playerCounterOf PlayerCounterKind.Poison S.alice g, S.playerCounterOf PlayerCounterKind.Poison S.bob g, S.playerCounterOf PlayerCounterKind.Poison S.carol g)
  -- CR 122.6: the gain reaches the CR 616.1 loop at all. Without the funnel the
  -- energy lands unreplaced at three, which is the control below.
  Spec.it s "CR 614.1 alice's own three energy are doubled to six" $ do
    sage <- S.printingOf s registry "Sage of Shaila's Claim"
    (_, doubled) <- board True sage S.alice
    (_, plain) <- board False sage S.alice
    Spec.assertEqWith s "twice that many" (energyIn doubled) 6
    Spec.assertEqWith s "and three without the praetor" (energyIn plain) 3
  -- CR 107.1a's rounding. bob is putting these on HIMSELF, so this case does not
  -- separate the putter from the recipient -- both are bob, and either reading
  -- halves. What it does prove is the halving clause and its rounding: three is
  -- one, not two and not three. The putter axis is the Ichor Rats case below.
  Spec.it s "CR 107.1a bob's three energy are halved to one, rounded down" $ do
    sage <- S.printingOf s registry "Sage of Shaila's Claim"
    (_, halved) <- board True sage S.bob
    (_, plain) <- board False sage S.bob
    Spec.assertEqWith s "half of three, rounded down" (S.playerCounterOf PlayerCounterKind.Energy S.bob halved) 1
    Spec.assertEqWith s "and three without the praetor" (S.playerCounterOf PlayerCounterKind.Energy S.bob plain) 3
    Spec.assertEqWith s "alice, who put none on herself, has none" (energyIn halved) 0
  -- CR 122.6a: alice is the one PUTTING all three counters, so all three are
  -- doubled -- including the ones bob and carol receive, who are her opponents.
  -- This is the case a recipient-based reading gets wrong in both directions at
  -- once: bob's and carol's would be halved to zero instead.
  Spec.it s "CR 122.6a alice's Ichor Rats poisons the whole table twice over" $ do
    ichorRats <- S.printingOf s registry "Ichor Rats"
    (rats, doubled) <- board True ichorRats S.alice
    (_, plain) <- board False ichorRats S.alice
    Spec.assertBool s (S.onBattlefield rats doubled) "the Rats are on the battlefield"
    Spec.assertEqWith s "twice that many, for opponents too" (poisonIn doubled) (2, 2, 2)
    Spec.assertEqWith s "one each without the praetor" (poisonIn plain) (1, 1, 1)
  -- CR 614.1a's "instead" taken to zero: half of one, rounded down, is a
  -- replacement that removes the event rather than resizing it. The Rats still
  -- entered and their trigger still resolved, which is what separates this from a
  -- trigger that never ran.
  Spec.it s "CR 107.1a bob's Ichor Rats poisons nobody: half of one is zero" $ do
    ichorRats <- S.printingOf s registry "Ichor Rats"
    (rats, erased) <- board True ichorRats S.bob
    (_, plain) <- board False ichorRats S.bob
    Spec.assertBool s (S.onBattlefield rats erased) "the Rats are on the battlefield"
    Spec.assertEqWith s "nobody is poisoned" (poisonIn erased) (0, 0, 0)
    Spec.assertEqWith s "one each without the praetor" (poisonIn plain) (1, 1, 1)
  -- CR 122.6's OBJECT half, on the same axis: alice casts Battlegrowth at bob's
  -- creature, so the counters go on a permanent she does not control and are
  -- doubled anyway. Doubling Season's clause -- which reads whose permanent it is
  -- -- would leave this one alone.
  Spec.it s "CR 122.6 alice doubles a counter she puts on bob's creature" $ do
    forest <- S.printingOf s registry "Forest"
    battlegrowth <- S.printingOf s registry "Battlegrowth"
    vorinclex <- S.printingOf s registry "Vorinclex, Monstrous Raider"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    let (gs, spellId, mine, theirs) = counterBoard forest battlegrowth [vorinclex] [pikerPrinting]
        (bare, bareSpell, _, bareTheirs) = counterBoard forest battlegrowth [] [pikerPrinting]
    case (mine, theirs, bareTheirs) of
      (praetor : _, piker : _, barePiker : _) -> do
        Spec.assertEqWith s "1 * 2" (countersOn CounterKind.PlusOnePlusOne piker (castAndResolve (raceAnswer praetor piker) gs spellId)) 2
        Spec.assertEqWith s "and one without the praetor" (countersOn CounterKind.PlusOnePlusOne barePiker (castAndResolve (raceAnswer barePiker barePiker) bare bareSpell)) 1
      _ -> Spec.assertFailure s "fixture did not build both boards"
  -- The object half's other direction, and the pair the byWhom check on that arm
  -- turns on: BOB casts the Battlegrowth, at his own creature, with alice's
  -- praetor watching. Half of one counter is none. The two boards differ in the
  -- praetor alone.
  Spec.it s "CR 122.6 bob's counter on bob's own creature is halved away" $ do
    forest <- S.printingOf s registry "Forest"
    battlegrowth <- S.printingOf s registry "Battlegrowth"
    vorinclex <- S.printingOf s registry "Vorinclex, Monstrous Raider"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    let bobsBoard withVorinclex =
          let (_, g1) = S.addCreature forest S.bob (S.landsInPlay forest 1)
              (piker, g2) = S.addCreature pikerPrinting S.bob g1
              g3 = if withVorinclex then snd (S.addCreature vorinclex S.alice g2) else g2
              (spell, g4) = S.addHandCard battlegrowth S.bob g3
           in (piker, S.runPure (raceAnswer piker piker) g4 (S.cast S.bob spell >> Stack.resolveTop))
        (halvedPiker, halved) = bobsBoard True
        (plainPiker, plain) = bobsBoard False
    Spec.assertEqWith s "half of one, rounded down" (countersOn CounterKind.PlusOnePlusOne halvedPiker halved) 0
    Spec.assertEqWith s "and one without the praetor" (countersOn CounterKind.PlusOnePlusOne plainPiker plain) 1
  -- The negative control for CounterPattern.onWho: Doubling Season says "on a
  -- permanent you control", which no player is, so alice's own energy is
  -- untouched by it. Same seat and same trigger as the doubling case above.
  Spec.it s "CR 614.16 Doubling Season does not reach a player's counters" $ do
    doublingSeason <- S.printingOf s registry "Doubling Season"
    sage <- S.printingOf s registry "Sage of Shaila's Claim"
    let (_, seated) = S.addCreature doublingSeason S.alice S.threePlayerGame
        (_, entered) = S.entersWithTrigger sage S.alice seated
        after = S.runPure S.identityAnswer entered (Engine.settleForPriority >> Stack.resolveTop >> Engine.settleForPriority)
    Spec.assertEqWith s "three, not six" (S.playerCounterOf PlayerCounterKind.Energy S.alice after) 3
  -- CR 122.6a's default putter, which is the one thing the cases above cannot
  -- see: an entering permanent's counters are put on by ITS controller, so bob's
  -- riot counter is halved by alice's praetor. A putter read off the active
  -- player, or off the applying row's own controller, is alice here and would
  -- DOUBLE the counter instead.
  --
  -- The goblin ends with neither the counter nor haste, which is what taking
  -- CR 702.136a's first half and having it halved away means.
  Spec.it s "CR 702.136a bob's riot counter is halved away before it lands" $ do
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    vorinclex <- S.printingOf s registry "Vorinclex, Monstrous Raider"
    zhurTaa <- S.printingOf s registry "Zhur-Taa Goblin"
    let goblinBoard withVorinclex =
          let (_, g1) = S.addCreature mountain S.bob (S.landsInPlay forest 1)
              (_, g2) = S.addCreature forest S.bob g1
              g3 = if withVorinclex then snd (S.addCreature vorinclex S.alice g2) else g2
              (held, g4) = S.addHandCard zhurTaa S.bob g3
              after = S.runPure (riotChoosing OptionalDecision.Exercises) g4 (S.cast S.bob held >> Stack.resolveTop)
           in (newestNamed (CardName.MkCardName $ Text.pack "Zhur-Taa Goblin") after, after)
    case (goblinBoard True, goblinBoard False) of
      ((Just halvedGoblin, halved), (Just plainGoblin, plain)) -> do
        Spec.assertEqWith s "half of one, rounded down" (countersOn CounterKind.PlusOnePlusOne halvedGoblin halved) 0
        Spec.assertBool s (not (Projection.hasKeyword Keyword.Haste halvedGoblin halved)) "and no haste: the counter was taken, not declined"
        Spec.assertEqWith s "and one without the praetor" (countersOn CounterKind.PlusOnePlusOne plainGoblin plain) 1
      _ -> Spec.assertFailure s "the goblin did not reach the battlefield"

-- CR 120.3b / 120.3d with CR 122.6: the counters a DAMAGE event causes are put on
-- through the same two placement funnels every other counter goes through, so a
-- counter replacement reaches them.
--
-- Ichor Rats ({1}{B}{B} Creature -- Phyrexian Rat 2/1, "Infect. When this
-- creature enters, each player gets a poison counter.") is the source, and its two
-- power is what keeps the three readings apart: two unreplaced, four doubled, one
-- halved. One power would make the halved reading zero, which no board can tell
-- from a placement that never happened.
--
-- The praetor's SEAT is the only difference between each pair, and it is the axis
-- CR 120.3b and CR 120.3d both name: "that source's controller" puts these
-- counters, so alice's praetor doubles them wherever they land, and bob's halves
-- the ones he himself receives. A reading that asked whose permanent or player
-- was AFFECTED would double bob's boards instead.
--
-- Three seats, so the praetor's controller can be a third party to the placement
-- rather than always one of its two ends.
damageCountersSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
damageCountersSpec s registry = Spec.describe s "Counters damage causes (CR 120.3b, CR 120.3d)" $ do
  let -- alice attacks bob with one Ichor Rats; `watcher` seats that card under that
      -- player, Nothing being the control board. bob gets one creature per printing
      -- in `blockers` and blocks with all of them.
      board watcher blockers = do
        rats <- S.printingOf s registry "Ichor Rats"
        seat <- Monad.mapM (\(name, _) -> S.printingOf s registry name) watcher
        printings <- Monad.mapM (S.printingOf s registry) blockers
        let (gs0, mine, theirs, _) = S.threePlayerCombat [rats] printings []
            seated = case (seat, watcher) of
              (Just printing, Just (_, pid)) -> snd (S.addCreature printing pid gs0)
              _ -> gs0
        pure $ case mine of
          [rat] -> Just (theirs, S.runCombat (fights rat theirs) seated)
          _ -> Nothing
      -- ONLY the Rats attack and only bob's `blockers` block, so a hasty praetor on
      -- either side neither attacks nor blocks and the boards differ in nothing but
      -- the replacement. With one blocker CR 510.1c forces the whole assignment, so
      -- no division is asked for.
      fights :: ObjectId.ObjectId -> [ObjectId.ObjectId] -> Prompt.Prompt r -> r
      fights rat blockers p = case p of
        Prompt.DeclareAttackers _ _ ids -> filter (== rat) ids
        Prompt.DeclareBlockers _ _ mine _ ->
          Map.fromList (fmap (\b -> (b, Set.singleton rat)) (filter (\b -> elem b blockers) mine))
        Prompt.ChooseDefender {} -> S.bob
        _ -> S.identityAnswer p
      poisonOn = S.playerCounterOf PlayerCounterKind.Poison
      shrunk = countersOn CounterKind.MinusOneMinusOne
  Spec.it s "CR 120.3b alice's praetor doubles the poison her Ichor Rats deals bob" $ do
    doubled <- board (Just ("Vorinclex, Monstrous Raider", S.alice)) []
    plain <- board Nothing []
    case (doubled, plain) of
      (Just (_, twice), Just (_, once)) -> do
        Spec.assertEqWith s "twice that many" (poisonOn S.bob twice) 4
        Spec.assertEqWith s "and two without the praetor" (poisonOn S.bob once) 2
        Spec.assertEqWith s "CR 120.3b's poison instead of the life loss" (S.lifeOf S.bob twice) (Just 20)
      _ -> Spec.assertFailure s "fixture did not build both boards"
  -- The putter axis: bob's own praetor HALVES the poison bob receives, because
  -- alice is the one putting it. This is the case a recipient-based reading gets
  -- backwards -- bob is its controller's "you", so it would double instead.
  Spec.it s "CR 120.3b bob's praetor halves the poison he receives" $ do
    halved <- board (Just ("Vorinclex, Monstrous Raider", S.bob)) []
    plain <- board Nothing []
    case (halved, plain) of
      (Just (_, half), Just (_, once)) -> do
        Spec.assertEqWith s "half of two, rounded down" (poisonOn S.bob half) 1
        Spec.assertEqWith s "and two without the praetor" (poisonOn S.bob once) 2
      _ -> Spec.assertFailure s "fixture did not build both boards"
  -- CR 120.3d's creature half, on the same axis. Wall of Stone ({1}{R}{R} Creature
  -- -- Wall 0/8, defender) is the blocker: it survives every reading, so the count
  -- is readable rather than gone with the creature, and its zero power assigns no
  -- damage back for CR 704.5h to act on.
  Spec.it s "CR 120.3d alice's praetor doubles the -1/-1 counters her Ichor Rats causes" $ do
    doubled <- board (Just ("Vorinclex, Monstrous Raider", S.alice)) ["Wall of Stone"]
    plain <- board Nothing ["Wall of Stone"]
    case (doubled, plain) of
      (Just ([wall], twice), Just ([bare], once)) -> do
        Spec.assertEqWith s "twice that many" (shrunk wall twice) 4
        Spec.assertEqWith s "and two without the praetor" (shrunk bare once) 2
        Spec.assertEqWith s "CR 120.3d's counters instead of marked damage" (S.damageOf wall twice) (Just 0)
      _ -> Spec.assertFailure s "fixture did not build both boards"
  -- The creature half's putter axis, and the pair that settles it: the Wall is
  -- BOB's permanent, so a recipient-based reading would double these four times
  -- over. Alice is putting them, so bob's praetor halves them.
  Spec.it s "CR 120.3d bob's praetor halves the counters put on his own Wall of Stone" $ do
    halved <- board (Just ("Vorinclex, Monstrous Raider", S.bob)) ["Wall of Stone"]
    plain <- board Nothing ["Wall of Stone"]
    case (halved, plain) of
      (Just ([wall], half), Just ([bare], once)) -> do
        Spec.assertEqWith s "half of two, rounded down" (shrunk wall half) 1
        Spec.assertEqWith s "and two without the praetor" (shrunk bare once) 2
      _ -> Spec.assertFailure s "fixture did not build both boards"
  -- CR 614.16 versus CR 614.1, which is what CounterCause.ByRule settles here: rule
  -- 120.3's results are dictated by the rules, so Doubling Season -- "if an EFFECT
  -- would put one or more counters on a permanent you control" -- does not reach
  -- them, where the praetor's clauses name a player and do. Bob controls the Season
  -- and the Wall, so the only thing keeping it off is the cause.
  --
  -- Not vacuous: the cases above are the same board with the praetor in the same
  -- seat, and they move the count in both directions.
  Spec.it s "CR 614.16 Doubling Season does not reach the counters damage causes" $ do
    seasoned <- board (Just ("Doubling Season", S.bob)) ["Wall of Stone"]
    plain <- board Nothing ["Wall of Stone"]
    case (seasoned, plain) of
      (Just ([wall], watched), Just ([bare], once)) -> do
        Spec.assertEqWith s "two, not four" (shrunk wall watched) 2
        Spec.assertEqWith s "the same two the bare board shows" (shrunk bare once) 2
      _ -> Spec.assertFailure s "fixture did not build both boards"

-- CR 122.6a with CR 701.53a: the counters an EFFECT says a token enters the
-- battlefield with. "To incubate N, create an Incubator token that enters the
-- battlefield with N +1/+1 counters on it" (CR 701.53a) is the pool's only
-- wording that writes Pawl.Types.EntryRiders' `counters` onto an Effect.Create --
-- undying and persist write it onto a MoveToZone -- and Eyes of Gitaxias
-- ({2}{U} Sorcery, "Incubate 3. Draw a card.") is the producer picked for it: its
-- other sentence asks for no choice, and its THREE is the count that tells the
-- readings apart.
--
-- Six doubled, one halved and three unreplaced are three different numbers, which
-- is what a board with one or two counters could not say.
--
-- The token is CR 111.10i's predefined Incubator token: a colorless Incubator
-- artifact with "{2}: Transform this token", whose back face is a 0/0 colorless
-- Phyrexian artifact creature named Phyrexian Token. The transform case is what
-- makes the count matter rather than merely be readable: a 0/0 wearing these
-- counters is the P/T the rest of the game sees.
--
-- Every assertion reads a permanent on the BATTLEFIELD: a token that left it
-- would cease to exist (CR 111.7) and take the assertion with it.
entryCountersSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
entryCountersSpec s registry = Spec.describe s "The counters a Create says its token enters with (CR 122.6a)" $ do
  let incubatorName = CardName.MkCardName (Text.pack "Incubator Token")
      phyrexianName = CardName.MkCardName (Text.pack "Phyrexian Token")
      -- BOTH seats hold five untapped Islands, so `caster` moves who is paying and
      -- nothing else; `watcher` seats one printing under one player, and Nothing is
      -- the control board. Five rather than three, because the transform case below
      -- pays {2} after the {2}{U}.
      --
      -- The caster's library is stocked because the sorcery's second sentence draws
      -- (CR 104.3c).
      board caster watcher = do
        island <- S.printingOf s registry "Island"
        eyes <- S.printingOf s registry "Eyes of Gitaxias"
        seat <- Monad.mapM (\(name, _) -> S.printingOf s registry name) watcher
        let bothSeated = List.foldl' (\g _ -> snd (S.addCreature island S.bob g)) (S.landsInPlay island 5) [1 .. 5 :: Int]
            seated = case (seat, watcher) of
              (Just printing, Just (_, pid)) -> snd (S.addCreature printing pid bothSeated)
              _ -> bothSeated
            (held, g1) = S.addHandCard eyes caster seated
            (_, g2) = S.addLibraryCard island caster g1
            after = S.runPure S.identityAnswer g2 (S.cast caster held >> Stack.resolveTop)
        pure (newestNamed incubatorName after, after)
      plusOnes = countersOn CounterKind.PlusOnePlusOne
      -- The token's own "{2}: Transform this token" (CR 111.10i), activated by the
      -- player who created it -- CR 111.2 makes that its controller -- and resolved.
      flipToken pid oid gs = case Game.faceOf oid gs >>= Maybe.listToMaybe . Face.activatedAbilities of
        Nothing -> gs
        Just ability -> S.runPure S.identityAnswer gs (Activate.activateAbility pid oid ability >> Stack.resolveTop)
  -- CR 614.16 reaches the placement at all, which is the whole of what the rider
  -- being routed through Event.putCounters buys: without it the token arrives with
  -- the three the effect asked for and no praetor can move them.
  Spec.it s "CR 122.6a alice's praetor doubles the three counters her Incubator token enters with" $ do
    (doubledToken, doubled) <- board S.alice (Just ("Vorinclex, Monstrous Raider", S.alice))
    (plainToken, plain) <- board S.alice Nothing
    case (doubledToken, plainToken) of
      (Just twice, Just once) -> do
        Spec.assertEqWith s "twice that many" (plusOnes twice doubled) 6
        Spec.assertEqWith s "and three without the praetor" (plusOnes once plain) 3
      _ -> Spec.assertFailure s "the token did not reach the battlefield"
  -- CR 122.6a's default putter: "if the effect doesn't specify a player, the
  -- object's controller puts those counters on it", and CR 111.2 makes that bob.
  -- So ALICE's praetor halves them, which is the direction a putter read off the
  -- praetor's own controller gets backwards -- it would double these instead.
  Spec.it s "CR 107.1a alice's praetor halves the counters on bob's Incubator token" $ do
    (halvedToken, halved) <- board S.bob (Just ("Vorinclex, Monstrous Raider", S.alice))
    (plainToken, plain) <- board S.bob Nothing
    case (halvedToken, plainToken) of
      (Just half, Just once) -> do
        Spec.assertEqWith s "half of three, rounded down" (plusOnes half halved) 1
        Spec.assertEqWith s "and three without the praetor" (plusOnes once plain) 3
      _ -> Spec.assertFailure s "the token did not reach the battlefield"
  -- What the counters are FOR: CR 111.10i's back face is a 0/0, so the count the
  -- funnel settled is the creature's power and toughness once the token turns over
  -- (CR 613.4c). Six and three, from the same pair of boards as the first case.
  Spec.it s "CR 701.53a the doubled counters are the transformed token's power and toughness" $ do
    (doubledToken, doubled) <- board S.alice (Just ("Vorinclex, Monstrous Raider", S.alice))
    (plainToken, plain) <- board S.alice Nothing
    case (doubledToken, plainToken) of
      (Just twice, Just once) -> do
        let flipped = flipToken S.alice twice doubled
            bare = flipToken S.alice once plain
        Spec.assertEqWith s "the token turned over" (fmap Face.name (Game.faceOf twice flipped)) (Just phyrexianName)
        Spec.assertEqWith s "0/0 plus six counters" (S.powerToughnessOf twice flipped) (Just (6, 6))
        Spec.assertEqWith s "and 0/0 plus three without the praetor" (S.powerToughnessOf once bare) (Just (3, 3))
      _ -> Spec.assertFailure s "the token did not reach the battlefield"

-- CR 122.1c: the replacement and the prevention effect one or more shield counters
-- create. Gameplay-level throughout: Swooping Protector is cast and enters with its
-- counter through the CR 122.6 funnel, and every spell aimed at it afterwards is a
-- real card cast and resolved.
--
-- The two effects are proven SEPARATELY, and that separation is the point rather
-- than tidiness: a board where a shielded creature merely survives cannot tell "the
-- destruction was replaced" from "the damage was prevented". So the destruction
-- cases destroy without dealing damage (Doom Blade) and the damage cases deal damage
-- without destroying -- Lightning Bolt's 3 kills a 2/1 only through CR 704.5g, which
-- is a rule's destruction and reaches the shield through neither sentence.
shieldCounterSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
shieldCounterSpec s registry = Spec.describe s "Shield counters (CR 122.1c)" $ do
  let protectorName = CardName.MkCardName (Text.pack "Swooping Protector")
      -- alice CASTS the bird rather than having it placed, so its counter arrives
      -- through Event.putCounters (CR 122.6's as-it-enters clause) and a scaling
      -- replacement can reach it. Four Plains pay the {3}{W}; `extra` seats the
      -- lands whatever spell the case aims at the bird needs, and `scaler` seats a
      -- counter-scaling permanent under a named player.
      board extra scaler = do
        plains <- S.printingOf s registry "Plains"
        protector <- S.printingOf s registry "Swooping Protector"
        extras <- Monad.mapM (S.printingOf s registry) extra
        seat <- Monad.mapM (\(name, _) -> S.printingOf s registry name) scaler
        let landed = List.foldl' (\g p -> snd (S.addCreature p S.alice g)) (S.landsInPlay plains 4) extras
            seated = case (seat, scaler) of
              (Just printing, Just (_, pid)) -> snd (S.addCreature printing pid landed)
              _ -> landed
            (held, g1) = S.addHandCard protector S.alice seated
            after = S.runPure S.identityAnswer g1 (S.cast S.alice held >> Stack.resolveTop)
        pure (newestNamed protectorName after, after)
      shields = countersOn CounterKind.Shield
      -- One of alice's cards, cast at the bird from her hand and resolved.
      castAt victim printing gs =
        let (held, g1) = S.addHandCard printing S.alice gs
         in S.runPure (raceAnswer victim victim) g1 (S.cast S.alice held >> Stack.resolveTop)
      -- One noncombat damage event, from `src`, at `n`.
      hit src recipient n =
        DamageEvent.MkDamageEvent src recipient n False False False 0 Nothing DamageKind.Noncombat
      amounts gs = fmap DamageEvent.amount (S.damageEventsOf gs)
      -- alice's Palace Guard with `n` shield counters written onto it, bob's
      -- Spider-Punk beside it when `withPunk`, and two Mountains for the Bolt the
      -- CR 615.12 cases below aim at it. A 1/4 rather than the bird because a
      -- permanent that DIES to the unprevented damage reads 0 counters under
      -- either reading of the rule (CR 122.2), which tells them apart not at all.
      guardBoard withPunk n = do
        mountain <- S.printingOf s registry "Mountain"
        guardPrinting <- S.printingOf s registry "Palace Guard"
        punkPrinting <- S.printingOf s registry "Spider-Punk"
        let (guard_, g1) = S.addCreature guardPrinting S.alice (S.landsInPlay mountain 2)
            shielded = S.addCounter CounterKind.Shield n guard_ g1
        pure (guard_, if withPunk then snd (S.addCreature punkPrinting S.bob shielded) else shielded)
      -- Spend the counter on `src`'s hit first (CR 101.4c), keyed on the SOURCE
      -- id rather than on a batch position, so the assertion does not depend on
      -- the order the batch was gathered in.
      counterFirst :: ObjectId.ObjectId -> Prompt.Prompt r -> r
      counterFirst src p = case p of
        Prompt.OrderDamage _ _ events ->
          let key e = (DamageEvent.source e /= src, DamageEvent.source e)
           in fmap fst (List.sortOn (key . snd) (zip [0 ..] events))
        _ -> S.identityAnswer p
  -- CR 122.6 / 614.16: the counter goes through the placement funnel, so the two
  -- replacements that scale a placement reach it. Three DISTINCT counts off one
  -- card -- doubled, unreplaced and halved -- which is what separates "the funnel
  -- was used" from "the number was written onto the object".
  Spec.it s "CR 122.6 the shield counter the bird enters with runs the placement funnel" $ do
    (doubledBird, doubled) <- board [] (Just ("Doubling Season", S.alice))
    (plainBird, plain) <- board [] Nothing
    (halvedBird, halved) <- board [] (Just ("Vorinclex, Monstrous Raider", S.bob))
    case (doubledBird, plainBird, halvedBird) of
      (Just twice, Just once, Just half) -> do
        Spec.assertEqWith s "Doubling Season: twice one" (shields twice doubled) 2
        Spec.assertEqWith s "unreplaced: the printed one" (shields once plain) 1
        Spec.assertEqWith s "bob's praetor halves alice's placement, rounded down" (shields half halved) 0
      _ -> Spec.assertFailure s "the bird did not reach the battlefield"
  -- CR 122.1c's SECOND sentence: "if damage would be dealt to this permanent,
  -- prevent that damage and remove a shield counter from it". Lightning Bolt's 3
  -- would be lethal to a 2/1, so an unprevented point of it is visible twice over --
  -- as marked damage and as a death.
  Spec.it s "CR 122.1c a Bolt at the bird is prevented and takes the counter" $ do
    bolt <- S.printingOf s registry "Lightning Bolt"
    (bird, entered) <- board ["Mountain"] Nothing
    case bird of
      Nothing -> Spec.assertFailure s "the bird did not reach the battlefield"
      Just oid -> do
        let once = S.settleSba (castAt oid bolt entered)
        Spec.assertBool s (Set.member oid (GameState.battlefield once)) "it survived the Bolt"
        Spec.assertEqWith s "no damage was marked (CR 615.6)" (S.damageOf oid once) (Just 0)
        Spec.assertEqWith s "and the counter paid for it" (shields oid once) 0
  -- The discriminating twin, one difference from the case above: bob's praetor
  -- halves the entry placement to nothing, so the same bird faces the same Bolt on
  -- the same board with NO counter on it. Same mana, same seats, same spell.
  Spec.it s "CR 122.1c the same Bolt kills the same bird with no counter on it" $ do
    bolt <- S.printingOf s registry "Lightning Bolt"
    (bird, entered) <- board ["Mountain"] (Just ("Vorinclex, Monstrous Raider", S.bob))
    case bird of
      Nothing -> Spec.assertFailure s "the bird did not reach the battlefield"
      Just oid -> do
        let once = S.settleSba (castAt oid bolt entered)
        Spec.assertEqWith s "setup: the halving left no shield" (shields oid entered) 0
        Spec.assertBool s (not (Set.member oid (GameState.battlefield once))) "so the Bolt killed it"
  -- CR 122.1c's FIRST sentence: "if this permanent would be destroyed as the result
  -- of an effect, instead remove a shield counter from it". Doom Blade destroys
  -- without dealing any damage, so nothing here can be mistaken for the prevention
  -- half -- and the bird is left untapped, which is how this also shows the removal
  -- is not a regeneration ("removing a shield counter in this way isn't the same as
  -- regenerating a creature"; CR 701.19a taps).
  Spec.it s "CR 122.1c Doom Blade is replaced by the counter, and the next one kills" $ do
    doomBlade <- S.printingOf s registry "Doom Blade"
    (bird, entered) <- board (replicate 4 "Swamp") Nothing
    case bird of
      Nothing -> Spec.assertFailure s "the bird did not reach the battlefield"
      Just oid -> do
        let once = S.settleSba (castAt oid doomBlade entered)
            twice = S.settleSba (castAt oid doomBlade once)
        Spec.assertBool s (Set.member oid (GameState.battlefield once)) "it survived the first Doom Blade"
        Spec.assertEqWith s "the counter paid for it" (shields oid once) 0
        Spec.assertEqWith s "and it was not regenerated" (fmap Object.tapped (Game.lookupObject oid once)) (Just TapState.Untapped)
        Spec.assertBool s (not (Set.member oid (GameState.battlefield twice))) "and the second Doom Blade killed it"
  -- Two counters, two destructions, and the third kills: the count is how many
  -- events the pair may still replace, and one counter comes off per application
  -- however many are there ("if a permanent that would be dealt damage has more than
  -- one shield counter on it ... only one shield counter is removed").
  Spec.it s "CR 122.1c Doubling Season's two counters replace two destructions" $ do
    doomBlade <- S.printingOf s registry "Doom Blade"
    (bird, entered) <- board (replicate 6 "Swamp") (Just ("Doubling Season", S.alice))
    case bird of
      Nothing -> Spec.assertFailure s "the bird did not reach the battlefield"
      Just oid -> do
        let once = S.settleSba (castAt oid doomBlade entered)
            twice = S.settleSba (castAt oid doomBlade once)
            thrice = S.settleSba (castAt oid doomBlade twice)
        Spec.assertEqWith s "one counter off, not both" (shields oid once) 1
        Spec.assertBool s (Set.member oid (GameState.battlefield twice)) "the second destruction is replaced too"
        Spec.assertEqWith s "and now there are none" (shields oid twice) 0
        Spec.assertBool s (not (Set.member oid (GameState.battlefield thrice))) "so the third kills it"
  -- CR 122.1c's "as the result of an EFFECT", as a pair of boards differing in
  -- nothing but the destruction's cause. Through the two doors rather than through
  -- gameplay because that is the only way to hold everything else equal: reaching CR
  -- 704.5g against a SHIELDED permanent needs marked damage equal to its toughness,
  -- and the prevention half stops damage being marked for as long as a counter is
  -- there, so the gameplay route has to break the prevention half first. The
  -- Spider-Punk case below is that route, and it proves the same gate a second time
  -- at gameplay level.
  Spec.it s "CR 122.1c the counter does not save the bird from a rule's destruction" $ do
    (bird, entered) <- board [] Nothing
    case bird of
      Nothing -> Spec.assertFailure s "the bird did not reach the battlefield"
      Just oid -> do
        let byEffect = S.runPure S.identityAnswer entered (Event.destroy Regenerability.Regenerable [oid])
            byRule = S.runPure S.identityAnswer entered (Event.destroyInBatch entered DestructionCause.ByRule Regenerability.Regenerable [oid])
        Spec.assertEqWith s "setup: one shield counter, on both boards" (shields oid entered) 1
        Spec.assertBool s (Set.member oid (GameState.battlefield byEffect)) "an effect's destruction is replaced"
        Spec.assertEqWith s "spending the counter" (shields oid byEffect) 0
        Spec.assertBool s (not (Set.member oid (GameState.battlefield byRule))) "the rule's destruction is not"
        -- CR 122.2: no assertion about the dead permanent's counters. They ceased to
        -- exist with the incarnation that held them, so the id reads 0 whether the
        -- shield was spent or ignored, which tells the two apart not at all.
        Spec.assertEqWith s "and it reached its owner's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice byRule)) 1
  -- CR 122.1c is a RULE rather than an ability the permanent has: "if a creature
  -- with a shield counter loses its abilities, the shield counter will still protect
  -- it as normal". So the pair survives layer 6, which is what minting it from
  -- Object.counters rather than from the projection's ability list buys. Humility
  -- arrives AFTER the bird, so what is under test is the shield outliving the
  -- abilities and not CR 614.12's question about an entry replacement under layer 6.
  Spec.it s "CR 613.1f a Humility'd bird keeps its shield" $ do
    doomBlade <- S.printingOf s registry "Doom Blade"
    humility <- S.printingOf s registry "Humility"
    (bird, entered) <- board (replicate 2 "Swamp") Nothing
    case bird of
      Nothing -> Spec.assertFailure s "the bird did not reach the battlefield"
      Just oid -> do
        let humbled = S.withHumility humility entered
            once = S.settleSba (castAt oid doomBlade humbled)
        Spec.assertBool s (Projection.hasKeyword Keyword.Flying oid entered) "setup: the bird has flying"
        Spec.assertBool s (not (Projection.hasKeyword Keyword.Flying oid humbled)) "setup: Humility took it away"
        Spec.assertEqWith s "setup: the counter is still there" (shields oid humbled) 1
        Spec.assertBool s (Set.member oid (GameState.battlefield once)) "and the shield still replaced the destruction"
        Spec.assertEqWith s "spending the counter" (shields oid once) 0
  -- The gather's SHORT-CIRCUIT reads base faces, and a shield counter is on none of
  -- them: Projection.replacementsAffecting would answer [] for a board whose only
  -- replacement is CR 122.1c's, so this case is what makes that disjunct
  -- load-bearing rather than a fence. Every producer in the pool is itself an entry
  -- replacement and so passes the short-circuit on its own printed text, which is
  -- why the counter here is written on directly -- a Goblin Piker prints nothing at
  -- all.
  Spec.it s "CR 122.1c a shield on a permanent that prints no replacement is still gathered" $ do
    swamp <- S.printingOf s registry "Swamp"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    doomBlade <- S.printingOf s registry "Doom Blade"
    let (pikerId, g1) = S.addCreature pikerPrinting S.alice (S.landsInPlay swamp 2)
        shielded = S.addCounter CounterKind.Shield 1 pikerId g1
        after = S.settleSba (castAt pikerId doomBlade shielded)
    Spec.assertBool s (Set.member pikerId (GameState.battlefield after)) "the Piker survived the Doom Blade"
    Spec.assertEqWith s "spending the counter" (shields pikerId after) 0
  -- CR 101.4c OVER CR 122.1c. One counter facing two simultaneous damage events is
  -- a resource that covers one of them and not the other, so which one it covers is
  -- a choice, and CR 101.4c gives it to the player making both CR 616.1 choices --
  -- "if no order is specified, the player chooses the order". The unit is the EVENT
  -- and not the amount: the counter prevents a whole event whatever its size, which
  -- is why the shield of CR 615.7 and this one are contested in different units.
  --
  -- The two answers leave DIFFERENT BOARDS, which is what makes the choice observable
  -- rather than bookkeeping: 5 and 2 at a 3/3 with one counter, so covering the 5
  -- leaves a survivor with 2 marked and covering the 2 leaves 5 marked on a creature
  -- CR 704.5g then destroys. Every number distinct -- 5, 2, toughness 3, one counter
  -- -- so no two readings of the rule land on the same board.
  --
  -- The counter is written onto a Hill Giant rather than carried by Swooping
  -- Protector because the bird's toughness of 1 makes "survived" unreachable, and
  -- survival is half of what tells the two answers apart. What is under test is
  -- which event the counter reaches and not how it got there; a real card putting
  -- it there is the CR 122.6 case at the top of this group.
  --
  -- The DAMAGE BATCH is hand-built and the shield is a real rule's, for
  -- mendingHandsSpec's reason -- and here the batch's gather order is itself the
  -- input the choice has to beat, which only a hand-built batch can state.
  Spec.it s "CR 101.4c one counter facing two simultaneous hits covers the one its controller says" $ do
    plains <- S.printingOf s registry "Plains"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    giantPrinting <- S.printingOf s registry "Hill Giant"
    let (giant, g1) = S.addCreature giantPrinting S.alice (S.landsInPlay plains 1)
        (big, g2) = S.addCreature pikerPrinting S.bob g1
        (small, g3) = S.addCreature pikerPrinting S.bob g2
        shielded = S.addCounter CounterKind.Shield 1 giant g3
        batch = [hit big (Recipient.ToCreature giant) 5, hit small (Recipient.ToCreature giant) 2]
        tookTheBig = settleDamage (counterFirst big) shielded batch
        tookTheSmall = settleDamage (counterFirst small) shielded batch
    Spec.assertEqWith s "setup: one counter, and two events it cannot both cover" (shields giant shielded) 1
    Spec.assertBool
      s
      (wasAskedToOrderDamage (answersFor S.identityAnswer shielded (Damage.applyDamage batch)))
      "alice was asked which damage the counter prevents"
    -- CR 615.6: a prevented event never happens, so the board says which of the two
    -- the counter reached twice over -- in what was marked and in what survived.
    Spec.assertEqWith s "the counter covers the 5: only the 2 happens" (amounts tookTheBig) [2]
    Spec.assertEqWith s "so 2 is marked on the 3/3" (S.damageOf giant tookTheBig) (Just 2)
    Spec.assertBool s (Set.member giant (GameState.battlefield (S.settleSba tookTheBig))) "and it survives"
    Spec.assertEqWith s "the counter covers the 2 instead: only the 5 happens" (amounts tookTheSmall) [5]
    Spec.assertEqWith s "so 5 is marked on the same 3/3" (S.damageOf giant tookTheSmall) (Just 5)
    Spec.assertBool s (not (Set.member giant (GameState.battlefield (S.settleSba tookTheSmall)))) "and CR 704.5g destroys it"
    -- CR 122.1c: one counter comes off per application either way, so the answer
    -- changes which event was covered and never how much the pair could cover.
    Spec.assertEqWith s "one counter spent either way" (shields giant tookTheBig) 0
    Spec.assertEqWith s "one counter spent either way" (shields giant tookTheSmall) 0
  -- The elision half, and the discriminating twin of the case above: two counters
  -- cover two events in any order, so there is nothing to decide and nothing is
  -- asked. One difference from that board -- the number of counters -- and the same
  -- seats, sources and amounts.
  Spec.it s "CR 122.1c two counters cover two simultaneous hits, and ask nothing" $ do
    plains <- S.printingOf s registry "Plains"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    giantPrinting <- S.printingOf s registry "Hill Giant"
    let (giant, g1) = S.addCreature giantPrinting S.alice (S.landsInPlay plains 1)
        (big, g2) = S.addCreature pikerPrinting S.bob g1
        (small, g3) = S.addCreature pikerPrinting S.bob g2
        shielded = S.addCounter CounterKind.Shield 2 giant g3
        batch = [hit big (Recipient.ToCreature giant) 5, hit small (Recipient.ToCreature giant) 2]
        after = settleDamage S.identityAnswer shielded batch
    Spec.assertBool
      s
      (not (wasAskedToOrderDamage (answersFor S.identityAnswer shielded (Damage.applyDamage batch))))
      "no OrderDamage was raised: two counters cover both events"
    Spec.assertEqWith s "neither event happened" (amounts after) []
    Spec.assertEqWith s "nothing is marked" (S.damageOf giant after) (Just 0)
    Spec.assertEqWith s "and both counters paid for it" (shields giant after) 0
    Spec.assertBool s (Set.member giant (GameState.battlefield (S.settleSba after))) "the Giant is untouched"
  -- CR 122.1c's "to THIS permanent": the pair protects the permanent its counters
  -- are on and no other recipient. Two Bolts off one board, one at bob and one at
  -- bob's Piker, so both shapes a wrongly scoped shield would reach are covered -- a
  -- damage event naming a PLAYER, whose recipient is no object at all, and one
  -- naming another creature.
  Spec.it s "CR 122.1c the shield covers its own permanent and no other recipient" $ do
    bolt <- S.printingOf s registry "Lightning Bolt"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    (bird, entered) <- board ["Mountain", "Mountain"] Nothing
    case bird of
      Nothing -> Spec.assertFailure s "the bird did not reach the battlefield"
      Just oid -> do
        let (pikerId, staged) = S.addCreature pikerPrinting S.bob entered
            castAtBob gs =
              let (held, g1) = S.addHandCard bolt S.alice gs
               in S.runPure (aimPlayer S.bob) g1 (S.cast S.alice held >> Stack.resolveTop)
            hitBob = S.settleSba (castAtBob staged)
            hitPiker = S.settleSba (castAt pikerId bolt hitBob)
        Spec.assertEqWith s "bob took the Bolt" (S.lifeOf S.bob hitBob) (fmap (subtract 3) (S.lifeOf S.bob staged))
        Spec.assertEqWith s "and the bird's counter is untouched" (shields oid hitBob) 1
        Spec.assertBool s (not (Set.member pikerId (GameState.battlefield hitPiker))) "the second Bolt killed bob's Piker"
        Spec.assertEqWith s "and the counter is still untouched" (shields oid hitPiker) 1
        Spec.assertBool s (Set.member oid (GameState.battlefield hitPiker)) "setup: the bird sat there through both"
  -- CR 615.12 with CR 122.1c: the pair's prevention half says "prevent", so CR 615.1a
  -- makes it a prevention effect and unpreventable damage is still MET by it and
  -- still prevented none of -- the Bolt lands in full and kills the 2/1. The
  -- discriminating twin is the first Bolt case above: same bird, same Bolt, and the
  -- only difference is Spider-Punk on the board.
  --
  -- The rule's MIDDLE clause takes the bird's counter off with it ("if a permanent
  -- with a shield counter is dealt unpreventable damage, that damage will be dealt
  -- and a shield counter will still be removed"), so the bird reaches CR 704.5g
  -- with nothing left to replace anything. The counter is unreadable after the
  -- fact here -- CR 122.2 -- which is why the cases below use a body that
  -- survives; the GAMEPLAY route to CR 122.1c's "as the result of an effect" is
  -- the last of them, where two Bolts leave a still-shielded permanent facing CR
  -- 704.5g.
  Spec.it s "CR 615.12 an unpreventable Bolt kills the shielded bird" $ do
    bolt <- S.printingOf s registry "Lightning Bolt"
    (bird, entered) <- board ["Mountain"] (Just ("Spider-Punk", S.bob))
    case bird of
      Nothing -> Spec.assertFailure s "the bird did not reach the battlefield"
      Just oid -> do
        let once = S.settleSba (castAt oid bolt entered)
        Spec.assertEqWith s "setup: the bird still entered with its counter" (shields oid entered) 1
        Spec.assertBool s (not (Set.member oid (GameState.battlefield once))) "and the Bolt killed it through the shield"
  -- CR 615.12's MIDDLE clause -- "those effects won't prevent any damage, but any
  -- additional effects they have will take place" -- over CR 122.1c's "prevent
  -- that damage and remove a shield counter from it". The removal is
  -- amount-INDEPENDENT, which is what tells this reading from "an inert prevention
  -- does nothing at all": the Bolt lands in full AND the counter comes off.
  --
  -- THE CONTROL is the same board minus Spider-Punk, one difference and nothing
  -- else, so no assertion here can pass on a board whose shield was inapplicable:
  -- the control's shield prevents the whole 3.
  Spec.it s "CR 615.12 an unpreventable Bolt is prevented not at all and takes the counter anyway" $ do
    bolt <- S.printingOf s registry "Lightning Bolt"
    (guard_, punked) <- guardBoard True 1
    (controlGuard, unpunked) <- guardBoard False 1
    let once = S.settleSba (castAt guard_ bolt punked)
        control = S.settleSba (castAt controlGuard bolt unpunked)
    Spec.assertEqWith s "setup: one shield counter on the 1/4, on both boards" (shields guard_ punked) 1
    Spec.assertEqWith s "the whole 3 is marked: the shield prevented none of it" (S.damageOf guard_ once) (Just 3)
    Spec.assertBool s (Set.member guard_ (GameState.battlefield once)) "and the 1/4 lived through it, so its counters are still readable"
    Spec.assertEqWith s "the counter came off anyway (CR 615.12's middle clause)" (shields guard_ once) 0
    Spec.assertEqWith s "control: without Spider-Punk the same Bolt is prevented whole" (S.damageOf controlGuard control) (Just 0)
    Spec.assertEqWith s "control: spending the same one counter" (shields controlGuard control) 0
  -- The same divergence where it reaches the BOARD rather than the bookkeeping,
  -- as its own case so that it fails on its own: a counter wrongly left on would
  -- go on to replace the next destruction (CR 122.1c's first sentence).
  Spec.it s "CR 122.1c the counter the unpreventable Bolt spent no longer replaces a destruction" $ do
    bolt <- S.printingOf s registry "Lightning Bolt"
    (guard_, punked) <- guardBoard True 1
    let once = S.settleSba (castAt guard_ bolt punked)
        destroyed = S.runPure S.identityAnswer once (Event.destroy Regenerability.Regenerable [guard_])
    Spec.assertBool s (Set.member guard_ (GameState.battlefield once)) "setup: the 1/4 survived the Bolt"
    Spec.assertBool s (not (Set.member guard_ (GameState.battlefield destroyed))) "and an effect's destruction then goes unreplaced"
  -- CR 615.12a: "a prevention effect is applied to any particular unpreventable
  -- damage event just once". The inert application does not re-invoke itself, so
  -- one of the two counters comes off and not both -- the same "only one shield
  -- counter is removed" the preventing path obeys, which is what makes the CR
  -- 616.1 applied-set load-bearing here: the event survives the application, so
  -- the loop goes round again and re-collects this very row.
  --
  -- One difference from the pair of cases above -- the number of counters -- and
  -- the same seats, spell, lands and body.
  Spec.it s "CR 615.12a the unpreventable Bolt's one application takes one counter, not both" $ do
    bolt <- S.printingOf s registry "Lightning Bolt"
    (guard_, punked) <- guardBoard True 2
    let once = S.settleSba (castAt guard_ bolt punked)
        destroyed = S.runPure S.identityAnswer once (Event.destroy Regenerability.Regenerable [guard_])
    Spec.assertEqWith s "setup: two shield counters" (shields guard_ punked) 2
    Spec.assertEqWith s "the whole 3 is still marked" (S.damageOf guard_ once) (Just 3)
    Spec.assertEqWith s "one counter came off, not both" (shields guard_ once) 1
    Spec.assertBool s (Set.member guard_ (GameState.battlefield destroyed)) "and the survivor still replaces a destruction"
  -- The pool's GAMEPLAY route to CR 122.1c's "as the result of an EFFECT", which
  -- the case above's counter arithmetic is what makes reachable: three counters
  -- and two unpreventable Bolts leave 6 marked on a 1/4 with a counter still on
  -- it, so CR 704.5g's state-based action destroys a SHIELDED permanent -- and a
  -- rule's destruction is not one the pair may replace. Reaching this any other
  -- way is impossible while a counter is there, since the prevention half stops
  -- the damage being marked; the door-pair case above proves the same gate
  -- without gameplay.
  --
  -- No settle between the two Bolts, or CR 704.5g would run on the first one's 3.
  Spec.it s "CR 122.1c a rule's destruction is not replaced, though a counter is still there" $ do
    bolt <- S.printingOf s registry "Lightning Bolt"
    (guard_, punked) <- guardBoard True 3
    let bolted = castAt guard_ bolt (castAt guard_ bolt punked)
        twice = S.settleSba bolted
    Spec.assertEqWith s "setup: one counter per Bolt came off, leaving one" (shields guard_ bolted) 1
    Spec.assertEqWith s "setup: and 6 is marked on the 1/4, which CR 704.5g calls lethal" (S.damageOf guard_ bolted) (Just 6)
    Spec.assertBool s (not (Set.member guard_ (GameState.battlefield twice))) "and CR 704.5g destroyed it anyway"
  -- CR 101.4c over CR 615.12: once an inert application spends a counter, an
  -- UNPREVENTABLE event competes for that counter exactly as a preventable one
  -- does, so the one counter facing one of each is contested and its controller
  -- says which event gets it. The CR 615.7 shield's opposite is excruciatorSpec's
  -- mixed batch, which asks nothing: that shield is not reduced by unpreventable
  -- damage at all (CR 615.12's last sentence), so the Excruciator's event is no
  -- claim on it.
  --
  -- The two answers leave DIFFERENT boards, which is what makes the choice
  -- observable: the counter on the Excruciator's 3 removes it and prevents
  -- nothing, so the Piker's 2 lands too and the 1/4 takes 5; the counter on the
  -- Piker's 2 prevents that event whole and leaves 3 marked on a survivor. Every
  -- number distinct -- 3, 2, toughness 4, one counter.
  Spec.it s "CR 101.4c an unpreventable event contests the shield counter it would spend" $ do
    plains <- S.printingOf s registry "Plains"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    guardPrinting <- S.printingOf s registry "Palace Guard"
    excruciator <- S.printingOf s registry "Excruciator"
    let (guard_, g1) = S.addCreature guardPrinting S.alice (S.landsInPlay plains 1)
        (avatar, g2) = S.addCreature excruciator S.bob g1
        (piker, g3) = S.addCreature pikerPrinting S.bob g2
        shielded = S.addCounter CounterKind.Shield 1 guard_ g3
        batch = [hit avatar (Recipient.ToCreature guard_) 3, hit piker (Recipient.ToCreature guard_) 2]
        tookTheAvatar = settleDamage (counterFirst avatar) shielded batch
        tookThePiker = settleDamage (counterFirst piker) shielded batch
    Spec.assertEqWith s "setup: one counter, and two events it cannot both reach" (shields guard_ shielded) 1
    Spec.assertBool
      s
      (wasAskedToOrderDamage (answersFor S.identityAnswer shielded (Damage.applyDamage batch)))
      "alice was asked which of the two the counter goes to"
    Spec.assertEqWith s "spent on the unpreventable 3, it prevents nothing and both events happen" (amounts tookTheAvatar) [3, 2]
    Spec.assertEqWith s "so the 1/4 takes 5" (S.damageOf guard_ tookTheAvatar) (Just 5)
    Spec.assertBool s (not (Set.member guard_ (GameState.battlefield (S.settleSba tookTheAvatar)))) "and CR 704.5g destroys it"
    Spec.assertEqWith s "spent on the Piker's 2 instead, that event never happens" (amounts tookThePiker) [3]
    Spec.assertEqWith s "so only the unpreventable 3 is marked" (S.damageOf guard_ tookThePiker) (Just 3)
    Spec.assertBool s (Set.member guard_ (GameState.battlefield (S.settleSba tookThePiker))) "and it survives"
    Spec.assertEqWith s "one counter spent either way" (shields guard_ tookTheAvatar) 0
    Spec.assertEqWith s "one counter spent either way" (shields guard_ tookThePiker) 0
