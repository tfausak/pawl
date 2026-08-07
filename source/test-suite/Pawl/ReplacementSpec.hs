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

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Numeric.Natural as Natural
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Damage as Damage
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
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
import qualified Pawl.Types.ActivationTiming as ActivationTiming
import qualified Pawl.Types.ActiveReplacement as ActiveReplacement
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Combat as Combat.Type
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.DamagePattern as DamagePattern
import qualified Pawl.Types.DamageRewrite as DamageRewrite
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.EntryOption as EntryOption
import qualified Pawl.Types.EntryRewrite as EntryRewrite
import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.Game as Game.Type
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeSelection as ModeSelection
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.Optionality as Optionality
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect
import qualified Pawl.Types.ReplacementOrigin as ReplacementOrigin
import qualified Pawl.Types.Response as Response
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.SourceRelation as SourceRelation
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Timestamp as Timestamp
import qualified Pawl.Types.Uses as Uses
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChangePattern as ZoneChangePattern

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
  [] -> ActivatedAbility.MkActivatedAbility (Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) []) (Modal.MkModal (Seq.singleton (Mode.MkMode Seq.empty Map.empty Optionality.Mandatory Nothing)) (ModeSelection.ChooseExactly 1)) ActivationTiming.AnyTime Nothing

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
  Prompt.ChooseReplacement _ _ sources -> maybe 0 Int.toNaturalSaturating (List.elemIndex preferred sources)
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Recipient.ToCreature victim)) sets
  _ -> S.identityAnswer p

-- Aim every target slot at one object. Recipient.ToObject, not ToCreature as
-- raceAnswer above uses: both slots this answers -- Liquimetal Coating's and
-- Skilled Animator's -- are Pool.Permanents, and a recipient tagged for the wrong
-- pool is not in the legal set at all.
aimObject :: ObjectId.ObjectId -> Prompt.Prompt r -> r
aimObject oid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Recipient.ToObject oid)) sets
  _ -> S.identityAnswer p

countersOn :: CounterKind.CounterKind -> ObjectId.ObjectId -> GameState.GameState -> Natural.Natural
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
        ReplacementEffect.ZoneChangeR
          (ZoneChangePattern.MkZoneChangePattern Zone.Graveyard ControllerRelation.Opponents (Filter.Type.And []))
          Zone.Exile,
      ActiveReplacement.source = src,
      ActiveReplacement.controller = S.alice,
      ActiveReplacement.timestamp = ts,
      ActiveReplacement.expiry = Expiry.Never,
      ActiveReplacement.uses = Uses.Unlimited,
      ActiveReplacement.origin = ReplacementOrigin.Other
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
      began step gs = List.elem (GameEvent.StepBegan step S.alice) (foldr (:) [] (GameState.events gs))
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
-- above; Fatigue's spec is Pool.Players, so a recipient tagged for any other
-- pool is not in its legal set at all.
aimPlayer :: PlayerId.PlayerId -> Prompt.Prompt r -> r
aimPlayer pid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Recipient.ToPlayer pid)) sets
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
      begun gs = length (filter (== GameEvent.StepBegan drawStep S.alice) (foldr (:) [] (GameState.events gs)))
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
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Recipient.ToPlayer victim)) sets
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
      stepsBegunBy pid gs = [ph | GameEvent.StepBegan ph who <- foldr (:) [] (GameState.events gs), who == pid]
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

-- CR 615.7's prevention shield, whose one producer in the pool is Mending Hands
-- ({W} Instant: "Prevent the next 4 damage that would be dealt to any target this
-- turn").
--
-- Three properties, and they are the three halves of the rule: the shield is
-- spent in DAMAGE rather than in events ("such effects count only the amount of
-- damage; the number of events or sources dealing it doesn't matter"), it is
-- scoped to the recipient it shields, and where two simultaneous sources contend
-- for it the shielded side chooses which damage it prevents rather than the
-- engine.
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
        DamageEvent.MkDamageEvent src recipient n False False 0 Nothing DamageKind.Noncombat
      amounts gs = fmap DamageEvent.amount (S.damageEventsOf gs)
      -- Order a contested batch by preferring the event from `src`, by SOURCE id
      -- rather than by position, so the assertion does not depend on the order
      -- the batch was gathered in.
      shieldFirst src p = case p of
        Prompt.OrderDamage _ _ events ->
          let key e = (DamageEvent.source e /= src, DamageEvent.source e)
           in fmap fst (List.sortOn (key . snd) (zip [0 ..] events))
        _ -> S.identityAnswer p
      wasAskedToOrderDamage responses =
        let isOrder r = case r of
              Response.OrderedDamage _ -> True
              _ -> False
         in any isOrder responses
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

-- CR 615.12's damage that "can't be prevented", whose one producer in the pool
-- is Spider-Punk ({1}{R} Legendary Creature -- Spider Human Hero 2/1, Marvel's
-- Spider-Man 92), set against the pool's one COUNTDOWN shield, Mending Hands
-- ("Prevent the next 4 damage that would be dealt to any target this turn").
-- Fog and Selfless Squire install prevention rows too, but CR 615.7's remaining
-- amount is what clause 3 is about, and Mending Hands is its one producer.
--
-- Two of the rule's three clauses, which are the two that are reachable: the
-- damage is dealt in full though an applicable shield is there, and "existing
-- damage prevention shields won't be reduced by damage that can't be prevented".
-- The middle clause -- the applied effect's additional effect still happening --
-- has no producer, since no prevention row can carry one (#689).
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
        DamageEvent.MkDamageEvent src recipient n False False 0 Nothing DamageKind.Noncombat
      amounts gs = fmap DamageEvent.amount (S.damageEventsOf gs)
      -- What each COUNTDOWN shield on the board has left (CR 615.7), read off
      -- the rows themselves. An empty list is a shield spent to 0 and dropped,
      -- which is why the count of rows would not say the same thing. A Fog-shaped
      -- row would not appear here at all -- shieldRemaining answers Nothing for
      -- one -- and none of these boards has one.
      shieldsLeft gs = Maybe.mapMaybe (Replacement.shieldRemaining . ActiveReplacement.effect) (GameState.replacements gs)
      wasAskedToOrderDamage responses =
        let isOrder r = case r of
              Response.OrderedDamage _ -> True
              _ -> False
         in any isOrder responses
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
  -- unpreventable batch costs the shield nothing in any order, so nothing is
  -- asked and the whole 8 lands either way.
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
  Prompt.ChooseReplacement _ _ sources ->
    maybe 0 Int.toNaturalSaturating (List.findIndex (/= furnace) sources)
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
        DamageEvent.MkDamageEvent src recipient n False False 0 Nothing DamageKind.Noncombat
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
-- `whichSource`.
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
        DamageEvent.MkDamageEvent src recipient n False False 0 Nothing DamageKind.Noncombat
      amounts gs = fmap DamageEvent.amount (S.damageEventsOf gs)
      shieldsLeft gs = Maybe.mapMaybe (Replacement.shieldRemaining . ActiveReplacement.effect) (GameState.replacements gs)
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
        DamageEvent.MkDamageEvent src recipient n False False 0 Nothing DamageKind.Noncombat
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

-- Apply one damage batch under a given interpreter. Top-level rather than a
-- `where` binding for castEach's reason: the answer is rank-2 and GHC will not
-- infer it.
settleDamage :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> [DamageEvent.DamageEvent] -> GameState.GameState
settleDamage answer gs batch = S.runPure answer gs (Damage.applyDamage batch)

-- Aim every target slot at one creature. Mending Hands' spec is Pool.AnyTarget
-- (CR 115.4), whose creature members are tagged ToCreature.
aimCreature :: ObjectId.ObjectId -> Prompt.Prompt r -> r
aimCreature oid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Recipient.ToCreature oid)) sets
  _ -> S.identityAnswer p

-- CR 614.5's applied set is what makes the CR 616.1 loop TERMINATE, not merely
-- correct: a regression there (an effect invoking itself repeatedly, e.g. two
-- Hardened Scales re-triggering each other forever) manifests as this group
-- hanging, not failing. "CR 614.5 two Hardened Scales are two instances" below
-- is the case that asserts the CORRECTNESS half (each gets exactly one
-- opportunity); this timeout is the safety net for the TERMINATION half -- it
-- asserts nothing on a green run. Five seconds, not two: this guards against a
-- hang, not a slowdown, and the group runs in ~0.01s today, so a tight bound
-- would only risk becoming a CI flake. The timeout is not applied here --
-- Pawl.Spec cannot express one -- but where this spec is wired into the tasty
-- runner.
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
        Spec.assertBool s (Replacement.matchesPermanent g1 (Filter.Type.HasCardType CardType.Creature) piker) "the creature matches HasCardType Creature"
        Spec.assertBool s (not (Replacement.matchesPermanent g1 (Filter.Type.HasCardType CardType.Creature) landId)) "the land does not match HasCardType Creature"
        Spec.assertBool s (Replacement.matchesPermanent g1 (Filter.Type.And []) landId) "the trivial filter matches the land too"
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
          [ DamageEvent.MkDamageEvent victimA (Recipient.ToCreature victimA) 2 False False 0 Nothing DamageKind.Combat,
            DamageEvent.MkDamageEvent victimB (Recipient.ToCreature victimB) 2 False False 0 Nothing DamageKind.Combat
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
        hurt = S.runPure S.identityAnswer armed (Damage.applyDamage [DamageEvent.MkDamageEvent troll (Recipient.ToCreature troll) 2 False False 0 Nothing DamageKind.Combat])
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
        ratsOut gs = length [o | o <- Set.toList (GameState.battlefield gs), Projection.nameOf o gs == ratsName]
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
            Resolve.applyEffect S.noSource S.noSource S.bob (Map.singleton slot True) (Map.singleton slot (Recipient.ToObject oid)) (Effect.GainControl Duration.Indefinite (ObjectRef.InSlot slot))
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
            { ActiveReplacement.effect = ReplacementEffect.EntryR Filter.Type.IsSource (EntryRewrite.ChoiceOf [onlyOption]),
              ActiveReplacement.source = piker,
              ActiveReplacement.controller = S.alice,
              ActiveReplacement.timestamp = ts,
              ActiveReplacement.expiry = Expiry.AtCleanup,
              ActiveReplacement.uses = Uses.Once,
              ActiveReplacement.origin = ReplacementOrigin.Other
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
        (settled, _) = S.runPureWith S.identityAnswer g1 (Event.resolveDestruction Nothing Regenerability.Regenerable piker)
    Spec.assertEqWith s "the object it was asked about" settled (Just piker)
  Spec.it s "CR 701.19a a regenerated destruction settles on nothing" $ do
    swamp <- S.printingOf s registry "Swamp"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    let base = S.landsInPlay swamp 1
        (piker, g1) = S.addCreature pikerPrinting S.alice base
        (settled, _) = S.runPureWith S.identityAnswer (S.addRegenShield piker g1) (Event.resolveDestruction Nothing Regenerability.Regenerable piker)
    Spec.assertEqWith s "consumed by the shield" settled Nothing
  stepSkipSpec s registry
  fatigueSpec s registry
  stonehornSpec s registry
  galvanicBlastSpec s registry
  mendingHandsSpec s registry
  spiderPunkSpec s registry
  apnapSpec s registry
  excruciatorSpec s registry
  selflessSquireSpec s registry
  gatherSpecimensSpec s registry
  shimatsuSpec s registry
  riotSpec s registry

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
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Recipient.ToPlayer S.bob)) sets
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
      -- The condition was false, so the clause created NOTHING -- not a floating
      -- row that failed to apply. An unspent row would be the visible difference.
      Spec.assertEqWith s "and no replacement was installed at all" (GameState.replacements after) []
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
          hit src = S.runPure S.identityAnswer armed (Damage.applyDamage [DamageEvent.MkDamageEvent src (Recipient.ToCreature victim) 2 False False 0 Nothing DamageKind.Noncombat])
      Spec.assertEqWith s "its own source's 2 becomes 4" (S.damageOf victim (hit mine)) (Just 4)
      Spec.assertEqWith s "another source's 2 stays 2" (S.damageOf victim (hit theirs)) (Just 2)

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
  Prompt.ChooseReplacement _ asked sources
    | asked == who ->
        maybe 0 Int.toNaturalSaturating (List.elemIndex preferred sources)
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

-- Galvanic Blast's metalcraft clause as a floating row: the damage THIS source is
-- dealing, whatever its kind, becomes 4 (CR 614.15 / 614.1a). Uses.Unlimited
-- rather than the card's Once, for the reason its one caller gives.
blastShape :: ObjectId.ObjectId -> Timestamp.Timestamp -> ActiveReplacement.ActiveReplacement
blastShape src ts =
  ActiveReplacement.MkActiveReplacement
    { ActiveReplacement.effect =
        ReplacementEffect.DamageR
          (DamagePattern.MkDamagePattern Nothing SourceRelation.TheSource Nothing)
          (DamageRewrite.SetAmount 4),
      ActiveReplacement.source = src,
      ActiveReplacement.controller = S.alice,
      ActiveReplacement.timestamp = ts,
      ActiveReplacement.expiry = Expiry.Never,
      ActiveReplacement.uses = Uses.Unlimited,
      ActiveReplacement.origin = ReplacementOrigin.SelfReplacement
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
wasAskedForRiot responses =
  let isRiot r = case r of
        Response.ChoseRiot _ -> True
        _ -> False
   in any isRiot responses

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
                -- 613.4d, layer 7d).
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
  -- prints none either, which is what Projection.grantsMintingKeyword is for.
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
