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

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Numeric.Natural as Natural
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Cast as Cast
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
import qualified Pawl.Types.Combat as Combat
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
import qualified Pawl.Types.Timestamp as Timestamp
import qualified Pawl.Types.Uses as Uses
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChangePattern as ZoneChangePattern
import qualified Pawl.Types.ZoneChangeSubject as ZoneChangeSubject

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
theAbility p = case Card.activatedAbilities (Printing.card p) of
  ab : _ -> ab
  [] -> ActivatedAbility.MkActivatedAbility (Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) []) (Modal.MkModal (Seq.singleton (Mode.MkMode Seq.empty Map.empty Optionality.Mandatory)) (ModeSelection.ChooseExactly 1)) ActivationTiming.AnyTime

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
  S.runPure answer gs (Cast.castSpell S.alice spellId >> Stack.resolveTop)

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
  let named oid = fmap Card.name (Game.cardOf oid gs) == Just wanted
   in Maybe.listToMaybe (List.sortOn Ord.Down (filter named (Set.toList (GameState.battlefield gs))))

-- Leyline of the Void's redirect, as a floating replacement: any card headed for
-- an OPPONENT's graveyard is exiled instead. CR 400.3 makes that graveyard the
-- card's OWNER's, which is what Replacement.matchesZoneOwner tests.
leylineShape :: ObjectId.ObjectId -> Timestamp.Timestamp -> ActiveReplacement.ActiveReplacement
leylineShape src ts =
  ActiveReplacement.MkActiveReplacement
    { ActiveReplacement.effect =
        ReplacementEffect.ZoneChangeR
          (ZoneChangePattern.MkZoneChangePattern Zone.Graveyard ControllerRelation.Opponents ZoneChangeSubject.AnyObject)
          Zone.Exile,
      ActiveReplacement.source = src,
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
          Action.Cast _ -> True
          _ -> False
     in case filter isCast actions of
          h : _ -> h
          [] -> Action.Pass
  _ -> skirmishAnswer victim p

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
        entered = S.runPure (aimObject aura) withSpell (Cast.castSpell S.alice spellId >> Stack.resolveTop)
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
        resolved = S.runPure S.identityAnswer g3 (Cast.castSpell S.alice fogId >> Stack.resolveTop)
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
        attacking = armed {GameState.combat = (GameState.combat armed) {Combat.attackers = Map.singleton skel (AttackTarget.OfPlayer S.bob)}}
        once = S.runPure S.identityAnswer attacking (Event.destroy Regenerability.Regenerable [skel])
        twice = S.runPure S.identityAnswer once (Event.destroy Regenerability.Regenerable [skel])
    Spec.assertBool s (Map.null (Combat.attackers (GameState.combat armed))) "combat started with no attackers"
    Spec.assertBool s (Set.member skel (GameState.battlefield once)) "survived the first destruction"
    Spec.assertEqWith s "the shield was spent" (GameState.replacements once) []
    Spec.assertBool s (not (Map.member skel (Combat.attackers (GameState.combat once)))) "removed from combat by the regeneration (CR 701.19a)"
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
        afterCast = S.runPure S.identityAnswer withTerror (Cast.castSpell S.alice spell)
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
        let asked = answersFor (raceAnswer scales piker) gs (Cast.castSpell S.alice spellId >> Stack.resolveTop)
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
            asked = answersFor (raceAnswer scales piker) gs (Cast.castSpell S.alice spellId >> Stack.resolveTop)
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
            asked = answersFor (raceAnswer scales piker) gs (Cast.castSpell S.alice spellId >> Stack.resolveTop)
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
  Spec.it s "CR 707.5 declining the copy leaves a 0/0 that dies (CR 704.5f)" $ do
    island <- S.printingOf s registry "Island"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    clone <- S.printingOf s registry "Clone"
    let base = S.landsInPlay island 4
        (_, withPiker) = S.addCreature pikerPrinting S.alice base
        (gs, cloneId) = S.handOne clone withPiker
        -- S.identityAnswer declines ChooseCopyTarget (Clone's own "may").
        resolved = S.runPure S.identityAnswer gs (Cast.castSpell S.alice cloneId >> Stack.resolveTop >> Engine.settleForPriority)
        named = filter (\oid -> fmap Card.name (Game.cardOf oid resolved) == Just (CardName.MkCardName $ Text.pack "Clone")) (Set.toList (GameState.battlefield resolved))
    Spec.assertEqWith s "the 0/0 Clone is gone" named []
  Spec.it s "CR 614.12a the copy choice is locked in BEFORE the enters event exists" $ do
    island <- S.printingOf s registry "Island"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    clonePrinting <- S.printingOf s registry "Clone"
    let base = S.landsInPlay island 4
        (piker, withPiker) = S.addCreature pikerPrinting S.alice base
        (gs, cloneId) = S.handOne clonePrinting withPiker
        -- No settle: the choice must already be made when resolveTop returns.
        resolved = S.runPure (copyOf piker) gs (Cast.castSpell S.alice cloneId >> Stack.resolveTop)
        named = filter (\oid -> fmap Card.name (Game.cardOf oid resolved) == Just (CardName.MkCardName $ Text.pack "Clone")) (Set.toList (GameState.battlefield resolved))
    case named of
      [] -> Spec.assertFailure s "Clone did not reach the battlefield"
      clone : _ -> Spec.assertEqWith s "already a 2/1, with no settle run" (Projection.powerOf clone resolved) (Just 2)
  Spec.it s "CR 208.2b Primal Plasma enters as the 2/2 with flying its controller picked" $ do
    island <- S.printingOf s registry "Island"
    primalPlasma <- S.printingOf s registry "Primal Plasma"
    let (gs, held) = blueBoard island 4 [primalPlasma]
    case held of
      plasmaCard : _ ->
        let after = S.runPure (enteringAs 1) gs (Cast.castSpell S.alice plasmaCard >> Stack.resolveTop)
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
        let withPlasma = S.runPure (enteringAs 1) gs (Cast.castSpell S.alice plasmaCard >> Stack.resolveTop)
            after = S.runPure (enteringAs 2) withPlasma (Cast.castSpell S.alice cloneCard >> Stack.resolveTop)
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
        let withPlasma = S.runPure (enteringAs 1) gs (Cast.castSpell S.alice plasmaCard >> Stack.resolveTop)
            after = S.runPure (enteringAs 0) withPlasma (Cast.castSpell S.alice cloneCard >> Stack.resolveTop)
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
        let s1 = S.runPure (enteringAs 1) gs (Cast.castSpell S.alice plasmaCard >> Stack.resolveTop)
            s2 = S.runPure (enteringAs 2) s1 (Cast.castSpell S.alice cloneA >> Stack.resolveTop)
            s3 = S.runPure (enteringAs 0) s2 (Cast.castSpell S.alice cloneB >> Stack.resolveTop)
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
            Resolve.applyEffect S.noSource S.noSource S.bob Map.empty (Map.singleton slot True) (Map.singleton slot (Recipient.ToObject oid)) (Effect.GainControl Duration.Indefinite (ObjectRef.InSlot slot))
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
            { ActiveReplacement.effect = ReplacementEffect.EntryR (EntryRewrite.ChoiceOf [onlyOption]),
              ActiveReplacement.source = piker,
              ActiveReplacement.timestamp = ts,
              ActiveReplacement.expiry = Expiry.AtCleanup,
              ActiveReplacement.uses = Uses.Once,
              ActiveReplacement.origin = ReplacementOrigin.Other
            }
        g3 = S.addReplacement active g2
        asked = answersFor S.identityAnswer g3 (Replacement.runEntry Set.empty piker)
        after = S.runPure S.identityAnswer g3 (Replacement.runEntry Set.empty piker)
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
        (settled, _) = S.runPureWith S.identityAnswer g1 (Replacement.resolveDestruction Nothing Regenerability.Regenerable piker)
    Spec.assertEqWith s "the object it was asked about" settled (Just piker)
  Spec.it s "CR 701.19a a regenerated destruction settles on nothing" $ do
    swamp <- S.printingOf s registry "Swamp"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    let base = S.landsInPlay swamp 1
        (piker, g1) = S.addCreature pikerPrinting S.alice base
        (settled, _) = S.runPureWith S.identityAnswer (S.addRegenShield piker g1) (Replacement.resolveDestruction Nothing Regenerability.Regenerable piker)
    Spec.assertEqWith s "consumed by the shield" settled Nothing
  stepSkipSpec s registry
  fatigueSpec s registry
  stonehornSpec s registry
  galvanicBlastSpec s registry

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
    Spec.it s "CR 614.15 with three artifacts the self-replacement applies: 4 instead of 2" $ do
      mountain <- S.printingOf s registry "Mountain"
      myr <- S.printingOf s registry "Darksteel Myr"
      galvanicBlast <- S.printingOf s registry "Galvanic Blast"
      let (gs, spellId) = metalcraftBoard mountain myr galvanicBlast 3 []
          after = castAndResolve atBob gs spellId
      Spec.assertEqWith s "bob takes 4" (S.lifeOf S.bob after) (Just 16)
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
          asked = answersFor atBob gs (Cast.castSpell S.alice spellId >> Stack.resolveTop)
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

-- Galvanic Blast's metalcraft clause as a floating row: the damage THIS source is
-- dealing, whatever its kind, becomes 4 (CR 614.15 / 614.1a). Uses.Unlimited
-- rather than the card's Once, for the reason its one caller gives.
blastShape :: ObjectId.ObjectId -> Timestamp.Timestamp -> ActiveReplacement.ActiveReplacement
blastShape src ts =
  ActiveReplacement.MkActiveReplacement
    { ActiveReplacement.effect =
        ReplacementEffect.DamageR
          (DamagePattern.MkDamagePattern Nothing SourceRelation.TheSource)
          (DamageRewrite.SetAmount 4),
      ActiveReplacement.source = src,
      ActiveReplacement.timestamp = ts,
      ActiveReplacement.expiry = Expiry.Never,
      ActiveReplacement.uses = Uses.Unlimited,
      ActiveReplacement.origin = ReplacementOrigin.SelfReplacement
    }
