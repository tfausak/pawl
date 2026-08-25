{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Pawl.Engine.Resolve over counters, proliferate, the library-order effects
-- (scry, surveil, fateseal, explore, look at), and the designations a
-- resolution can hand out. The machinery is Pawl.ResolveSpec.
module Pawl.LibraryOrderSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Expiry as Expiry
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Monarch as Monarch
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Replay as Replay
import qualified Pawl.Engine.Resolve as Resolve
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Clause as Clause
import qualified Pawl.Types.ClauseIndex as ClauseIndex
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Decider as Decider
import qualified Pawl.Types.Departure as Departure.Type
import qualified Pawl.Types.Designation as Designation
import qualified Pawl.Types.Draw as Draw
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.DurationRef as DurationRef
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.Face as Face
-- Aliased Filter.Type, not Filter, per the project-wide convention (FilterSpec):
-- the evaluator module Pawl.Engine.Filter may later be imported and must not collide.
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeIndex as ModeIndex
import qualified Pawl.Types.ModeInstance as ModeInstance
import qualified Pawl.Types.ModeSelection as ModeSelection
import qualified Pawl.Types.MonarchTarget as MonarchTarget
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.Optionality as Optionality
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.PlayerCounters as PlayerCounters
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PlayerQuantity as PlayerQuantity
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.PlayerSacrifices as PlayerSacrifices
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.RemoveCounters as RemoveCounters
import qualified Pawl.Types.Response as Response
import qualified Pawl.Types.Revealed as Revealed
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.SlotArity as SlotArity
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Status as Status
import qualified Pawl.Types.StepBegan as StepBegan
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Zone as Zone

countersSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
countersSpec s registry = Spec.describe s "Counters" $ do
  Spec.it s "CR 122.6 Battlegrowth puts a +1/+1 counter (gate)" $ do
    -- alice casts Battlegrowth on bob's Piker (2/1). After resolution the Piker
    -- is 3/2 and carries one +1/+1 counter.
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    battlegrowth <- S.printingOf s registry "Battlegrowth"
    let base = S.landsInPlay forest 1
        (victim, withFoe) = S.addCreature piker S.bob base
        (gs, spellId) = S.handOne battlegrowth withFoe
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "power 3" (Projection.powerOf victim after) (Just 3)
    Spec.assertEqWith s "toughness 2" (Projection.toughnessOf victim after) (Just 2)
  Spec.it s "CR 122 counter persists through cleanup (vs Giant Growth wearing off)" $ do
    -- After a cleanup step, the +1/+1 counter is still on the Piker.
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    battlegrowth <- S.printingOf s registry "Battlegrowth"
    let base = S.landsInPlay forest 1
        (victim, withFoe) = S.addCreature piker S.bob base
        (gs, spellId) = S.handOne battlegrowth withFoe
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        afterCleanup = Expiry.dropAtCleanup resolved
    Spec.assertEqWith s "still 3/2 after cleanup" (Projection.powerOf victim afterCleanup) (Just 3)
    Spec.assertEqWith s "still 3/2 after cleanup" (Projection.toughnessOf victim afterCleanup) (Just 2)
  -- CR 122.1b: Spontaneous Flight is the one card where the two halves have
  -- DIFFERENT durations, which is what proves the flying is a counter rather
  -- than a second until-end-of-turn effect. The +2/+2 wears off at cleanup
  -- (CR 514.2); the flying counter does not.
  Spec.it s "CR 122.1b whole card: Spontaneous Flight pumps until EOT and grants flying for good" $ do
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    spontaneousFlight <- S.printingOf s registry "Spontaneous Flight"
    let base = S.landsInPlay plains 3
        (target, withCreature) = S.addCreature piker S.alice base
        (gs, spellId) = S.handOne spontaneousFlight withCreature
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        afterCleanup = Expiry.dropAtCleanup resolved
    Spec.assertBool s (not (Projection.hasKeyword Keyword.Flying target withCreature)) "the Piker did not fly to begin with"
    Spec.assertEqWith s "pumped to 4/3" (Projection.powerOf target resolved) (Just 4)
    Spec.assertEqWith s "pumped to 4/3" (Projection.toughnessOf target resolved) (Just 3)
    Spec.assertBool s (Projection.hasKeyword Keyword.Flying target resolved) "and it flies"
    -- The discriminator between a counter and another until-EOT effect.
    Spec.assertEqWith s "the pump wore off" (Projection.powerOf target afterCleanup) (Just 2)
    Spec.assertBool s (Projection.hasKeyword Keyword.Flying target afterCleanup) "the flying did not"
  Spec.it s "CR 122.6 Instill Infection puts a -1/-1 counter and draws" $ do
    -- alice casts Instill Infection on bob's Piker; Piker becomes 1/0 and dies
    -- (704.5f); alice draws a card.
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    instillInfection <- S.printingOf s registry "Instill Infection"
    forest <- S.printingOf s registry "Forest"
    let base = S.landsInPlay swamp 4
        (_, withFoe) = S.addCreature piker S.bob base
        -- Baseline before Instill Infection itself enters alice's hand: casting
        -- moves that same card from hand to the stack, so measuring after it is
        -- already there would net the draw against the spell's own departure.
        handBefore = S.handSize S.alice withFoe
        (gs0, spellId) = S.handOne instillInfection withFoe
        -- put a card in alice's library so the draw has something to find.
        (_, gs) = S.addLibraryCard forest S.alice gs0
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        after = S.settleSba (snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop))
    Spec.assertEqWith s "Piker died to the -1/-1 counter (704.5f)" (S.creaturesInPlay S.bob after) 0
    Spec.assertEqWith s "alice drew a card" (S.handSize S.alice after) (handBefore + 1)
  Spec.it s "CR 704.5q both counter kinds on one creature annihilate; net 2/1 survives" $ do
    -- Both counters on the same creature (placed directly); the SBA removes both.
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    let base = S.landsInPlay forest 5
        (victim, withFoe) = S.addCreature piker S.alice base
        gs1 = S.addCounter CounterKind.PlusOnePlusOne 1 victim withFoe
        gs2 = S.addCounter CounterKind.MinusOneMinusOne 1 victim gs1
        after = S.settleSba gs2
    Spec.assertEqWith s "creature survives (net 2/1)" (S.creaturesInPlay S.alice after) 1
    Spec.assertEqWith s "no counters remain" (maybe (Map.fromList [(CounterKind.PlusOnePlusOne, 99)]) Object.counters (Game.lookupObject victim after)) Map.empty
  Spec.it s "CR 122 RemoveCounters takes counters off the slot's target" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (oid, base0) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
        base = S.addCounter CounterKind.MinusOneMinusOne 2 oid base0
        slot = SlotName.MkSlotName (Text.pack "target")
        run =
          Resolve.applyEffect
            oid
            oid
            S.alice
            (Map.singleton slot (Set.singleton (Recipient.ToCreature oid)))
            (Map.singleton slot (Set.singleton (Recipient.ToCreature oid)))
            (Effect.RemoveCounters (RemoveCounters.MkRemoveCounters CounterKind.MinusOneMinusOne (Quantity.Literal 1) slot))
        after = snd (Engine.runGamePure S.identityAnswer base run)
    Spec.assertEqWith s "one of the two counters is gone" (fmap Object.counters (Game.lookupObject oid after)) (Just (Map.singleton CounterKind.MinusOneMinusOne 1))
  -- CR 122 states no rule making the instruction fail when there are fewer
  -- counters than asked for, so it takes what is there. The kind leaves the map
  -- entirely rather than sitting at zero, which is what keeps Object.counters a
  -- tally of what is present.
  Spec.it s "CR 122 removing more counters than are present removes what is there" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (oid, base0) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
        base = S.addCounter CounterKind.MinusOneMinusOne 1 oid base0
        slot = SlotName.MkSlotName (Text.pack "target")
        run =
          Resolve.applyEffect
            oid
            oid
            S.alice
            (Map.singleton slot (Set.singleton (Recipient.ToCreature oid)))
            (Map.singleton slot (Set.singleton (Recipient.ToCreature oid)))
            (Effect.RemoveCounters (RemoveCounters.MkRemoveCounters CounterKind.MinusOneMinusOne (Quantity.Literal 3) slot))
        after = snd (Engine.runGamePure S.identityAnswer base run)
    Spec.assertEqWith s "the kind is gone, not negative" (fmap Object.counters (Game.lookupObject oid after)) (Just Map.empty)
  -- CR 608.2d over CR 608.2e's unit, on a whole card: Shed Weakness ({G} Instant,
  -- Amonkhet 185) reads "Target creature gets +2/+2 until end of turn. You may
  -- remove a -1/-1 counter from it." Two clauses, one target, and only the second
  -- clause is gated -- so the pump lands whichever way the "may" is answered.
  --
  -- The -1/-1 counter is placed directly, as the CR 704.5q case just below does:
  -- casting Instill Infection for it would add a draw and a second resolution
  -- that this case does not want in the way of what it is proving.
  Spec.it s "CR 608.2d Shed Weakness pumps either way; only the removal is optional" $ do
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    shedWeakness <- S.printingOf s registry "Shed Weakness"
    let (victim, withFoe) = S.addCreature piker S.bob (S.landsInPlay forest 1)
        withCounter = S.addCounter CounterKind.MinusOneMinusOne 1 victim withFoe
        (gs, spellId) = S.handOne shedWeakness withCounter
        -- Written out per answerer rather than through a helper taking one: a
        -- let-bound function over an answerer would need a rank-2 argument, and
        -- the neighbouring Deem Worthy case inlines them for the same reason.
        --
        -- S.identityAnswer declines every optional prompt (Script.declining), so
        -- it is the declining half unaided; exerciseOptional is its opposite.
        castDeclining = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        declined = snd (Engine.runGamePure S.identityAnswer castDeclining Stack.resolveTop)
        castExercising = snd (Engine.runGamePure exerciseOptional gs (S.cast S.alice spellId))
        exercised = snd (Engine.runGamePure exerciseOptional castExercising Stack.resolveTop)
    Spec.assertEqWith s "before: the -1/-1 counter makes the 2/2 a 1/1" (Projection.powerOf victim gs) (Just 1)
    -- The discriminator. Under a MODE-wide gate, declining would skip the pump
    -- too and this would read 1.
    Spec.assertEqWith s "declined: pumped to 3/3 anyway" (Projection.powerOf victim declined) (Just 3)
    Spec.assertEqWith s "declined: the counter is still there" (fmap Object.counters (Game.lookupObject victim declined)) (Just (Map.singleton CounterKind.MinusOneMinusOne 1))
    Spec.assertEqWith s "exercised: pumped to 4/4" (Projection.powerOf victim exercised) (Just 4)
    Spec.assertEqWith s "exercised: no counters remain" (fmap Object.counters (Game.lookupObject victim exercised)) (Just Map.empty)
  Spec.it s "CR 122.2 Unsummon removes a counter-bearing creature's counters" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    unsummon <- S.printingOf s registry "Unsummon"
    let base = S.landsInPlay island 1
        (victim, withFoe) = S.addCreature piker S.bob base
        withCounter = S.addCounter CounterKind.PlusOnePlusOne 1 victim withFoe
        (gs, spellId) = S.handOne unsummon withCounter
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        -- Total (no `head`): expect exactly one bounced card in hand, empty counters.
        handCounters = fmap (\h -> maybe (Map.fromList [(CounterKind.PlusOnePlusOne, 99)]) Object.counters (Game.lookupObject h after)) (Game.zoneMembers Zone.Hand S.bob after)
    Spec.assertEqWith s "the bounced incarnation in hand has no counters" handCounters [Map.empty]

-- CR 701.46a: "'Adapt N' means 'If this permanent has no +1/+1 counters on it,
-- put N +1/+1 counters on it.'" Sauroform Hybrid prints adapt 4 and nothing
-- else -- no other ability to reach the counters -- so the SECOND activation
-- isolates the clause gate.
--
-- The gate is on the EFFECT, not on the activation: the second activation is
-- legal, is paid for, resolves, and does nothing. `tappedCount` is what keeps
-- the negative from passing because the ability was never activated, and the
-- projected P/T is what keeps it from passing because the layer walk never saw
-- the counters.
--
-- Twelve Forests: two activations at {4}{G}{G}, so a short board cannot be the
-- reason the second one changes nothing.
sauroformHybridSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
sauroformHybridSpec s registry = Spec.describe s "SauroformHybrid" $ do
  Spec.it s "CR 701.46a whole card: adapt 4 fills an empty Hybrid, and a second adapt does nothing" $ do
    forest <- S.printingOf s registry "Forest"
    hybrid <- S.printingOf s registry "Sauroform Hybrid"
    let (hybridId, placed) = S.addCreature hybrid S.alice (S.landsInPlay forest 12)
        board = placed {GameState.priority = Just S.alice}
        adapt gs ability = S.runPure S.identityAnswer gs $ do
          Activate.activateAbility S.alice hybridId ability
          Stack.resolveTop
        countersOn gs = fmap (Map.findWithDefault 0 CounterKind.PlusOnePlusOne . Object.counters) (Game.lookupObject hybridId gs)
    case Activate.abilitiesFor hybridId board of
      [ability] -> do
        let once = adapt board ability
            twice = adapt once ability
        Spec.assertEqWith s "no counters to begin with" (countersOn board) (Just 0)
        Spec.assertEqWith s "a 2/2 to begin with" (S.powerToughnessOf hybridId board) (Just (2, 2))
        Spec.assertEqWith s "the first adapt puts four counters on" (countersOn once) (Just 4)
        Spec.assertEqWith s "and the projection reads 6/6" (S.powerToughnessOf hybridId once) (Just (6, 6))
        Spec.assertEqWith s "six Forests paid for it" (S.tappedCount S.alice once) 6
        Spec.assertEqWith s "the second adapt adds none" (countersOn twice) (Just 4)
        Spec.assertEqWith s "and it is still 6/6" (S.powerToughnessOf hybridId twice) (Just (6, 6))
        Spec.assertEqWith s "but it was activated and paid for all the same" (S.tappedCount S.alice twice) 12
        Spec.assertEqWith s "and nothing is left on the stack" (length (GameState.stack twice)) 0
      abilities -> Spec.assertFailure s ("expected one adapt ability, got " <> show (length abilities))

-- CR 701.37a: "'Monstrosity N' means 'If this permanent isn't monstrous, put N
-- +1/+1 counters on it and it becomes monstrous.'" Nessian Asp prints monstrosity
-- 4 and reach, so the SECOND activation isolates the gate the way Sauroform
-- Hybrid's does above -- legal, paid for, resolves, does nothing.
--
-- What separates this from adapt is the second case. Adapt's gate reads
-- COUNTERS; monstrosity's reads the DESIGNATION, and an Asp that was given a
-- +1/+1 counter from elsewhere is still not monstrous, so it still becomes
-- monstrous and still takes its four. An implementation that reused adapt's
-- condition passes the first case and fails that one.
--
-- Sixteen Forests: two activations at {6}{G}, so a short board cannot be the
-- reason the second one changes nothing.
nessianAspSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
nessianAspSpec s registry = Spec.describe s "NessianAsp" $ do
  let monstrousOf oid gs = fmap (Set.member Designation.Monstrous . Object.designations) (Game.lookupObject oid gs)
      countersOn oid gs = fmap (Map.findWithDefault 0 CounterKind.PlusOnePlusOne . Object.counters) (Game.lookupObject oid gs)
  Spec.it s "CR 701.37a whole card: monstrosity 4 marks the Asp, and a second monstrosity does nothing" $ do
    forest <- S.printingOf s registry "Forest"
    asp <- S.printingOf s registry "Nessian Asp"
    let (aspId, placed) = S.addCreature asp S.alice (S.landsInPlay forest 16)
        board = placed {GameState.priority = Just S.alice}
        monstrosity gs ability = S.runPure S.identityAnswer gs $ do
          Activate.activateAbility S.alice aspId ability
          Stack.resolveTop
    case Activate.abilitiesFor aspId board of
      [ability] -> do
        let once = monstrosity board ability
            twice = monstrosity once ability
        Spec.assertEqWith s "not monstrous to begin with" (monstrousOf aspId board) (Just False)
        Spec.assertEqWith s "a 4/5 to begin with" (S.powerToughnessOf aspId board) (Just (4, 5))
        Spec.assertEqWith s "the first monstrosity puts four counters on" (countersOn aspId once) (Just 4)
        Spec.assertEqWith s "and marks it monstrous" (monstrousOf aspId once) (Just True)
        Spec.assertEqWith s "and the projection reads 8/9" (S.powerToughnessOf aspId once) (Just (8, 9))
        Spec.assertEqWith s "seven Forests paid for it" (S.tappedCount S.alice once) 7
        Spec.assertEqWith s "the second monstrosity adds none" (countersOn aspId twice) (Just 4)
        Spec.assertEqWith s "and it is still 8/9" (S.powerToughnessOf aspId twice) (Just (8, 9))
        Spec.assertEqWith s "but it was activated and paid for all the same" (S.tappedCount S.alice twice) 14
        Spec.assertEqWith s "and nothing is left on the stack" (length (GameState.stack twice)) 0
      abilities -> Spec.assertFailure s ("expected one monstrosity ability, got " <> show (length abilities))
  -- CR 701.37b's designation, not CR 701.46a's counter count: the two gates agree
  -- on every board where the only counters are monstrosity's own, and this is the
  -- board where they part.
  Spec.it s "CR 701.37a the gate reads the designation, so counters from elsewhere do not stop it" $ do
    forest <- S.printingOf s registry "Forest"
    asp <- S.printingOf s registry "Nessian Asp"
    let (aspId, placed) = S.addCreature asp S.alice (S.landsInPlay forest 16)
        board = (S.addCounter CounterKind.PlusOnePlusOne 1 aspId placed) {GameState.priority = Just S.alice}
    case Activate.abilitiesFor aspId board of
      [ability] -> do
        let after = S.runPure S.identityAnswer board $ do
              Activate.activateAbility S.alice aspId ability
              Stack.resolveTop
        Spec.assertEqWith s "one counter on it, and not monstrous" (countersOn aspId board, monstrousOf aspId board) (Just 1, Just False)
        Spec.assertEqWith s "monstrosity still puts its four on" (countersOn aspId after) (Just 5)
        Spec.assertEqWith s "and still marks it monstrous" (monstrousOf aspId after) (Just True)
        Spec.assertEqWith s "so it reads 9/10" (S.powerToughnessOf aspId after) (Just (9, 10))
      abilities -> Spec.assertFailure s ("expected one monstrosity ability, got " <> show (length abilities))
  -- CR 701.37b: "once a permanent becomes monstrous, it stays monstrous until it
  -- leaves the battlefield". The designation is per-incarnation state, so CR
  -- 400.7's new object has none -- the same reading Object.newIncarnation gives
  -- counters, which the Unsummon case above proves for CR 122.2.
  Spec.it s "CR 701.37b the designation leaves with the permanent" $ do
    forest <- S.printingOf s registry "Forest"
    island <- S.printingOf s registry "Island"
    asp <- S.printingOf s registry "Nessian Asp"
    unsummon <- S.printingOf s registry "Unsummon"
    let (aspId, placed) = S.addCreature asp S.alice (S.landsInPlay forest 16)
        (_, withIsland) = S.addCreature island S.alice placed
        board = withIsland {GameState.priority = Just S.alice}
    case Activate.abilitiesFor aspId board of
      [ability] -> do
        let once = S.runPure S.identityAnswer board $ do
              Activate.activateAbility S.alice aspId ability
              Stack.resolveTop
            (withSpell, spellId) = S.handOne unsummon once
            bounced = S.runPure S.identityAnswer withSpell $ do
              S.cast S.alice spellId
              Stack.resolveTop
            -- Total (no `head`): the Asp is the only card that can be in hand.
            inHand = fmap (\h -> maybe True (Set.member Designation.Monstrous . Object.designations) (Game.lookupObject h bounced)) (Game.zoneMembers Zone.Hand S.alice bounced)
        Spec.assertEqWith s "monstrous on the battlefield" (monstrousOf aspId once) (Just True)
        Spec.assertEqWith s "the bounced incarnation is not monstrous" inHand [False]
      abilities -> Spec.assertFailure s ("expected one monstrosity ability, got " <> show (length abilities))

untapSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
untapSpec s registry = Spec.describe s "Untap" $ do
  Spec.it s "CR 701.26b Untap untaps the slot's target" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (oid, base0) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
        base = S.tapObject oid base0
        slot = SlotName.MkSlotName (Text.pack "target")
        run =
          Resolve.applyEffect
            oid
            oid
            S.alice
            (Map.singleton slot (Set.singleton (Recipient.ToCreature oid)))
            (Map.singleton slot (Set.singleton (Recipient.ToCreature oid)))
            (Effect.Untap (ObjectRef.InSlot slot))
        after = snd (Engine.runGamePure S.identityAnswer base run)
    Spec.assertEqWith s "target is untapped" (fmap Object.tapped (Game.lookupObject oid after)) (Just TapState.Untapped)

gainControlSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
gainControlSpec s registry = Spec.describe s "GainControl" $ do
  Spec.it s "GainControl gives the source's controller control until end of turn and re-Sicks (CR 302.6)" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (oid, base) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
        slot = SlotName.MkSlotName (Text.pack "target")
        -- Apply as though a spell alice controls (controller = alice) resolved it.
        run =
          Resolve.applyEffect
            oid
            oid
            S.alice
            (Map.singleton slot (Set.singleton (Recipient.ToCreature oid)))
            (Map.singleton slot (Set.singleton (Recipient.ToCreature oid)))
            (Effect.GainControl (DurationRef.MkDurationRef Duration.UntilEndOfTurn (ObjectRef.InSlot slot)))
        after = snd (Engine.runGamePure S.identityAnswer base run)
    Spec.assertEqWith s "alice now controls it" (Projection.controllerOf oid after) (Just S.alice)
    Spec.assertEqWith s "it is summoning sick for the new controller" (fmap Object.sickness (Game.lookupObject oid after)) (Just Sickness.Sick)
    Spec.assertEqWith s "control reverts after cleanup" (Projection.controllerOf oid (Expiry.dropAtCleanup after)) (Just S.bob)
  -- CR 302.6 asks whether control was CONTINUOUS. Gaining control of a
  -- permanent you already control interrupts nothing, so the clock must not
  -- reset. The sibling case above is the one where it must.
  --
  -- Isolated from haste on purpose: Act of Treason is the card that reaches
  -- this, and it grants haste, which would mask the difference on the ability
  -- path. Driving Effect.GainControl directly shows the sickness itself.
  Spec.it s "CR 302.6 GainControl does NOT re-Sick a permanent its controller already controlled" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (oid, base) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
        settled = S.runPure S.identityAnswer base (Engine.settleAll S.alice)
        slot = SlotName.MkSlotName (Text.pack "target")
        run =
          Resolve.applyEffect
            oid
            oid
            S.alice
            (Map.singleton slot (Set.singleton (Recipient.ToCreature oid)))
            (Map.singleton slot (Set.singleton (Recipient.ToCreature oid)))
            (Effect.GainControl (DurationRef.MkDurationRef Duration.UntilEndOfTurn (ObjectRef.InSlot slot)))
        after = snd (Engine.runGamePure S.identityAnswer settled run)
    Spec.assertEqWith s "alice controlled it before" (Projection.controllerOf oid settled) (Just S.alice)
    Spec.assertEqWith s "and still does" (Projection.controllerOf oid after) (Just S.alice)
    Spec.assertEqWith s "its settle under alice is untouched" (fmap Object.sickness (Game.lookupObject oid after)) (Just (Sickness.Settled S.alice))

gainPlayerCountersSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
gainPlayerCountersSpec s registry = Spec.describe s "GainPlayerCounters" $ do
  Spec.it s "CR 107.14 GainPlayerCounters gives the resolving controller energy" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (src, gs0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
        act = Resolve.applyEffect src src S.alice Map.empty Map.empty (Effect.GainPlayerCounters (PlayerCounters.MkPlayerCounters (PlayerRef.Relative PlayerRelation.You) PlayerCounterKind.Energy (Quantity.Literal 2)))
        after = S.runPure S.identityAnswer gs0 act
    Spec.assertEqWith s "alice has two energy" (S.playerCounterOf PlayerCounterKind.Energy S.alice after) 2
  -- CR 122.1: `EachPlayer` on GainPlayerCounters had no card producer until
  -- Ichor Rats ({1}{B}{B} Creature -- Phyrexian Rat 2/1, "Infect. When this
  -- creature enters, each player gets a poison counter."), and design.md
  -- section 4 says an implemented, unproven arm is not done. This case is
  -- what proves it.
  --
  -- THREE seats, and the caster's own counter is the discriminator. At two
  -- seats `EachPlayer` and `Relative Opponent` differ only in whether the
  -- caster is included, so a single wrong `Opponent` authoring would be
  -- invisible against the Prologue to Phyresis cases (which prove the
  -- `Relative Opponent` arm, in proliferateSpec below) -- alice holding a
  -- poison counter is the one reading `Relative Opponent` cannot produce, at
  -- any number of seats. The counts are asserted as one tuple because every
  -- number here is 1: three separate checks would let a partial answer look
  -- like a coincidence rather than a failure.
  Spec.it s "CR 122.1 whole card: Ichor Rats poisons all three players, the caster included" $ do
    swamp <- S.printingOf s registry "Swamp"
    ichorRats <- S.printingOf s registry "Ichor Rats"
    -- Three Swamps for the {1}{B}{B}. S.landsInPlay builds its own two-seat
    -- game, so a three-seat board adds them one at a time instead.
    let withMana = List.foldl' (\g _ -> snd (S.addCreature swamp S.alice g)) S.threePlayerGame [1 .. (3 :: Int)]
        (gs, spellId) = S.handOne ichorRats withMana
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        settled = snd (Engine.runGamePure S.identityAnswer cast Engine.priorityLoop)
        poisonIn g = (S.playerCounterOf PlayerCounterKind.Poison S.alice g, S.playerCounterOf PlayerCounterKind.Poison S.bob g, S.playerCounterOf PlayerCounterKind.Poison S.carol g)
    -- Nobody is poisoned before the Rats resolve, so the 1s below are the
    -- effect's doing rather than the fixture's.
    Spec.assertEqWith s "the table starts clean" (poisonIn gs) (0, 0, 0)
    Spec.assertEqWith s "the Rats resolved onto the battlefield" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Ichor Rats") S.alice settled) 1
    Spec.assertEqWith s "alice, bob and carol each got one" (poisonIn settled) (1, 1, 1)
  -- The gameplay-level consequence: CR 704.5c's tenth poison counter. carol
  -- sits on nine, so the counter `EachPlayer` hands her is the one that loses
  -- her the game -- and alice, the caster, is poisoned in the same resolution
  -- without reaching ten.
  Spec.it s "CR 704.5c Ichor Rats' counter is carol's tenth, and she loses the game" $ do
    swamp <- S.printingOf s registry "Swamp"
    ichorRats <- S.printingOf s registry "Ichor Rats"
    let withMana = List.foldl' (\g _ -> snd (S.addCreature swamp S.alice g)) S.threePlayerGame [1 .. (3 :: Int)]
        nearlyDead = S.addPlayerCounter PlayerCounterKind.Poison 9 S.carol withMana
        (gs, spellId) = S.handOne ichorRats nearlyDead
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        settled = snd (Engine.runGamePure S.identityAnswer cast Engine.priorityLoop)
        statusOf pid = fmap Player.status (Map.lookup pid (GameState.players settled))
    Spec.assertEqWith s "carol reached ten" (S.playerCounterOf PlayerCounterKind.Poison S.carol settled) 10
    Spec.assertEqWith s "and lost the game" (statusOf S.carol) (Just (Status.Departed Departure.Type.Lost))
    Spec.assertEqWith s "alice, who cast it, is poisoned but playing" (S.playerCounterOf PlayerCounterKind.Poison S.alice settled, statusOf S.alice) (1, Just Status.Playing)

-- Answers Prompt.ChooseProliferate by taking everything on offer. Its sibling
-- declines everything: between them the tests prove the ANSWER decides who gets
-- counters, rather than the order the candidates happen to be enumerated in.
proliferatesAll :: Prompt.Prompt r -> r
proliferatesAll p = case p of
  Prompt.ChooseProliferate _ _ oids pids -> (Set.fromList oids, Set.fromList pids)
  _ -> S.identityAnswer p

proliferatesNothing :: Prompt.Prompt r -> r
proliferatesNothing p = case p of
  Prompt.ChooseProliferate {} -> (Set.empty, Set.empty)
  _ -> S.identityAnswer p

-- Resolve one Proliferate for alice against `gs`, answered by `answer`.
proliferate :: (forall r. Prompt.Prompt r -> r) -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
proliferate answer src gs =
  S.runPure answer gs (Resolve.applyEffect src src S.alice Map.empty Map.empty Effect.Proliferate)

proliferateSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
proliferateSpec s registry = Spec.describe s "Proliferate" $ do
  -- CR 701.34a: "give each one additional counter of each kind that permanent
  -- or player already has." One more, never a doubling, and never a kind that
  -- was not already there.
  Spec.it s "CR 701.34a proliferate adds exactly one counter of a kind already there" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (src, g0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
        gs = S.addCounter CounterKind.PlusOnePlusOne 2 src g0
        after = proliferate proliferatesAll src gs
    Spec.assertEqWith s "two became three" (S.counterOf CounterKind.PlusOnePlusOne src after) 3
  -- "each kind" is the clause a naive implementation drops: a creature holding
  -- both kinds gets one more of BOTH, not one of whichever was found first.
  --
  -- Holding both kinds at once is a state CR 704.5q would annihilate on the
  -- next state-based-action pass, which is exactly why this drives the opcode
  -- directly instead of resolving a spell: the question here is what
  -- Proliferate does to the counters it finds, not what survives afterwards.
  Spec.it s "CR 701.34a a permanent with two kinds gets one more of each" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (src, g0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
        g1 = S.addCounter CounterKind.PlusOnePlusOne 1 src g0
        gs = S.addCounter CounterKind.MinusOneMinusOne 3 src g1
        after = proliferate proliferatesAll src gs
    Spec.assertEqWith s "+1/+1 went up" (S.counterOf CounterKind.PlusOnePlusOne src after) 2
    Spec.assertEqWith s "-1/-1 went up too" (S.counterOf CounterKind.MinusOneMinusOne src after) 4
  -- CR 701.34a: only permanents "that have a counter" are choosable, so a bare
  -- permanent is never offered and never gains a first counter this way.
  Spec.it s "CR 701.34a a permanent with no counters is not a candidate" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (src, g0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
        (bare, g1) = S.addCreature piker S.alice g0
        gs = S.addCounter CounterKind.PlusOnePlusOne 1 src g1
        after = proliferate proliferatesAll src gs
    Spec.assertEqWith s "the bare Piker gained nothing" (S.counterOf CounterKind.PlusOnePlusOne bare after) 0
    Spec.assertEqWith s "the countered one moved" (S.counterOf CounterKind.PlusOnePlusOne src after) 2
  -- CR 102.2 / 109.5: `Relative Opponent` on GainPlayerCounters had no card
  -- producer until Prologue to Phyresis. The arm was implemented and
  -- unproven, which design.md section 4 says is not done; these cases are
  -- what prove it.
  Spec.it s "CR 122.1 whole card: Prologue to Phyresis poisons the opponent, not the caster" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    prologueToPhyresis <- S.printingOf s registry "Prologue to Phyresis"
    let base = S.landsInPlay island 2
        (_, withLibrary) = S.addLibraryCard piker S.alice base
        handBefore = S.handSize S.alice withLibrary
        (gs, spellId) = S.handOne prologueToPhyresis withLibrary
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "bob is poisoned" (S.playerCounterOf PlayerCounterKind.Poison S.bob after) 1
    Spec.assertEqWith s "alice is not" (S.playerCounterOf PlayerCounterKind.Poison S.alice after) 0
    Spec.assertEqWith s "and alice drew" (S.handSize S.alice after) (handBefore + 1)
  -- The discriminator, and it needs a THIRD seat: at two players `Relative
  -- Opponent` and `EachPlayer` differ only in whether the caster is included,
  -- which the case above catches -- but `Opponent` reaching only ONE of two
  -- opponents would still pass there. CR 806.1: in a Free-for-All the
  -- players compete as individuals, so every other player is an opponent and
  -- both must be poisoned. (CR 102.2 is the TWO-player rule, which is
  -- exactly what a third seat is here to get past.)
  Spec.it s "CR 806.1 at three seats every opponent is poisoned, and only opponents" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    prologueToPhyresis <- S.printingOf s registry "Prologue to Phyresis"
    let (_, withLibrary) = S.addLibraryCard piker S.alice S.threePlayerGame
        -- Two Islands for the {1}{U}. S.landsInPlay builds its own two-seat
        -- game, so a three-seat board adds them one at a time instead.
        withMana = List.foldl' (\g _ -> snd (S.addCreature island S.alice g)) withLibrary [1 .. (2 :: Int)]
        (gs, spellId) = S.handOne prologueToPhyresis withMana
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    -- No separate "the fixture is payable" assertion: an unpayable cast is a
    -- no-op, so the poison counts below are what prove it resolved.
    Spec.assertEqWith s "bob poisoned" (S.playerCounterOf PlayerCounterKind.Poison S.bob after) 1
    Spec.assertEqWith s "carol poisoned too" (S.playerCounterOf PlayerCounterKind.Poison S.carol after) 1
    Spec.assertEqWith s "alice untouched" (S.playerCounterOf PlayerCounterKind.Poison S.alice after) 0
  -- CR 102.1 / CR 800.4a: an opponent is one of the OTHER people in the
  -- game, and carol is no longer one of them (#279). Poison on a departed
  -- player's record is not idle bookkeeping -- the proliferate case below
  -- reads Player.counters to build its candidate list, so this is the write
  -- that would put a non-player on the next prompt.
  Spec.it s "CR 800.4a Prologue to Phyresis does not poison a player who has left the game" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    prologueToPhyresis <- S.printingOf s registry "Prologue to Phyresis"
    let (_, withLibrary) = S.addLibraryCard piker S.alice S.threePlayerGame
        withMana = List.foldl' (\g _ -> snd (S.addCreature island S.alice g)) withLibrary [1 .. (2 :: Int)]
        (gs0, spellId) = S.handOne prologueToPhyresis withMana
        gs = S.departs Departure.Type.Conceded S.carol gs0
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "bob, still in the game, is poisoned" (S.playerCounterOf PlayerCounterKind.Poison S.bob after) 1
    Spec.assertEqWith s "carol, who left, is not" (S.playerCounterOf PlayerCounterKind.Poison S.carol after) 0
    Spec.assertEqWith s "and neither is the caster" (S.playerCounterOf PlayerCounterKind.Poison S.alice after) 0
  -- CR 701.34a: players carry counters too, and proliferate reaches them.
  Spec.it s "CR 701.34a proliferate adds to a player's poison and energy" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (src, g0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
        g1 = S.addPlayerCounter PlayerCounterKind.Poison 3 S.bob g0
        gs = S.addPlayerCounter PlayerCounterKind.Energy 1 S.alice g1
        after = proliferate proliferatesAll src gs
    Spec.assertEqWith s "bob's poison" (S.playerCounterOf PlayerCounterKind.Poison S.bob after) 4
    Spec.assertEqWith s "alice's energy" (S.playerCounterOf PlayerCounterKind.Energy S.alice after) 2
  -- A player with no counters is not a candidate, the same clause the bare
  -- permanent above tests -- so proliferate never starts someone on poison.
  Spec.it s "CR 701.34a a player with no counters is not a candidate" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (src, g0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
        gs = S.addPlayerCounter PlayerCounterKind.Poison 2 S.bob g0
        after = proliferate proliferatesAll src gs
    Spec.assertEqWith s "alice stays clean" (S.playerCounterOf PlayerCounterKind.Poison S.alice after) 0
  -- CR 102.1: proliferate reaches "any number of permanents and/or PLAYERS",
  -- and a player is one of the people in the game -- so a departed seat is
  -- not a candidate (#279). This is the case that made the filter worth
  -- writing rather than deferring again: CR 800.4a removes a departing
  -- player's OBJECTS, and a player counter is not an object (CR 109.1), so
  -- carol's poison is still sitting on her record for kindsFor to find. The
  -- engine would offer someone who is not in the game as a choice, which is
  -- the second invariant's other half -- where the rules leave nothing to
  -- ask, do not ask.
  --
  -- proliferatesAll takes everything offered, so the assertion is exactly
  -- "carol was not offered". bob is the discriminator: he is poisoned too and
  -- still in the game, so a filter that dropped every player would fail here.
  Spec.it s "CR 800.4a a player who has left the game is not a proliferate candidate" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (src, g0) = S.addCreature piker S.alice S.threePlayerGame
        g1 = S.addPlayerCounter PlayerCounterKind.Poison 2 S.bob g0
        g2 = S.addPlayerCounter PlayerCounterKind.Poison 3 S.carol g1
        gs = S.departs Departure.Type.Conceded S.carol g2
        after = proliferate proliferatesAll src gs
    Spec.assertEqWith s "carol has left, so her poison does not move" (S.playerCounterOf PlayerCounterKind.Poison S.carol after) 3
    Spec.assertEqWith s "bob is still in the game, so his does" (S.playerCounterOf PlayerCounterKind.Poison S.bob after) 3
  -- CR 701.34a: "any number" includes none. The discriminating twin of the
  -- first test -- same board, opposite answer -- so this fails if the engine
  -- proliferates for the player instead of asking.
  Spec.it s "CR 701.34a choosing nothing is legal and adds nothing" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (src, g0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
        g1 = S.addCounter CounterKind.PlusOnePlusOne 2 src g0
        gs = S.addPlayerCounter PlayerCounterKind.Poison 3 S.bob g1
        after = proliferate proliferatesNothing src gs
    Spec.assertEqWith s "the creature is untouched" (S.counterOf CounterKind.PlusOnePlusOne src after) 2
    Spec.assertEqWith s "bob is untouched" (S.playerCounterOf PlayerCounterKind.Poison S.bob after) 3
  -- The counter placement rides Event.putCounters, so CR 614's counter
  -- replacements get their opportunity -- proliferate is not a side door that
  -- bypasses Hardened Scales.
  Spec.it s "CR 614 Hardened Scales applies to the counter proliferate adds" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    hardenedScales <- S.printingOf s registry "Hardened Scales"
    let (src, g0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
        (_, g1) = S.addCreature hardenedScales S.alice g0
        gs = S.addCounter CounterKind.PlusOnePlusOne 1 src g1
        after = proliferate proliferatesAll src gs
    Spec.assertEqWith s "one proliferated counter became two" (S.counterOf CounterKind.PlusOnePlusOne src after) 3
  -- Where the rules leave nothing to ask, do not ask: no permanent and no
  -- player holds a counter, so there is no choice to make.
  Spec.it s "CR 701.34a an empty candidate set raises no prompt" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (src, gs) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
        countingAnswer :: Prompt.Prompt r -> State.State Int r
        countingAnswer p = case p of
          Prompt.ChooseProliferate {} -> do
            State.modify (+ 1)
            pure (S.identityAnswer p)
          _ -> pure (S.identityAnswer p)
        asks g = State.execState (Engine.runGame countingAnswer g (Resolve.applyEffect src src S.alice Map.empty Map.empty Effect.Proliferate)) 0
    Spec.assertEqWith s "nobody has a counter: nothing to ask" (asks gs) 0
    Spec.assertEqWith s "someone does: one real decision" (asks (S.addCounter CounterKind.PlusOnePlusOne 1 src gs)) 1
  -- The gameplay-level proof (design.md section 4): a real card, cast and
  -- resolved, doing both halves of its text.
  Spec.it s "Steady Progress whole card: proliferate, then draw a card" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    steadyProgress <- S.printingOf s registry "Steady Progress"
    let base = S.landsInPlay island 3
        (creature, g1) = S.addCreature piker S.alice base
        g2 = S.addCounter CounterKind.PlusOnePlusOne 1 creature g1
        -- Something to draw: an empty library would make the draw a no-op
        -- (and a CR 104.3c loss), hiding whether the effect ran at all.
        (_, g3) = S.addLibraryCard island S.alice g2
        (withSpell, spell) = S.handOne steadyProgress g3
        handBefore = length (Game.zoneMembers Zone.Hand S.alice withSpell)
        afterCast = S.runPure proliferatesAll withSpell (S.cast S.alice spell)
        resolved = S.runPure proliferatesAll afterCast Stack.resolveTop
    Spec.assertEqWith s "stack empty" (length (GameState.stack resolved)) 0
    Spec.assertEqWith s "the counter was proliferated" (S.counterOf CounterKind.PlusOnePlusOne creature resolved) 2
    -- The spell left the hand and one card was drawn, so the hand is level.
    Spec.assertEqWith s "drew a card" (length (Game.zoneMembers Zone.Hand S.alice resolved)) handBefore

-- CR 701.22a: "to 'scry N' means to look at the top N cards of your library,
-- then put any number of them on the bottom of your library in any order and
-- the rest on top of your library in any order."
--
-- Crystal Ball ({3} Artifact, "{1}, {T}: Scry 2") is the producer, and scry TWO
-- is what lets this group discriminate at all: scry 1 cannot tell "any number
-- to the bottom" from all-or-nothing, and neither end's ORDER is a question
-- when only one card can reach it.
--
-- Four DIFFERENT printings in alice's library, top-first [piker, maiden,
-- mountain, forest]. Interchangeable cards could not tell "put back in the
-- chosen order" from "put back in the order they were found", which is exactly
-- the reading a scry that ignored its answer would produce.
--
-- `stock` is how many of them to deal, taken from the TOP so a shorter library
-- keeps the same top cards and the elision pair below differs in one card only.
scryBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  Int ->
  m ([ObjectId.ObjectId], ObjectId.ObjectId, GameState.GameState)
scryBoard s registry stock = do
  forest <- S.printingOf s registry "Forest"
  piker <- S.printingOf s registry "Goblin Piker"
  maiden <- S.printingOf s registry "Bird Maiden"
  mountain <- S.printingOf s registry "Mountain"
  crystalBall <- S.printingOf s registry "Crystal Ball"
  let (ballId, placed) = S.addCreature crystalBall S.alice (S.landsInPlay forest 4)
      -- addLibraryCard puts its card ON TOP, so the deepest is stocked first.
      deck = reverse (take stock [piker, maiden, mountain, forest])
      deal (acc, gs) printing = let (oid, gs') = S.addLibraryCard printing S.alice gs in (oid : acc, gs')
      (ids, stocked) = List.foldl' deal ([], placed) deck
  pure (ids, ballId, stocked {GameState.priority = Just S.alice})

-- Answers Prompt.ChooseScry with a FIXED pair of lists, whatever the engine
-- offers. Pinned rather than derived from the offered list: an answerer that
-- searched what it was handed for a legal pick would find the right cards again
-- after a mutation broke which cards the engine looked at, and this group would
-- stay green over a broken choice.
scryAnswer :: ([ObjectId.ObjectId], [ObjectId.ObjectId]) -> Prompt.Prompt r -> r
scryAnswer split p = case p of
  Prompt.ChooseScry {} -> split
  _ -> S.identityAnswer p

-- Activates Crystal Ball's one activated ability and resolves it. A board
-- offering any other number of abilities activates none, leaving the state
-- untouched -- which fails every assertion below rather than passing one for a
-- reason the case did not choose.
runScry ::
  (forall r. Prompt.Prompt r -> r) ->
  ObjectId.ObjectId ->
  GameState.GameState ->
  GameState.GameState
runScry answer ballId gs = case Activate.abilitiesFor ballId gs of
  [ability] -> S.runPure answer gs $ do
    Activate.activateAbility S.alice ballId ability
    Stack.resolveTop
  _ -> gs

scryLibrary :: GameState.GameState -> [ObjectId.ObjectId]
scryLibrary = Game.zoneMembers Zone.Library S.alice

scrySpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
scrySpec s registry = Spec.describe s "Scry" $ do
  -- The SPLIT, which scry 1 cannot reach: one looked-at card goes under and the
  -- other stays on top, so neither "all of them" nor "none of them" produces
  -- this library.
  Spec.it s "CR 701.22a whole card: Crystal Ball's scry 2 bottoms one and keeps one" $ do
    (ids, ballId, board) <- scryBoard s registry 4
    case ids of
      [piker, maiden, mountain, forest] -> do
        let after = runScry (scryAnswer ([piker], [maiden])) ballId board
        Spec.assertEqWith s "the library started top-first piker, maiden, mountain, forest" (scryLibrary board) [piker, maiden, mountain, forest]
        Spec.assertEqWith s "the kept card is on top and the bottomed one is last" (scryLibrary after) [maiden, mountain, forest, piker]
        Spec.assertEqWith s "the ability left the stack" (length (GameState.stack after)) 0
      _ -> Spec.assertFailure s "expected four library cards"
  -- CR 701.22a's "the rest on top of your library IN ANY ORDER": both cards stay
  -- on top, swapped. A scry that put them back in the order it found them
  -- leaves the library untouched, which is the reading this case rules out.
  Spec.it s "CR 701.22a the kept cards go back in the CHOSEN order, not the order they were in" $ do
    (ids, ballId, board) <- scryBoard s registry 4
    case ids of
      [piker, maiden, mountain, forest] -> do
        let after = runScry (scryAnswer ([], [maiden, piker])) ballId board
        Spec.assertEqWith s "the top two are swapped and the rest is untouched" (scryLibrary after) [maiden, piker, mountain, forest]
      _ -> Spec.assertFailure s "expected four library cards"
  -- The other "in any order", on the bottom half: both go under, in an order
  -- that is not the order they were looked at in.
  Spec.it s "CR 701.22a the bottomed cards go under in the CHOSEN order too" $ do
    (ids, ballId, board) <- scryBoard s registry 4
    case ids of
      [piker, maiden, mountain, forest] -> do
        let after = runScry (scryAnswer ([maiden, piker], [])) ballId board
        Spec.assertEqWith s "mountain and forest rose, maiden above piker beneath them" (scryLibrary after) [mountain, forest, maiden, piker]
      _ -> Spec.assertFailure s "expected four library cards"
  -- A card the answer names in NEITHER list still has to end up somewhere, and
  -- an effect has no way to reject an answer -- Effect.Discard's completion
  -- posture. It stays on top, behind the one that was named.
  Spec.it s "CR 701.22a a looked-at card the answer never names stays on top" $ do
    (ids, ballId, board) <- scryBoard s registry 4
    case ids of
      [piker, maiden, mountain, forest] -> do
        let after = runScry (scryAnswer ([], [maiden])) ballId board
        Spec.assertEqWith s "maiden was named and piker fell in behind it" (scryLibrary after) [maiden, piker, mountain, forest]
      _ -> Spec.assertFailure s "expected four library cards"
  -- Rule 701.22 states no penalty for scrying more cards than there are, unlike
  -- CR 104.3c's draw: a two-card library is looked at whole and still split.
  Spec.it s "CR 701.22a a library shorter than the count is looked at as far as it goes" $ do
    (ids, ballId, board) <- scryBoard s registry 2
    case ids of
      [piker, maiden] -> do
        let after = runScry (scryAnswer ([piker], [maiden])) ballId board
        Spec.assertEqWith s "the whole library was looked at" (scryLibrary board) [piker, maiden]
        Spec.assertEqWith s "and the answer swapped it" (scryLibrary after) [maiden, piker]
      _ -> Spec.assertFailure s "expected two library cards"

-- The elision half. Each case counts the scry prompts one activation raises,
-- and the two-card board is the one-card board's PAIR: same seats, same mana,
-- same ability, one more card in the library.
scryPromptSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
scryPromptSpec s registry = Spec.describe s "ScryPrompt" $ do
  let counting :: Prompt.Prompt r -> State.State Int r
      counting p = case p of
        Prompt.ChooseScry {} -> do
          State.modify (+ 1)
          pure (S.identityAnswer p)
        _ -> pure (S.identityAnswer p)
      asks ballId gs = case Activate.abilitiesFor ballId gs of
        [ability] ->
          State.execState
            ( Engine.runGame counting gs $ do
                Activate.activateAbility S.alice ballId ability
                Stack.resolveTop
            )
            0
        -- Negative, so a board that could not activate at all fails every case
        -- rather than passing the two that expect no prompt.
        _ -> -1
  -- Nothing to LOOK at. CR 701.22a's process has no cards to run on, so there is
  -- no question to put.
  Spec.it s "CR 701.22a an empty library raises no scry prompt" $ do
    (_, ballId, board) <- scryBoard s registry 0
    Spec.assertEqWith s "not asked" (asks ballId board) 0
  -- Nothing to DECIDE: one card that IS the whole library. Its top and its
  -- bottom are the same position, so both answers produce the same library and
  -- declining to ask takes no choice away from the player.
  Spec.it s "CR 701.22a one card that is the whole library raises no scry prompt" $ do
    (ids, ballId, board) <- scryBoard s registry 1
    let after = runScry (scryAnswer ([], [])) ballId board
    Spec.assertEqWith s "not asked" (asks ballId board) 0
    Spec.assertEqWith s "and the library is what it was" (scryLibrary after) ids
  -- The pair's other half, one card deeper: with something beneath it the top
  -- card is a real top-or-bottom question, so it IS asked -- and the answer is
  -- honoured, which is what separates "asked" from "asked and ignored".
  Spec.it s "CR 701.22a a second card beneath makes it a real choice, and it is asked" $ do
    (ids, ballId, board) <- scryBoard s registry 2
    case ids of
      [piker, maiden] -> do
        let after = runScry (scryAnswer ([maiden, piker], [])) ballId board
        Spec.assertEqWith s "asked once" (asks ballId board) 1
        Spec.assertEqWith s "and both went under, maiden above piker" (scryLibrary after) [maiden, piker]
      _ -> Spec.assertFailure s "expected two library cards"
  -- CR 701.22b: "if a player is instructed to scry 0, no scry event occurs."
  -- Driven through the opcode rather than a card, no printing scrying zero and
  -- Crystal Ball's count being fixed at two.
  Spec.it s "CR 701.22b scry 0 raises no prompt and moves nothing" $ do
    (ids, ballId, board) <- scryBoard s registry 4
    let scryZero = Effect.Scry (PlayerQuantity.MkPlayerQuantity (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal 0))
        zero = Resolve.applyEffect ballId ballId S.alice Map.empty Map.empty scryZero
        asked = State.execState (Engine.runGame counting board zero) 0
        after = S.runPure (scryAnswer ([], [])) board zero
    Spec.assertEqWith s "not asked" asked 0
    Spec.assertEqWith s "and the library is what it was" (scryLibrary after) ids

-- CR 701.25a: "to 'surveil N' means to look at the top N cards of your library,
-- then put any number of them into your graveyard and the rest on top of your
-- library in any order."
--
-- Curate ({1}{U} instant, "Surveil 2. Draw a card.") is the producer, cast for
-- real: surveil TWO for the reason the scry group takes two, and the draw is
-- what makes the kept ORDER observable from outside the library -- whichever
-- card the answer left on top is the card that ends up in hand.
--
-- Four DIFFERENT printings in alice's library, top-first [piker, maiden,
-- mountain, forest]. Interchangeable cards could not tell a chosen order from
-- the order they were found in, and could not tell a graveyard arrival from any
-- other.
surveilBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  m ([ObjectId.ObjectId], ObjectId.ObjectId, GameState.GameState)
surveilBoard s registry = do
  island <- S.printingOf s registry "Island"
  piker <- S.printingOf s registry "Goblin Piker"
  maiden <- S.printingOf s registry "Bird Maiden"
  mountain <- S.printingOf s registry "Mountain"
  forest <- S.printingOf s registry "Forest"
  curate <- S.printingOf s registry "Curate"
  let deal (acc, g) printing = let (oid, g') = S.addLibraryCard printing S.alice g in (oid : acc, g')
      -- addLibraryCard puts its card ON TOP, so the deepest is stocked first and
      -- `ids` comes back top-first.
      (ids, stocked) = List.foldl' deal ([], S.landsInPlay island 2) [forest, mountain, maiden, piker]
      (board, spellId) = S.handOne curate stocked
  pure (ids, spellId, board)

-- Answers Prompt.ChooseSurveil with a FIXED pair of lists, scryAnswer's posture
-- and for its reason: an answerer that searched the offered list for a legal
-- pick would repair the assertion after a mutation broke which cards the engine
-- looked at.
surveilAnswer :: ([ObjectId.ObjectId], [ObjectId.ObjectId]) -> Prompt.Prompt r -> r
surveilAnswer split p = case p of
  Prompt.ChooseSurveil {} -> split
  _ -> S.identityAnswer p

-- The card names in alice's graveyard, bottom-first (Pawl.Engine.Game's arrival
-- end), which is the only way to read a graveyard arrival: CR 400.7 minted a
-- fresh id for it, so the id the prompt named is not the id that landed.
surveilGraveyard :: GameState.GameState -> [Maybe CardName.CardName]
surveilGraveyard gs = fmap (\oid -> fmap S.nameOf (Game.cardOf oid gs)) (Game.zoneMembers Zone.Graveyard S.alice gs)

cardNamed :: String -> Maybe CardName.CardName
cardNamed = Just . CardName.MkCardName . Text.pack

surveilSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
surveilSpec s registry = Spec.describe s "Surveil" $ do
  -- The SPLIT: one looked-at card into the graveyard, the other kept, and then
  -- Curate's own draw takes the kept one. A surveil that BOTTOMED the unwanted
  -- card instead -- CR 701.22a's scry, the neighbouring reading -- would leave
  -- piker under forest and the graveyard holding nothing but Curate.
  Spec.it s "CR 701.25a whole card: Curate's surveil 2 bins one, keeps one, then draws it" $ do
    (ids, spellId, board) <- surveilBoard s registry
    case ids of
      [piker, maiden, mountain, forest] -> do
        let after = S.runPure (surveilAnswer ([piker], [maiden])) board $ do
              S.cast S.alice spellId
              Stack.resolveTop
        Spec.assertEqWith s "the library started top-first piker, maiden, mountain, forest" (Game.zoneMembers Zone.Library S.alice board) [piker, maiden, mountain, forest]
        Spec.assertEqWith s "maiden was drawn off the top, leaving mountain and forest" (Game.zoneMembers Zone.Library S.alice after) [mountain, forest]
        Spec.assertEqWith
          s
          "and the drawn card is the one surveil kept"
          (fmap (\oid -> fmap S.nameOf (Game.cardOf oid after)) (Game.zoneMembers Zone.Hand S.alice after))
          [cardNamed "Bird Maiden"]
        -- Curate follows its own surveilled card in, CR 608.2n putting the spell
        -- into its owner's graveyard as the final part of its resolution.
        Spec.assertEqWith s "piker is in the graveyard, under Curate" (surveilGraveyard after) [cardNamed "Goblin Piker", cardNamed "Curate"]
      _ -> Spec.assertFailure s "expected four library cards"
  -- CR 701.25a's "the rest on top of your library IN ANY ORDER": nothing is
  -- binned and the two looked-at cards go back swapped, so the draw takes the
  -- card that was SECOND. A surveil that put them back as it found them draws
  -- piker instead.
  Spec.it s "CR 701.25a the kept cards go back in the CHOSEN order" $ do
    (ids, spellId, board) <- surveilBoard s registry
    case ids of
      [piker, maiden, mountain, forest] -> do
        let after = S.runPure (surveilAnswer ([], [maiden, piker])) board $ do
              S.cast S.alice spellId
              Stack.resolveTop
        Spec.assertEqWith s "piker fell to second and was left there by the draw" (Game.zoneMembers Zone.Library S.alice after) [piker, mountain, forest]
        Spec.assertEqWith
          s
          "maiden was on top, so maiden was drawn"
          (fmap (\oid -> fmap S.nameOf (Game.cardOf oid after)) (Game.zoneMembers Zone.Hand S.alice after))
          [cardNamed "Bird Maiden"]
        Spec.assertEqWith s "and nothing but the spell reached the graveyard" (surveilGraveyard after) [cardNamed "Curate"]
      _ -> Spec.assertFailure s "expected four library cards"
  -- "Any number" reaching ALL of them, and the graveyard's own order: the answer
  -- names maiden first, so maiden is put in first and ends up UNDER piker.
  Spec.it s "CR 701.25a both looked-at cards can go, in the order the answer names them" $ do
    (ids, spellId, board) <- surveilBoard s registry
    case ids of
      [piker, maiden, _, forest] -> do
        let after = S.runPure (surveilAnswer ([maiden, piker], [])) board $ do
              S.cast S.alice spellId
              Stack.resolveTop
        Spec.assertEqWith s "mountain rose to the top and was drawn, leaving forest" (Game.zoneMembers Zone.Library S.alice after) [forest]
        Spec.assertEqWith
          s
          "the draw took mountain, the card that was third"
          (fmap (\oid -> fmap S.nameOf (Game.cardOf oid after)) (Game.zoneMembers Zone.Hand S.alice after))
          [cardNamed "Mountain"]
        Spec.assertEqWith s "maiden went in first, so piker sits on top of it" (surveilGraveyard after) [cardNamed "Bird Maiden", cardNamed "Goblin Piker", cardNamed "Curate"]
      _ -> Spec.assertFailure s "expected four library cards"

-- The elision half, driven through the opcode: Curate's count is fixed at two,
-- and casting it on a one-card library would deck alice (CR 104.3c) before the
-- assertion could read anything.
--
-- alice has a Piker on the battlefield to apply the effect from, and `stock`
-- distinct cards in her library, top-first.
surveilOpcodeBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  Int ->
  m ([ObjectId.ObjectId], ObjectId.ObjectId, GameState.GameState)
surveilOpcodeBoard s registry stock = do
  island <- S.printingOf s registry "Island"
  piker <- S.printingOf s registry "Goblin Piker"
  maiden <- S.printingOf s registry "Bird Maiden"
  mountain <- S.printingOf s registry "Mountain"
  let (sourceId, base) = S.addCreature piker S.alice (S.landsInPlay island 1)
      deal (acc, gs) printing = let (oid, gs') = S.addLibraryCard printing S.alice gs in (oid : acc, gs')
      (ids, stocked) = List.foldl' deal ([], base) (reverse (take stock [maiden, mountain]))
  pure (ids, sourceId, stocked {GameState.priority = Just S.alice})

surveilPromptSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
surveilPromptSpec s registry = Spec.describe s "SurveilPrompt" $ do
  let counting :: Prompt.Prompt r -> State.State Int r
      counting p = case p of
        Prompt.ChooseSurveil {} -> do
          State.modify (+ 1)
          pure (S.identityAnswer p)
        _ -> pure (S.identityAnswer p)
      surveilTwo = Effect.Surveil (PlayerQuantity.MkPlayerQuantity (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal 2))
      apply effect sourceId = Resolve.applyEffect sourceId sourceId S.alice Map.empty Map.empty effect
      asks effect sourceId gs = State.execState (Engine.runGame counting gs (apply effect sourceId)) 0
  -- Nothing to LOOK at, the one case rule 701.25a's process cannot run on.
  Spec.it s "CR 701.25a an empty library raises no surveil prompt" $ do
    (_, sourceId, board) <- surveilOpcodeBoard s registry 0
    Spec.assertEqWith s "not asked" (asks surveilTwo sourceId board) 0
  -- The case that separates surveil from scry, and the reason this pair exists:
  -- with ONE card that is the whole library, Pawl.Engine.Resolve.scryOne asks
  -- nothing because top and bottom are the same position -- but a graveyard is
  -- somewhere else, so the player IS asked, and the answer is honoured.
  Spec.it s "CR 701.25a one card that is the whole library is still a real choice" $ do
    (ids, sourceId, board) <- surveilOpcodeBoard s registry 1
    let after = S.runPure (surveilAnswer (ids, [])) board (apply surveilTwo sourceId)
    Spec.assertEqWith s "asked once" (asks surveilTwo sourceId board) 1
    Spec.assertEqWith s "and the card it named left the library" (Game.zoneMembers Zone.Library S.alice after) []
    Spec.assertEqWith s "for the graveyard" (surveilGraveyard after) [cardNamed "Bird Maiden"]
  -- CR 701.25c: "if a player is instructed to surveil 0, no surveil event
  -- occurs." Driven through the opcode, no printing surveilling zero.
  Spec.it s "CR 701.25c surveil 0 raises no prompt and moves nothing" $ do
    (ids, sourceId, board) <- surveilOpcodeBoard s registry 2
    let surveilZero = Effect.Surveil (PlayerQuantity.MkPlayerQuantity (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal 0))
        after = S.runPure (surveilAnswer ([], [])) board (apply surveilZero sourceId)
    Spec.assertEqWith s "not asked" (asks surveilZero sourceId board) 0
    Spec.assertEqWith s "and the library is what it was" (Game.zoneMembers Zone.Library S.alice after) ids
    Spec.assertEqWith s "with an empty graveyard" (surveilGraveyard after) []

-- CR 701.29a: "to 'fateseal N' means to look at the top N cards of an opponent's
-- library, then put any number of them on the bottom of that library in any
-- order and the rest on top of that library in any order."
--
-- Spin into Myth ({4}{U} instant, "Put target creature on top of its owner's
-- library, then fateseal 2") is the producer, cast for real.
--
-- THREE SEATS, because two cannot tell "the opponent the fatesealer chose" from
-- "an opponent" or from "every opponent" -- and the answer names CAROL, who is
-- not the first candidate, so an implementation that ignored the answer and took
-- the head would fateseal bob and fail.
--
-- alice targets HER OWN Piker with the first half, so the library the creature
-- lands in and the library the fateseal reorders are different libraries: a
-- fateseal that looked at its own controller's library would have to disturb the
-- card just placed there.
--
-- Returns (alice's library card, bob's library top-first, carol's library
-- top-first, alice's creature, the spell in hand, the board).
fatesealBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  NonEmpty.NonEmpty PlayerId.PlayerId ->
  m (ObjectId.ObjectId, [ObjectId.ObjectId], [ObjectId.ObjectId], ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
fatesealBoard s registry seats = do
  island <- S.printingOf s registry "Island"
  piker <- S.printingOf s registry "Goblin Piker"
  maiden <- S.printingOf s registry "Bird Maiden"
  mountain <- S.printingOf s registry "Mountain"
  forest <- S.printingOf s registry "Forest"
  spin <- S.printingOf s registry "Spin into Myth"
  let deal pid (acc, g) printing = let (oid, g') = S.addLibraryCard printing pid g in (oid : acc, g')
      (creatureId, b1) = S.addCreature piker S.alice (S.landsFor island S.alice 5 (Setup.emptyGame seats))
      (aliceLib, b2) = S.addLibraryCard forest S.alice b1
      (bobIds, b3) = List.foldl' (deal S.bob) ([], b2) [forest, mountain]
      -- Only when carol is at the table: a library belonging to a seat the game
      -- does not have would be a fixture nothing in the rules can reach.
      (carolIds, b4)
        | List.elem S.carol (NonEmpty.toList seats) = List.foldl' (deal S.carol) ([], b3) [forest, mountain, maiden]
        | otherwise = ([], b3)
      (board, spellId) = S.handOne spin b4
  pure (aliceLib, bobIds, carolIds, creatureId, spellId, board)

-- Answers Prompt.ChooseFateseal with a FIXED pair of lists, surveilAnswer's
-- posture and for its reason.
fatesealAnswer :: ([ObjectId.ObjectId], [ObjectId.ObjectId]) -> Prompt.Prompt r -> r
fatesealAnswer split p = case p of
  Prompt.ChooseFateseal {} -> split
  _ -> S.identityAnswer p

fatesealSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
fatesealSpec s registry = Spec.describe s "Fateseal" $ do
  let aimAt :: ObjectId.ObjectId -> PlayerId.PlayerId -> ([ObjectId.ObjectId], [ObjectId.ObjectId]) -> Prompt.Prompt r -> r
      aimAt creatureId victim split p = case p of
        Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToCreature creatureId))) sets
        -- PINNED to the second candidate, not the first: S.identityAnswer and
        -- Replay.defaultAnswer both take the head, so a fateseal that dropped
        -- this answer would still reorder a library and still pass a membership
        -- assertion -- against the WRONG seat.
        Prompt.ChooseOpponent {} -> victim
        Prompt.ChooseFateseal {} -> split
        _ -> S.identityAnswer p
      castSpin :: (forall r. Prompt.Prompt r -> r) -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
      castSpin answer spellId board = S.runPure answer board $ do
        S.cast S.alice spellId
        Stack.resolveTop
  Spec.it s "CR 701.29a whole card: Spin into Myth reorders the CHOSEN opponent's library and nobody else's" $ do
    (aliceLib, bobIds, carolIds, creatureId, spellId, board) <- fatesealBoard s registry S.threePlayers
    case (bobIds, carolIds) of
      ([bobTop, bobDeep], [carolTop, carolMiddle, carolDeep]) -> do
        -- The pinned answer names a card from BOB'S library as well as carol's.
        -- Against carol's prompt the stray id is filtered out and changes
        -- nothing; against a fateseal that swept every opponent it would bottom
        -- bob's top card, which is what makes the next assertion discriminate
        -- rather than pass because the answer named nothing bob owns.
        let after = castSpin (aimAt creatureId S.carol ([carolTop, bobTop], [carolMiddle])) spellId board
        Spec.assertEqWith s "carol's library started top-first maiden, mountain, forest" (Game.zoneMembers Zone.Library S.carol board) [carolTop, carolMiddle, carolDeep]
        Spec.assertEqWith s "the kept card is on top of carol's library and the bottomed one is last" (Game.zoneMembers Zone.Library S.carol after) [carolMiddle, carolDeep, carolTop]
        -- The seat the answer did NOT name. Two opponents is what makes this
        -- assertion mean anything: on a two-player board it would hold for a
        -- fateseal that swept every opponent.
        Spec.assertEqWith s "bob's library is untouched" (Game.zoneMembers Zone.Library S.bob after) [bobTop, bobDeep]
        -- CR 701.29a's library is an OPPONENT'S, never the fatesealer's: alice's
        -- library holds the returned creature on top of the card she started
        -- with, in the order the first half of the card put them there.
        Spec.assertBool s (not (S.onBattlefield creatureId after)) "the targeted creature left the battlefield"
        Spec.assertEqWith
          s
          "alice's library is the returned creature on top of her own card"
          (fmap (\oid -> fmap S.nameOf (Game.cardOf oid after)) (Game.zoneMembers Zone.Library S.alice after))
          [cardNamed "Goblin Piker", cardNamed "Forest"]
        Spec.assertEqWith s "and the card she started with is still the one underneath" (drop 1 (Game.zoneMembers Zone.Library S.alice after)) [aliceLib]
      _ -> Spec.assertFailure s "expected two library cards for bob and three for carol"
  -- WHO is asked and about WHOSE library -- the half a board cannot show by its
  -- final state. The fatesealer is shown the cards; the library's owner is shown
  -- nothing and asked nothing.
  Spec.it s "CR 701.29a the fatesealer is asked, about the chosen opponent's top cards" $ do
    (_, _, carolIds, creatureId, spellId, board) <- fatesealBoard s registry S.threePlayers
    case carolIds of
      [carolTop, carolMiddle, _] -> do
        let recording :: Prompt.Prompt r -> State.State [(PlayerId.PlayerId, PlayerId.PlayerId, [ObjectId.ObjectId])] r
            recording p = case p of
              Prompt.ChooseFateseal _ seat owner looked -> do
                State.modify (<> [(seat, owner, looked)])
                pure ([], looked)
              _ -> pure (aimAt creatureId S.carol ([], []) p)
            asked =
              State.execState
                ( Engine.runGame recording board $ do
                    S.cast S.alice spellId
                    Stack.resolveTop
                )
                []
        Spec.assertEqWith s "alice asked, about carol's library, showing its top two" asked [(S.alice, S.carol, [carolTop, carolMiddle])]
      _ -> Spec.assertFailure s "expected three library cards for carol"
  -- The elision pair for the OPPONENT choice, two boards differing in seat count
  -- alone: CR 102.2's two-player game leaves exactly one opponent and nothing to
  -- ask, and a third seat makes it a real question.
  Spec.it s "CR 102.2 the opponent is chosen only when there are two of them" $ do
    let counting :: ObjectId.ObjectId -> Prompt.Prompt r -> State.State Int r
        counting creatureId p = case p of
          Prompt.ChooseOpponent {} -> do
            State.modify (+ 1)
            pure (aimAt creatureId S.carol ([], []) p)
          _ -> pure (aimAt creatureId S.carol ([], []) p)
        asks (_, _, _, creatureId, spellId, board) =
          State.execState
            ( Engine.runGame (counting creatureId) board $ do
                S.cast S.alice spellId
                Stack.resolveTop
            )
            0
    two <- fatesealBoard s registry S.bothPlayers
    three <- fatesealBoard s registry S.threePlayers
    Spec.assertEqWith s "one opponent, not asked" (asks two) 0
    Spec.assertEqWith s "two opponents, asked once" (asks three) 1
    -- And the two-seat board still fateseals: the elision skips the question,
    -- not the action.
    case two of
      (_, bobIds, _, creatureId, spellId, board) -> case bobIds of
        [bobTop, bobDeep] -> do
          let after = castSpin (aimAt creatureId S.carol ([bobTop], [])) spellId board
          Spec.assertEqWith s "bob's only opponent fatesealed him" (Game.zoneMembers Zone.Library S.bob after) [bobDeep, bobTop]
        _ -> Spec.assertFailure s "expected two library cards for bob"
  -- The elision pair for the SPLIT question, two boards differing in one card:
  -- a lone card that is the whole library has its top and its bottom at the same
  -- position, so both answers give the same library and there is nothing to ask
  -- -- scryOne's case, and NOT surveil's, where the two destinations differ.
  -- Driven through the opcode, Spin into Myth's count being fixed at two.
  Spec.it s "CR 701.29a a one-card library raises no fateseal prompt, and a card beneath it does" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    let (sourceId, base) = S.addCreature piker S.alice (S.landsInPlay island 1)
        (deep, one) = S.addLibraryCard forest S.bob base
        (top, two) = S.addLibraryCard mountain S.bob one
        counting :: Prompt.Prompt r -> State.State Int r
        counting p = case p of
          Prompt.ChooseFateseal {} -> do
            State.modify (+ 1)
            pure (S.identityAnswer p)
          _ -> pure (S.identityAnswer p)
        fatesealTwo = Effect.Fateseal (PlayerQuantity.MkPlayerQuantity (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal 2))
        apply = Resolve.applyEffect sourceId sourceId S.alice Map.empty Map.empty fatesealTwo
        asks gs = State.execState (Engine.runGame counting gs apply) 0
    Spec.assertEqWith s "one card, not asked" (asks one) 0
    Spec.assertEqWith s "a card beneath it, asked once" (asks two) 1
    -- Asked AND honoured, which is what separates the pair from an engine that
    -- raises the prompt and drops the answer.
    Spec.assertEqWith
      s
      "and the named card went under"
      (Game.zoneMembers Zone.Library S.bob (S.runPure (fatesealAnswer ([top], [])) two apply))
      [deep, top]

-- CR 701.44a: "certain spells and abilities instruct a permanent to explore. To
-- do so, that permanent's controller reveals the top card of their library. If a
-- land card is revealed this way, that player puts that card into their hand.
-- Otherwise, that player puts a +1/+1 counter on the exploring permanent and may
-- put the revealed card into their graveyard."
--
-- Merfolk Branchwalker {1}{G} Creature -- Merfolk Scout 2/1, "When this creature
-- enters, it explores", cast off two Forests and run to a stable board -- the
-- gameplay-level route Pawl.MassEffectSpec's baneOfProgressSpec takes, so CR 603.6a's enters trigger
-- is placed by the engine rather than by the fixture.
--
-- The library is STACKED so the branch is chosen rather than drawn: the top card
-- is this helper's argument and a Bird Maiden always sits beneath it. Every case
-- below is the same board with one card changed, which is what makes the land
-- and nonland branches a pair rather than two unrelated boards. Branchwalker
-- enters BARE, so the one +1/+1 counter the nonland branch adds cannot be
-- confused with a counter it already had.
exploreBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  [String] ->
  m (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
exploreBoard s registry deck = do
  forest <- S.printingOf s registry "Forest"
  branchwalker <- S.printingOf s registry "Merfolk Branchwalker"
  myr <- S.printingOf s registry "Darksteel Myr"
  printings <- mapM (S.printingOf s registry) deck
  let -- A second creature alice controls, so "the exploring permanent" is told
      -- apart from "a creature you control": rule 701.44a's counter goes on the
      -- one that explored and this one must stay bare.
      (bystander, withMyr) = S.addCreature myr S.alice (S.landsInPlay forest 2)
      (withSpell, spell) = S.handOne branchwalker withMyr
      -- addLibraryCard puts its card ON TOP, so the deepest is stocked first.
      deal gs printing = snd (S.addLibraryCard printing S.alice gs)
      stocked = List.foldl' deal withSpell (reverse printings)
  pure (spell, bystander, stocked)

-- exploreBoard's board with Synthetic Fossil Warren on the battlefield when
-- `petrified`, and identical to it otherwise -- same lands, same hand, same
-- library, same bystander. The Warren reads "Goblin cards you own that aren't on
-- the battlefield are lands in addition to their other types": CR 613.1d's layer
-- 4 over an Affected.MatchingOffBattlefield set, adding a card type the way CR
-- 205.1b's "in addition to its other types" does, so the Goblin Piker on top of
-- the library is a LAND card that no printed characteristic of it says it is.
--
-- Synthetic (#1910): Teferi, Mage of Zhalfir and Biotransference print this
-- affected set, and Toph, the First Metalbender prints "are lands in addition to
-- their other types", but nothing prints the two together -- Scryfall
-- o:"are lands" and o:"land in addition to its other types", 2026-08-19, no card
-- that makes an off-battlefield nonland card a land. Toph is what would refute
-- that; its set is the battlefield.
--
-- The Warren reaches the battlefield AFTER the library is stocked, which is what
-- separates "the card was always a land" and "the effect applied to it as it
-- arrived" from a continuous effect applying to a card sitting in a library. It
-- is scoped to GOBLIN cards so it cannot reach the Merfolk Branchwalker while
-- that is a spell on the stack, which would change what resolves rather than
-- what the explore reads.
warrenBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  Bool ->
  [String] ->
  m (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
warrenBoard s registry petrified deck = do
  warren <- S.printingOf s registry "Synthetic Fossil Warren"
  (spell, bystander, board) <- exploreBoard s registry deck
  pure (spell, bystander, if petrified then snd (S.addCreature warren S.alice board) else board)

-- Answers Prompt.ChooseExplore with a FIXED decision, whatever the engine
-- offers. Pinned rather than derived: an answerer that read the prompt's own
-- fields would still produce a legal answer after a mutation broke which card
-- was revealed, and these cases would stay green over a broken choice.
exploreAnswer :: OptionalDecision.OptionalDecision -> Prompt.Prompt r -> r
exploreAnswer decision p = case p of
  Prompt.ChooseExplore {} -> decision
  _ -> S.identityAnswer p

-- Cast the Branchwalker and settle: the creature spell resolves, its enters
-- trigger is placed, and the next round of passes resolves that.
runExplore ::
  (forall r. Prompt.Prompt r -> r) ->
  ObjectId.ObjectId ->
  GameState.GameState ->
  GameState.GameState
runExplore answer spell gs =
  let afterCast = S.runPure answer gs (S.cast S.alice spell)
   in S.runPure answer afterCast Engine.priorityLoop

-- The card NAMES in one of alice's zones, in zone order. Names and not ids
-- because CR 400.7 mints a fresh incarnation for the card a move takes out of
-- the library, so the id the fixture stocked is gone by the time the assertion
-- reads the hand or the battlefield.
zoneNames :: Zone.Zone -> GameState.GameState -> [String]
zoneNames zone gs =
  fmap
    (\oid -> maybe "?" (Text.unpack . CardName.unwrap . Face.name) (Game.faceOf oid gs))
    (Game.zoneMembers zone S.alice gs)

-- The names alice revealed this turn, in order. A reveal is PUBLIC (CR 701.20a),
-- so it leaves a GameEvent behind and that event is the only thing an assertion
-- can read it through -- which is also what makes the empty list the assertion
-- that CR 701.20e's look was NOT one.
revealedNames :: GameState.GameState -> [String]
revealedNames gs = Maybe.mapMaybe revealedName (S.eventsOf gs)
  where
    revealedName event = case event of
      GameEvent.Revealed (Revealed.MkRevealed pid _ _ pc)
        | pid == S.alice ->
            fmap (Text.unpack . CardName.unwrap) (Maybe.listToMaybe (Set.toList (PC.names pc)))
      _ -> Nothing

exploreSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
exploreSpec s registry = Spec.describe s "Explore" $ do
  -- The LAND branch. Nothing else on the board differs from the two cases below:
  -- the top card is a Mountain rather than a Goblin Piker.
  Spec.it s "CR 701.44a a revealed land card goes to hand, with no counter and no question" $ do
    (spell, bystander, board) <- exploreBoard s registry ["Mountain", "Bird Maiden"]
    let after = runExplore (exploreAnswer OptionalDecision.Exercises) spell board
        walker = namedOnBattlefield "Merfolk Branchwalker" after
    Spec.assertBool s (Maybe.isJust walker) "the Branchwalker resolved onto the battlefield"
    Spec.assertEqWith s "stack empty: the spell and its trigger both resolved" (length (GameState.stack after)) 0
    Spec.assertEqWith s "the Mountain left the top of the library" (zoneNames Zone.Library after) ["Bird Maiden"]
    Spec.assertEqWith s "and is in hand" (zoneNames Zone.Hand after) ["Mountain"]
    -- CR 701.20a: the reveal is public, so it is in the log every player reads.
    Spec.assertEqWith s "the Mountain was revealed on the way" (revealedNames after) ["Mountain"]
    Spec.assertEqWith s "nothing was binned" (zoneNames Zone.Graveyard after) []
    -- The counter is the discriminator between the branches: rule 701.44a's
    -- "otherwise" is the only sentence that puts one on.
    Spec.assertEqWith s "CR 701.44a no +1/+1 counter on the land branch" (plusOnePlusOnesOn walker after) 0
    Spec.assertEqWith s "and none on the other creature alice controls" (plusOnePlusOnesOn (Just bystander) after) 0
  -- The NONLAND branch, exercising the "may". Same board, Goblin Piker on top.
  Spec.it s "CR 701.44a a revealed nonland card grows the explorer, and the choice bins it" $ do
    (spell, bystander, board) <- exploreBoard s registry ["Goblin Piker", "Bird Maiden"]
    let after = runExplore (exploreAnswer OptionalDecision.Exercises) spell board
        walker = namedOnBattlefield "Merfolk Branchwalker" after
    Spec.assertBool s (Maybe.isJust walker) "the Branchwalker resolved onto the battlefield"
    Spec.assertEqWith s "one +1/+1 counter" (plusOnePlusOnesOn walker after) 1
    Spec.assertEqWith s "the Piker is in the graveyard" (zoneNames Zone.Graveyard after) ["Goblin Piker"]
    Spec.assertEqWith s "the Maiden it was sitting on is now the top card" (zoneNames Zone.Library after) ["Bird Maiden"]
    -- The TOP card and not just a card: the Maiden beneath it was never shown.
    Spec.assertEqWith s "only the Piker was revealed" (revealedNames after) ["Goblin Piker"]
    Spec.assertEqWith s "a nonland card never reaches the hand" (zoneNames Zone.Hand after) []
    -- The counter went on the permanent that EXPLORED, not on every creature.
    Spec.assertEqWith s "the bystanding creature stayed bare" (plusOnePlusOnesOn (Just bystander) after) 0
  -- The other half of the "may", the ONE thing changed being the answer. Without
  -- this case a bin-always implementation passes the case above.
  Spec.it s "CR 701.44a declining leaves the revealed card on top of the library" $ do
    (spell, bystander, board) <- exploreBoard s registry ["Goblin Piker", "Bird Maiden"]
    let after = runExplore (exploreAnswer OptionalDecision.Declines) spell board
        walker = namedOnBattlefield "Merfolk Branchwalker" after
    Spec.assertEqWith s "the counter went on either way" (plusOnePlusOnesOn walker after) 1
    Spec.assertEqWith s "the library is untouched, Piker still on top" (zoneNames Zone.Library after) ["Goblin Piker", "Bird Maiden"]
    Spec.assertEqWith s "nothing was binned" (zoneNames Zone.Graveyard after) []
    Spec.assertEqWith s "the bystanding creature stayed bare" (plusOnePlusOnesOn (Just bystander) after) 0
  -- CR 701.44a's "if a land card is revealed" asked of the card's CR 613
  -- projection rather than of its printed face. Rule 613.1 starts from the actual
  -- object and names no zone, so the Piker the Warren made a land IS a land card
  -- while it sits on top of the library, and the first sentence of rule 701.44a
  -- settles it: hand, no counter, nothing asked.
  --
  -- The zone is the assertion. A prompt count alone would be green for a board
  -- that never explored at all.
  Spec.it s "CR 613.1d a revealed card a continuous effect made a land goes to hand" $ do
    (spell, bystander, board) <- warrenBoard s registry True ["Goblin Piker", "Bird Maiden"]
    let after = runExplore (exploreAnswer OptionalDecision.Exercises) spell board
        walker = namedOnBattlefield "Merfolk Branchwalker" after
    Spec.assertEqWith s "the Piker the Warren made a land is in hand" (zoneNames Zone.Hand after) ["Goblin Piker"]
    Spec.assertEqWith s "and reached it rather than the graveyard" (zoneNames Zone.Graveyard after) []
    Spec.assertBool s (Maybe.isJust walker) "the Branchwalker resolved onto the battlefield"
    Spec.assertEqWith s "CR 701.44a no +1/+1 counter on the land branch" (plusOnePlusOnesOn walker after) 0
    Spec.assertEqWith s "the Maiden it was sitting on is now the top card" (zoneNames Zone.Library after) ["Bird Maiden"]
    Spec.assertEqWith s "the bystanding creature stayed bare" (plusOnePlusOnesOn (Just bystander) after) 0
  -- The negative half of the pair, differing in exactly one thing: whether the
  -- Warren is on the battlefield. Same library, same lands, same answer -- and
  -- the answer is Exercises either way, so the graveyard here is CR 701.44a's
  -- "otherwise" branch being taken and not a different choice.
  Spec.it s "CR 701.44a without the Warren the same Piker is a nonland card and is binned" $ do
    (spell, bystander, board) <- warrenBoard s registry False ["Goblin Piker", "Bird Maiden"]
    let after = runExplore (exploreAnswer OptionalDecision.Exercises) spell board
        walker = namedOnBattlefield "Merfolk Branchwalker" after
    Spec.assertEqWith s "the Piker is in the graveyard" (zoneNames Zone.Graveyard after) ["Goblin Piker"]
    Spec.assertEqWith s "and never reached the hand" (zoneNames Zone.Hand after) []
    Spec.assertEqWith s "one +1/+1 counter" (plusOnePlusOnesOn walker after) 1
    Spec.assertEqWith s "the bystanding creature stayed bare" (plusOnePlusOnesOn (Just bystander) after) 0
  -- CR 701.44b: the permanent explores "even if some or all of those actions were
  -- impossible". No card is revealed, so nothing is a land card and the
  -- "otherwise" branch runs -- the counter goes on with no card to ask about.
  Spec.it s "CR 701.44b an empty library still grows the explorer" $ do
    (spell, bystander, board) <- exploreBoard s registry []
    let after = runExplore (exploreAnswer OptionalDecision.Exercises) spell board
        walker = namedOnBattlefield "Merfolk Branchwalker" after
    Spec.assertBool s (Maybe.isJust walker) "the Branchwalker resolved onto the battlefield"
    Spec.assertEqWith s "one +1/+1 counter" (plusOnePlusOnesOn walker after) 1
    Spec.assertEqWith s "no card moved anywhere" (zoneNames Zone.Hand after <> zoneNames Zone.Graveyard after) []
    Spec.assertEqWith s "and nothing was revealed" (revealedNames after) []
    Spec.assertEqWith s "the bystanding creature stayed bare" (plusOnePlusOnesOn (Just bystander) after) 0

-- The elision half: which boards raise CR 701.44a's question at all. Each case
-- counts the explore prompts one cast-and-settle raises.
explorePromptSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
explorePromptSpec s registry = Spec.describe s "ExplorePrompt" $ do
  let counting :: Prompt.Prompt r -> State.State Int r
      counting p = case p of
        Prompt.ChooseExplore {} -> do
          State.modify (+ 1)
          pure (S.identityAnswer p)
        _ -> pure (S.identityAnswer p)
      asks spell gs =
        State.execState
          ( Engine.runGame counting gs $ do
              S.cast S.alice spell
              Engine.priorityLoop
          )
          0
  -- A real fork, and it is put to the player: the card can end on top or in the
  -- graveyard, and no rule settles which.
  Spec.it s "CR 701.44a a revealed nonland card is asked about" $ do
    (spell, _, board) <- exploreBoard s registry ["Goblin Piker", "Bird Maiden"]
    Spec.assertEqWith s "asked once" (asks spell board) 1
  -- The pair's other half, one card changed. CR 701.44a's first sentence settles
  -- a land card outright, so there is nothing to ask.
  Spec.it s "CR 701.44a a revealed land card raises no question" $ do
    (spell, _, board) <- exploreBoard s registry ["Mountain", "Bird Maiden"]
    Spec.assertEqWith s "not asked" (asks spell board) 0
  -- The land branch reached through CR 613.1d instead of through the printed
  -- face: rule 701.44a settles a land card outright, so a card the Warren made a
  -- land is not asked about either.
  Spec.it s "CR 613.1d a revealed card a continuous effect made a land raises no question" $ do
    (spell, _, board) <- warrenBoard s registry True ["Goblin Piker", "Bird Maiden"]
    Spec.assertEqWith s "not asked" (asks spell board) 0
  -- Nothing was revealed, so there is no card the answer could be about.
  Spec.it s "CR 701.44b an empty library raises no question" $ do
    (spell, _, board) <- exploreBoard s registry []
    Spec.assertEqWith s "not asked" (asks spell board) 0

-- Into the Wilds on the battlefield under alice's control, over a library
-- stocked from the top down. Two seats and no other permanent: the card reads
-- only its controller's own library, so nothing here needs telling apart.
wildsBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> [String] -> m GameState.GameState
wildsBoard s registry deck = do
  wilds <- S.printingOf s registry "Into the Wilds"
  printings <- mapM (S.printingOf s registry) deck
  let (_, withWilds) = S.addCreature wilds S.alice (Setup.emptyGame S.bothPlayers)
      -- addLibraryCard puts its card ON TOP, so the deepest is stocked first.
      deal gs printing = snd (S.addLibraryCard printing S.alice gs)
   in pure (List.foldl' deal withWilds (reverse printings))

-- Begin alice's upkeep, place what triggers (CR 603.3) and resolve it.
runWildsUpkeep :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
runWildsUpkeep answer gs =
  let upkeep = Phase.Beginning BeginningStep.Upkeep
      began =
        Event.recordEvent
          (GameEvent.StepBegan (StepBegan.MkStepBegan upkeep S.alice))
          (gs {GameState.phase = upkeep, GameState.activePlayer = S.alice})
      settled = S.runPure answer began Engine.settleForPriority
   in S.runPure answer settled Engine.priorityLoop

-- Answers CR 603.5's "may" with a FIXED decision, exploreAnswer's posture and
-- for its reason: an answerer deriving its answer from the prompt would still
-- answer legally after a mutation broke which card was looked at.
wildsAnswer :: OptionalDecision.OptionalDecision -> Prompt.Prompt r -> r
wildsAnswer decision p = case p of
  Prompt.ChooseOptional {} -> decision
  _ -> S.identityAnswer p

-- CR 701.20e's look, through Into the Wilds: "At the beginning of your upkeep,
-- look at the top card of your library. If it's a land card, you may put it onto
-- the battlefield."
--
-- The look itself changes NOTHING a board can see, so every case here is about
-- what the clause after it does: the branch has to be taken from the card that
-- was looked at rather than from the library it sits in, which is what the
-- second case pins down.
lookAtSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
lookAtSpec s registry = Spec.describe s "LookAt" $ do
  Spec.it s "CR 701.20e the looked-at land card reaches the battlefield" $ do
    board <- wildsBoard s registry ["Forest", "Bird Maiden"]
    let after = runWildsUpkeep (wildsAnswer OptionalDecision.Exercises) board
    Spec.assertEqWith s "the Forest left the library" (zoneNames Zone.Library after) ["Bird Maiden"]
    Spec.assertEqWith s "and is on the battlefield" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Forest")) S.alice after) 1
    Spec.assertEqWith s "stack empty: the trigger resolved" (length (GameState.stack after)) 0
    -- The whole of CR 701.20e: a look is shown to one player, so it records
    -- nothing where CR 701.20a's reveal would have.
    Spec.assertEqWith s "nothing was revealed on the way" (revealedNames after) []
  -- The pair's other half, and the ONE thing changed is which of the two cards
  -- is on top. A land is in the library either way, so an implementation reading
  -- the library rather than the looked-at card passes the case above and fails
  -- this one.
  Spec.it s "CR 701.20e a nonland top card leaves the land beneath it alone" $ do
    board <- wildsBoard s registry ["Bird Maiden", "Forest"]
    let after = runWildsUpkeep (wildsAnswer OptionalDecision.Exercises) board
    Spec.assertEqWith s "the library is untouched" (zoneNames Zone.Library after) ["Bird Maiden", "Forest"]
    Spec.assertEqWith s "and nothing entered the battlefield" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Forest")) S.alice after) 0
  -- CR 603.5's "may", declined. Without this case a put-always implementation
  -- passes the first one.
  Spec.it s "CR 603.5 declining leaves the land on top of the library" $ do
    board <- wildsBoard s registry ["Forest", "Bird Maiden"]
    let after = runWildsUpkeep (wildsAnswer OptionalDecision.Declines) board
    Spec.assertEqWith s "the library is untouched" (zoneNames Zone.Library after) ["Forest", "Bird Maiden"]
    Spec.assertEqWith s "and nothing entered the battlefield" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Forest")) S.alice after) 0
  -- The SIBLING reader of the land test this unit changed, and the reason the
  -- change did not have to touch it: Into the Wilds asks "if it's a land card"
  -- as a Filter.HasCardType inside the effect DSL, and that path has read the CR
  -- 613 projection of a library card since #1552. With Synthetic Fossil Warren
  -- out, the Goblin Piker it looks at IS a land card and the clause fires --
  -- the same answer Resolve.exploreOne's own land test now gives. Green before
  -- this unit's change as well as after: a fence holding the two readers
  -- together rather than a proof of the change.
  Spec.it s "CR 613.1d a looked-at card a continuous effect made a land reaches the battlefield" $ do
    warren <- S.printingOf s registry "Synthetic Fossil Warren"
    board <- wildsBoard s registry ["Goblin Piker", "Bird Maiden"]
    let after = runWildsUpkeep (wildsAnswer OptionalDecision.Exercises) (snd (S.addCreature warren S.alice board))
    Spec.assertEqWith s "the Piker the Warren made a land left the library" (zoneNames Zone.Library after) ["Bird Maiden"]
    Spec.assertEqWith s "and is on the battlefield" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Goblin Piker")) S.alice after) 1
  -- The negative half, differing in exactly one thing: whether the Warren is on
  -- the battlefield. Same library, same answer.
  Spec.it s "CR 701.20e without the Warren the same Piker is no land card" $ do
    board <- wildsBoard s registry ["Goblin Piker", "Bird Maiden"]
    let after = runWildsUpkeep (wildsAnswer OptionalDecision.Exercises) board
    Spec.assertEqWith s "the library is untouched" (zoneNames Zone.Library after) ["Goblin Piker", "Bird Maiden"]
    Spec.assertEqWith s "and nothing entered the battlefield" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Goblin Piker")) S.alice after) 0
  -- CR 609.3: an empty library has no top card, so the look names nothing, the
  -- slot goes unbound and the clause after it finds no land.
  Spec.it s "CR 609.3 an empty library looks at nothing and does nothing" $ do
    board <- wildsBoard s registry []
    let after = runWildsUpkeep (wildsAnswer OptionalDecision.Exercises) board
    Spec.assertEqWith s "the library is still empty" (zoneNames Zone.Library after) []
    Spec.assertEqWith s "stack empty: the trigger resolved" (length (GameState.stack after)) 0

-- The elision half: CR 608.2a's gate is asked BEFORE CR 603.5's "may", so a top
-- card that is not a land is never a question. Counts the optional prompts one
-- upkeep raises.
lookAtPromptSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
lookAtPromptSpec s registry = Spec.describe s "LookAtPrompt" $ do
  let counting :: Prompt.Prompt r -> State.State Int r
      counting p = case p of
        Prompt.ChooseOptional {} -> do
          State.modify (+ 1)
          pure (S.identityAnswer p)
        _ -> pure (S.identityAnswer p)
      asks gs =
        let upkeep = Phase.Beginning BeginningStep.Upkeep
            began =
              Event.recordEvent
                (GameEvent.StepBegan (StepBegan.MkStepBegan upkeep S.alice))
                (gs {GameState.phase = upkeep, GameState.activePlayer = S.alice})
         in State.execState
              (Engine.runGame counting began (Engine.settleForPriority >> Engine.priorityLoop))
              0
  Spec.it s "a looked-at land card is asked about" $ do
    board <- wildsBoard s registry ["Forest", "Bird Maiden"]
    Spec.assertEqWith s "asked once" (asks board) 1
  Spec.it s "a looked-at nonland card raises no question" $ do
    board <- wildsBoard s registry ["Bird Maiden", "Forest"]
    Spec.assertEqWith s "not asked" (asks board) 0
  Spec.it s "CR 609.3 an empty library raises no question" $ do
    board <- wildsBoard s registry []
    Spec.assertEqWith s "not asked" (asks board) 0

slotTarget :: SlotName.SlotName
slotTarget = SlotName.MkSlotName (Text.pack "target")

-- Diabolic Edict's "a creature of their choice".
creatureFilter :: Filter.Type.Filter Keyword.Keyword
creatureFilter = Filter.Type.HasCardType CardType.Creature

-- Targets `victim` with every slot that offers them, deferring the rest to
-- S.identityAnswer -- which picks the lowest ObjectId/PlayerId and so would aim
-- an edict at its own caster.
targetsPlayer :: PlayerId.PlayerId -> Prompt.Prompt r -> r
targetsPlayer victim p = case p of
  Prompt.ChooseTargets _ _ _ sets ->
    fmap
      (\(n, legal) -> Set.fromList (take (Natural.toIntSaturating n) (List.nub (filter (== Recipient.ToPlayer victim) (Set.toAscList legal) <> Set.toAscList legal))))
      sets
  _ -> S.identityAnswer p

-- A lying interpreter: names `wanted` for a sacrifice regardless of whether it
-- was offered. The only way to reach CR 701.21a's guard from a test, since the
-- candidate list is built from what the sacrificing player controls.
namesInstead :: ObjectId.ObjectId -> Prompt.Prompt r -> r
namesInstead wanted p = case p of
  Prompt.ChooseSacrifices {} -> Set.singleton wanted
  Prompt.ChooseAnyNumberToSacrifice {} -> Set.empty
  Prompt.ChooseTapsForTotalPower _ _ _ candidates _ -> Set.fromList candidates
  _ -> S.identityAnswer p

-- Answers Prompt.ChooseSacrifices with `wanted`, when it is on offer. A pair of
-- tests differing only in this argument proves the ANSWER decides which permanent
-- is sacrificed, rather than the order the candidates are enumerated in.
sacrifices :: ObjectId.ObjectId -> Prompt.Prompt r -> r
sacrifices wanted p = case p of
  Prompt.ChooseSacrifices _ _ _ candidates _ ->
    if elem wanted candidates then Set.singleton wanted else Set.fromList (take 1 candidates)
  Prompt.ChooseAnyNumberToSacrifice {} -> Set.empty
  Prompt.ChooseTapsForTotalPower _ _ _ candidates _ -> Set.fromList candidates
  _ -> S.identityAnswer p

playerSacrificesSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
playerSacrificesSpec s registry = Spec.describe s "PlayerSacrifices" $ do
  -- CR 701.21a: "its controller moves it from the battlefield directly to its
  -- owner's graveyard." Diabolic Edict names a PLAYER, and that player picks.
  Spec.it s "Diabolic Edict: the targeted player chooses which of their creatures dies" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    rats <- S.printingOf s registry "Typhoid Rats"
    let (src, g0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
        (hisPiker, g1) = S.addCreature piker S.bob g0
        (hisRats, gs) = S.addCreature rats S.bob g1
        edict = Resolve.applyEffect src src S.alice (Map.singleton slotTarget (Set.singleton (Recipient.ToPlayer S.bob))) (Map.singleton slotTarget (Set.singleton (Recipient.ToPlayer S.bob))) (Effect.PlayerSacrifices (PlayerSacrifices.MkPlayerSacrifices slotTarget creatureFilter (Quantity.Literal 1)))
        keptRats = S.runPure (sacrifices hisPiker) gs edict
        keptPiker = S.runPure (sacrifices hisRats) gs edict
    Spec.assertBool s (S.onBattlefield hisRats keptRats) "choosing the Piker leaves the Rats"
    Spec.assertBool s (not (S.onBattlefield hisPiker keptRats)) "and the Piker is gone"
    -- The discriminating twin: same board, same effect, opposite answer.
    Spec.assertBool s (S.onBattlefield hisPiker keptPiker) "choosing the Rats leaves the Piker"
    Spec.assertBool s (not (S.onBattlefield hisRats keptPiker)) "and the Rats are gone"
    Spec.assertBool s (S.onBattlefield src keptRats) "alice's own creature is never touched"
  -- CR 701.21a: "A player can't sacrifice ... a permanent they don't control."
  -- The guard the whole issue is about, reached the only way it can be: an
  -- interpreter naming a permanent outside the offered set.
  --
  -- Bob controls TWO creatures on purpose. With one, candidates <= count and
  -- the prompt is elided, so the lying answerer is never consulted and the
  -- test passes without exercising anything -- which is what it did before
  -- review caught it.
  Spec.it s "CR 701.21a an answer naming a permanent the player does not control is refused" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    rats <- S.printingOf s registry "Typhoid Rats"
    let (src, g0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
        (hers, g1) = S.addCreature piker S.alice g0
        (hisPiker, g2) = S.addCreature piker S.bob g1
        (hisRats, gs) = S.addCreature rats S.bob g2
        after = S.runPure (namesInstead hers) gs (Resolve.applyEffect src src S.alice (Map.singleton slotTarget (Set.singleton (Recipient.ToPlayer S.bob))) (Map.singleton slotTarget (Set.singleton (Recipient.ToPlayer S.bob))) (Effect.PlayerSacrifices (PlayerSacrifices.MkPlayerSacrifices slotTarget creatureFilter (Quantity.Literal 1))))
        bobsLeft = length (filter (`S.onBattlefield` after) [hisPiker, hisRats])
    Spec.assertBool s (S.onBattlefield hers after) "alice's creature is untouched"
    -- The edict still takes exactly one: an answer the engine refuses does not
    -- become an answer of "none". CR 609.3 caps it at what bob controls, and
    -- he controls two.
    Spec.assertEqWith s "bob still lost exactly one of his own" bobsLeft 1
  -- Where the rules leave nothing to ask, don't prompt: one candidate is
  -- forced (CR 609.3 does as much as possible, which here is all of it).
  Spec.it s "CR 609.3 a lone creature is sacrificed without a prompt" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (src, g0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
        (his, gs) = S.addCreature piker S.bob g0
        countingAnswer :: Prompt.Prompt r -> State.State Int r
        countingAnswer p = case p of
          Prompt.ChooseSacrifices {} -> do
            State.modify (+ 1)
            pure (S.identityAnswer p)
          _ -> pure (S.identityAnswer p)
        act = Resolve.applyEffect src src S.alice (Map.singleton slotTarget (Set.singleton (Recipient.ToPlayer S.bob))) (Map.singleton slotTarget (Set.singleton (Recipient.ToPlayer S.bob))) (Effect.PlayerSacrifices (PlayerSacrifices.MkPlayerSacrifices slotTarget creatureFilter (Quantity.Literal 1)))
        asked = State.execState (Engine.runGame countingAnswer gs act) 0
        after = S.runPure S.identityAnswer gs act
    Spec.assertEqWith s "nothing to choose" asked 0
    Spec.assertBool s (not (S.onBattlefield his after)) "but it still died"
  -- CR 609.3 again: a player with no creatures sacrifices nothing, and the
  -- edict simply does as much as it can -- which is nothing.
  Spec.it s "CR 609.3 an edict against an empty board does nothing" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (src, gs) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
        after = S.runPure S.identityAnswer gs (Resolve.applyEffect src src S.alice (Map.singleton slotTarget (Set.singleton (Recipient.ToPlayer S.bob))) (Map.singleton slotTarget (Set.singleton (Recipient.ToPlayer S.bob))) (Effect.PlayerSacrifices (PlayerSacrifices.MkPlayerSacrifices slotTarget creatureFilter (Quantity.Literal 1))))
    Spec.assertBool s (S.onBattlefield src after) "alice keeps hers"
  -- The gameplay-level proof: the real card, cast and resolved.
  Spec.it s "Diabolic Edict whole card: cast off two Swamps, bob sacrifices" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    diabolicEdict <- S.printingOf s registry "Diabolic Edict"
    let base = S.landsInPlay swamp 2
        (his, g1) = S.addCreature piker S.bob base
        (withSpell, spell) = S.handOne diabolicEdict g1
        afterCast = S.runPure (targetsPlayer S.bob) withSpell (S.cast S.alice spell)
        resolved = S.runPure (targetsPlayer S.bob) afterCast Stack.resolveTop
    Spec.assertEqWith s "stack empty" (length (GameState.stack resolved)) 0
    Spec.assertBool s (not (S.onBattlefield his resolved)) "bob's creature was sacrificed"

createEmblemSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
createEmblemSpec s registry = Spec.describe s "CreateEmblem" $ do
  Spec.it s "CR 114.2 CreateEmblem puts an emblem in the command zone under the resolver" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (src, gs0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
        act = Resolve.applyEffect src src S.alice Map.empty Map.empty (Effect.CreateEmblem (Printing.card piker))
        after = S.runPure S.identityAnswer gs0 act
        emblems = filter (\oid -> fmap Object.zone (Game.lookupObject oid after) == Just Zone.Command) (Set.toList (GameState.command after))
    Spec.assertEqWith s "one emblem in command" (Set.size (GameState.command after)) 1
    Spec.assertEqWith s "owned by the resolver" (fmap (\oid -> fmap Object.owner (Game.lookupObject oid after)) emblems) [Just S.alice]

becomeMonarchSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
becomeMonarchSpec s registry = Spec.describe s "BecomeMonarch" $ do
  Spec.it s "CR 725 BecomeMonarch TheController makes the resolver the monarch" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (src, gs0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
        after = S.runPure S.identityAnswer gs0 (Resolve.applyEffect src src S.alice Map.empty Map.empty (Effect.BecomeMonarch MonarchTarget.TheController))
    Spec.assertEqWith s "alice is monarch" (GameState.monarch after) (Just S.alice)
    Spec.assertBool s (elem (GameEvent.BecameMonarch S.alice) (S.eventsOf after)) "a BecameMonarch event was recorded"

-- The slot Denethor's crown half names, and the slot its damage half names.
denethorCrownSlot, denethorDamageSlot :: SlotName.SlotName
denethorCrownSlot = SlotName.MkSlotName (Text.pack "player")
denethorDamageSlot = SlotName.MkSlotName (Text.pack "damage")

-- Fills every CR 601.2c slot BY NAME from `answers`, and records the map it
-- returned.
--
-- Both halves matter, and both are about this being the pool's first card with
-- TWO target slots in one mode. S.identityAnswer fills every slot with the
-- lowest-sorting legal recipient, so left to itself it answers both slots the
-- same shape and "bob became the monarch" could be an accident of PlayerId
-- ordering rather than of the crown reading its own slot; overriding by name is
-- what makes the two slots differ. Recording is how the test asserts the map it
-- fed in is the map the engine received, instead of inferring it from the board.
answerSlots ::
  Map.Map SlotName.SlotName (Set.Set Recipient.Recipient) ->
  Prompt.Prompt r ->
  State.State [Map.Map SlotName.SlotName (Set.Set Recipient.Recipient)] r
answerSlots answers p = case p of
  Prompt.ChooseTargets {} -> do
    let deflt = S.identityAnswer p
        filled = Map.union (Map.intersection answers deflt) deflt
    State.modify' (<> [filled])
    pure filled
  _ -> pure (S.identityAnswer p)

-- Denethor, Stone Seer -- "{3}{R}, {T}, Sacrifice Denethor: Target player
-- becomes the monarch. Denethor deals 3 damage to any target."
--
-- The printed card also has "When Denethor enters, scry 2", which
-- data/cards/denethor-stone-seer.json now carries: Effect.Scry landed with
-- Crystal Ball. It reaches none of the assertions below -- S.addCreature places
-- the permanent rather than moving it there, so no CR 603.2 entry trigger is
-- gathered, and the ability under test is the activated one.
--
-- Settled under alice, who already holds the crown, with four Mountains to pay
-- the {3}{R} and priority in hand. The first activated ability of the card is
-- the one under test; the empty fallback is ActivateSpec.theAbility's, and would
-- fail every assertion below rather than silently pass one.
--
-- FOUR seats. At two players "target player" and "the controller's one opponent"
-- name the same seat, so a two-seat board cannot tell which arm the resolver
-- took; three separate the crown's target (bob) from the damage's (carol) from
-- the controller (alice). The fourth (dave) is what lets CR 608.2b's
-- all-targets-illegal case be reached by conceding both targets without CR
-- 104.2a ending the game first.
denethorBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  m (ActivatedAbility.ActivatedAbility Card.Type.Card, ObjectId.ObjectId, GameState.GameState)
denethorBoard s registry = do
  denethor <- S.printingOf s registry "Denethor, Stone Seer"
  mountain <- S.printingOf s registry "Mountain"
  let lands = List.foldl' (\gs _ -> snd (S.addCreature mountain S.alice gs)) (Setup.emptyGame S.fourPlayers) [1 .. 4 :: Int]
      (srcId, gs1) = S.addCreature denethor S.alice lands
      ability = case Face.activatedAbilities (S.combinedFace denethor) of
        ab : _ -> ab
        [] -> ActivatedAbility.MkActivatedAbility (Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) []) (Modal.MkModal (Seq.singleton (Mode.MkMode Seq.empty Map.empty)) (ModeSelection.ChooseExactly 1)) [] Nothing Nothing
  pure (ability, srcId, S.withMonarch S.alice (gs1 {GameState.priority = Just S.alice}))

-- CR 725.1: "The monarch is a designation a player can have. There is no monarch
-- in a game until an effect instructs a player to become the monarch." Every
-- BecomeMonarch before this one derived the player it crowned -- the resolving
-- controller, or CR 725.2's controller of the damaging creature. Denethor is the
-- first card in the pool whose crown reads a TARGET slot, so the player it names
-- is the activator's CHOICE, announced under CR 601.2c and re-checked under CR
-- 608.2b like any other target.
--
-- CR 601.2c is what lets the two slots coexist: "if the spell uses the word
-- 'target' in multiple places, the same object or player can be chosen once for
-- each instance of the word 'target'". Denethor writes it twice, so the crown
-- and the damage are independent choices that may or may not land on one player.
targetedMonarchSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
targetedMonarchSpec s registry = Spec.describe s "TargetedMonarch" $ do
  Spec.it s "CR 725.1/725.3 the crown goes to the TARGETED player, not the controller and not the damage's target" $ do
    (ability, srcId, gs0) <- denethorBoard s registry
    let answers = Map.fromList [(denethorCrownSlot, Set.singleton (Recipient.ToPlayer S.bob)), (denethorDamageSlot, Set.singleton (Recipient.ToPlayer S.carol))]
        act = do Activate.activateAbility S.alice srcId ability; Stack.resolveTop
        ((_, after), asked) = State.runState (Engine.runGame (answerSlots answers) gs0 act) []
    Spec.assertEqWith s "alice held the crown going in" (GameState.monarch gs0) (Just S.alice)
    Spec.assertEqWith s "CR 601.2c asked once, for both slots, and got the map fed in" asked [answers]
    Spec.assertEqWith s "CR 725.3 the crown moved to bob, the targeted player" (GameState.monarch after) (Just S.bob)
    Spec.assertBool s (elem (GameEvent.BecameMonarch S.bob) (S.eventsOf after)) "and the crowning event names bob"
    Spec.assertEqWith s "CR 115.4 carol, the any-target, took the 3" (S.lifeOf S.carol after) (Just 17)
    Spec.assertEqWith s "bob took none of it" (S.lifeOf S.bob after) (Just 20)
    Spec.assertEqWith s "and neither did alice" (S.lifeOf S.alice after) (Just 20)
    Spec.assertEqWith s "the cost sacrificed Denethor into alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
    Spec.assertEqWith s "and the ability left the stack" (GameState.stack after) []

  -- CR 725.3: "Only one player can be the monarch at a time. As a player becomes
  -- the monarch, the current monarch ceases to be the monarch." The unseating is
  -- observable only through CR 725.2's inherent end-step draw, which belongs to
  -- whoever holds the crown -- so alice, who held it, must stop drawing and bob,
  -- who took it, must start.
  Spec.it s "CR 725.3 the unseated monarch stops drawing at end step, and the new one starts" $ do
    (ability, srcId, gs0) <- denethorBoard s registry
    piker <- S.printingOf s registry "Goblin Piker"
    let answers = Map.fromList [(denethorCrownSlot, Set.singleton (Recipient.ToPlayer S.bob)), (denethorDamageSlot, Set.singleton (Recipient.ToPlayer S.carol))]
        act = do Activate.activateAbility S.alice srcId ability; Stack.resolveTop
        ((_, after), _) = State.runState (Engine.runGame (answerSlots answers) gs0 act) []
        -- CR 104.3c: a seat asked to draw from an empty library loses instead, so
        -- both candidates get a card. That also makes "drew nothing" mean the
        -- trigger did not fire rather than that there was nothing to take.
        stocked = snd (S.addLibraryCard piker S.bob (snd (S.addLibraryCard piker S.alice after)))
        endStep = Phase.Ending EndingStep.EndStep
        endStepOf pid gs = Event.recordEvent (GameEvent.StepBegan (StepBegan.MkStepBegan endStep pid)) (gs {GameState.phase = endStep, GameState.activePlayer = pid})
        run gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
    Spec.assertEqWith s "bob really has the crown" (GameState.monarch after) (Just S.bob)
    Spec.assertEqWith s "CR 725.2 bob, the new monarch, draws on his own end step" (length (Game.zoneMembers Zone.Hand S.bob (run (endStepOf S.bob stocked)))) 1
    Spec.assertEqWith s "CR 725.3 alice, unseated, draws nothing on hers" (length (Game.zoneMembers Zone.Hand S.alice (run (endStepOf S.alice stocked)))) 0

  -- CR 608.2b: "If all its targets, for every instance of the word 'target', are
  -- now illegal, the spell or ability doesn't resolve. ... Otherwise, the spell
  -- or ability will resolve normally. Illegal targets, if any, won't be affected
  -- by parts of a resolving spell's effect for which they're illegal. Other parts
  -- of the effect for which those targets are not illegal may still affect them."
  --
  -- Denethor is the first card in the pool that can reach the PARTIAL clause: it
  -- takes two targets in one mode, so exactly one of them can go illegal. A
  -- conceding player is the lever: CR 104.3a takes them out of the game
  -- immediately, so Target.playerRecipients -- which is built from
  -- Game.stillPlaying -- stops offering them, and both of Denethor's slots take
  -- players. CR 800.4 is what lets the game go on around the concession.
  Spec.it s "CR 608.2b one illegal target does not fizzle the ability, and the other half still happens" $ do
    (ability, srcId, gs0) <- denethorBoard s registry
    let answers = Map.fromList [(denethorCrownSlot, Set.singleton (Recipient.ToPlayer S.bob)), (denethorDamageSlot, Set.singleton (Recipient.ToPlayer S.carol))]
        activated = snd (State.evalState (Engine.runGame (answerSlots answers) gs0 (Activate.activateAbility S.alice srcId ability)) [])
        concede = S.departs Departure.Type.Conceded
        resolveAfter f = S.runPure S.identityAnswer (f activated) Stack.resolveTop
        damageIllegal = resolveAfter (concede S.carol)
        crownIllegal = resolveAfter (concede S.bob)
        bothIllegal = resolveAfter (concede S.bob . concede S.carol)
    Spec.assertEqWith s "the ability waits on the stack with both targets chosen" (length (GameState.stack activated)) 1
    -- The damage's target is gone; the crown's is not, so the crown still moves.
    Spec.assertEqWith s "carol illegal: bob is still crowned" (GameState.monarch damageIllegal) (Just S.bob)
    Spec.assertEqWith s "and the 3 damage went nowhere" (fmap (`S.lifeOf` damageIllegal) [S.alice, S.bob, S.dave]) [Just 20, Just 20, Just 20]
    -- The mirror: the crown's target is gone, the damage's is not.
    Spec.assertEqWith s "bob illegal: the crown does not move" (GameState.monarch crownIllegal) (Just S.alice)
    Spec.assertBool s (notElem (GameEvent.BecameMonarch S.bob) (S.eventsOf crownIllegal)) "and nobody was crowned"
    Spec.assertEqWith s "but carol still took the 3" (S.lifeOf S.carol crownIllegal) (Just 17)
    -- Both gone: CR 608.2b's first clause, the ability does not resolve. Denethor
    -- cannot tell that apart from resolving with both slots skipped -- every
    -- effect it has is slot-gated, so the two produce the same board -- so what
    -- is asserted here is the OUTCOME, which the rule fixes either way. The
    -- discriminating half of CR 608.2b is the partial clause above.
    Spec.assertEqWith s "both illegal: no crown moves" (GameState.monarch bothIllegal) (Just S.alice)
    Spec.assertEqWith s "and no damage is dealt" (fmap (`S.lifeOf` bothIllegal) [S.alice, S.dave]) [Just 20, Just 20]
    Spec.assertEqWith s "the ability leaves the stack either way" (GameState.stack bothIllegal) []

  -- The classification half, asserted directly. slotsOf is the READ side of the
  -- D4 dataflow lint and has no runtime consumer: Resolve.resolveModes re-derives
  -- CR 608.2b's legality from the card's declared targetSlots, so the gameplay
  -- cases above pass whatever slotsOf answers.
  --
  -- The InSlot line is now ALSO covered by CardSpec's dataflow lint, which since
  -- #1043 states its equality over an activated ability's modes too -- reverting
  -- this arm to Set.empty fails Denethor there as well as here, and the
  -- TheController line is swept the same way, six cards writing that arm (Palace
  -- Jailer, Queen Marchesa, Custodi Lich, Dawnglade Regent, Entourage of Trest,
  -- Marchesa's Decree). Kept rather than deleted for the one line the lint cannot
  -- reach: CR 725.2's crown steal is a rules-minted ability, so
  -- ControllerOfSource comes from Pawl.Engine.Monarch.crownSteal rather than from
  -- any card file, and an arm wrongly REPORTING a slot for it would be swept by
  -- nothing. That is the arm-level pin; the first two lines are a locality
  -- convenience, keeping all three answers in one place.
  Spec.it s "CR 725.1 slotsOf reads the targeted monarch's slot, and only that arm's" $ do
    let slot = SlotName.MkSlotName (Text.pack "player")
    Spec.assertEqWith s "the targeted arm names its slot" (Resolve.slotsOf (Effect.BecomeMonarch (MonarchTarget.InSlot slot))) (Map.singleton slot SlotArity.One)
    Spec.assertEqWith s "the resolving controller names none" (Resolve.slotsOf (Effect.BecomeMonarch MonarchTarget.TheController)) Map.empty
    Spec.assertEqWith s "and neither does CR 725.2's crown steal" (Resolve.slotsOf (Effect.BecomeMonarch MonarchTarget.ControllerOfSource)) Map.empty

-- Palace Jailer's ruling (Scryfall, 2021-03-19): "If you're not the monarch as
-- Palace Jailer's second ability resolves, the creature will be exiled until
-- there's a new monarch and that player is one of your opponents. The creature
-- won't immediately return just because an opponent is the monarch." A companion
-- ruling fixes the same reading from the other side: "Palace Jailer leaving the
-- battlefield won't cause the exiled creature to return. The game will continue
-- to watch for the NEXT TIME an opponent becomes the monarch."
--
-- So the watch is for an EVENT -- a new monarch being crowned who is an opponent
-- -- not for the STATE "an opponent currently holds the crown".
exileUntilMonarchSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
exileUntilMonarchSpec s registry = Spec.describe s "ExileUntilMonarch" $ do
  -- Reachable at two seats: CR 603.3b lets alice order Palace Jailer's two
  -- entry triggers, so the exile can resolve BEFORE she becomes the monarch,
  -- while bob still holds the crown.
  Spec.it s "CR 725 an exile that resolves while an opponent is already the monarch does not return at once" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (oid, base0) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
        base = base0 {GameState.monarch = Just S.bob}
        slot = SlotName.MkSlotName (Text.pack "target")
        exile =
          Resolve.applyEffect
            S.noSource
            S.noSource
            S.alice
            (Map.singleton slot (Set.singleton (Recipient.ToCreature oid)))
            (Map.singleton slot (Set.singleton (Recipient.ToCreature oid)))
            (Effect.ExileUntilMonarch slot)
        exiled = snd (Engine.runGamePure S.identityAnswer base exile)
        settled = snd (Engine.runGamePure S.identityAnswer exiled Monarch.returnExiledForMonarch)
    Spec.assertEqWith s "the watch was registered" (Map.size (GameState.exiledUntilMonarch exiled)) 1
    Spec.assertEqWith s "bob is still the monarch, unchanged" (GameState.monarch settled) (Just S.bob)
    Spec.assertEqWith s "nothing came back to the battlefield" (Set.size (GameState.battlefield settled)) 0
    Spec.assertEqWith s "and the watch is still armed" (Map.size (GameState.exiledUntilMonarch settled)) 1
  -- The whole arc, still two seats. The crown must actually CHANGE HANDS to an
  -- opponent before the creature comes back, and alice taking it herself in
  -- between must not discharge the watch.
  Spec.it s "CR 725 the exile returns when a NEW monarch is crowned who is an opponent" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (oid, base0) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
        base = base0 {GameState.monarch = Just S.bob}
        slot = SlotName.MkSlotName (Text.pack "target")
        exile =
          Resolve.applyEffect
            S.noSource
            S.noSource
            S.alice
            (Map.singleton slot (Set.singleton (Recipient.ToCreature oid)))
            (Map.singleton slot (Set.singleton (Recipient.ToCreature oid)))
            (Effect.ExileUntilMonarch slot)
        exiled = snd (Engine.runGamePure S.identityAnswer base exile)
        -- Palace Jailer's OTHER entry trigger: alice takes the crown. She is
        -- not her own opponent, so this must not return the creature. Through
        -- Monarch.crown and not a write to GameState.monarch, because that is
        -- where a crowning marks the watches now -- a bare field write is not a
        -- crowning at all.
        alicesCrown = snd (Engine.runGamePure S.identityAnswer (Monarch.crown S.alice exiled) Monarch.returnExiledForMonarch)
        -- bob deals combat damage to the monarch (CR 725.3) and takes it back.
        bobsCrown = snd (Engine.runGamePure S.identityAnswer (Monarch.crown S.bob alicesCrown) Monarch.returnExiledForMonarch)
    Spec.assertEqWith s "alice holding the crown does not discharge the watch" (Map.size (GameState.exiledUntilMonarch alicesCrown)) 1
    Spec.assertEqWith s "nor return the creature" (Set.size (GameState.battlefield alicesCrown)) 0
    Spec.assertEqWith s "bob retaking it does return the creature" (Set.size (GameState.battlefield bobsCrown)) 1
    Spec.assertEqWith s "and discharges the watch" (Map.size (GameState.exiledUntilMonarch bobsCrown)) 0
  -- The crown VANISHING is not an opponent becoming the monarch. CR 725.1's
  -- ruling says the game keeps exactly one monarch once it has one, and the
  -- single way back to none is CR 725.4's last player standing leaving -- but
  -- the watch must not read "no monarch" as "not the controller" and fire.
  Spec.it s "CR 725.1 the crown vanishing is not an opponent becoming the monarch" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (oid, base0) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
        base = base0 {GameState.monarch = Just S.bob}
        slot = SlotName.MkSlotName (Text.pack "target")
        exile =
          Resolve.applyEffect
            S.noSource
            S.noSource
            S.alice
            (Map.singleton slot (Set.singleton (Recipient.ToCreature oid)))
            (Map.singleton slot (Set.singleton (Recipient.ToCreature oid)))
            (Effect.ExileUntilMonarch slot)
        exiled = snd (Engine.runGamePure S.identityAnswer base exile)
        -- CR 725.4's third sentence is the only way back to no monarch, and it
        -- crowns nobody, so this is a bare field write by construction.
        noMonarch = snd (Engine.runGamePure S.identityAnswer exiled {GameState.monarch = Nothing} Monarch.returnExiledForMonarch)
    Spec.assertEqWith s "the watch is still armed" (Map.size (GameState.exiledUntilMonarch noMonarch)) 1
    Spec.assertEqWith s "and nothing returned" (Set.size (GameState.battlefield noMonarch)) 0

  -- SYNTHETIC. "Synthetic Regency Swap" {1}{W} Sorcery: "Target player becomes
  -- the monarch. Then you become the monarch." Two crownings in ONE resolution,
  -- which is what #208 needs and what no printing does: Scryfall
  -- oracle:"become the monarch" (2026-08-20) returns fifty-five cards, and the
  -- five that can crown somebody other than their controller -- Denethor, Stone
  -- Seer, Eomer, King of Rohan, Garland, Royal Kidnapper, Jared Carthalion, True
  -- Heir and M'Baku, Jabari Chieftain -- each crown exactly one player per
  -- resolution, so every printed sequence of two crownings has a settle between
  -- them. Nothing in rule 725 forbids a card that crowns twice; a printing that
  -- does replaces this one.
  --
  -- The crown ends the resolution where it began, so NO reading of the current
  -- monarch, at this settle or any later one, can see that bob held it. Only the
  -- crowning itself can.
  Spec.it s "CR 725 a crown that goes to an opponent and back inside one resolution still frees the prisoner" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    plains <- S.printingOf s registry "Plains"
    palaceJailer <- S.printingOf s registry "Palace Jailer"
    regencySwap <- S.printingOf s registry "Synthetic Regency Swap"
    let (_, g1) = S.addCreature piker S.carol S.threePlayerGame
        g2 = S.landsFor plains S.alice 2 g1
        (_, g3) = S.entersWithTrigger palaceJailer S.alice g2
        -- Palace Jailer's two entry triggers resolve: alice takes the crown, and
        -- carol's Piker -- the only creature an opponent controls, so the target
        -- is forced -- is exiled under the watch.
        armed = S.runPure S.identityAnswer g3 Engine.priorityLoop
        (withSpell, spell) = S.handOne regencySwap armed
        -- FILTER the offered set rather than building a recipient: CR 608.2b
        -- re-reads the target at resolution, and a hand-built one is a different
        -- recipient the re-read would drop.
        crownsTo :: PlayerId.PlayerId -> Prompt.Prompt r -> r
        crownsTo who p = case p of
          Prompt.ChooseTargets _ _ _ sets -> S.preferring (== Recipient.ToPlayer who) sets
          _ -> S.identityAnswer p
        -- `crownsTo who` is spelled out at each call rather than let-bound:
        -- MonoLocalBinds (this module turns GADTs on) would fix a local binding
        -- at one `r`, where S.runPure asks for a rank-2 answerer.
        run who =
          let castGs = S.runPure (crownsTo who) withSpell (S.cast S.alice spell)
              resolved = S.runPure (crownsTo who) castGs Stack.resolveTop
           in S.runPure (crownsTo who) resolved Engine.settleForPriority
        -- Run A: the crown goes to bob and comes straight back to alice.
        toBob = run S.bob
        -- Run B: the same board and the same spell, with alice naming HERSELF.
        -- She is not her own opponent, and the second crowning finds her already
        -- crowned, so no opponent becomes the monarch at any point.
        toAlice = run S.alice
    -- The fixture really is what the test claims.
    Spec.assertEqWith s "alice holds the crown before the spell" (GameState.monarch withSpell) (Just S.alice)
    Spec.assertEqWith s "exactly one creature is under the watch" (Map.size (GameState.exiledUntilMonarch withSpell)) 1
    Spec.assertEqWith s "and carol's Piker is off the battlefield" (S.creaturesInPlay S.carol withSpell) 0
    -- Run A, the behaviour this case exists to prove. CR 400.7 gives the
    -- returning card yet another id, so carol's creature COUNT is what survives.
    Spec.assertEqWith s "bob's reign inside the resolution freed the prisoner" (S.creaturesInPlay S.carol toBob) 1
    Spec.assertEqWith s "though the crown is back with alice, so no later look at the monarch could tell" (GameState.monarch toBob) (Just S.alice)
    Spec.assertEqWith s "and the watch is discharged" (Map.size (GameState.exiledUntilMonarch toBob)) 0
    -- Run B: one different answer, and nothing else.
    Spec.assertEqWith s "alice crowning herself frees nobody" (S.creaturesInPlay S.carol toAlice) 0
    Spec.assertEqWith s "she is still the monarch" (GameState.monarch toAlice) (Just S.alice)
    Spec.assertEqWith s "and the watch is still armed" (Map.size (GameState.exiledUntilMonarch toAlice)) 1
    Spec.assertEqWith s "both runs resolved the spell" (length (GameState.stack toBob), length (GameState.stack toAlice)) (0, 0)

-- M4.5 P1 gate: Act of Treason strings GainControl + Untap + ModifyTarget
-- (GainKeyword Haste) together end to end -- cast, resolve, attack, revert.
actOfTreasonSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
actOfTreasonSpec s registry = Spec.describe s "Act of Treason" $ do
  Spec.it s "steal, untap, haste, attack, then revert" $ do
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    actOfTreason <- S.printingOf s registry "Act of Treason"
    let base0 = S.landsInPlay mountain 3 -- alice: {R}{R}{R} for {2}{R}
        (oid, base1) = S.addCreature piker S.bob base0
        base = S.tapObject oid base1 -- start it tapped to prove the untap rider
        (gs1, spellId) = S.handOne actOfTreason base
        cast = snd (Engine.runGamePure S.identityAnswer gs1 (S.cast S.alice spellId))
        resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "alice controls the Piker" (Projection.controllerOf oid resolved) (Just S.alice)
    Spec.assertEqWith s "the untap rider untapped it" (fmap Object.tapped (Game.lookupObject oid resolved)) (Just TapState.Untapped)
    Spec.assertBool s (Projection.hasKeyword Keyword.Haste oid resolved) "it has haste"
    Spec.assertBool s (oid `elem` Combat.legalAttackers S.alice resolved) "alice may attack with it this turn"
    Spec.assertBool s (oid `notElem` Combat.legalAttackers S.bob resolved) "bob may not attack with it"
    Spec.assertEqWith s "control reverts at cleanup" (Projection.controllerOf oid (Expiry.dropAtCleanup resolved)) (Just S.bob)

-- CR 603.5 / 608.2d: an OPTIONAL effect -- "you may" -- decided as the ability
-- resolves, not as it is put on the stack.
--
-- Renewed Faith is the card: a {2}{W} instant with "You gain 6 life", Cycling
-- {1}{W}, and "When you cycle this card, you may gain 2 life". It targets
-- nothing, so nothing here can be passing on the targeting machinery: the only
-- new thing is whether the trigger's one effect happens.
optionalEffectSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
optionalEffectSpec s registry =
  let -- Takes the option ONLY if the prompt names the right decider, the right
      -- player and the right mode. A prompt addressed to anybody else, or naming
      -- a mode this ability does not have, declines -- so the life total below
      -- is discriminating about the whole payload, not just about the answer.
      takeOptional :: Prompt.Prompt r -> r
      takeOptional p = case p of
        Prompt.ChooseOptional (Decider.MkDecider d) player _ idx cIdx
          | d == S.alice && player == S.alice && idx == ModeIndex.MkModeIndex 0 && cIdx == ClauseIndex.MkClauseIndex 0 ->
              OptionalDecision.Exercises
        Prompt.ChooseOptional {} -> OptionalDecision.Declines
        _ -> S.identityAnswer p
      -- The named card in alice's hand with two of the named land in play, which
      -- is what Renewed Faith's {1}{W} cycling costs, and alice holding priority.
      handWithTwoLands printing land = do
        faith <- S.printingOf s registry printing
        plains <- S.printingOf s registry land
        let (g1, faithId) = S.handOne faith (S.landsInPlay plains 2)
        pure (g1 {GameState.priority = Just S.alice}, faithId)
      -- Deem Worthy in hand with four Mountains for its {3}{R} cycling, and one
      -- Goblin Piker on the battlefield as the only legal creature target.
      deemWorthyBoard = do
        worthy <- S.printingOf s registry "Deem Worthy"
        mountain <- S.printingOf s registry "Mountain"
        piker <- S.printingOf s registry "Goblin Piker"
        let (creature, g0) = S.addCreature piker S.alice (S.landsInPlay mountain 4)
            (g1, worthyId) = S.handOne worthy g0
        pure (g1 {GameState.priority = Just S.alice}, worthyId, creature)
      -- Corpse Churn {1}{B} Instant, "Mill three cards, then you may return a
      -- creature card from your graveyard to your hand." (name, cost, type line
      -- and oracle text checked against Scryfall.) The PRINTED form of the
      -- two-clause mode the hand-built case below fakes: clause 0 a mandatory
      -- Mill, clause 1 an optional MoveToZone.
      --
      -- alice: two Swamps in play for the cost, Corpse Churn in hand, and a
      -- THREE-card library of two Goblin Pikers and a Forest. Two creature
      -- cards, so CR 608.2d's choice among graveyard cards has more candidates
      -- than it takes and cannot be elided as a forced one, and the Forest is
      -- the non-creature the clause's filter has to exclude. Nothing on this
      -- board draws, so milling the library empty is not CR 104.3c.
      corpseChurnBoard = do
        swamp <- S.printingOf s registry "Swamp"
        churn <- S.printingOf s registry "Corpse Churn"
        piker <- S.printingOf s registry "Goblin Piker"
        forest <- S.printingOf s registry "Forest"
        let base = S.landsInPlay swamp 2
            (_, g1) = S.addLibraryCard piker S.alice base
            (_, g2) = S.addLibraryCard piker S.alice g1
            (_, g3) = S.addLibraryCard forest S.alice g2
            (g4, spellId) = S.handOne churn g3
        pure (g4 {GameState.priority = Just S.alice}, spellId)
      -- Takes Corpse Churn's SECOND clause, pinned by clause index: the group's
      -- takeOptional above pins clause 0, which on this card is the mandatory
      -- mill, so the taking half needs its own answerer. Everything else,
      -- including the choice of which graveyard card comes back, falls through
      -- to S.identityAnswer.
      returnsChurn :: Prompt.Prompt r -> r
      returnsChurn p = case p of
        Prompt.ChooseOptional _ _ _ _ cIdx
          | cIdx == ClauseIndex.MkClauseIndex 1 -> OptionalDecision.Exercises
        _ -> S.identityAnswer p
      churnName = CardName.MkCardName (Text.pack "Corpse Churn")
      complicationName = CardName.MkCardName (Text.pack "Deadly Complication")
      forestName = CardName.MkCardName (Text.pack "Forest")
      pikerName = CardName.MkCardName (Text.pack "Goblin Piker")
      aliceNamesIn zone gs = List.sort (Maybe.mapMaybe (\oid -> fmap Face.name (Game.faceOf oid gs)) (Game.zoneMembers zone S.alice gs))
      -- The graveyard as a sorted LIST rather than a set -- two Goblin Pikers
      -- are two cards and a set would collapse them -- with Corpse Churn itself
      -- dropped, since CR 608.2n puts the finished instant in the very
      -- graveyard the mill fills.
      milledNames gs = filter (/= churnName) (aliceNamesIn Zone.Graveyard gs)
      -- Deadly Complication {1}{B}{R} Sorcery, "Choose one or both -- * Destroy
      -- target creature. * Put a +1/+1 counter on target suspected creature you
      -- control. You may have it become no longer suspected." (name, cost, type
      -- line and oracle text checked against Scryfall.) The shape CR 608.2b's
      -- fizzle cannot remove: choosing BOTH modes means one live target keeps the
      -- whole spell resolving, so the other mode's "may" is reached with every
      -- slot it reads already dead.
      --
      -- alice: two Swamps and two Mountains for the cost, the spell in hand, and
      -- a Person of Interest whose CR 603.6a enters trigger has resolved, so she
      -- controls the board's one suspected creature. The Detective that trigger
      -- also makes is a second creature she controls that is NOT suspected, so
      -- the suspect slot's filter is doing work. bob's Goblin Piker is mode 0's
      -- victim -- his, so its destruction is visible as a permanent alice never
      -- controlled and cannot be confused with the Person mode 1 names.
      deadlyComplicationBoard = do
        swamp <- S.printingOf s registry "Swamp"
        mountain <- S.printingOf s registry "Mountain"
        complication <- S.printingOf s registry "Deadly Complication"
        poi <- S.printingOf s registry "Person of Interest"
        piker <- S.printingOf s registry "Goblin Piker"
        let base = S.landsFor mountain S.alice 2 (S.landsInPlay swamp 2)
            (victim, g1) = S.addCreature piker S.bob base
            (poiId, g2) = S.entersWithTrigger poi S.alice g1
            settled = S.runPure S.identityAnswer g2 (Engine.settleForPriority >> Stack.resolveTop >> Engine.settleForPriority)
            (g3, spellId) = S.handOne complication settled
        pure (g3 {GameState.priority = Just S.alice}, spellId, victim, poiId)
      isSuspected oid gs = fmap (Set.member Designation.Suspected . Object.designations) (Game.lookupObject oid gs)
   in Spec.describe s "OptionalEffect" $ do
        Spec.it s "CR 603.5 declining the may gains nothing, and the ability still resolves" $ do
          (gs, faithId) <- handWithTwoLands "Renewed Faith" "Plains"
          case Activate.abilitiesFor faithId gs of
            [ability] -> do
              let cycled = S.runPure S.identityAnswer gs (Activate.activateAbility S.alice faithId ability)
                  placed = S.runPure S.identityAnswer cycled Engine.settleForPriority
                  after = S.runPure S.identityAnswer placed Stack.resolveTop
              Spec.assertEqWith s "the trigger is on the stack, above the draw" (length (GameState.stack placed)) 2
              Spec.assertEqWith s "declining gains no life" (S.lifeOf S.alice after) (Just 20)
              -- CR 608.2n, not CR 608.2b: a declined "may" is not a fizzle.
              -- The ability resolved -- it just did nothing -- and leaving the
              -- stack is the last part of that resolution.
              Spec.assertEqWith s "and the ability left the stack anyway -- it did not fizzle" (length (GameState.stack after)) 1
            abilities -> Spec.assertFailure s ("expected one cycling ability, got " <> show (length abilities))
        Spec.it s "CR 603.5 whole card: cycling Renewed Faith and taking the may gains exactly 2" $ do
          (gs, faithId) <- handWithTwoLands "Renewed Faith" "Plains"
          case Activate.abilitiesFor faithId gs of
            [ability] -> do
              let cycled = S.runPure takeOptional gs (Activate.activateAbility S.alice faithId ability)
                  placed = S.runPure takeOptional cycled Engine.settleForPriority
                  after = S.runPure takeOptional placed Stack.resolveTop
              Spec.assertEqWith s "the Faith is in the graveyard, cycled" (length (Game.zoneMembers Zone.Graveyard S.alice cycled)) 1
              Spec.assertEqWith s "taking it gains exactly 2" (S.lifeOf S.alice after) (Just 22)
            abilities -> Spec.assertFailure s ("expected one cycling ability, got " <> show (length abilities))
        -- The prompt itself, not just its consequence: recording the run puts
        -- the answer in the transcript, which is the only place a raised
        -- prompt is directly observable. Twinned with the mandatory control
        -- below, which must record NO such response.
        Spec.it s "CR 608.2d the choice is announced as a real prompt, and lands in the transcript" $ do
          (gs, faithId) <- handWithTwoLands "Renewed Faith" "Plains"
          case Activate.abilitiesFor faithId gs of
            [ability] -> do
              let cycled = S.runPure takeOptional gs (Activate.activateAbility S.alice faithId ability)
                  placed = S.runPure takeOptional cycled Engine.settleForPriority
                  (_, transcript) = Replay.record takeOptional placed Stack.resolveTop
              Spec.assertEqWith
                s
                "exactly one may was asked, and it was taken"
                (filter isOptionalResponse transcript)
                [Response.ChoseOptional OptionalDecision.Exercises]
            abilities -> Spec.assertFailure s ("expected one cycling ability, got " <> show (length abilities))
        -- The control: Windcaller Aven's cycling trigger is the SAME shape one
        -- word short of a "may", and it must not be asked about at all.
        Spec.it s "CR 603.5 a mandatory cycling trigger raises no such prompt" $ do
          aven <- S.printingOf s registry "Windcaller Aven"
          island <- S.printingOf s registry "Island"
          piker <- S.printingOf s registry "Goblin Piker"
          let (_, g0) = S.addCreature piker S.alice (S.landsInPlay island 1)
              (g1, avenId) = S.handOne aven g0
              gs = g1 {GameState.priority = Just S.alice}
          case Activate.abilitiesFor avenId gs of
            [ability] -> do
              let cycled = S.runPure takeOptional gs (Activate.activateAbility S.alice avenId ability)
                  placed = S.runPure takeOptional cycled Engine.settleForPriority
                  (_, transcript) = Replay.record takeOptional placed Stack.resolveTop
              Spec.assertEqWith s "nothing was asked about a may" (filter isOptionalResponse transcript) []
            abilities -> Spec.assertFailure s ("expected one cycling ability, got " <> show (length abilities))
        -- The second card, and the one that puts a TARGET under the "may":
        -- Deem Worthy {4}{R} Instant, "Deem Worthy deals 7 damage to target
        -- creature. Cycling {3}{R}. When you cycle this card, you may have it
        -- deal 2 damage to target creature." The target is chosen as the
        -- trigger goes on the stack (CR 603.3d) and the option only on
        -- resolution (CR 603.5), which is the ordering a mode-selection
        -- encoding of "may" would have collapsed.
        Spec.it s "CR 603.5 whole card: cycling Deem Worthy and taking the may deals 2 to the target" $ do
          (gs, worthyId, piker) <- deemWorthyBoard
          case Activate.abilitiesFor worthyId gs of
            [ability] -> do
              let cycled = S.runPure takeOptional gs (Activate.activateAbility S.alice worthyId ability)
                  placed = S.runPure takeOptional cycled Engine.settleForPriority
                  taken = S.runPure takeOptional placed Stack.resolveTop
                  declined = S.runPure S.identityAnswer placed Stack.resolveTop
              Spec.assertEqWith s "taking it marks 2 damage" (fmap Object.damage (Game.lookupObject piker taken)) (Just 2)
              Spec.assertEqWith s "declining marks none" (fmap Object.damage (Game.lookupObject piker declined)) (Just 0)
            abilities -> Spec.assertFailure s ("expected one cycling ability, got " <> show (length abilities))
        -- CR 608.2b before CR 603.5: with its only target gone, the ability
        -- "doesn't resolve. It's removed from the stack" -- so there is nothing
        -- left for the "may" to decide and the prompt is never raised. The
        -- engine does not ask a question whose answer cannot matter.
        Spec.it s "CR 608.2b a fizzled optional trigger is not asked about at all" $ do
          (gs, worthyId, piker) <- deemWorthyBoard
          case Activate.abilitiesFor worthyId gs of
            [ability] -> do
              let cycled = S.runPure takeOptional gs (Activate.activateAbility S.alice worthyId ability)
                  placed = S.runPure takeOptional cycled Engine.settleForPriority
                  gone = S.runPure S.identityAnswer placed (Event.changeZone piker Zone.Graveyard)
                  ((_, after), transcript) = Replay.record takeOptional gone Stack.resolveTop
              Spec.assertEqWith s "the trigger left the stack" (length (GameState.stack after)) (length (GameState.stack placed) - 1)
              Spec.assertEqWith s "and no may was ever asked" (filter isOptionalResponse transcript) []
            abilities -> Spec.assertFailure s ("expected one cycling ability, got " <> show (length abilities))
        -- CR 608.2d over CR 608.2e's unit: a "may" covers the CLAUSE it is
        -- printed on, not the whole mode. Two clauses in one mode -- a mandatory
        -- Draw and an optional Draw -- and declining the second must still leave
        -- the first having happened.
        --
        -- resolveModes is the ABILITY clause loop; a spell's clauses run through
        -- Resolve.resolveSpellWith's own fold, which the Corpse Churn cases below
        -- reach through a real cast. The pair is printed on four cards --
        -- Corpse Churn and Shed Weakness as spells, Into the Wilds and Nissa,
        -- Steward of Elements as abilities -- but both abilities open with an
        -- Effect.LookAt, which changes nothing a board can see, so neither can
        -- tell this rule's reading from a mode-wide one. Not implemented: a
        -- gameplay-level twin for the ability loop, which wants a card whose
        -- mandatory clause is observable (#1887). Until one lands, this is a
        -- unit-level pin for THAT shape, and it is also the only case for the
        -- two-DRAW shape, where the library count alone separates "declined"
        -- from "drew". Aetherplasm reaches the loop from a real trigger with two
        -- OPTIONAL clauses, the second hanging on the first, which
        -- Pawl.CombatEffectSpec proves at gameplay level.
        Spec.it s "CR 608.2d a declined clause skips only its own effects" $ do
          forest <- S.printingOf s registry "Forest"
          piker <- S.printingOf s registry "Goblin Piker"
          let base = Setup.emptyGame S.bothPlayers
              -- Two cards in alice's library, so BOTH draws could find one and
              -- the count separates "declined" from "drew off an empty library".
              (_, gs0) = S.addLibraryCard forest S.alice base
              (_, gs1) = S.addLibraryCard forest S.alice gs0
              -- A Stack-zone object whose Object.owner is the effect controller
              -- resolveModes reads, without paying to cast anything.
              (stackId, gs) = S.spellOnStack piker S.alice gs1
              draw = Effect.Draw (Draw.MkDraw (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal 1) Nothing)
              mode =
                Mode.MkMode
                  ( Seq.fromList
                      [ Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing (Seq.singleton draw),
                        Clause.MkClause Nothing Nothing Nothing (Optionality.Optional (PlayerRef.Relative PlayerRelation.You)) Nothing (Seq.singleton draw)
                      ]
                  )
                  Map.empty
              before = S.handSize S.alice gs
              -- S.identityAnswer declines every optional prompt, so this is the
              -- declining half with no bespoke answerer needed.
              after = S.runPure S.identityAnswer gs (Resolve.resolveModes stackId stackId [(ModeInstance.MkModeInstance (ModeIndex.MkModeIndex 0) 0, mode)])
          Spec.assertEqWith s "the mandatory clause drew, the declined one did not" (S.handSize S.alice after) (before + 1)
        -- The same rule at gameplay level, through a real cast: Corpse Churn
        -- PRINTS the mandatory-then-optional pair the case above builds by hand.
        -- Milling three and then DECLINING the return must leave all three
        -- milled cards in the graveyard -- the decline skips its own clause and
        -- nothing else. Paired with the taking half below, which differs in
        -- exactly one thing: the answer to the "may".
        --
        -- The Shed Weakness case in the Counters group proves the same rule on
        -- the same spell loop; what this card adds is the other half of CR
        -- 608.2d, "the player announces these while applying the effect" -- the
        -- optional clause here CHOOSES its object at resolution rather than
        -- acting on a target fixed at CR 601.2c -- and a mandatory clause whose
        -- effect is a zone change, so the decline has to leave cards where an
        -- earlier clause put them.
        Spec.it s "CR 608.2d whole card: Corpse Churn's declined return leaves the mill done" $ do
          (gs, spellId) <- corpseChurnBoard
          let cast = S.runPure S.identityAnswer gs (S.cast S.alice spellId)
              after = S.runPure S.identityAnswer cast Stack.resolveTop
          Spec.assertEqWith s "declining the return leaves all three milled cards in the graveyard" (milledNames after) [forestName, pikerName, pikerName]
          Spec.assertEqWith s "and nothing came back to the hand" (aliceNamesIn Zone.Hand after) []
        Spec.it s "CR 608.2d whole card: taking Corpse Churn's return moves one card and leaves the rest milled" $ do
          (gs, spellId) <- corpseChurnBoard
          let cast = S.runPure returnsChurn gs (S.cast S.alice spellId)
              after = S.runPure returnsChurn cast Stack.resolveTop
          Spec.assertEqWith s "taking the return leaves the other two milled cards in the graveyard" (milledNames after) [forestName, pikerName]
          Spec.assertEqWith s "and exactly the creature card is in the hand" (aliceNamesIn Zone.Hand after) [pikerName]
        -- The live half of the pair below, and the control that says the guard is
        -- not simply refusing to ask: the same board, the same answer, the same
        -- two modes, differing only in whether mode 1's target is still there.
        -- With it there the "may" is a real question, is asked once, and taking
        -- it ends CR 701.60a's designation.
        Spec.it s "CR 603.5 whole card: Deadly Complication's optional clause is asked about while its target lives" $ do
          (gs, spellId, victim, poiId) <- deadlyComplicationBoard
          let cast = S.runPure (deadlyComplicationAnswer victim poiId) gs (S.cast S.alice spellId)
              ((_, after), transcript) = Replay.record (deadlyComplicationAnswer victim poiId) cast Stack.resolveTop
          Spec.assertEqWith s "the may was asked exactly once, and taken" (filter isOptionalResponse transcript) [Response.ChoseOptional OptionalDecision.Exercises]
          Spec.assertEqWith s "so the Person is no longer suspected" (isSuspected poiId after) (Just False)
          Spec.assertEqWith s "the mandatory clause put its +1/+1 counter on" (plusOnePlusOnesOn (Just poiId) after) 1
          Spec.assertEqWith s "and the other mode destroyed bob's Goblin Piker" (Game.lookupObject victim after) Nothing
        -- CR 608.2b before CR 603.5, one mode over: with mode 1's only target
        -- gone the spell does NOT fizzle -- mode 0's target is still legal, so CR
        -- 608.2b's union survives -- and mode 1's clauses are reached anyway. Its
        -- optional clause reads only the dead slot, so both answers leave the same
        -- board and the prompt is not raised. The board differs from the case
        -- above in exactly one thing: the Person is in the graveyard.
        Spec.it s "CR 608.2b whole card: Deadly Complication's dead mode is not asked about" $ do
          (gs, spellId, victim, poiId) <- deadlyComplicationBoard
          let cast = S.runPure (deadlyComplicationAnswer victim poiId) gs (S.cast S.alice spellId)
              gone = S.runPure (deadlyComplicationAnswer victim poiId) cast (Event.changeZone poiId Zone.Graveyard)
              ((_, after), transcript) = Replay.record (deadlyComplicationAnswer victim poiId) gone Stack.resolveTop
          Spec.assertEqWith s "no may was ever asked: every slot the clause reads is dead" (filter isOptionalResponse transcript) []
          Spec.assertEqWith s "the spell did not fizzle: the live mode destroyed bob's Goblin Piker" (Game.lookupObject victim after) Nothing
          Spec.assertEqWith s "and Deadly Complication resolved into alice's graveyard" (List.elem complicationName (aliceNamesIn Zone.Graveyard after)) True

-- Chooses BOTH of Deadly Complication's modes, aims each slot at the permanent
-- that slot's mode is about, and takes the "may" whenever one is offered. Rank-1
-- for exerciseOptional's reason. The recipients are FILTERED out of the offered
-- set rather than built, so a slot the engine offers under another recipient
-- shape is not silently replaced by one CR 608.2b would drop. A slot named
-- neither of the card's two gets the empty answer, which fails the target
-- announcement rather than aiming somewhere plausible.
deadlyComplicationAnswer :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
deadlyComplicationAnswer victim suspect p = case p of
  Prompt.ChooseModes {} -> Seq.fromList (fmap ModeIndex.MkModeIndex [0, 1])
  Prompt.ChooseTargets _ _ _ sets -> Map.mapWithKey aimAt sets
  Prompt.ChooseOptional {} -> OptionalDecision.Exercises
  _ -> S.identityAnswer p
  where
    aimAt :: SlotName.SlotName -> (Natural, Set.Set Recipient.Recipient) -> Set.Set Recipient.Recipient
    aimAt slot (_, offered)
      | slot == creatureSlot = Set.filter ((== Just victim) . Recipient.objectOf) offered
      | slot == suspectSlot = Set.filter ((== Just suspect) . Recipient.objectOf) offered
      | otherwise = Set.empty

-- Deadly Complication's two slot names (data/cards/deadly-complication.json).
creatureSlot, suspectSlot :: SlotName.SlotName
creatureSlot = SlotName.MkSlotName (Text.pack "creature")
suspectSlot = SlotName.MkSlotName (Text.pack "suspect")

-- Takes every printed "may" it is offered. Rank-1 like Pawl.Support.attackTo: the
-- implicit forall is outermost, so this is the `forall r. Prompt r -> r` that
-- Engine.runGamePure wants, which a let-bound local could not be.
exerciseOptional :: Prompt.Prompt r -> r
exerciseOptional p = case p of
  Prompt.ChooseOptional {} -> OptionalDecision.Exercises
  _ -> S.identityAnswer p

-- Is this transcript entry an answer to a printed "may"? The filter both
-- optional-effect transcript assertions share.
isOptionalResponse :: Response.Response -> Bool
isOptionalResponse r = case r of
  Response.ChoseOptional _ -> True
  _ -> False

-- The one battlefield permanent whose card carries this name. CR 400.7 mints a
-- fresh object on every move, so a test that crossed a zone change cannot hold
-- the id it started from. Pawl.MassEffectSpec keeps its own copy.
namedOnBattlefield :: String -> GameState.GameState -> Maybe ObjectId.ObjectId
namedOnBattlefield name gs =
  List.find
    (\oid -> fmap Face.name (Game.faceOf oid gs) == Just (CardName.MkCardName $ Text.pack name))
    (Set.toList (GameState.battlefield gs))

-- How many +1/+1 counters (CR 122.6) sit on a permanent, 0 for none.
plusOnePlusOnesOn :: Maybe ObjectId.ObjectId -> GameState.GameState -> Natural
plusOnePlusOnesOn moid gs =
  Maybe.fromMaybe 0 $ do
    oid <- moid
    obj <- Game.lookupObject oid gs
    Map.lookup CounterKind.PlusOnePlusOne (Object.counters obj)

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Resolve" $ do
  countersSpec s registry
  sauroformHybridSpec s registry
  nessianAspSpec s registry
  untapSpec s registry
  gainControlSpec s registry
  gainPlayerCountersSpec s registry
  proliferateSpec s registry
  scrySpec s registry
  scryPromptSpec s registry
  surveilSpec s registry
  surveilPromptSpec s registry
  fatesealSpec s registry
  exploreSpec s registry
  explorePromptSpec s registry
  lookAtSpec s registry
  lookAtPromptSpec s registry
  playerSacrificesSpec s registry
  createEmblemSpec s registry
  becomeMonarchSpec s registry
  targetedMonarchSpec s registry
  exileUntilMonarchSpec s registry
  actOfTreasonSpec s registry
  optionalEffectSpec s registry
