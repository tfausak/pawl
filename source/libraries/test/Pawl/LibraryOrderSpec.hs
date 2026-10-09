{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Pawl.Engine.Resolve over counters, proliferate, the library-order effects
-- (scry, surveil, fateseal, explore, look at), and the designations a
-- resolution can hand out. The machinery is Pawl.ResolveSpec.
module Pawl.LibraryOrderSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Activatable as Activatable
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Expiry as Expiry
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Monarch as Monarch
import qualified Pawl.Engine.MoveDuration as MoveDuration
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Projection.View as Projection
import qualified Pawl.Engine.Replay as Replay
import qualified Pawl.Engine.Resolve as Resolve
import qualified Pawl.Engine.Resolve.Effect as Resolve
import qualified Pawl.Engine.Resolve.Slots as Resolve
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.Activator as Activator
import qualified Pawl.Types.Asked as Asked
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Clause as Clause
import qualified Pawl.Types.ClauseIndex as ClauseIndex
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Decider as Decider
import qualified Pawl.Types.Departure as Departure.Type
import qualified Pawl.Types.Designation as Designation
import qualified Pawl.Types.Draw as Draw
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.DurationRef as DurationRef
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.EntryRiders as EntryRiders
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.LibraryPlacement as LibraryPlacement
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeIndex as ModeIndex
import qualified Pawl.Types.ModeInstance as ModeInstance
import qualified Pawl.Types.ModeSelection as ModeSelection
import qualified Pawl.Types.MonarchTarget as MonarchTarget
import qualified Pawl.Types.MoveDuration as MoveDuration.Type
import qualified Pawl.Types.MoveToZone as MoveToZone
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.Optionality as Optionality
import qualified Pawl.Types.Phase as Phase
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
import qualified Pawl.Types.ReturnEnding as ReturnEnding
import qualified Pawl.Types.ReturnWatch as ReturnWatch
import qualified Pawl.Types.Revealed as Revealed
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.SlotArity as SlotArity
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.StepBegan as StepBegan
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TopOfLibrary as TopOfLibrary
import qualified Pawl.Types.Zone as Zone

countersSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
countersSpec s registry = Spec.describe s "Counters" $ do
  Spec.it s "CR 704.5q both counter kinds on one creature annihilate; net 2/1 survives" $ do
    -- Both counters on the same creature (placed directly); the SBA removes both.
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    let base = S.landsInPlay forest 5
        (victim, withFoe) = S.addPermanent piker S.alice base
        gs1 = S.addCounter CounterKind.PlusOnePlusOne 1 victim withFoe
        gs2 = S.addCounter CounterKind.MinusOneMinusOne 1 victim gs1
        after = S.settleSba gs2
    Spec.assertEqWith s "creature survives (net 2/1)" (S.creaturesInPlay S.alice after) 1
    Spec.assertEqWith s "no counters remain" (maybe (Map.fromList [(CounterKind.PlusOnePlusOne, 99)]) Object.counters (Game.lookupObject victim after)) Map.empty
  Spec.it s "CR 122 RemoveCounters takes counters off the slot's target" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (oid, base0) = S.addPermanent piker S.bob (Setup.emptyGame S.bothPlayers)
        base = S.addCounter CounterKind.MinusOneMinusOne 2 oid base0
        slot = SlotName.MkSlotName (Text.pack "target")
        run =
          Resolve.applyEffect
            oid
            oid
            S.alice
            (Map.singleton slot (Set.singleton (Recipient.ToCreature oid)))
            (Map.singleton slot (Set.singleton (Recipient.ToCreature oid)))
            (Effect.RemoveCounters (RemoveCounters.MkRemoveCounters CounterKind.MinusOneMinusOne (Quantity.Literal 1) slot Nothing))
        after = snd (Engine.runGamePure S.identityAnswer base run)
    Spec.assertEqWith s "one of the two counters is gone" (fmap Object.counters (Game.lookupObject oid after)) (Just (Map.singleton CounterKind.MinusOneMinusOne 1))
  -- CR 122 states no rule making the instruction fail when there are fewer
  -- counters than asked for, so it takes what is there. The kind leaves the map
  -- entirely rather than sitting at zero, which is what keeps Object.counters a
  -- tally of what is present.
  Spec.it s "CR 122 removing more counters than are present removes what is there" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (oid, base0) = S.addPermanent piker S.bob (Setup.emptyGame S.bothPlayers)
        base = S.addCounter CounterKind.MinusOneMinusOne 1 oid base0
        slot = SlotName.MkSlotName (Text.pack "target")
        run =
          Resolve.applyEffect
            oid
            oid
            S.alice
            (Map.singleton slot (Set.singleton (Recipient.ToCreature oid)))
            (Map.singleton slot (Set.singleton (Recipient.ToCreature oid)))
            (Effect.RemoveCounters (RemoveCounters.MkRemoveCounters CounterKind.MinusOneMinusOne (Quantity.Literal 3) slot Nothing))
        after = snd (Engine.runGamePure S.identityAnswer base run)
    Spec.assertEqWith s "the kind is gone, not negative" (fmap Object.counters (Game.lookupObject oid after)) (Just Map.empty)
  -- CR 608.2d on the same card: "the player can't choose an option that's
  -- illegal or impossible", so "you may remove a -1/-1 counter from it" over a
  -- creature that bears none is not offered at all.
  --
  -- A PAIR of boards differing in exactly one thing -- whether S.addCounter ran
  -- -- because the board after resolution reads the same either way: a removal
  -- that happened and one that could not both leave the creature with no -1/-1
  -- counters. The transcript is what tells them apart, so it is the whole proof
  -- here, and the counter-bearing half is the control that says the prompt
  -- survives where the rule leaves a choice.
  Spec.it s "CR 608.2d Shed Weakness's removal is not offered to a creature with no counter" $ do
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    shedWeakness <- S.printingOf s registry "Shed Weakness"
    let build withCounter =
          let (victim, withFoe) = S.addPermanent piker S.bob (S.landsInPlay forest 1)
              placed = if withCounter then S.addCounter CounterKind.MinusOneMinusOne 1 victim withFoe else withFoe
              (gs, spellId) = S.handOne shedWeakness placed
           in (victim, gs, spellId)
        resolveIt (_, gs, spellId) =
          let ((_, after), asked) = Replay.record exerciseOptional gs (S.cast S.alice spellId >> Stack.resolveTop)
           in (asked, after)
        victimOf (victim, _, _) = victim
        bare = build False
        borne = build True
        (bareAsked, bareAfter) = resolveIt bare
        (borneAsked, borneAfter) = resolveIt borne
    Spec.assertEqWith s "CR 608.2d with no counter to remove, the \"may\" is never put" (filter isOptionalResponse bareAsked) []
    Spec.assertEqWith s "the control: the same card over a counter is still asked" (filter isOptionalResponse borneAsked) [Response.ChoseOptional OptionalDecision.Exercises]
    Spec.assertEqWith s "and the mandatory pump landed on the bare creature all the same" (Projection.powerOf (victimOf bare) bareAfter) (Just 4)
    Spec.assertEqWith s "the control's counter is what the answer removed" (fmap Object.counters (Game.lookupObject (victimOf borne) borneAfter)) (Just Map.empty)

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
    let (hybridId, placed) = S.addPermanent hybrid S.alice (S.landsInPlay forest 12)
        board = placed {GameState.priority = Just S.alice}
        adapt gs ability = S.runPure S.identityAnswer gs $ do
          Activate.activateAbility S.alice hybridId ability
          Stack.resolveTop
        countersOn gs = fmap (Map.findWithDefault 0 CounterKind.PlusOnePlusOne . Object.counters) (Game.lookupObject hybridId gs)
    case Activatable.abilitiesFor hybridId board of
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
    let (aspId, placed) = S.addPermanent asp S.alice (S.landsInPlay forest 16)
        board = placed {GameState.priority = Just S.alice}
        monstrosity gs ability = S.runPure S.identityAnswer gs $ do
          Activate.activateAbility S.alice aspId ability
          Stack.resolveTop
    case Activatable.abilitiesFor aspId board of
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
    let (aspId, placed) = S.addPermanent asp S.alice (S.landsInPlay forest 16)
        board = (S.addCounter CounterKind.PlusOnePlusOne 1 aspId placed) {GameState.priority = Just S.alice}
    case Activatable.abilitiesFor aspId board of
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
    let (aspId, placed) = S.addPermanent asp S.alice (S.landsInPlay forest 16)
        (_, withIsland) = S.addPermanent island S.alice placed
        board = withIsland {GameState.priority = Just S.alice}
    case Activatable.abilitiesFor aspId board of
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
    let (oid, base0) = S.addPermanent piker S.bob (Setup.emptyGame S.bothPlayers)
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
    let (oid, base) = S.addPermanent piker S.bob (Setup.emptyGame S.bothPlayers)
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
    let (oid, base) = S.addPermanent piker S.alice (Setup.emptyGame S.bothPlayers)
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

gainPlayerCountersSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
gainPlayerCountersSpec s registry = Spec.describe s "GainPlayerCounters" $ do
  Spec.it s "CR 107.14 GainPlayerCounters gives the resolving controller energy" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (src, gs0) = S.addPermanent piker S.alice (Setup.emptyGame S.bothPlayers)
        act = Resolve.applyEffect src src S.alice Map.empty Map.empty (Effect.GainPlayerCounters (PlayerCounters.MkPlayerCounters (PlayerRef.Relative PlayerRelation.You) PlayerCounterKind.Energy (Quantity.Literal 2)))
        after = S.runPure S.identityAnswer gs0 act
    Spec.assertEqWith s "alice has two energy" (S.playerCounterOf PlayerCounterKind.Energy S.alice after) 2

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
    let (src, g0) = S.addPermanent piker S.alice (Setup.emptyGame S.bothPlayers)
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
    let (src, g0) = S.addPermanent piker S.alice (Setup.emptyGame S.bothPlayers)
        g1 = S.addCounter CounterKind.PlusOnePlusOne 1 src g0
        gs = S.addCounter CounterKind.MinusOneMinusOne 3 src g1
        after = proliferate proliferatesAll src gs
    Spec.assertEqWith s "+1/+1 went up" (S.counterOf CounterKind.PlusOnePlusOne src after) 2
    Spec.assertEqWith s "-1/-1 went up too" (S.counterOf CounterKind.MinusOneMinusOne src after) 4
  -- CR 701.34a: only permanents "that have a counter" are choosable, so a bare
  -- permanent is never offered and never gains a first counter this way.
  Spec.it s "CR 701.34a a permanent with no counters is not a candidate" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (src, g0) = S.addPermanent piker S.alice (Setup.emptyGame S.bothPlayers)
        (bare, g1) = S.addPermanent piker S.alice g0
        gs = S.addCounter CounterKind.PlusOnePlusOne 1 src g1
        after = proliferate proliferatesAll src gs
    Spec.assertEqWith s "the bare Piker gained nothing" (S.counterOf CounterKind.PlusOnePlusOne bare after) 0
    Spec.assertEqWith s "the countered one moved" (S.counterOf CounterKind.PlusOnePlusOne src after) 2
  -- CR 701.34a: players carry counters too, and proliferate reaches them.
  Spec.it s "CR 701.34a proliferate adds to a player's poison and energy" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (src, g0) = S.addPermanent piker S.alice (Setup.emptyGame S.bothPlayers)
        g1 = S.addPlayerCounter PlayerCounterKind.Poison 3 S.bob g0
        gs = S.addPlayerCounter PlayerCounterKind.Energy 1 S.alice g1
        after = proliferate proliferatesAll src gs
    Spec.assertEqWith s "bob's poison" (S.playerCounterOf PlayerCounterKind.Poison S.bob after) 4
    Spec.assertEqWith s "alice's energy" (S.playerCounterOf PlayerCounterKind.Energy S.alice after) 2
  -- A player with no counters is not a candidate, the same clause the bare
  -- permanent above tests -- so proliferate never starts someone on poison.
  Spec.it s "CR 701.34a a player with no counters is not a candidate" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (src, g0) = S.addPermanent piker S.alice (Setup.emptyGame S.bothPlayers)
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
    let (src, g0) = S.addPermanent piker S.alice S.threePlayerGame
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
    let (src, g0) = S.addPermanent piker S.alice (Setup.emptyGame S.bothPlayers)
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
    let (src, g0) = S.addPermanent piker S.alice (Setup.emptyGame S.bothPlayers)
        (_, g1) = S.addPermanent hardenedScales S.alice g0
        gs = S.addCounter CounterKind.PlusOnePlusOne 1 src g1
        after = proliferate proliferatesAll src gs
    Spec.assertEqWith s "one proliferated counter became two" (S.counterOf CounterKind.PlusOnePlusOne src after) 3
  -- Where the rules leave nothing to ask, do not ask: no permanent and no
  -- player holds a counter, so there is no choice to make.
  Spec.it s "CR 701.34a an empty candidate set raises no prompt" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (src, gs) = S.addPermanent piker S.alice (Setup.emptyGame S.bothPlayers)
        countingAnswer :: Prompt.Prompt r -> State.State Int r
        countingAnswer p = case p of
          Prompt.ChooseProliferate {} -> do
            State.modify (+ 1)
            pure (S.identityAnswer p)
          _ -> pure (S.identityAnswer p)
        asks g = State.execState (Engine.runGame countingAnswer g (Resolve.applyEffect src src S.alice Map.empty Map.empty Effect.Proliferate)) 0
    Spec.assertEqWith s "nobody has a counter: nothing to ask" (asks gs) 0
    Spec.assertEqWith s "someone does: one real decision" (asks (S.addCounter CounterKind.PlusOnePlusOne 1 src gs)) 1
  -- CR 614.5 / 616.1f: each Tekuthal gets one opportunity, the second on the
  -- event the first made, so two double twice. Two under one controller is the
  -- legend rule's to settle (CR 704.5j), so this drives the opcode directly with
  -- no state-based-action pass between; a non-legendary copy reaches it in play.
  Spec.it s "CR 614.5 two Tekuthals make one proliferate four" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    tekuthal <- S.printingOf s registry "Tekuthal, Inquiry Dominus"
    let (src, g0) = S.addPermanent piker S.alice (Setup.emptyGame S.bothPlayers)
        (_, g1) = S.addPermanent tekuthal S.alice g0
        (_, g2) = S.addPermanent tekuthal S.alice g1
        gs = S.addCounter CounterKind.PlusOnePlusOne 1 src g2
        counting :: Prompt.Prompt r -> State.State Int r
        counting p = case p of
          Prompt.ChooseProliferate _ _ oids pids -> do
            State.modify (+ 1)
            pure (Set.fromList oids, Set.fromList pids)
          _ -> pure (S.identityAnswer p)
        (after, asked) = State.runState (Engine.runGame counting gs (Resolve.applyEffect src src S.alice Map.empty Map.empty Effect.Proliferate)) 0
    Spec.assertEqWith s "CR 701.34a: one counter became five" (S.counterOf CounterKind.PlusOnePlusOne src (snd after)) 5
    Spec.assertEqWith s "four proliferate choices" asked 4

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
  let (ballId, placed) = S.addPermanent crystalBall S.alice (S.landsInPlay forest 4)
      -- addLibraryCard puts its card ON TOP, so the deepest is stocked first.
      deck = reverse (take stock [piker, maiden, mountain, forest])
      deal (acc, gs) printing = let (oid, gs2) = S.addLibraryCard printing S.alice gs in (oid : acc, gs2)
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
runScry answer ballId gs = case Activatable.abilitiesFor ballId gs of
  [ability] -> S.runPure answer gs $ do
    Activate.activateAbility S.alice ballId ability
    Stack.resolveTop
  _ -> gs

-- Eager Construct's answerer: every seat takes the may and bottoms what it
-- looked at, and each ChooseScry logs its seat with every library as the ASKING
-- game held it.
constructAnswer :: Asked.Asked r -> State.State [(PlayerId.PlayerId, [[ObjectId.ObjectId]])] r
constructAnswer a = case Asked.prompt a of
  Prompt.ChooseOptional {} -> pure OptionalDecision.Exercises
  Prompt.ChooseScry _ pid looked -> do
    let seen = fmap (\seat -> Game.zoneMembers Zone.Library seat (Asked.game a)) [S.alice, S.bob, S.carol]
    State.modify' (<> [(pid, seen)])
    pure (looked, [])
  p -> pure (S.identityAnswer p)

scryLibrary :: GameState.GameState -> [ObjectId.ObjectId]
scryLibrary = Game.zoneMembers Zone.Library S.alice

scrySpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
scrySpec s registry = Spec.describe s "Scry" $ do
  -- Eager Construct -- "{2} Artifact Creature -- Construct 2/2. When this
  -- creature enters, each player may scry 1." -- at three seats, every seat
  -- taking the may and bottoming its top card. The answerer runs through the
  -- Asked seam (Engine.runGameAsked), which the Board harness does not expose,
  -- because what the case reads is the BOARD each scryer decided over: CR
  -- 701.22c has every player decide before any card moves, so bob and carol
  -- must be asked over libraries alice's answer has not yet changed. Moving
  -- each scryer's card before asking the next leaves every final library the
  -- same and fails the first assertion.
  Spec.it s "CR 701.22c Eager Construct: every scryer decides before any card moves" $ do
    construct <- S.printingOf s registry "Eager Construct"
    piker <- S.printingOf s registry "Goblin Piker"
    maiden <- S.printingOf s registry "Bird Maiden"
    island <- S.printingOf s registry "Island"
    let -- addLibraryCard puts its card ON TOP, so each list is stocked bottom first.
        stock pid g printings = List.foldl' (\h p -> snd (S.addLibraryCard p pid h)) g (reverse printings)
        stocked =
          stock S.carol (stock S.bob (stock S.alice S.threePlayerGame [piker, maiden, island]) [maiden, island, piker]) [island, piker, maiden]
        (_, entered) = S.entersWithTrigger construct S.alice stocked
        onStack = S.runPure S.identityAnswer entered Engine.settleForPriority
        seats = [S.alice, S.bob, S.carol]
        libraries gs = fmap (\pid -> Game.zoneMembers Zone.Library pid gs) seats
        ((_, after), asked) = State.runState (Engine.runGameAsked constructAnswer onStack Stack.resolveTop) []
        rotated = fmap (\lib -> drop 1 lib <> take 1 lib) (libraries onStack)
    Spec.assertEqWith
      s
      "CR 701.22c: each seat, in APNAP order, decided over every library as it started"
      asked
      (fmap (\pid -> (pid, libraries onStack)) seats)
    Spec.assertEqWith s "then every top card went to the bottom" (libraries after) rotated
    Spec.assertEqWith
      s
      "CR 701.22d: each seat scried"
      (filter (`elem` fmap GameEvent.Scried seats) (S.eventsOf after))
      (fmap GameEvent.Scried seats)
    Spec.assertEqWith s "CR 603.6a: the enters trigger, and nothing else, was on the stack" (length (GameState.stack onStack)) 1

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
      asks ballId gs = case Activatable.abilitiesFor ballId gs of
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
  -- CR 614.6: Eligeth, Crossroads Augur ("If you would scry a number of cards,
  -- draw that many cards instead.") replaces the scry outright: two cards drawn,
  -- nothing looked at, and no CR 701.22d event for "whenever you scry".
  Spec.it s "CR 614.6 Eligeth draws two instead of Crystal Ball's scry 2" $ do
    (ids, ballId, board) <- scryBoard s registry 4
    withEligeth <- withScryRow s registry ["Eligeth, Crossroads Augur"] board
    case ids of
      [_, _, mountain, forest] -> do
        let (after, looked) = scryBottomingAll ballId (fst withEligeth)
        Spec.assertEqWith s "CR 614.6: two cards were drawn" (S.handSize S.alice after) 2
        Spec.assertEqWith s "and the rest stayed put" (scryLibrary after) [mountain, forest]
        Spec.assertEqWith s "nothing was looked at" looked []
        Spec.assertBool s (notElem (GameEvent.Scried S.alice) (S.eventsOf after)) "CR 614.6 no scry happened"
      _ -> Spec.assertFailure s "expected four library cards"
  -- CR 701.22b: scry 0 is no scry event, so there is nothing for Kenessos to
  -- enlarge: nothing is looked at. Scry 1 is the pair's other half, enlarged to
  -- two.
  Spec.it s "CR 701.22b Kenessos does not turn a scry 0 into a scry 1" $ do
    (_, ballId, board) <- scryBoard s registry 4
    (withKenessos, _) <- withScryRow s registry ["Kenessos, Priest of Thassa"] board
    let scryN n = Resolve.applyEffect ballId ballId S.alice Map.empty Map.empty (Effect.Scry (PlayerQuantity.MkPlayerQuantity (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal n)))
        offered n =
          let answering :: Prompt.Prompt r -> State.State [[ObjectId.ObjectId]] r
              answering p = case p of
                Prompt.ChooseScry _ _ cards -> State.modify' (<> [cards]) >> pure (cards, [])
                _ -> pure (S.identityAnswer p)
           in fmap length (State.execState (Engine.runGame answering withKenessos (scryN n)) [])
    Spec.assertEqWith s "scry 0: nothing looked at" (offered 0) []
    Spec.assertEqWith s "scry 1: two looked at" (offered 1) [2]

-- Each named card on the battlefield under alice, and their ids in order.
withScryRow :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> [String] -> GameState.GameState -> m (GameState.GameState, [ObjectId.ObjectId])
withScryRow s registry names board = do
  printings <- traverse (S.printingOf s registry) names
  let place (g, acc) printing = let (oid, g2) = S.addPermanent printing S.alice g in (g2, acc <> [oid])
  pure (List.foldl' place (board, []) printings)

-- Crystal Ball's scry, bottoming every card looked at in the order offered.
-- Answers with the board and each ChooseScry's offered cards, in order.
scryBottomingAll :: ObjectId.ObjectId -> GameState.GameState -> (GameState.GameState, [[ObjectId.ObjectId]])
scryBottomingAll ballId gs =
  let answering :: Prompt.Prompt r -> State.State [[ObjectId.ObjectId]] r
      answering p = case p of
        Prompt.ChooseScry _ _ offered -> State.modify' (<> [offered]) >> pure (offered, [])
        _ -> pure (S.identityAnswer p)
      run = case Activatable.abilitiesFor ballId gs of
        [ability] -> Activate.activateAbility S.alice ballId ability >> Stack.resolveTop
        _ -> pure ()
      ((_, after), looked) = State.runState (Engine.runGame answering gs run) []
   in (after, looked)

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
  let (sourceId, base) = S.addPermanent piker S.alice (S.landsInPlay island 1)
      deal (acc, gs) printing = let (oid, gs2) = S.addLibraryCard printing S.alice gs in (oid : acc, gs2)
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
  -- with ONE card that is the whole library, Pawl.Engine.Resolve.Effect.decideScry asks
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

enhancedSurveillanceSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
enhancedSurveillanceSpec s registry = Spec.describe s "EnhancedSurveillance" $ do
  -- CR 701.25c: surveil 0 is no surveil, so there is nothing for the
  -- enchantment to widen. Driven through the opcode, no printing surveilling
  -- zero.
  Spec.it s "CR 701.25c Enhanced Surveillance does not turn surveil 0 into a surveil" $ do
    (ids, sourceId, base) <- surveilOpcodeBoard s registry 2
    enhanced <- S.printingOf s registry "Enhanced Surveillance"
    let board = snd (S.addPermanent enhanced S.alice base)
        surveilZero = Effect.Surveil (PlayerQuantity.MkPlayerQuantity (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal 0))
        after = S.runPure (surveilAnswer (ids, [])) board (Resolve.applyEffect sourceId sourceId S.alice Map.empty Map.empty surveilZero)
    Spec.assertEqWith s "the library is what it was" (Game.zoneMembers Zone.Library S.alice after) ids
    Spec.assertEqWith s "with an empty graveyard" (surveilGraveyard after) []

-- Answers Prompt.ChooseFateseal with a FIXED pair of lists, surveilAnswer's
-- posture and for its reason.
fatesealAnswer :: ([ObjectId.ObjectId], [ObjectId.ObjectId]) -> Prompt.Prompt r -> r
fatesealAnswer split p = case p of
  Prompt.ChooseFateseal {} -> split
  _ -> S.identityAnswer p

fatesealSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
fatesealSpec s registry = Spec.describe s "Fateseal" $ do
  -- The elision pair for the SPLIT question, two boards differing in one card:
  -- a lone card that is the whole library has its top and its bottom at the same
  -- position, so both answers give the same library and there is nothing to ask
  -- -- decideScry's case, and NOT surveil's, where the two destinations differ.
  -- Driven through the opcode, Spin into Myth's count being fixed at two.
  Spec.it s "CR 701.29a a one-card library raises no fateseal prompt, and a card beneath it does" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    let (sourceId, base) = S.addPermanent piker S.alice (S.landsInPlay island 1)
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
-- enters, it explores", cast off two Forests and run to a stable board, so CR
-- 603.6a's enters trigger is placed by the engine rather than by the fixture.
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
      (bystander, withMyr) = S.addPermanent myr S.alice (S.landsInPlay forest 2)
      (withSpell, spell) = S.handOne branchwalker withMyr
      -- addLibraryCard puts its card ON TOP, so the deepest is stocked first.
      deal gs printing = snd (S.addLibraryCard printing S.alice gs)
      stocked = List.foldl' deal withSpell (reverse printings)
  pure (spell, bystander, stocked)

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
revealedNames gs =
  let revealedName event = case event of
        GameEvent.Revealed (Revealed.MkRevealed pid _ _ pc)
          | pid == S.alice ->
              fmap (Text.unpack . CardName.unwrap) (Maybe.listToMaybe (Set.toList (PC.names pc)))
        _ -> Nothing
   in Maybe.mapMaybe revealedName (S.eventsOf gs)

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

-- Hakbal of the Surging Soul {2}{G}{U} Legendary Creature -- Merfolk Scout 3/3,
-- "At the beginning of combat on your turn, each Merfolk creature you control
-- explores", on the battlefield under alice with the board sitting in her
-- beginning of combat step -- not yet run, since Engine.runStep is what writes
-- the CR 603.2b StepBegan record the trigger matches.
--
-- alice also controls a Merfolk Spy and a Merfolk Seer when `crowded`, a
-- Darksteel Myr that is no Merfolk either way, and bob controls a Merfolk Spy of
-- his own: the batch is "each Merfolk creature YOU control", so the Myr proves
-- the subtype half and bob's Spy the control half. The three Merfolk are added
-- Hakbal, Spy, Seer, so the engine's canonical order within alice's group --
-- ascending ObjectId -- is that order and a non-identity answer is visibly not
-- it.
--
-- alice's library is stocked NONLAND, LAND, NONLAND: whoever explores second
-- reveals the Mountain, which CR 701.44a's first sentence sends to hand with no
-- counter, while the first and third grow and bin. Exactly one of the three
-- Merfolk therefore ends bare, and WHICH one is the order CR 701.44d asked for.
-- The Forest beneath is never reached and keeps the library non-empty.
--
-- Hakbal's second printed ability ("whenever Hakbal attacks, you may put a land
-- card from your hand onto the battlefield. If you don't, draw a card") is on the
-- card too; nothing here attacks, so it never triggers.
-- Pawl.CounterspellSpec's Hakbal group is what proves it.
hakbalBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  Bool ->
  m (ObjectId.ObjectId, Maybe ObjectId.ObjectId, Maybe ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
hakbalBoard s registry crowded = do
  hakbal <- S.printingOf s registry "Hakbal of the Surging Soul"
  spy <- S.printingOf s registry "Merfolk Spy"
  seer <- S.printingOf s registry "Merfolk Seer"
  myr <- S.printingOf s registry "Darksteel Myr"
  printings <- mapM (S.printingOf s registry) ["Goblin Piker", "Mountain", "Bird Maiden", "Forest"]
  let (hakbalId, withHakbal) = S.addPermanent hakbal S.alice (Setup.emptyGame S.bothPlayers)
      (others, withOthers) =
        if crowded
          then
            let (spyId, one) = S.addPermanent spy S.alice withHakbal
                (seerId, two) = S.addPermanent seer S.alice one
             in ((Just spyId, Just seerId), two)
          else ((Nothing, Nothing), withHakbal)
      (myrId, withMyr) = S.addPermanent myr S.alice withOthers
      (_, withBob) = S.addPermanent spy S.bob withMyr
      -- addLibraryCard puts its card ON TOP, so the deepest is stocked first.
      deal gs printing = snd (S.addLibraryCard printing S.alice gs)
      stocked = List.foldl' deal withBob (reverse printings)
  pure
    ( hakbalId,
      fst others,
      snd others,
      myrId,
      stocked
        { GameState.phase = Phase.Combat CombatStep.BeginningOfCombat,
          GameState.activePlayer = S.alice,
          GameState.priority = Just S.alice
        }
    )

-- Answers CR 701.44d's order with `order` VERBATIM and always bins the revealed
-- card. Pinned rather than searched for a legal permutation: an answerer that
-- looked for one would find the engine's own again after a mutation, and these
-- cases would stay green over a choice that had stopped being made.
exploreOrdering :: [Natural] -> Prompt.Prompt r -> r
exploreOrdering order p = case p of
  Prompt.OrderForEach {} -> order
  Prompt.ChooseExplore {} -> OptionalDecision.Exercises
  _ -> S.identityAnswer p

-- Run the beginning of combat step and then the stack it put a trigger on.
throughCombatStart ::
  (forall r. Prompt.Prompt r -> r) ->
  GameState.GameState ->
  GameState.GameState
throughCombatStart answer gs =
  S.runPure answer (S.runPure answer gs Engine.runStep) Engine.priorityLoop

-- CR 701.44d: "if multiple permanents are instructed to explore at the same
-- time, the first player in APNAP order who controls ... one or more of those
-- permanents chooses one of them and it explores. Then this process is repeated
-- for each remaining instruction to explore."
--
-- Hakbal is the pool's only producer: no other printing instructs more than one
-- permanent to explore at once (Scryfall `oracle:/each .{0,40}explores/`,
-- 2026-08-28, Hakbal alone). Jadelight Spelunker's "explores X times" is ONE
-- permanent exploring repeatedly, which rule 701.44d does not reach.
--
-- What these cases prove is the SECOND key: the order is asked of a player
-- rather than read off ascending ObjectId. The rule's other two halves are
-- regression fences here rather than proofs, because no card can discriminate
-- them: every printed explore instruction names permanents "you control", so the
-- batch is always the resolving controller's own seat, which makes CR 701.44d's
-- chooser (that seat) and CR 608.2f's (the resolving controller) the same player
-- on every reachable board, and leaves the APNAP grouping across seats with one
-- group to order.
exploreOrderSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
exploreOrderSpec s registry = Spec.describe s "ExploreOrder" $ do
  -- The engine's canonical order, answered as itself. The pair below is the same
  -- board with the ANSWER as the only difference.
  Spec.it s "CR 701.44d the Merfolk chosen second meets the land and stays bare" $ do
    (hakbalId, spyId, seerId, myrId, board) <- hakbalBoard s registry True
    let after = throughCombatStart (exploreOrdering [0, 1, 2]) board
    Spec.assertEqWith
      s
      "Hakbal first and the Seer third grew; the Spy went second, revealed the Mountain and grew nothing"
      (plusOnePlusOnesOn (Just hakbalId) after, plusOnePlusOnesOn spyId after, plusOnePlusOnesOn seerId after)
      (1, 0, 1)
    Spec.assertEqWith s "the Mountain the second explore revealed is in hand" (zoneNames Zone.Hand after) ["Mountain"]
    Spec.assertEqWith s "the two nonland cards were binned, in the order they were revealed" (zoneNames Zone.Graveyard after) ["Goblin Piker", "Bird Maiden"]
    Spec.assertEqWith s "all three cards were revealed, top down" (revealedNames after) ["Goblin Piker", "Mountain", "Bird Maiden"]
    Spec.assertEqWith s "the Forest beneath them was never reached" (zoneNames Zone.Library after) ["Forest"]
    -- The batch is "each MERFOLK creature you control", so the Myr is no part of
    -- it however the order came out.
    Spec.assertEqWith s "the Darksteel Myr is no Merfolk and stayed bare" (plusOnePlusOnesOn (Just myrId) after) 0
  -- One thing changed from the case above: the answer. Hakbal goes second now,
  -- so Hakbal is the one that meets the Mountain.
  Spec.it s "CR 701.44d a different answer puts a different Merfolk in front of the land" $ do
    (hakbalId, spyId, seerId, myrId, board) <- hakbalBoard s registry True
    let after = throughCombatStart (exploreOrdering [1, 0, 2]) board
    Spec.assertEqWith
      s
      "the Spy went first and grew; Hakbal went second and stayed bare"
      (plusOnePlusOnesOn (Just hakbalId) after, plusOnePlusOnesOn spyId after, plusOnePlusOnesOn seerId after)
      (0, 1, 1)
    Spec.assertEqWith s "the same three cards moved the same way" (zoneNames Zone.Hand after, zoneNames Zone.Graveyard after) (["Mountain"], ["Goblin Piker", "Bird Maiden"])
    Spec.assertEqWith s "the Darksteel Myr is no Merfolk and stayed bare" (plusOnePlusOnesOn (Just myrId) after) 0
  -- The third seat of the same triple, so no reading of the answer can put the
  -- Mountain in front of a fixed creature and pass all three.
  Spec.it s "CR 701.44d and again with the Seer second" $ do
    (hakbalId, spyId, seerId, _, board) <- hakbalBoard s registry True
    let after = throughCombatStart (exploreOrdering [0, 2, 1]) board
    Spec.assertEqWith
      s
      "the Seer went second and stayed bare; Hakbal and the Spy grew"
      (plusOnePlusOnesOn (Just hakbalId) after, plusOnePlusOnesOn spyId after, plusOnePlusOnesOn seerId after)
      (1, 1, 0)
  -- CR 701.44d's own "one of them": one permanent is one order, so there is
  -- nothing to ask. The board differs from the cases above in exactly one thing
  -- -- whether the Spy and the Seer are on the battlefield.
  Spec.it s "CR 701.44d a lone exploring Merfolk raises no order question" $ do
    (hakbalId, _, _, myrId, board) <- hakbalBoard s registry False
    let asked = orderPrompts board
        after = throughCombatStart (exploreOrdering [0]) board
    Spec.assertEqWith s "not asked" asked []
    Spec.assertEqWith s "Hakbal explored anyway: the Piker grew it and was binned" (plusOnePlusOnesOn (Just hakbalId) after, zoneNames Zone.Graveyard after) (1, ["Goblin Piker"])
    Spec.assertEqWith s "and the Mountain the second explore would have taken is still on top" (zoneNames Zone.Library after) ["Mountain", "Bird Maiden", "Forest"]
    Spec.assertEqWith s "the Darksteel Myr is no Merfolk and stayed bare" (plusOnePlusOnesOn (Just myrId) after) 0
  -- ONE question for the three, not one per permanent: CR 701.44d's repeat
  -- clause keeps the first APNAP seat first while it holds any of them, so a
  -- seat's whole group is ordered at once.
  --
  -- Whom it is asked of is a REGRESSION FENCE and not a proof: alice controls the
  -- permanents and alice controls the resolving ability, so this board cannot
  -- tell CR 701.44d's chooser from CR 608.2f's, and no printing can.
  Spec.it s "CR 701.44d one order question for the seat, asked of that seat" $ do
    (_, _, _, _, board) <- hakbalBoard s registry True
    Spec.assertEqWith s "asked once, of alice, over all three of her Merfolk" (orderPrompts board) [(S.alice, 3)]

-- Who was asked CR 701.44d's order, and over how many permanents, in the order
-- the questions came. Threaded through State rather than answered purely,
-- since two groups would otherwise be indistinguishable to the answerer.
orderPrompts :: GameState.GameState -> [(PlayerId.PlayerId, Int)]
orderPrompts gs =
  let recording :: Prompt.Prompt r -> State.State [(PlayerId.PlayerId, Int)] r
      recording p = case p of
        Prompt.OrderForEach _ pid _ group -> do
          State.modify (<> [(pid, length group)])
          pure (S.identityAnswer p)
        _ -> pure (S.identityAnswer p)
   in State.execState (Engine.runGame recording gs (Engine.runStep >> Engine.priorityLoop)) []

-- Into the Wilds on the battlefield under alice's control, over a library
-- stocked from the top down. Two seats and no other permanent: the card reads
-- only its controller's own library, so nothing here needs telling apart.
wildsBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> [String] -> m GameState.GameState
wildsBoard s registry deck = do
  wilds <- S.printingOf s registry "Into the Wilds"
  printings <- mapM (S.printingOf s registry) deck
  let (_, withWilds) = S.addPermanent wilds S.alice (Setup.emptyGame S.bothPlayers)
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
    let after = runWildsUpkeep (wildsAnswer OptionalDecision.Exercises) (snd (S.addPermanent warren S.alice board))
    Spec.assertEqWith s "the Piker the Warren made a land left the library" (zoneNames Zone.Library after) ["Bird Maiden"]
    Spec.assertEqWith s "and is on the battlefield" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Goblin Piker")) S.alice after) 1
  -- CR 609.3: an empty library has no top card, so the look names nothing, the
  -- slot goes unbound and the clause after it finds no land.
  Spec.it s "CR 609.3 an empty library looks at nothing and does nothing" $ do
    board <- wildsBoard s registry []
    let after = runWildsUpkeep (wildsAnswer OptionalDecision.Exercises) board
    Spec.assertEqWith s "the library is still empty" (zoneNames Zone.Library after) []
    Spec.assertEqWith s "stack empty: the trigger resolved" (length (GameState.stack after)) 0

-- alice's Clone copying bob's Mudbutton Clanger, over a library holding one
-- card. The Clone is a Goblin Warrior by CR 707.2 and a Shapeshifter as printed,
-- and bob's own Clanger does not trigger in alice's upkeep.
kinshipBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> String -> m (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
kinshipBoard s registry top = do
  clanger <- S.printingOf s registry "Mudbutton Clanger"
  clone <- S.printingOf s registry "Clone"
  card <- S.printingOf s registry top
  let (original, g1) = S.addPermanent clanger S.bob (Setup.emptyGame S.bothPlayers)
      (_, g2) = S.spellOnStack clone S.alice g1
      g3 = S.runPure (kinshipAnswer original) g2 (Stack.resolveTop >> Engine.settleForPriority)
      (_, g4) = S.addLibraryCard card S.alice g3
  case Game.zoneMembers Zone.Battlefield S.alice g4 of
    [copy] -> pure (original, copy, g4)
    _ -> Spec.assertFailure s "the Clone did not resolve onto the battlefield"

-- Copies `wanted` at Clone's as-enters choice and takes every CR 603.5 "may".
kinshipAnswer :: ObjectId.ObjectId -> Prompt.Prompt r -> r
kinshipAnswer wanted p = case p of
  Prompt.ChooseCopyTarget _ _ _ legal -> if elem wanted legal then Just wanted else Nothing
  Prompt.ChooseOptional {} -> OptionalDecision.Exercises
  _ -> S.identityAnswer p

-- Kinship, through Mudbutton Clanger: CR 205.3m's comparison against the
-- SOURCE, which Filter.SharesCreatureTypeWithBound reaches as
-- Binding.triggerSource. The pair differs only in alice's top card, and each leg
-- answers the other way if the source's types were read off the printed Clone.
kinshipSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
kinshipSpec s registry = Spec.describe s "Kinship" $ do
  Spec.it s "CR 205.3m a top card sharing the Clone's copied Warrior type pumps it" $ do
    (original, copy, board) <- kinshipBoard s registry "Mardu Skullhunter"
    let after = runWildsUpkeep (kinshipAnswer original) board
    Spec.assertEqWith s "CR 205.3m the Clone got +1/+1" (Projection.powerOf copy after) (Just 2)
    Spec.assertEqWith s "the Human Warrior was revealed" (revealedNames after) ["Mardu Skullhunter"]
    Spec.assertEqWith s "the Clone entered as a 1/1 copy" (Projection.powerOf copy board) (Just 1)
  Spec.it s "CR 707.2 a top card sharing only the Clone's printed Shapeshifter type does not" $ do
    (original, copy, board) <- kinshipBoard s registry "Primal Plasma"
    let after = runWildsUpkeep (kinshipAnswer original) board
    Spec.assertEqWith s "CR 707.2 the Clone is still 1/1" (Projection.powerOf copy after) (Just 1)
    Spec.assertEqWith s "and nothing was revealed" (revealedNames after) []

-- The elision half: CR 608.2a's gate is asked BEFORE CR 603.5's "may", so a top
-- card that is not a land is never a question. Counts the optional prompts one
-- upkeep raises.
-- CR 401.4: "if an effect puts two or more cards in a specific position in a
-- library at the same time, the owner of those cards may arrange them in any
-- order."
--
-- Ponder ({U} Sorcery, "Look at the top three cards of your library, then put
-- them back in any order. You may shuffle. / Draw a card." -- Oracle text
-- checked on Scryfall, 2026-09-18) is the producer, cast for real. The LOOK
-- shows three and changes nothing; the arrangement is the whole of what a board
-- can see, and Ponder's own draw is what makes it visible from outside the
-- library -- whichever card the answer put on top is the card that ends up in
-- hand.
--
-- Four DIFFERENT printings in alice's library, top-first [piker, maiden,
-- mountain, forest]. Interchangeable cards could not tell a chosen order from
-- the order they were found in, and the fourth is what shows that the cards go
-- back at the positions they already held rather than being stacked onto the
-- library afresh.
--
-- THE SHUFFLE IS DECLINED in every case: rule 701.24a would randomise the order
-- this group exists to read.
ponderBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  m ([ObjectId.ObjectId], ObjectId.ObjectId, GameState.GameState)
ponderBoard s registry = do
  island <- S.printingOf s registry "Island"
  piker <- S.printingOf s registry "Goblin Piker"
  maiden <- S.printingOf s registry "Bird Maiden"
  mountain <- S.printingOf s registry "Mountain"
  forest <- S.printingOf s registry "Forest"
  ponder <- S.printingOf s registry "Ponder"
  let deal (acc, g) printing = let (oid, g2) = S.addLibraryCard printing S.alice g in (oid : acc, g2)
      -- addLibraryCard puts its card ON TOP, so the deepest is stocked first and
      -- `ids` comes back top-first.
      (ids, stocked) = List.foldl' deal ([], S.landsInPlay island 1) [forest, mountain, maiden, piker]
      (board, spellId) = S.handOne ponder stocked
  pure (ids, spellId, board)

ponderSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
ponderSpec s registry = Spec.describe s "PutBackInOrder" $ do
  -- CR 401.4's own "two or more", driven through the opcode because Ponder's
  -- count is fixed at three: one card has one order, so nobody is asked. The
  -- pair differs in the ref's count alone, on one board.
  Spec.it s "CR 401.4 one card is one order and raises no prompt, where two do" $ do
    (_, _, board) <- ponderBoard s registry
    let topOf n = Effect.ArrangeInLibrary (ObjectRef.TopOfLibrary (TopOfLibrary.MkTopOfLibrary (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal n)))
        counting :: Prompt.Prompt r -> State.State Int r
        counting p = case p of
          Prompt.ArrangeLibraryCards {} -> do
            State.modify (+ 1)
            pure (S.identityAnswer p)
          _ -> pure (S.identityAnswer p)
        asks n = State.execState (Engine.runGame counting board (Resolve.applyEffect S.noSource S.noSource S.alice Map.empty Map.empty (topOf n))) 0
    Spec.assertEqWith s "one card asks nothing; two are a decision" (asks 1, asks 2) (0, 1)

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
  Prompt.ChooseSacrifices _ _ _ candidates _ _ ->
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
    let (src, g0) = S.addPermanent piker S.alice (Setup.emptyGame S.bothPlayers)
        (hisPiker, g1) = S.addPermanent piker S.bob g0
        (hisRats, gs) = S.addPermanent rats S.bob g1
        edict = Resolve.applyEffect src src S.alice (Map.singleton slotTarget (Set.singleton (Recipient.ToPlayer S.bob))) (Map.singleton slotTarget (Set.singleton (Recipient.ToPlayer S.bob))) (Effect.PlayerSacrifices (PlayerSacrifices.MkPlayerSacrifices (PlayerRef.EachInSlot slotTarget) creatureFilter (Quantity.Literal 1)))
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
    let (src, g0) = S.addPermanent piker S.alice (Setup.emptyGame S.bothPlayers)
        (hers, g1) = S.addPermanent piker S.alice g0
        (hisPiker, g2) = S.addPermanent piker S.bob g1
        (hisRats, gs) = S.addPermanent rats S.bob g2
        after = S.runPure (namesInstead hers) gs (Resolve.applyEffect src src S.alice (Map.singleton slotTarget (Set.singleton (Recipient.ToPlayer S.bob))) (Map.singleton slotTarget (Set.singleton (Recipient.ToPlayer S.bob))) (Effect.PlayerSacrifices (PlayerSacrifices.MkPlayerSacrifices (PlayerRef.EachInSlot slotTarget) creatureFilter (Quantity.Literal 1))))
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
    let (src, g0) = S.addPermanent piker S.alice (Setup.emptyGame S.bothPlayers)
        (his, gs) = S.addPermanent piker S.bob g0
        countingAnswer :: Prompt.Prompt r -> State.State Int r
        countingAnswer p = case p of
          Prompt.ChooseSacrifices {} -> do
            State.modify (+ 1)
            pure (S.identityAnswer p)
          _ -> pure (S.identityAnswer p)
        act = Resolve.applyEffect src src S.alice (Map.singleton slotTarget (Set.singleton (Recipient.ToPlayer S.bob))) (Map.singleton slotTarget (Set.singleton (Recipient.ToPlayer S.bob))) (Effect.PlayerSacrifices (PlayerSacrifices.MkPlayerSacrifices (PlayerRef.EachInSlot slotTarget) creatureFilter (Quantity.Literal 1)))
        asked = State.execState (Engine.runGame countingAnswer gs act) 0
        after = S.runPure S.identityAnswer gs act
    Spec.assertEqWith s "nothing to choose" asked 0
    Spec.assertBool s (not (S.onBattlefield his after)) "but it still died"

createEmblemSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
createEmblemSpec s registry = Spec.describe s "CreateEmblem" $ do
  Spec.it s "CR 114.2 CreateEmblem puts an emblem in the command zone under the resolver" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (src, gs0) = S.addPermanent piker S.alice (Setup.emptyGame S.bothPlayers)
        act = Resolve.applyEffect src src S.alice Map.empty Map.empty (Effect.CreateEmblem (Printing.card piker))
        after = S.runPure S.identityAnswer gs0 act
        emblems = filter (\oid -> fmap Object.zone (Game.lookupObject oid after) == Just Zone.Command) (Set.toList (GameState.command after))
    Spec.assertEqWith s "one emblem in command" (Set.size (GameState.command after)) 1
    Spec.assertEqWith s "owned by the resolver" (fmap (\oid -> fmap Object.owner (Game.lookupObject oid after)) emblems) [Just S.alice]

becomeMonarchSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
becomeMonarchSpec s registry = Spec.describe s "BecomeMonarch" $ do
  Spec.it s "CR 725 BecomeMonarch TheController makes the resolver the monarch" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (src, gs0) = S.addPermanent piker S.alice (Setup.emptyGame S.bothPlayers)
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
-- Crystal Ball. It reaches none of the assertions below -- S.addPermanent places
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
  m (ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card), ObjectId.ObjectId, GameState.GameState)
denethorBoard s registry = do
  denethor <- S.printingOf s registry "Denethor, Stone Seer"
  mountain <- S.printingOf s registry "Mountain"
  let lands = List.foldl' (\gs _ -> snd (S.addPermanent mountain S.alice gs)) (Setup.emptyGame S.fourPlayers) [1 .. 4 :: Int]
      (srcId, gs1) = S.addPermanent denethor S.alice lands
      ability = case Face.activatedAbilities (S.combinedFace denethor) of
        ab : _ -> ab
        [] -> ActivatedAbility.MkActivatedAbility (Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) []) [] 0 (Modal.MkModal (Seq.singleton (Mode.MkMode Seq.empty Map.empty)) (ModeSelection.ChooseExactly 1)) [] Activator.Controller Nothing Nothing Nothing
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
    let (oid, base0) = S.addPermanent piker S.bob (Setup.emptyGame S.bothPlayers)
        base = base0 {GameState.monarch = Just S.bob}
        slot = SlotName.MkSlotName (Text.pack "target")
        exile =
          Resolve.applyEffect
            S.noSource
            S.noSource
            S.alice
            (Map.singleton slot (Set.singleton (Recipient.ToCreature oid)))
            (Map.singleton slot (Set.singleton (Recipient.ToCreature oid)))
            (jailerExile slot)
        exiled = snd (Engine.runGamePure S.identityAnswer base exile)
        settled = snd (Engine.runGamePure S.identityAnswer exiled MoveDuration.returnDue)
    Spec.assertEqWith s "the watch was registered" (Map.size (GameState.movedUntil exiled)) 1
    Spec.assertEqWith s "bob is still the monarch, unchanged" (GameState.monarch settled) (Just S.bob)
    Spec.assertEqWith s "nothing came back to the battlefield" (Set.size (GameState.battlefield settled)) 0
    Spec.assertEqWith s "and the watch is still armed" (Map.size (GameState.movedUntil settled)) 1
  -- The whole arc, still two seats. The crown must actually CHANGE HANDS to an
  -- opponent before the creature comes back, and alice taking it herself in
  -- between must not discharge the watch.
  Spec.it s "CR 725 the exile returns when a NEW monarch is crowned who is an opponent" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (oid, base0) = S.addPermanent piker S.bob (Setup.emptyGame S.bothPlayers)
        base = base0 {GameState.monarch = Just S.bob}
        slot = SlotName.MkSlotName (Text.pack "target")
        exile =
          Resolve.applyEffect
            S.noSource
            S.noSource
            S.alice
            (Map.singleton slot (Set.singleton (Recipient.ToCreature oid)))
            (Map.singleton slot (Set.singleton (Recipient.ToCreature oid)))
            (jailerExile slot)
        exiled = snd (Engine.runGamePure S.identityAnswer base exile)
        -- Palace Jailer's OTHER entry trigger: alice takes the crown. She is
        -- not her own opponent, so this must not return the creature. Through
        -- Monarch.crown and not a write to GameState.monarch, because that is
        -- where a crowning marks the watches now -- a bare field write is not a
        -- crowning at all.
        alicesCrown = snd (Engine.runGamePure S.identityAnswer (Monarch.crown S.alice exiled) MoveDuration.returnDue)
        -- bob deals combat damage to the monarch (CR 725.3) and takes it back.
        bobsCrown = snd (Engine.runGamePure S.identityAnswer (Monarch.crown S.bob alicesCrown) MoveDuration.returnDue)
    Spec.assertEqWith s "alice holding the crown does not discharge the watch" (Map.size (GameState.movedUntil alicesCrown)) 1
    Spec.assertEqWith s "nor return the creature" (Set.size (GameState.battlefield alicesCrown)) 0
    Spec.assertEqWith s "bob retaking it does return the creature" (Set.size (GameState.battlefield bobsCrown)) 1
    Spec.assertEqWith s "and discharges the watch" (Map.size (GameState.movedUntil bobsCrown)) 0
  -- The crown VANISHING is not an opponent becoming the monarch. CR 725.1's
  -- ruling says the game keeps exactly one monarch once it has one, and the
  -- single way back to none is CR 725.4's last player standing leaving -- but
  -- the watch must not read "no monarch" as "not the controller" and fire.
  Spec.it s "CR 725.1 the crown vanishing is not an opponent becoming the monarch" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (oid, base0) = S.addPermanent piker S.bob (Setup.emptyGame S.bothPlayers)
        base = base0 {GameState.monarch = Just S.bob}
        slot = SlotName.MkSlotName (Text.pack "target")
        exile =
          Resolve.applyEffect
            S.noSource
            S.noSource
            S.alice
            (Map.singleton slot (Set.singleton (Recipient.ToCreature oid)))
            (Map.singleton slot (Set.singleton (Recipient.ToCreature oid)))
            (jailerExile slot)
        exiled = snd (Engine.runGamePure S.identityAnswer base exile)
        -- CR 725.4's third sentence is the only way back to no monarch, and it
        -- crowns nobody, so this is a bare field write by construction.
        noMonarch = snd (Engine.runGamePure S.identityAnswer exiled {GameState.monarch = Nothing} MoveDuration.returnDue)
    Spec.assertEqWith s "the watch is still armed" (Map.size (GameState.movedUntil noMonarch)) 1
    Spec.assertEqWith s "and nothing returned" (Set.size (GameState.battlefield noMonarch)) 0
  -- CR 610.3d across the two registers: a Banisher Priest-style source leaves,
  -- THEN an opponent is crowned, both before one settle, so the Priest's
  -- prisoner returns first. Driven at the sweep rather than through a game: the
  -- one printing that does both in one resolution, Heart-Shaped Herb, is not in
  -- the pool (gap #4583).
  Spec.it s "CR 610.3d a prisoner whose source left before a crowning returns before the crowning's" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    warden <- S.printingOf s registry "Soul Warden"
    let (jailed, g1) = S.addPermanent piker S.bob (Setup.emptyGame S.bothPlayers)
        (held, g2) = S.addPermanent warden S.bob g1
        (source, g3) = S.addPermanent piker S.alice g2
        base = g3 {GameState.monarch = Just S.alice}
        slot = SlotName.MkSlotName (Text.pack "target")
        exile =
          Resolve.applyEffect
            S.noSource
            S.noSource
            S.alice
            (Map.singleton slot (Set.singleton (Recipient.ToCreature jailed)))
            (Map.singleton slot (Set.singleton (Recipient.ToCreature jailed)))
            (jailerExile slot)
        banish = do
          moved <- Event.changeZoneReturning held Zone.Exile
          State.modify' (\g -> g {GameState.movedUntil = GameState.movedUntil g <> Map.fromList [(m, ReturnWatch.MkReturnWatch (ReturnEnding.SourceLeaves source) Zone.Battlefield) | m <- Foldable.toList moved]})
        settled =
          snd
            ( Engine.runGamePure S.identityAnswer base $ do
                exile
                banish
                Event.changeZone source Zone.Graveyard
                State.modify' (Monarch.crown S.bob)
                MoveDuration.returnDue
            )
        arrivals = fmap snd (List.sort [(Object.timestamp obj, fmap S.nameOf (Game.cardOf oid settled)) | oid <- Set.toList (GameState.battlefield settled), Just obj <- [Game.lookupObject oid settled]])
    Spec.assertEqWith s "the Soul Warden came back before the Goblin Piker" arrivals [Just (S.printingName warden), Just (S.printingName piker)]
    Spec.assertEqWith s "both watches are discharged" (Map.size (GameState.movedUntil settled)) 0

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
    let (_, g1) = S.addPermanent piker S.carol S.threePlayerGame
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
    Spec.assertEqWith s "exactly one creature is under the watch" (Map.size (GameState.movedUntil withSpell)) 1
    Spec.assertEqWith s "and carol's Piker is off the battlefield" (S.creaturesInPlay S.carol withSpell) 0
    -- Run A, the behaviour this case exists to prove. CR 400.7 gives the
    -- returning card yet another id, so carol's creature COUNT is what survives.
    Spec.assertEqWith s "bob's reign inside the resolution freed the prisoner" (S.creaturesInPlay S.carol toBob) 1
    Spec.assertEqWith s "though the crown is back with alice, so no later look at the monarch could tell" (GameState.monarch toBob) (Just S.alice)
    Spec.assertEqWith s "and the watch is discharged" (Map.size (GameState.movedUntil toBob)) 0
    -- Run B: one different answer, and nothing else.
    Spec.assertEqWith s "alice crowning herself frees nobody" (S.creaturesInPlay S.carol toAlice) 0
    Spec.assertEqWith s "she is still the monarch" (GameState.monarch toAlice) (Just S.alice)
    Spec.assertEqWith s "and the watch is still armed" (Map.size (GameState.movedUntil toAlice)) 1
    Spec.assertEqWith s "both runs resolved the spell" (length (GameState.stack toBob), length (GameState.stack toAlice)) (0, 0)

-- Palace Jailer's exile as its card file spells it: CR 610.3's move with CR
-- 725's ending, out of the slot's target and into exile.
jailerExile :: SlotName.SlotName -> Effect.Effect card ability
jailerExile slot =
  Effect.MoveToZone
    ( MoveToZone.MkMoveToZone
        (ObjectRef.InSlot slot)
        Zone.Exile
        EntryRiders.MkEntryRiders {EntryRiders.tapped = TapState.Untapped, EntryRiders.attacking = Nothing, EntryRiders.blocking = Nothing, EntryRiders.transformed = False, EntryRiders.counters = Map.empty, EntryRiders.underOwner = False, EntryRiders.exiledFaceDown = False, EntryRiders.attachedTo = Nothing, EntryRiders.faceDown = Nothing, EntryRiders.noted = False, EntryRiders.characteristics = Seq.empty}
        Nothing
        Nothing
        LibraryPlacement.defaultValue
        (Just MoveDuration.Type.UntilAnOpponentBecomesTheMonarch)
    )

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
        Prompt.ChooseOptional (Decider.MkDecider d) player _ idx cIdx _
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
        let (creature, g0) = S.addPermanent piker S.alice (S.landsInPlay mountain 4)
            (g1, worthyId) = S.handOne worthy g0
        pure (g1 {GameState.priority = Just S.alice}, worthyId, creature)
      -- Eccentric Farmer {2}{G} Creature -- Human Peasant 2/3, "When this
      -- creature enters, mill three cards, then you may return a land card from
      -- your graveyard to your hand." (checked against Scryfall.) Corpse Churn's
      -- clause pair on a TRIGGERED ability, so it resolves through
      -- Resolve.resolveModesWith rather than the spell loop.
      --
      -- The Farmer has just entered with its trigger pending, over a three-card
      -- library of a Forest, a Swamp and a Goblin Piker: two lands, so the
      -- return's choice is a real one, and a non-land for the filter to exclude.
      farmerBoard = do
        farmer <- S.printingOf s registry "Eccentric Farmer"
        piker <- S.printingOf s registry "Goblin Piker"
        forest <- S.printingOf s registry "Forest"
        swamp <- S.printingOf s registry "Swamp"
        let (_, g1) = S.addLibraryCard piker S.alice (Setup.emptyGame S.bothPlayers)
            (_, g2) = S.addLibraryCard swamp S.alice g1
            (_, g3) = S.addLibraryCard forest S.alice g2
        pure (snd (S.entersWithTrigger farmer S.alice g3))
      -- Takes the SECOND clause of Corpse Churn or Eccentric Farmer, pinned by
      -- clause index: the group's takeOptional above pins clause 0, which on
      -- these cards is the mandatory mill, so the taking half needs its own
      -- answerer. Everything else,
      -- including the choice of which graveyard card comes back, falls through
      -- to S.identityAnswer.
      returnsChurn :: Prompt.Prompt r -> r
      returnsChurn p = case p of
        Prompt.ChooseOptional _ _ _ _ cIdx _
          | cIdx == ClauseIndex.MkClauseIndex 1 -> OptionalDecision.Exercises
        _ -> S.identityAnswer p
      forestName = CardName.MkCardName (Text.pack "Forest")
      pikerName = CardName.MkCardName (Text.pack "Goblin Piker")
      swampName = CardName.MkCardName (Text.pack "Swamp")
      aliceNamesIn zone gs = List.sort (Maybe.mapMaybe (\oid -> fmap Face.name (Game.faceOf oid gs)) (Game.zoneMembers zone S.alice gs))
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
            (victim, g1) = S.addPermanent piker S.bob base
            (poiId, g2) = S.entersWithTrigger poi S.alice g1
            settled = S.runPure S.identityAnswer g2 (Engine.settleForPriority >> Stack.resolveTop >> Engine.settleForPriority)
            (g3, spellId) = S.handOne complication settled
        pure (g3 {GameState.priority = Just S.alice}, spellId, victim, poiId)
      isSuspected oid gs = fmap (Set.member Designation.Suspected . Object.designations) (Game.lookupObject oid gs)
   in Spec.describe s "OptionalEffect" $ do
        -- The prompt itself, not just its consequence: recording the run puts
        -- the answer in the transcript, which is the only place a raised
        -- prompt is directly observable. Twinned with the mandatory control
        -- below, which must record NO such response.
        Spec.it s "CR 608.2d the choice is announced as a real prompt, and lands in the transcript" $ do
          (gs, faithId) <- handWithTwoLands "Renewed Faith" "Plains"
          case Activatable.abilitiesFor faithId gs of
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
        -- CR 608.2b before CR 603.5: with its only target gone, the ability
        -- "doesn't resolve. It's removed from the stack" -- so there is nothing
        -- left for the "may" to decide and the prompt is never raised. The
        -- engine does not ask a question whose answer cannot matter.
        Spec.it s "CR 608.2b a fizzled optional trigger is not asked about at all" $ do
          (gs, worthyId, piker) <- deemWorthyBoard
          case Activatable.abilitiesFor worthyId gs of
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
        -- reach through a real cast. Eccentric Farmer's cases below are this
        -- loop's gameplay-level proof; this one is the only case for the
        -- two-DRAW shape, where the library count alone separates "declined"
        -- from "drew".
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
              after = S.runPure S.identityAnswer gs (Resolve.resolveModes stackId stackId [(ModeInstance.MkModeInstance 0 (ModeIndex.MkModeIndex 0) 0, mode)])
          Spec.assertEqWith s "the mandatory clause drew, the declined one did not" (S.handSize S.alice after) (before + 1)
        -- The same pair on the ABILITY clause loop, from a real trigger: the
        -- mandatory mill is observable, so declining the "may" is told apart
        -- from skipping the whole mode. Paired with the taking half below.
        Spec.it s "CR 608.2d whole card: Eccentric Farmer's declined return leaves the mill done" $ do
          gs <- farmerBoard
          let ((_, after), transcript) = Replay.record S.identityAnswer gs (Engine.settleForPriority >> Stack.resolveTop)
          Spec.assertEqWith s "declining the return leaves all three milled cards in the graveyard" (aliceNamesIn Zone.Graveyard after) [forestName, pikerName, swampName]
          Spec.assertEqWith s "and nothing came back to the hand" (aliceNamesIn Zone.Hand after) []
          Spec.assertEqWith s "the may was asked once, and declined" (filter isOptionalResponse transcript) [Response.ChoseOptional OptionalDecision.Declines]
        Spec.it s "CR 608.2d whole card: taking Eccentric Farmer's return moves one land and leaves the rest milled" $ do
          gs <- farmerBoard
          let ((_, after), transcript) = Replay.record returnsChurn gs (Engine.settleForPriority >> Stack.resolveTop)
          Spec.assertEqWith s "taking the return leaves the other two milled cards in the graveyard" (aliceNamesIn Zone.Graveyard after) [pikerName, swampName]
          Spec.assertEqWith s "and exactly one land card is in the hand" (aliceNamesIn Zone.Hand after) [forestName]
          Spec.assertEqWith s "the may was asked once, and taken" (filter isOptionalResponse transcript) [Response.ChoseOptional OptionalDecision.Exercises]
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

-- Chooses BOTH of Deadly Complication's modes, aims each slot at the permanent
-- that slot's mode is about, and takes the "may" whenever one is offered. Rank-1
-- for exerciseOptional's reason. The recipients are FILTERED out of the offered
-- set rather than built, so a slot the engine offers under another recipient
-- shape is not silently replaced by one CR 608.2b would drop. A slot named
-- neither of the card's two gets the empty answer, which fails the target
-- announcement rather than aiming somewhere plausible.
deadlyComplicationAnswer :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
deadlyComplicationAnswer victim suspect p =
  let aimAt :: SlotName.SlotName -> (Natural, Set.Set Recipient.Recipient) -> Set.Set Recipient.Recipient
      aimAt slot (_, offered)
        | slot == creatureSlot = Set.filter ((== Just victim) . Recipient.objectOf) offered
        | slot == suspectSlot = Set.filter ((== Just suspect) . Recipient.objectOf) offered
        | otherwise = Set.empty
   in case p of
        Prompt.ChooseModes {} -> Seq.fromList (fmap ModeIndex.MkModeIndex [0, 1])
        Prompt.ChooseTargets _ _ _ sets -> Map.mapWithKey aimAt sets
        Prompt.ChooseOptional {} -> OptionalDecision.Exercises
        _ -> S.identityAnswer p

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
  surveilPromptSpec s registry
  enhancedSurveillanceSpec s registry
  fatesealSpec s registry
  exploreSpec s registry
  exploreOrderSpec s registry
  lookAtSpec s registry
  kinshipSpec s registry
  ponderSpec s registry
  lookAtPromptSpec s registry
  playerSacrificesSpec s registry
  createEmblemSpec s registry
  becomeMonarchSpec s registry
  targetedMonarchSpec s registry
  exileUntilMonarchSpec s registry
  optionalEffectSpec s registry
