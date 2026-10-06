{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers Pawl.Engine.Mana: pools, production and castability -- plus the CR
-- 601.2g mana window itself, which lives in Pawl.Engine.Cost (payMana,
-- chooseSource, tapForMana) because CR 602.2b makes activating a mana ability a
-- cost payment. The window is tested here rather than in CostSpec: the subsystem
-- is mana, and CostSpec covers what a cost IS. CR 605.3a's OTHER window -- a mana
-- ability activated with priority and no payment in flight -- is here too, and
-- is reached through Pawl.Engine.Action and Pawl.Engine.Engine.
--
-- CR 118.13's announcement lives
-- here too (Mana.announce), so the cases that reach it through
-- Cast.castSpell, Activate.activateAbility and Resolve.payGatePaidBy are in this
-- spec rather than in CastSpec, ActivateSpec or ResolveSpec -- the module under
-- test is this one, and the three entry points are how the rule is reached.
module Pawl.ManaSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Action as Action
import qualified Pawl.Engine.Activatable as Activatable
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Cost as Cost
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Mana as Mana
import qualified Pawl.Engine.ManaAbility as ManaAbility
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Projection.View as Projection
import qualified Pawl.Engine.Replay as Replay
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Engine.Subtype as SubtypeEngine
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as Action.Type
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.Activator as Activator
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Clause as Clause
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.CounterName as CounterName
import qualified Pawl.Types.DamagePart as DamagePart
import qualified Pawl.Types.DealDamage as DealDamage
import qualified Pawl.Types.Decider as Decider
import qualified Pawl.Types.Departure as Departure.Type
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Mana as Mana.Type
import qualified Pawl.Types.ManaAddition as ManaAddition
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaOption as ManaOption
import qualified Pawl.Types.ManaProduction as ManaProduction
import qualified Pawl.Types.ManaRetention as ManaRetention
import qualified Pawl.Types.ManaSpending as ManaSpending
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.ManaUnit as ManaUnit
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeSelection as ModeSelection
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.Optionality as Optionality
import qualified Pawl.Types.PaymentDecision as PaymentDecision
import qualified Pawl.Types.PaymentMoment as PaymentMoment
import qualified Pawl.Types.PaymentSubject as PaymentSubject
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Pool as Pool
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.ProductionTag as ProductionTag
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.Response as Response
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Status as Status
import qualified Pawl.Types.StepBegan as StepBegan
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TargetSlot as TargetSlot
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChange as ZoneChange

-- A single forced mode (ChooseExactly 1, M4g's non-modal shape) wrapping one
-- ability's effects and target slots -- the fixture shape every pre-M4h
-- single-mode ActivatedAbility now takes.
singleModeAbility :: [Effect.Effect card ability] -> Map.Map SlotName.SlotName TargetSlot.TargetSlot -> Modal.Modal card ability
singleModeAbility effects slots =
  Modal.MkModal (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing (Seq.fromList effects))) slots)) (ModeSelection.ChooseExactly 1)

-- Answers Prompt.ChooseManaSource with `wanted` whenever it is on offer, and
-- defers everything else to S.identityAnswer. Its sibling avoids that source
-- instead: between them they prove the ANSWER is what decides, rather than the
-- order Mana.manaSources happens to return (#12).
prefersSource :: ObjectId.ObjectId -> Prompt.Prompt r -> r
prefersSource wanted p = case p of
  Prompt.ChooseManaSource _ _ candidates ->
    Just (if elem wanted (NonEmpty.toList candidates) then wanted else NonEmpty.head candidates)
  _ -> S.identityAnswer p

avoidsSource :: ObjectId.ObjectId -> Prompt.Prompt r -> r
avoidsSource unwanted p = case p of
  Prompt.ChooseManaSource _ _ candidates -> Just $ case filter (/= unwanted) (NonEmpty.toList candidates) of
    h : _ -> h
    [] -> NonEmpty.head candidates
  _ -> S.identityAnswer p

-- The board the three CR 604.2 cases below share: alice controls Zhao, the Moon
-- Slayer ("As long as Zhao has a conqueror counter on him, nonbasic lands are
-- Mountains") and a Reliquary Tower, and Zhao carries one counter of each of
-- `kinds`. The runs differ in NOTHING but which counters sit on Zhao, so neither
-- the Tower's mana nor Zhao's own text can be what moved between them.
--
-- Two permanents and not three: the Tower is the only nonbasic land, so it is
-- the only object Zhao's affected set names. Reliquary Tower's "{T}: Add {C}" is
-- the discriminator, and colorless is a mana type this board can produce no
-- other way -- a Mountain'd Tower makes red.
--
-- Zhao's "Nonbasic lands enter tapped" never fires here: S.addPermanent writes
-- the Object record directly rather than going through Event.placeObject, so the
-- Tower arrives untapped and there is mana to tap for.
zhaoBoard :: Printing.Printing -> Printing.Printing -> [CounterKind.CounterKind Keyword.Keyword] -> (ObjectId.ObjectId, GameState.GameState)
zhaoBoard zhao reliquaryTower kinds =
  let (towerId, g1) = S.addPermanent reliquaryTower S.alice (Setup.emptyGame S.bothPlayers)
      (zhaoId, g2) = S.addPermanent zhao S.alice g1
   in (towerId, foldr (\k -> S.addCounter k 1 zhaoId) g2 kinds)

-- CR 122.1: the kind Zhao's two sentences both name. A counter's identity is its
-- name, so this is exact Text equality with what the card file writes.
conquerorCounter :: CounterKind.CounterKind Keyword.Keyword
conquerorCounter = CounterKind.Named (CounterName.UnsafeMkCounterName (Text.pack "conqueror"))

-- One mana unit of `mt`, untagged -- what tapping a land for its one mana ability
-- floats.
oneUnit :: ManaType.ManaType -> Mana.Type.Mana
oneUnit mt = Mana.Type.MkMana [ManaUnit.MkManaUnit {ManaUnit.manaType = mt, ManaUnit.tags = Set.empty, ManaUnit.retention = ManaRetention.Ordinary, ManaUnit.restriction = Nothing, ManaUnit.rider = Nothing, ManaUnit.spendTrigger = Nothing, ManaUnit.sourceChosenSubtype = Nothing, ManaUnit.sourceLastExiled = Nothing}]

pikerCost :: ManaCost.ManaCost
pikerCost = ManaCost.MkManaCost [ManaSymbol.Generic 1, ManaSymbol.OfType (ManaType.Colored Color.Red)]

-- {G}{G}: the cost alice's Dryad Arbor and Forest together pay and either alone
-- cannot, which is what makes the CR 613.1f case below discriminating.
greenGreen :: ManaCost.ManaCost
greenGreen = ManaCost.MkManaCost [ManaSymbol.OfType (ManaType.Colored Color.Green), ManaSymbol.OfType (ManaType.Colored Color.Green)]

poolSize :: PlayerId.PlayerId -> GameState.GameState -> Int
poolSize pid gs = case Game.poolOf pid gs of
  Mana.Type.MkMana units -> length units

manaSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
manaSpec s registry = Spec.describe s "Mana" $ do
  Spec.it s "substituteX replaces each Variable with Generic X, keeping order" $
    let red = ManaSymbol.OfType (ManaType.Colored Color.Red)
        cost = ManaCost.MkManaCost [ManaSymbol.Variable, red]
     in Spec.assertEqWith
          s
          "X=3 -> {3}{R}"
          (Mana.substituteX 3 cost)
          (ManaCost.MkManaCost [ManaSymbol.Generic 3, red])

  Spec.it s "substituteX 0 leaves a Variable-free cost payable" $
    let red = ManaSymbol.OfType (ManaType.Colored Color.Red)
     in Spec.assertEqWith
          s
          "floor is {0}{R}"
          (Mana.substituteX 0 (ManaCost.MkManaCost [ManaSymbol.Variable, red]))
          (ManaCost.MkManaCost [ManaSymbol.Generic 0, red])

  Spec.it s "CR 305.6 a Mountain's red mana ability comes from its subtype" $
    Spec.assertEqWith
      s
      "red"
      (SubtypeEngine.subtypeMana Subtype.Mountain)
      (Just (ManaType.Colored Color.Red))

  Spec.it s "a Goblin grants no mana ability" $
    Spec.assertEqWith s "none" (SubtypeEngine.subtypeMana Subtype.Goblin) Nothing

  Spec.it s "CR 305.6 Island taps blue, Plains taps white" $ do
    Spec.assertEqWith s "island" (SubtypeEngine.subtypeMana Subtype.Island) (Just (ManaType.Colored Color.Blue))
    Spec.assertEqWith s "plains" (SubtypeEngine.subtypeMana Subtype.Plains) (Just (ManaType.Colored Color.White))

  Spec.it s "CR 205.3h: Aura is an enchantment type, so it has no CR 305.6 intrinsic mana" $
    Spec.assertEqWith s "no mana" (SubtypeEngine.subtypeMana Subtype.Aura) Nothing

  -- CR 305.6 grants its intrinsic ability to "an object with the land card
  -- type and A BASIC LAND TYPE", and CR 205.3i lists which of the land types
  -- those are: "Of that list, Forest, Island, Mountain, Plains, and Swamp are
  -- the basic land types." So a Desert is a land type with no mana of its own
  -- -- the one constructor where this answer and Pawl.Engine.Subtype.isLandType's
  -- (asserted in Pawl.ProjectionSpec) come apart, and the reason they are two
  -- functions.
  Spec.it s "CR 305.6 Desert is a land type but not a BASIC one, so it grants no mana" $
    Spec.assertEqWith s "no mana" (SubtypeEngine.subtypeMana Subtype.Desert) Nothing

  Spec.it s "an empty pool starts empty" $ do
    mountain <- S.printingOf s registry "Mountain"
    Spec.assertEqWith s "empty" (poolSize S.alice (S.landsInPlay mountain 2)) 0

  Spec.it s "tapping a Mountain taps it and adds one red unit" $ do
    mountain <- S.printingOf s registry "Mountain"
    let gs = S.landsInPlay mountain 1
    case Game.zoneMembers Zone.Battlefield S.alice gs of
      [] -> Spec.assertFailure s "fixture should have one Mountain"
      oid : _ -> do
        let after = S.runPure S.identityAnswer gs (S.tapForMana oid)
        Spec.assertEqWith s "tapped" (S.tappedCount S.alice after) 1
        Spec.assertEqWith
          s
          "pool"
          (Game.poolOf S.alice after)
          (Mana.Type.MkMana [ManaUnit.MkManaUnit {ManaUnit.manaType = ManaType.Colored Color.Red, ManaUnit.tags = Set.empty, ManaUnit.retention = ManaRetention.Ordinary, ManaUnit.restriction = Nothing, ManaUnit.rider = Nothing, ManaUnit.spendTrigger = Nothing, ManaUnit.sourceChosenSubtype = Nothing, ManaUnit.sourceLastExiled = Nothing}])

  Spec.it s "two Mountains can pay {1}{R}" $ do
    mountain <- S.printingOf s registry "Mountain"
    Spec.assertBool s (Mana.canPay Cost.manaActivations S.alice pikerCost (S.landsInPlay mountain 2)) "affordable"

  Spec.it s "one Mountain cannot pay {1}{R}" $ do
    mountain <- S.printingOf s registry "Mountain"
    Spec.assertBool s (not (Mana.canPay Cost.manaActivations S.alice pikerCost (S.landsInPlay mountain 1))) "unaffordable"

  Spec.it s "no Mountains cannot pay {1}{R}" $ do
    mountain <- S.printingOf s registry "Mountain"
    Spec.assertBool s (not (Mana.canPay Cost.manaActivations S.alice pikerCost (S.landsInPlay mountain 0))) "unaffordable"

  -- Three identical Mountains: every candidate is a copy of the same card, so
  -- the choice is genuinely indistinguishable and Cost.payMana must NOT ask
  -- (#12). S.identityAnswer would answer anyway; what this pins is the tap
  -- count.
  Spec.it s "paying {1}{R} taps exactly two of three Mountains and leaves no float" $ do
    mountain <- S.printingOf s registry "Mountain"
    let (paid, after) = S.runPureWith S.identityAnswer (S.landsInPlay mountain 3) (Cost.payMana S.manaPerformer PaymentSubject.ForNeither ManaSpending.AsProduced S.alice pikerCost)
    Spec.assertBool s paid "three Mountains should pay {1}{R}"
    Spec.assertEqWith s "tapped" (S.tappedCount S.alice after) 2
    Spec.assertEqWith s "no float" (poolSize S.alice after) 0

  Spec.it s "CR 500.5 mana pools empty" $ do
    mountain <- S.printingOf s registry "Mountain"
    let gs = S.landsInPlay mountain 1
    case Game.zoneMembers Zone.Battlefield S.alice gs of
      [] -> Spec.assertFailure s "fixture should have one Mountain"
      oid : _ ->
        Spec.assertEqWith s "emptied" (poolSize S.alice (Mana.emptiedManaPools (S.runPure S.identityAnswer gs (S.tapForMana oid)))) 0

  -- CR 122.1 / CR 105.4: "{T}: Add {C}. If Gemstone Caverns has a luck counter on
  -- it, instead add one mana of any color." pawl carries the sentence as two
  -- gated abilities whose conditions are complements (ActivatedAbility.condition
  -- over Quantity.ObjectCounters), so exactly one exists at a time and "instead"
  -- falls out of the pair.
  Spec.it s "CR 122.1 a luck counter swaps Gemstone Caverns' {C} for any color" $ do
    caverns <- S.printingOf s registry "Gemstone Caverns"
    let base = Setup.emptyGame S.bothPlayers
        (cavernsId, gs) = S.addPermanent caverns S.alice base
        lucky = S.addCounter (CounterKind.Named (CounterName.UnsafeMkCounterName (Text.pack "luck"))) 1 cavernsId gs
    Spec.assertEqWith s "no counter: colorless and nothing else" (Mana.manaTypesOf cavernsId gs) [ManaType.Colorless]
    Spec.assertBool s (ManaType.Colored Color.White `elem` Mana.manaTypesOf cavernsId lucky) "with a luck counter, white is available"
    Spec.assertBool s (ManaType.Colored Color.Green `elem` Mana.manaTypesOf cavernsId lucky) "and so is green -- CR 105.4's whole five"
    -- The "instead": the {C} ability is gone, not joined.
    Spec.assertBool s (ManaType.Colorless `notElem` Mana.manaTypesOf cavernsId lucky) "and colorless is not"

  Spec.it s "CR 305.6/305.7 an Urborg'd Mountain taps for black too" $ do
    mountain <- S.printingOf s registry "Mountain"
    urborg <- S.printingOf s registry "Urborg, Tomb of Yawgmoth"
    let base = Setup.emptyGame S.bothPlayers
        (mountainId, g1) = S.addPermanent mountain S.alice base
        (_, gs) = S.addPermanent urborg S.alice g1
    -- Urborg adds Swamp to all lands, so the Mountain taps for black too.
    Spec.assertBool s (ManaType.Colored Color.Black `elem` Mana.manaTypesOf mountainId gs) "black available"
    Spec.assertBool s (ManaType.Colored Color.Red `elem` Mana.manaTypesOf mountainId gs) "red still available"

  Spec.it s "CR 305.6/305.7 a Blood Moon'd Urborg taps for red only" $ do
    urborg <- S.printingOf s registry "Urborg, Tomb of Yawgmoth"
    bloodMoon <- S.printingOf s registry "Blood Moon"
    let base = Setup.emptyGame S.bothPlayers
        (urborgId, g1) = S.addPermanent urborg S.alice base
        (_, gs) = S.addPermanent bloodMoon S.alice g1
    Spec.assertBool s (ManaType.Colored Color.Red `elem` Mana.manaTypesOf urborgId gs) "red available"
    Spec.assertBool s (ManaType.Colored Color.Black `notElem` Mana.manaTypesOf urborgId gs) "black not available (stripped)"

  -- CR 305.7 takes away the land's PRINTED mana ability and hands back the
  -- one its new basic land type carries: "It loses all abilities generated
  -- from its rules text ... and it gains the appropriate mana ability for
  -- each new basic land type." Reliquary Tower's "{T}: Add {C}" is an
  -- ACTIVATED ability, and it is the pool's sharpest witness that the strip
  -- reaches all of a land's rules text: PlayerEffectSpec already pins the
  -- other half of this same card's text going away under the same Blood Moon.
  Spec.it s "CR 305.7 a Blood Moon'd Reliquary Tower taps for red, not colorless" $ do
    reliquaryTower <- S.printingOf s registry "Reliquary Tower"
    bloodMoon <- S.printingOf s registry "Blood Moon"
    let base = Setup.emptyGame S.bothPlayers
        (towerId, g1) = S.addPermanent reliquaryTower S.alice base
        (_, gs) = S.addPermanent bloodMoon S.alice g1
    Spec.assertBool s (ManaType.Colored Color.Red `elem` Mana.manaTypesOf towerId gs) "red available (CR 305.6, from the new Mountain type)"
    Spec.assertBool s (ManaType.Colorless `notElem` Mana.manaTypesOf towerId gs) "colorless gone (the printed {T}: Add {C} was stripped)"

  -- CR 604.2's "as long as" clause on the STRIPPER, end to end: the layer-4 set and
  -- the CR 305.7 strip that follows it both switch on with the clause, so the Tower
  -- taps for its printed {C} while the clause is false and for the new Mountain's
  -- {R} once it holds. The pool is the pool a player would actually have floated.
  --
  -- A REGRESSION FENCE for this half rather than a proof of it: gatherStatic
  -- already gated the ability, so the fold was right before these cases existed
  -- and none of them goes red when that gate is wired open. What the gate half's
  -- divergence needed was a reader outside the fold, and Pawl.PlayerEffectSpec's
  -- Zhao pair is that -- it is the one this trio composes with.
  --
  -- A PROOF for the counter kind, which is the new half: CR 122.1 makes a
  -- counter's identity its name, and the trio below reads the kind three ways --
  -- absent, present-but-a-different-kind, present.
  Spec.it s "CR 604.2/305.7 with no counter on Zhao the clause is false, and the Tower still taps for {C}" $ do
    zhao <- S.printingOf s registry "Zhao, the Moon Slayer"
    reliquaryTower <- S.printingOf s registry "Reliquary Tower"
    let (towerId, gs) = zhaoBoard zhao reliquaryTower []
    Spec.assertEqWith s "pool" (Game.poolOf S.alice (S.runPure S.identityAnswer gs (S.tapForMana towerId))) (oneUnit ManaType.Colorless)
    Spec.assertBool s (Subtype.Mountain `notElem` Set.toList (Projection.subtypesOf towerId gs)) "and the layer-4 set did not happen either"

  -- The discriminating case: Zhao carries a counter, but of the WRONG KIND. An
  -- implementation that sums Object.counters instead of looking the kind up --
  -- or that matches any Named counter -- turns the clause on here and floats
  -- {R}. +1/+1 and not an obscure kind, because the engine already places and
  -- reads that one (layer 7c), so a "count any counter" bug is guaranteed to see
  -- it rather than missing it for an unrelated reason.
  Spec.it s "CR 122.1 a +1/+1 counter is not a conqueror counter, and the Tower still taps for {C}" $ do
    zhao <- S.printingOf s registry "Zhao, the Moon Slayer"
    reliquaryTower <- S.printingOf s registry "Reliquary Tower"
    let (towerId, gs) = zhaoBoard zhao reliquaryTower [CounterKind.PlusOnePlusOne]
    Spec.assertEqWith s "pool" (Game.poolOf S.alice (S.runPure S.identityAnswer gs (S.tapForMana towerId))) (oneUnit ManaType.Colorless)
    Spec.assertBool s (Subtype.Mountain `notElem` Set.toList (Projection.subtypesOf towerId gs)) "and the layer-4 set did not happen either"

  -- The same board with the clause satisfied, which is what keeps the two cases
  -- above from passing on a gate wired SHUT: one conqueror counter arrives,
  -- Zhao's effect starts to apply, and the Tower is a Mountain that taps for red
  -- alone.
  Spec.it s "CR 604.2/305.7 a conqueror counter turns the clause on, and the Tower taps for {R}" $ do
    zhao <- S.printingOf s registry "Zhao, the Moon Slayer"
    reliquaryTower <- S.printingOf s registry "Reliquary Tower"
    let (towerId, gs) = zhaoBoard zhao reliquaryTower [conquerorCounter]
    Spec.assertEqWith s "pool" (Game.poolOf S.alice (S.runPure S.identityAnswer gs (S.tapForMana towerId))) (oneUnit (ManaType.Colored Color.Red))
    Spec.assertBool s (Subtype.Mountain `elem` Set.toList (Projection.subtypesOf towerId gs)) "the Tower is a Mountain (CR 305.7's set)"
    Spec.assertEqWith s "and its printed ability is gone" (Projection.abilitiesOf towerId gs) []

  -- The counter arriving from the CARD rather than from S.addCounter: "{7}: Put a
  -- conqueror counter on Zhao" is activated and resolved, and the land subtype
  -- set switches on afterwards. That is Effect.PutCounters carrying a card-named
  -- kind end to end -- through the codec, through resolution, into the Map key
  -- the static ability then looks up.
  --
  -- Seven BASIC Islands pay the {7}, which keeps the Tower the only nonbasic
  -- land on the board and so the only object Zhao's affected set names.
  Spec.it s "CR 122.1 Zhao's own ability puts a conqueror counter on him, and the Tower becomes a Mountain" $ do
    zhao <- S.printingOf s registry "Zhao, the Moon Slayer"
    reliquaryTower <- S.printingOf s registry "Reliquary Tower"
    island <- S.printingOf s registry "Island"
    let (zhaoId, g1) = S.addPermanent zhao S.alice (S.landsInPlay island 7)
        (towerId, g2) = S.addPermanent reliquaryTower S.alice g1
        gs = g2 {GameState.priority = Just S.alice}
        ability = case Face.activatedAbilities (S.combinedFace zhao) of
          ab : _ -> Just ab
          [] -> Nothing
    case ability of
      Nothing -> Spec.assertFailure s "expected Zhao to have an activated ability"
      Just ab -> do
        let activated = S.runPure S.identityAnswer gs (Activate.activateAbility S.alice zhaoId ab)
            after = S.runPure S.identityAnswer activated Stack.resolveTop
        Spec.assertBool s (Subtype.Mountain `elem` Set.toList (Projection.subtypesOf towerId after)) "the Tower is a Mountain once the ability has resolved"
        Spec.assertBool s (Subtype.Mountain `notElem` Set.toList (Projection.subtypesOf towerId activated)) "and was not one while the ability was still on the stack"
        Spec.assertEqWith s "one conqueror counter" (S.counterOf conquerorCounter zhaoId after) 1
        Spec.assertEqWith s "and no +1/+1 counter, so the kind is what was placed" (S.counterOf CounterKind.PlusOnePlusOne zhaoId after) 0

  -- CR 305.7's strip again, with the new type CHOSEN as an Aura entered (CR
  -- 614.1c) rather than printed on the stripper. Reliquary Tower's "{T}: Add
  -- {C}" is an activated ability, so this is the sharpest witness that the
  -- strip reaches a land's whole rules text whichever modification performed
  -- it -- the same claim the Blood Moon case above makes, now for the arm that
  -- reads Object.chosenSubtype. Pawl.AuraSpec's whole-card case is what proves
  -- the choice is really MADE; this proves what it costs the land.
  Spec.it s "CR 305.6/305.7 a Convincing Mirage'd Reliquary Tower taps for the chosen colour" $ do
    reliquaryTower <- S.printingOf s registry "Reliquary Tower"
    convincingMirage <- S.printingOf s registry "Convincing Mirage"
    let base = Setup.emptyGame S.bothPlayers
        (towerId, g1) = S.addPermanent reliquaryTower S.alice base
        (mirageId, g2) = S.addPermanent convincingMirage S.alice g1
        gs = S.withChosenSubtype Subtype.Plains mirageId (S.attach mirageId towerId g2)
    Spec.assertBool s (ManaType.Colored Color.White `elem` Mana.manaTypesOf towerId gs) "white available (CR 305.6, from the chosen Plains)"
    Spec.assertBool s (ManaType.Colorless `notElem` Mana.manaTypesOf towerId gs) "colorless gone (the printed {T}: Add {C} was stripped)"

  -- The same strip, on a land whose rules text is not a mana ability at all.
  Spec.it s "CR 305.7 a Blood Moon'd Evolving Wilds has no activated ability left" $ do
    evolvingWilds <- S.printingOf s registry "Evolving Wilds"
    bloodMoon <- S.printingOf s registry "Blood Moon"
    let base = Setup.emptyGame S.bothPlayers
        (wildsId, g1) = S.addPermanent evolvingWilds S.alice base
        (_, gs) = S.addPermanent bloodMoon S.alice g1
    Spec.assertEqWith s "the fetch ability is gone" (Projection.abilitiesOf wildsId gs) []
    Spec.assertBool s (ManaType.Colored Color.Red `elem` Mana.manaTypesOf wildsId gs) "and it taps for red instead"

  -- CR 305.6: the intrinsic mana ability comes with the land TYPE, whether
  -- the type was printed or added at layer 4 -- so an Ashaya-animated
  -- creature taps for green, and Blood Moon (CR 305.7) rewrites that to red
  -- by setting the same subtype it reads.
  Spec.it s "CR 305.6 a creature Ashaya made a Forest land taps for green" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    ashaya <- S.printingOf s registry "Ashaya, Soul of the Wild"
    let base = Setup.emptyGame S.bothPlayers
        (pikerId, g1) = S.addPermanent piker S.alice base
        (_, gs) = S.addPermanent ashaya S.alice g1
    Spec.assertBool s (ManaType.Colored Color.Green `elem` Mana.manaTypesOf pikerId gs) "green available"
    Spec.assertBool s (pikerId `elem` Mana.manaSources Cost.manaActivations S.alice gs) "and it is a mana source"

  Spec.it s "CR 305.7 Blood Moon turns that same creature-land red" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    ashaya <- S.printingOf s registry "Ashaya, Soul of the Wild"
    bloodMoon <- S.printingOf s registry "Blood Moon"
    let base = Setup.emptyGame S.bothPlayers
        (pikerId, g1) = S.addPermanent piker S.alice base
        (_, g2) = S.addPermanent bloodMoon S.alice g1
        (_, gs) = S.addPermanent ashaya S.alice g2
    Spec.assertBool s (ManaType.Colored Color.Red `elem` Mana.manaTypesOf pikerId gs) "red available"
    Spec.assertBool s (ManaType.Colored Color.Green `notElem` Mana.manaTypesOf pikerId gs) "green gone"

  -- Ashaya's reminder text: "(They're still affected by summoning sickness.)"
  -- CR 302.6 gates a CREATURE's {T} ability, and CR 205.1b's "in addition to
  -- their other types" keeps the creature type -- so gaining CR 305.6's mana
  -- ability does not hand a fresh creature a land's exemption. Nothing had to
  -- be built for this; it falls out of Mana.manaSources reading the PROJECTED
  -- card types.
  Spec.it s "CR 302.6 a summoning-sick creature Ashaya animated still cannot tap for mana" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    ashaya <- S.printingOf s registry "Ashaya, Soul of the Wild"
    let base = Setup.emptyGame S.bothPlayers
        (pikerId, g1) = S.addPermanent piker S.alice base
        (_, g2) = S.addPermanent ashaya S.alice g1
        sick = g2 {GameState.objects = Map.adjust (\o -> o {Object.sickness = Sickness.Sick}) pikerId (GameState.objects g2)}
    Spec.assertBool s (Set.member CardType.Land (Projection.cardTypesOf pikerId sick)) "it is a land now"
    Spec.assertBool s (Projection.isCreatureOf pikerId sick) "and still a creature"
    Spec.assertBool s (pikerId `notElem` Mana.manaSources Cost.manaActivations S.alice sick) "so the sick creature is no mana source"

  -- CR 305.6 makes the intrinsic "{T}: Add {G}" an ability OF the land, so CR
  -- 613.1f's layer-6 "loses all abilities" takes it with the printed ones: bob's
  -- Humility leaves alice's Dryad Arbor (Land Creature -- Forest Dryad) tapping
  -- for nothing. The Forest beside it is the control on one board -- same
  -- subtype, same controller, no creature type -- so the {G}{G} that stops being
  -- payable can only be the Arbor's half; see #3267.
  Spec.it s "CR 613.1f Humility strips Dryad Arbor's CR 305.6 mana ability" $ do
    arbor <- S.printingOf s registry "Dryad Arbor"
    forest <- S.printingOf s registry "Forest"
    humility <- S.printingOf s registry "Humility"
    let base = Setup.emptyGame S.bothPlayers
        (arborId, g1) = S.addPermanent arbor S.alice base
        (forestId, g2) = S.addPermanent forest S.alice g1
        (humilityId, gs) = S.addPermanent humility S.bob g2
        -- CR 611.3b: a static ability's effect applies only while its permanent
        -- is on the battlefield, so the ability rule 305.6 gives the land comes
        -- back once Humility is destroyed.
        gone = S.runPure S.identityAnswer gs (Event.destroy Regenerability.Regenerable [humilityId])
    Spec.assertBool s (not (Mana.canPay Cost.manaActivations S.alice greenGreen gs)) "under Humility alice cannot pay {G}{G}"
    Spec.assertBool s (Mana.canPay Cost.manaActivations S.alice greenGreen gone) "and can once Humility has left"
    Spec.assertEqWith s "the Arbor adds nothing under Humility" (Mana.manaTypesOf arborId gs) []
    Spec.assertBool s (arborId `notElem` Mana.manaSources Cost.manaActivations S.alice gs) "so it is no mana source"
    Spec.assertEqWith s "the Forest is untouched -- Humility reaches creatures only" (Mana.manaTypesOf forestId gs) [ManaType.Colored Color.Green]
    Spec.assertEqWith s "and the Arbor taps for green again" (Mana.manaTypesOf arborId gone) [ManaType.Colored Color.Green]

  Spec.it s "CR 605.1a a {T}: Add {G} ability is a mana ability" $
    let ab =
          ActivatedAbility.MkActivatedAbility
            { ActivatedAbility.cost = Cost.Type.MkCost {Cost.Type.mana = Just (ManaCost.MkManaCost []), Cost.Type.components = []},
              ActivatedAbility.modal = singleModeAbility [Effect.AddMana (ManaAddition.MkManaAddition (PlayerRef.Relative PlayerRelation.You) (ManaProduction.OfType (ManaType.Colored Color.Green)) (Quantity.Literal 1) ManaRetention.Ordinary Nothing Nothing Nothing)] Map.empty,
              ActivatedAbility.maximumX = [],
              ActivatedAbility.minimumX = 0,
              ActivatedAbility.restrictions = [],
              ActivatedAbility.activator = Activator.Controller,
              ActivatedAbility.condition = Nothing,
              ActivatedAbility.name = Nothing,
              ActivatedAbility.keyword = Nothing
            }
     in Spec.assertBool s (ManaAbility.isManaAbility ab) "mana ability"

  Spec.it s "CR 605.1a an ability that targets is NOT a mana ability" $
    let ab =
          ActivatedAbility.MkActivatedAbility
            { ActivatedAbility.cost = Cost.Type.MkCost {Cost.Type.mana = Just (ManaCost.MkManaCost []), Cost.Type.components = []},
              ActivatedAbility.modal =
                singleModeAbility
                  [Effect.AddMana (ManaAddition.MkManaAddition (PlayerRef.Relative PlayerRelation.You) (ManaProduction.OfType (ManaType.Colored Color.Green)) (Quantity.Literal 1) ManaRetention.Ordinary Nothing Nothing Nothing)]
                  (Map.singleton (SlotName.MkSlotName (Text.pack "x")) (TargetSlot.required Pool.AnyTarget Nothing)),
              ActivatedAbility.maximumX = [],
              ActivatedAbility.minimumX = 0,
              ActivatedAbility.restrictions = [],
              ActivatedAbility.activator = Activator.Controller,
              ActivatedAbility.condition = Nothing,
              ActivatedAbility.name = Nothing,
              ActivatedAbility.keyword = Nothing
            }
     in Spec.assertBool s (not (ManaAbility.isManaAbility ab)) "targets -> not mana"

  Spec.it s "CR 605.1a a damage ability is NOT a mana ability" $
    let ab =
          ActivatedAbility.MkActivatedAbility
            { ActivatedAbility.cost = Cost.Type.MkCost {Cost.Type.mana = Just (ManaCost.MkManaCost []), Cost.Type.components = []},
              ActivatedAbility.modal =
                singleModeAbility
                  [Effect.DealDamage (DealDamage.MkDealDamage (Seq.singleton (DamagePart.MkDamagePart (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "x"))) (Quantity.Literal 1))) Nothing Nothing)]
                  (Map.singleton (SlotName.MkSlotName (Text.pack "x")) (TargetSlot.required Pool.AnyTarget Nothing)),
              ActivatedAbility.maximumX = [],
              ActivatedAbility.minimumX = 0,
              ActivatedAbility.restrictions = [],
              ActivatedAbility.activator = Activator.Controller,
              ActivatedAbility.condition = Nothing,
              ActivatedAbility.name = Nothing,
              ActivatedAbility.keyword = Nothing
            }
     in Spec.assertBool s (not (ManaAbility.isManaAbility ab)) "no mana produced -> not mana"

  Spec.it s "CR 605 a settled Llanowar Elves is a green mana source" $ do
    llanowarElves <- S.printingOf s registry "Llanowar Elves"
    let (elfId, gs) = S.addPermanent llanowarElves S.alice (Setup.emptyGame S.bothPlayers)
    Spec.assertBool s (elem (ManaType.Colored Color.Green) (Mana.manaTypesOf elfId gs)) "taps green"
    Spec.assertBool s (elem elfId (Mana.manaSources Cost.manaActivations S.alice gs)) "is a mana source"

  Spec.it s "CR 302.6 a summoning-sick Llanowar Elves is NOT a mana source" $ do
    llanowarElves <- S.printingOf s registry "Llanowar Elves"
    let (elfId, g0) = S.addPermanent llanowarElves S.alice (Setup.emptyGame S.bothPlayers)
        sick = g0 {GameState.objects = Map.adjust (\o -> o {Object.sickness = Sickness.Sick}) elfId (GameState.objects g0)}
    Spec.assertBool s (notElem elfId (Mana.manaSources Cost.manaActivations S.alice sick)) "sick elf excluded"

  -- CR 302.6's other half, and the same trap #198 sprang on attacking: bob's
  -- Elves settled under BOB, so the settle it carries says nothing about
  -- alice. Stealing it does not hand her a mana source this turn.
  Spec.it s "CR 302.6 a stolen Llanowar Elves is not a mana source for the thief" $ do
    llanowarElves <- S.printingOf s registry "Llanowar Elves"
    controlMagic <- S.printingOf s registry "Control Magic"
    let (elfId, g0) = S.addPermanent llanowarElves S.bob (Setup.emptyGame S.bothPlayers)
        settled = S.runPure S.identityAnswer g0 (Engine.settleAll S.bob)
        (aura, withAura) = S.addPermanent controlMagic S.alice settled
        stolen = S.attach aura elfId withAura
    Spec.assertBool s (elem elfId (Mana.manaSources Cost.manaActivations S.bob settled)) "bob could tap it"
    Spec.assertBool s (elem elfId (Projection.controls S.alice stolen)) "alice controls it now"
    Spec.assertBool s (notElem elfId (Mana.manaSources Cost.manaActivations S.alice stolen)) "but it is sick for her, so it is not her mana source"

  -- CR 702.10c is the exemption that makes the steal above pay off when the
  -- thief also grants haste: "If a creature has haste, its controller can
  -- activate its activated abilities whose cost includes the tap symbol or
  -- the untap symbol even if that creature hasn't been controlled by that
  -- player continuously since their most recent turn began."
  --
  -- Act of Treason grants haste for exactly this reason -- the whole point of
  -- the card is that the stolen creature is usable the turn you take it. End
  -- to end through cast and resolution, so the haste is really granted rather
  -- than stipulated.
  Spec.it s "CR 702.10c a hasted stolen Llanowar Elves IS a mana source for the thief" $ do
    mountain <- S.printingOf s registry "Mountain"
    llanowarElves <- S.printingOf s registry "Llanowar Elves"
    actOfTreason <- S.printingOf s registry "Act of Treason"
    let base0 = S.landsInPlay mountain 3
        (elfId, base1) = S.addPermanent llanowarElves S.bob base0
        base = S.runPure S.identityAnswer base1 (Engine.settleAll S.bob)
        (withSpell, spellId) = S.handOne actOfTreason base
        cast = snd (Engine.runGamePure S.identityAnswer withSpell (S.cast S.alice spellId))
        resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "alice controls the Elves" (Projection.controllerOf elfId resolved) (Just S.alice)
    Spec.assertBool s (Projection.hasKeyword Keyword.Haste elfId resolved) "it has haste"
    Spec.assertBool s (elem elfId (Mana.manaSources Cost.manaActivations S.alice resolved)) "so she may tap it for mana this turn"

  -- CR 601.2g / 602.1: WHICH sources to activate is the player's choice, and
  -- pawl's second invariant is that the engine never makes one. A Forest and a
  -- Llanowar Elves both pay {G}, but they are not interchangeable -- tapping
  -- the Elf spends a creature that could otherwise block -- so the choice must
  -- be asked, and the answer must be honoured (#12).
  Spec.it s "CR 601.2g paying {G} with a Forest AND a Llanowar Elves asks which to tap" $ do
    forest <- S.printingOf s registry "Forest"
    llanowarElves <- S.printingOf s registry "Llanowar Elves"
    let base0 = S.landsInPlay forest 1
        (elfId, base1) = S.addPermanent llanowarElves S.alice base0
        gs = S.runPure S.identityAnswer base1 (Engine.settleAll S.alice)
        green = ManaCost.MkManaCost [ManaSymbol.OfType (ManaType.Colored Color.Green)]
        cost = Cost.Type.MkCost (Just green) []
        tappedElf g = fmap Object.tapped (Game.lookupObject elfId g)
    Spec.assertEqWith s "asked to tap the Elf, it is tapped" (tappedElf (S.runPure (prefersSource elfId) gs (Cost.pay S.manaPerformer gs PaymentMoment.OutsideResolution PaymentSubject.ForNeither Nothing ManaSpending.AsProduced S.alice elfId cost))) (Just TapState.Tapped)
    Spec.assertEqWith s "asked to spare the Elf, it is untapped" (tappedElf (S.runPure (avoidsSource elfId) gs (Cost.pay S.manaPerformer gs PaymentMoment.OutsideResolution PaymentSubject.ForNeither Nothing ManaSpending.AsProduced S.alice elfId cost))) (Just TapState.Untapped)

  -- The other half of the invariant: WHEN the window asks, counted directly --
  -- without which an implementation that never asks would still pass the test
  -- above's first half. Nothing is elided any more, so a lone Forest is asked
  -- about too: CR 118.3c makes declining an answer on every board (#218).
  --
  -- The two counters separate CR 118.3c's question from CR 601.2g's. One Forest
  -- pays {G} and then no source is left, so the second window has nothing to
  -- offer; three Forests leave two, so it opens once and is declined.
  Spec.it s "CR 118.3c/601.2g the window asks while short, then asks again once covered" $ do
    forest <- S.printingOf s registry "Forest"
    let green = ManaCost.MkManaCost [ManaSymbol.OfType (ManaType.Colored Color.Green)]
        countingAnswer :: Prompt.Prompt r -> State.State (Int, Int) r
        countingAnswer p = case p of
          Prompt.ChooseManaSource _ _ candidates -> do
            State.modify' (\(short, extra) -> (short + 1, extra))
            pure (Just (NonEmpty.head candidates))
          Prompt.ChooseExtraManaSource {} -> do
            State.modify' (\(short, extra) -> (short, extra + 1))
            pure Nothing
          _ -> pure (S.identityAnswer p)
        promptsFor g = State.execState (Engine.runGame countingAnswer g (Cost.payMana S.manaPerformer PaymentSubject.ForNeither ManaSpending.AsProduced S.alice green)) (0, 0)
    Spec.assertEqWith s "a lone Forest: asked once, and nothing left to float" (promptsFor (S.landsInPlay forest 1)) (1, 0)
    Spec.assertEqWith s "three Forests: asked once short, then offered the float" (promptsFor (S.landsInPlay forest 3)) (1, 1)

  -- FILTERED, NOT TRUSTED: an interpreter naming a source that was not offered
  -- must not be honoured. The fallback is CR 118.3c's refusal rather than the
  -- head candidate, because the alternative is the engine tapping a permanent
  -- nobody named; the payment then fails and CR 601.2h reverses it.
  Spec.it s "CR 601.2g an answer outside the offered set declines, it does not tap" $ do
    forest <- S.printingOf s registry "Forest"
    let green = ManaCost.MkManaCost [ManaSymbol.OfType (ManaType.Colored Color.Green)]
        bogus = ObjectId.MkObjectId 9999
        liar p = case p of
          Prompt.ChooseManaSource {} -> Just bogus
          _ -> S.identityAnswer p
        gs = S.landsInPlay forest 3
        (paid, after) = S.runPureWith liar gs (Cost.payMana S.manaPerformer PaymentSubject.ForNeither ManaSpending.AsProduced S.alice green)
    Spec.assertBool s (not paid) "the cost goes unpaid"
    Spec.assertEqWith s "and no Forest was tapped in its name" (S.tappedCount S.alice after) 0

  -- Paying {0}{G}{U} against three Islands and six Forests was observed to tap
  -- FOUR lands where two suffice, see #1610. The payer is not over-tapping. CR
  -- 601.2g's window asks on every pass and taps exactly what the answer names,
  -- which is the first assertion: name the LAST Island and the LAST Forest --
  -- the two a head-taking payer would never reach -- and those two are the only
  -- lands tapped.
  --
  -- The four are the ANSWERER's, which is the second assertion: the tapped set
  -- equals the set of ids it named, one per pass. Replay.defaultAnswer takes the
  -- head because a Prompt.ChooseManaSource carries object ids, a Decider and
  -- nothing else -- no board, no cost -- so a state-free fallback cannot tell an
  -- Island from a Forest, nor which colour the cost still wants. Three Islands
  -- named ahead of a Forest is a legal, wasteful line of play, and choosing it
  -- for a caller with no player attached is what that function is for.
  --
  -- WHICH lands, never how many: a count cannot tell a payer that spent a source
  -- nobody named from one that did not.
  Spec.it s "CR 601.2g paying {0}{G}{U} off nine lands taps exactly the ones the answer named" $ do
    forest <- S.printingOf s registry "Forest"
    island <- S.printingOf s registry "Island"
    let gs = S.landsFor forest S.alice 6 (S.landsInPlay island 3)
        lands = Game.zoneMembers Zone.Battlefield S.alice gs
        named nm oid = fmap Face.name (Game.faceOf oid gs) == Just (CardName.MkCardName (Text.pack nm))
        cost =
          ManaCost.MkManaCost
            [ ManaSymbol.Generic 0,
              ManaSymbol.OfType (ManaType.Colored Color.Green),
              ManaSymbol.OfType (ManaType.Colored Color.Blue)
            ]
        tappedLands g = Set.fromList (filter (\oid -> fmap Object.tapped (Game.lookupObject oid g) == Just TapState.Tapped) lands)
        -- The FIRST of each name, which is the one the window offers: nine
        -- indistinguishable lands are two candidates, one per name
        -- (Pawl.Engine.Interchangeable.representatives), and an id that is not
        -- on offer reads as declining.
        firstNamed nm = case filter (named nm) lands of
          oid : _ -> oid
          [] -> S.noSource
        theIsland = firstNamed "Island"
        theForest = firstNamed "Forest"
        naming :: Prompt.Prompt r -> r
        naming p = case p of
          Prompt.ChooseManaSource _ _ candidates -> List.find (`elem` NonEmpty.toList candidates) [theForest, theIsland]
          _ -> S.identityAnswer p
        (paid, chosen) = S.runPureWith naming gs (Cost.payMana S.manaPerformer PaymentSubject.ForNeither ManaSpending.AsProduced S.alice cost)
        heading :: Prompt.Prompt r -> State.State (Set.Set ObjectId.ObjectId) r
        heading p = case p of
          Prompt.ChooseManaSource _ _ candidates -> do
            State.modify' (Set.insert (NonEmpty.head candidates))
            pure (Just (NonEmpty.head candidates))
          _ -> pure (S.identityAnswer p)
        ((headPaid, headed), asked) = State.runState (Engine.runGame heading gs (Cost.payMana S.manaPerformer PaymentSubject.ForNeither ManaSpending.AsProduced S.alice cost)) Set.empty
    Spec.assertBool s paid "two lands pay the cost"
    Spec.assertEqWith s "and they are the two that were named" (tappedLands chosen) (Set.fromList [theIsland, theForest])
    Spec.assertBool s headPaid "the head-taking answer pays too"
    Spec.assertEqWith s "tapping every source it named, and no other" (tappedLands headed) asked

  Spec.it s "mana from a controlled permanent goes to its controller, not owner" $ do
    llanowarElves <- S.printingOf s registry "Llanowar Elves"
    let (oid, base) = S.addPermanent llanowarElves S.bob (Setup.emptyGame S.bothPlayers)
        gs0 = S.giveControl oid S.alice base
        after = S.runPure S.identityAnswer gs0 (S.tapForMana oid)
        manaUnitsOf pool = case pool of
          Mana.Type.MkMana units -> units
    Spec.assertBool s (not (null (manaUnitsOf (Game.poolOf S.alice after)))) "alice received a mana unit"
    Spec.assertBool s (null (manaUnitsOf (Game.poolOf S.bob after))) "bob received none"

-- Answers Prompt.ChooseManaYield with the ONE-UNIT yield of `wanted` whenever it
-- is on offer, and defers everything else to S.identityAnswer. The
-- ChooseManaYield sibling of prefersSource: a pair of tests differing only in
-- this colour proves the ANSWER decides what is produced, rather than the order
-- Mana.manaYieldsOf happens to return.
--
-- One unit, because a source whose yield is longer offers it whole (Sol Ring).
-- Overgrown Zealot's second ability both chooses a colour and adds twice, so this
-- answerer does not reach it; Pawl.ManaSourceSpec's zealotPaying hands
-- optionOfTypes the pair instead.
prefersColor :: Color.Color -> Prompt.Prompt r -> r
prefersColor wanted p = case p of
  Prompt.ChooseManaYield _ _ _ candidates -> optionOfTypes [ManaType.Colored wanted] candidates
  _ -> S.identityAnswer p

-- S.optionYielding by mana TYPE rather than by whole unit. CR 106.3's production
-- tags are the engine's to stamp (Pawl.Types.ProductionTag), so an answerer
-- choosing a COLOUR must not have to predict them: the same answerer serves Birds
-- of Paradise and Chromatic Star, and only the second one's units carry
-- ProductionTag.Artifact. S.optionYielding stays the whole-unit matcher, which is
-- what Pawl.ManaSourceSpec's Halfling case needs to tell a restricted unit from
-- an unrestricted one.
optionOfTypes :: [ManaType.ManaType] -> NonEmpty.NonEmpty ManaOption.ManaOption -> ManaOption.ManaOption
optionOfTypes wanted candidates =
  Maybe.fromMaybe
    (NonEmpty.head candidates)
    (List.find ((==) wanted . fmap ManaUnit.manaType . Mana.yieldUnits) (NonEmpty.toList candidates))

-- Alice controls `permanents` and holds `spell`; she casts it and resolves it,
-- with every prompt answered by `answer`.
castOffBoard :: (forall r. Prompt.Prompt r -> r) -> [Printing.Printing] -> Printing.Printing -> GameState.GameState
castOffBoard answer permanents = castFrom answer (alicePermanents permanents)

-- The same two steps off a board the caller has already built, which is what a
-- case wanting alice at some particular life total needs.
castFrom :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> Printing.Printing -> GameState.GameState
castFrom answer board spell =
  let (withSpell, oid) = S.handOne spell board
      afterCast = S.runPure answer withSpell (S.cast S.alice oid)
   in S.runPure answer afterCast Stack.resolveTop

-- The mana Alice's pool holds after tapping `oid` with every prompt answered by
-- `answer` -- the observable that says WHAT a source produced: which type, where
-- it offers several, and how much, where one activation adds more than one.
tappedFor :: (forall r. Prompt.Prompt r -> r) -> ObjectId.ObjectId -> GameState.GameState -> [ManaType.ManaType]
tappedFor answer oid gs = case Game.poolOf S.alice (S.runPure answer gs (S.tapForMana oid)) of
  Mana.Type.MkMana units -> fmap ManaUnit.manaType units

-- A fixture write that untaps one permanent, so a card that entered tapped can
-- be asked what it produces once CR 107.5 no longer refuses its cost. Not the
-- untap step: the turn structure is not what is under test here.
untapObject :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
untapObject oid gs =
  gs
    { GameState.objects = Map.adjust (\o -> o {Object.tapped = TapState.Untapped}) oid (GameState.objects gs)
    }

anyColorSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
anyColorSpec s registry = Spec.describe s "Mana of any color" $ do
  -- CR 105.4: "If a player is asked to choose a color, they must choose one of
  -- the five colors. 'Multicolored' is not a color. Neither is 'colorless.'"
  -- So AnyColor offers exactly five options, and {C} is not among them.
  Spec.it s "CR 105.4 Birds of Paradise offers the five colors and not colorless" $ do
    birds <- S.printingOf s registry "Birds of Paradise"
    let (birdsId, gs) = S.addPermanent birds S.alice (Setup.emptyGame S.bothPlayers)
    Spec.assertEqWith
      s
      "exactly the five colors"
      (Mana.manaTypesOf birdsId gs)
      (fmap ManaType.Colored [Color.White, Color.Blue, Color.Black, Color.Red, Color.Green])
    Spec.assertBool s (elem birdsId (Mana.manaSources Cost.manaActivations S.alice gs)) "it is a mana source"

  -- CR 118.3 exactness. A greedy walk fails this one: it taps the Forest for
  -- {G}, then takes the Birds' FIRST colour (white) and reports {G}{B}
  -- unaffordable. Only a matching over what each source COULD produce gets it
  -- right.
  Spec.it s "CR 118.3 a Forest and a Birds of Paradise can pay {G}{B}" $ do
    forest <- S.printingOf s registry "Forest"
    birds <- S.printingOf s registry "Birds of Paradise"
    let (_, g1) = S.addPermanent forest S.alice (Setup.emptyGame S.bothPlayers)
        (_, gs) = S.addPermanent birds S.alice g1
        cost = ManaCost.MkManaCost [ManaSymbol.OfType (ManaType.Colored Color.Green), ManaSymbol.OfType (ManaType.Colored Color.Black)]
    Spec.assertBool s (Mana.canPay Cost.manaActivations S.alice cost gs) "affordable"

  Spec.it s "CR 118.3 two Birds of Paradise can pay {B}{B}, one cannot" $ do
    birds <- S.printingOf s registry "Birds of Paradise"
    let (_, one) = S.addPermanent birds S.alice (Setup.emptyGame S.bothPlayers)
        (_, two) = S.addPermanent birds S.alice one
        black = ManaSymbol.OfType (ManaType.Colored Color.Black)
        cost = ManaCost.MkManaCost [black, black]
    Spec.assertBool s (Mana.canPay Cost.manaActivations S.alice cost two) "two suffice"
    Spec.assertBool s (not (Mana.canPay Cost.manaActivations S.alice cost one)) "one does not"

  -- The OTHER way to get this wrong, which Hall's condition also rules out:
  -- checking each symbol independently ("is there a source that could make
  -- white?") passes both {W} symbols, because the same Birds answers each
  -- one. Only weighing the whole demand set against the supplies that could
  -- serve it catches that one source cannot make two mana. Two demands, three
  -- sources, plenty of mana, still unpayable.
  Spec.it s "CR 118.3 a Birds and two Forests cannot pay {W}{W}" $ do
    birds <- S.printingOf s registry "Birds of Paradise"
    forest <- S.printingOf s registry "Forest"
    let (_, g1) = S.addPermanent birds S.alice (Setup.emptyGame S.bothPlayers)
        (_, g2) = S.addPermanent forest S.alice g1
        (_, gs) = S.addPermanent forest S.alice g2
        white = ManaSymbol.OfType (ManaType.Colored Color.White)
    Spec.assertBool s (not (Mana.canPay Cost.manaActivations S.alice (ManaCost.MkManaCost [white, white]) gs)) "only one white source"
    Spec.assertBool s (Mana.canPay Cost.manaActivations S.alice (ManaCost.MkManaCost [white, ManaSymbol.Generic 2]) gs) "but one {W} plus {2} is fine"

  -- Not only "any color": a source with two BASIC LAND TYPES has been a real
  -- choice in this pool since Urborg landed, and tapForMana was silently
  -- taking the first. Both directions, so the answer is proven to decide.
  Spec.it s "CR 305.6/305.7 an Urborg'd Mountain's controller chooses red or black" $ do
    mountain <- S.printingOf s registry "Mountain"
    urborg <- S.printingOf s registry "Urborg, Tomb of Yawgmoth"
    let base = Setup.emptyGame S.bothPlayers
        (mountainId, g1) = S.addPermanent mountain S.alice base
        (_, gs) = S.addPermanent urborg S.alice g1
    Spec.assertEqWith s "choosing black" (tappedFor (prefersColor Color.Black) mountainId gs) [ManaType.Colored Color.Black]
    Spec.assertEqWith s "choosing red" (tappedFor (prefersColor Color.Red) mountainId gs) [ManaType.Colored Color.Red]

  -- The elision side of the invariant: where the rules leave nothing to ask,
  -- do not ask. A Forest offers one yield, so no ChooseManaYield is raised.
  Spec.it s "CR 605 a single-yield source is not asked what to produce" $ do
    forest <- S.printingOf s registry "Forest"
    birds <- S.printingOf s registry "Birds of Paradise"
    let countingAnswer :: Prompt.Prompt r -> State.State Int r
        countingAnswer p = case p of
          Prompt.ChooseManaYield {} -> do
            State.modify (+ 1)
            pure (S.identityAnswer p)
          _ -> pure (S.identityAnswer p)
        asks printing =
          let (oid, gs) = S.addPermanent printing S.alice (Setup.emptyGame S.bothPlayers)
           in State.execState (Engine.runGame countingAnswer gs (S.tapForMana oid)) 0
    Spec.assertEqWith s "a Forest: nothing to ask" (asks forest) 0
    Spec.assertEqWith s "a Birds of Paradise: one real decision" (asks birds) 1

-- Alice controls one Forest and each printing in `alices`; bob controls one
-- Forest of his own. Both Forests are tapped for mana, so each player's pool
-- holds one unspent {G} and NOTHING has been spent -- the CR 106.4 "unspent
-- mana" the step end would take away.
--
-- Both seats float, because the symmetry is the assertion: Upwelling's scope is
-- CR 613.11 EachPlayer, and a You-scoped implementation keeps only alice's.
--
-- Returns the ids of the `alices` printings, in the order given, so a caller can
-- destroy one without hunting the battlefield for it by name.
floatedPools :: [Printing.Printing] -> Printing.Printing -> ([ObjectId.ObjectId], GameState.GameState)
floatedPools alices forest =
  let addOne (ids, gs) printing =
        let (oid, gs1) = S.addPermanent printing S.alice gs
         in (ids <> [oid], gs1)
      (extras, withAlices) = List.foldl' addOne ([], Setup.emptyGame S.bothPlayers) alices
      (aliceForest, g1) = S.addPermanent forest S.alice withAlices
      (bobForest, g2) = S.addPermanent forest S.bob g1
      g3 = S.runPure S.identityAnswer g2 (S.tapForMana aliceForest)
   in (extras, S.runPure S.identityAnswer g3 (S.tapForMana bobForest))

-- CR 500.5: "As a step or phase ends ... any unspent mana left in a player's
-- mana pool empties. This is a turn-based action that doesn't use the stack (see
-- rule 703.4q)." CR 106.4 says it from the mana side and supplies the wording
-- modern Oracle text uses: "the player is said to lose this mana."
--
-- Upwelling ({3}{G} Enchantment, "Players don't lose unspent mana as steps and
-- phases end.") is the card that stops it, and it stops it for EVERY player.
upwellingSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
upwellingSpec s registry = Spec.describe s "Upwelling" $ do
  -- The control. Same board, same float, no Upwelling: both pools go.
  Spec.it s "CR 500.5 without Upwelling both players lose their unspent mana" $ do
    forest <- S.printingOf s registry "Forest"
    let (_, floated) = floatedPools [] forest
        ended = Mana.emptiedManaPools floated
    Spec.assertEqWith s "alice floated one" (poolSize S.alice floated) 1
    Spec.assertEqWith s "bob floated one" (poolSize S.bob floated) 1
    Spec.assertEqWith s "alice lost it" (poolSize S.alice ended) 0
    Spec.assertEqWith s "bob lost it" (poolSize S.bob ended) 0

  Spec.it s "CR 613.11 Upwelling keeps its controller's unspent mana" $ do
    forest <- S.printingOf s registry "Forest"
    upwelling <- S.printingOf s registry "Upwelling"
    let (_, floated) = floatedPools [upwelling] forest
    Spec.assertEqWith s "alice kept it" (poolSize S.alice (Mana.emptiedManaPools floated)) 1

  -- The discriminating half of the scope. Alice controls the Upwelling and
  -- bob keeps his mana anyway -- that is what PlayerScope.EachPlayer means,
  -- and a You-scoped implementation passes the test above and fails this one.
  Spec.it s "CR 613.11 Upwelling is symmetric: an opponent's unspent mana is kept too" $ do
    forest <- S.printingOf s registry "Forest"
    upwelling <- S.printingOf s registry "Upwelling"
    let (_, floated) = floatedPools [upwelling] forest
    Spec.assertEqWith s "bob kept it, though alice controls the Upwelling" (poolSize S.bob (Mana.emptiedManaPools floated)) 1

  -- CR 604.2: a static ability's continuous effect is active only while its
  -- permanent "remains on the battlefield and has the ability". The effect is
  -- read LIVE at the moment the pools empty, so destroying the Upwelling in
  -- the same step restores the emptying with nothing to unwind.
  Spec.it s "CR 604.2 destroying Upwelling restores the emptying in the same step" $ do
    forest <- S.printingOf s registry "Forest"
    upwelling <- S.printingOf s registry "Upwelling"
    let (extras, floated) = floatedPools [upwelling] forest
    case extras of
      [] -> Spec.assertFailure s "fixture should have an Upwelling on the battlefield"
      oid : _ -> do
        let gone = S.runPure S.identityAnswer floated (Event.destroy Regenerability.Regenerable [oid])
        Spec.assertEqWith s "kept while it stands" (poolSize S.alice (Mana.emptiedManaPools floated)) 1
        Spec.assertEqWith s "alice loses it once it is gone" (poolSize S.alice (Mana.emptiedManaPools gone)) 0
        Spec.assertEqWith s "and so does bob" (poolSize S.bob (Mana.emptiedManaPools gone)) 0

  -- The gameplay-level proof (design.md section 4), end to end through
  -- Engine.runStep. Alice taps a Birds of Paradise for BLUE toward a green
  -- spell. CR 105.4 makes that colour HER choice, and blue cannot pay {G}, so
  -- the Forest is tapped as well and the {U} is genuinely unspent when the
  -- precombat main phase ends -- CR 106.4's "unspent mana", reached by
  -- playing rather than by writing a pool into a fixture. Upwelling keeps it,
  -- and it pays for an Unsummon in the upkeep step that follows.
  Spec.it s "CR 500.5 whole card: mana Upwelling keeps across a step boundary pays for Unsummon" $ do
    forest <- S.printingOf s registry "Forest"
    birds <- S.printingOf s registry "Birds of Paradise"
    upwelling <- S.printingOf s registry "Upwelling"
    llanowarElves <- S.printingOf s registry "Llanowar Elves"
    unsummon <- S.printingOf s registry "Unsummon"
    let base = Setup.emptyGame S.bothPlayers
        (_, g1) = S.addPermanent upwelling S.alice base
        (birdsId, g2) = S.addPermanent birds S.alice g1
        (_, g3) = S.addPermanent forest S.alice g2
        (withElves, elvesId) = S.handOne llanowarElves g3
        (unsummonId, board) = S.addHandCard unsummon S.alice withElves
        -- Prefer the Birds and take blue from it: it cannot pay {G}, so the
        -- Forest is tapped next and the {U} is left over.
        floatBlue :: Prompt.Prompt r -> r
        floatBlue p = case p of
          Prompt.ChooseManaSource _ _ candidates ->
            Just (if elem birdsId (NonEmpty.toList candidates) then birdsId else NonEmpty.head candidates)
          _ -> prefersColor Color.Blue p
        cast = S.runPure floatBlue board (S.cast S.alice elvesId)
        afterStep = S.runPure S.identityAnswer cast Engine.runStep
    Spec.assertEqWith s "the Elves are cast off the Forest, floating the Birds' blue" (poolSize S.alice cast) 1
    Spec.assertEqWith s "both sources tapped" (S.tappedCount S.alice afterStep) 2
    Spec.assertEqWith s "the float survived the end of the precombat main phase" (poolSize S.alice afterStep) 1
    Spec.assertEqWith s "Unsummon is still in hand" (Game.zoneMembers Zone.Hand S.alice afterStep) [unsummonId]
    let spent = S.runPure S.identityAnswer afterStep (S.cast S.alice unsummonId)
    Spec.assertEqWith s "the retained {U} paid for it" (poolSize S.alice spent) 0
    Spec.assertEqWith s "and nothing new was tapped" (S.tappedCount S.alice spent) 2
    Spec.assertEqWith s "Unsummon is on the stack" (length (GameState.stack spent)) 1

-- CR 500.5 / 106.4 again, one granularity up. Omnath, Locus of Mana ({2}{G}
-- Legendary Creature -- Elemental) says "You don't lose unspent green mana as
-- steps and phases end", which differs from Upwelling on both axes the carrier
-- has: it names a MANA TYPE (CR 106.1a), so the rest of the pool still empties,
-- and its CR 613.11 scope is PlayerScope.You, so an opponent's pool still
-- empties. Each axis has its own falsifier below. Oracle text verified against
-- Scryfall.
--
-- The other half of the card -- "Omnath gets +1/+1 for each unspent green mana
-- you have" -- is Pawl.PowerToughnessSpec's, since the module under test there
-- is the projection.
omnathSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
omnathSpec s registry = Spec.describe s "Omnath, Locus of Mana" $ do
  -- The type axis. Alice floats one green and one blue; only the green is hers
  -- to keep. A whole-pool retention keeps both and fails the second assertion.
  Spec.it s "CR 106.1a Omnath keeps the green mana and loses the rest" $ do
    forest <- S.printingOf s registry "Forest"
    island <- S.printingOf s registry "Island"
    omnath <- S.printingOf s registry "Omnath, Locus of Mana"
    let (_, g1) = S.addPermanent omnath S.alice (Setup.emptyGame S.bothPlayers)
        (forestId, g2) = S.addPermanent forest S.alice g1
        (islandId, g3) = S.addPermanent island S.alice g2
        floated = S.runPure S.identityAnswer (S.runPure S.identityAnswer g3 (S.tapForMana forestId)) (S.tapForMana islandId)
        ended = Mana.emptiedManaPools floated
    Spec.assertEqWith s "two floated" (poolSize S.alice floated) 2
    Spec.assertEqWith s "one survives the step's end" (poolSize S.alice ended) 1
    Spec.assertEqWith
      s
      "and it is the green one"
      (fmap ManaUnit.manaType (poolUnits ended))
      [ManaType.Colored Color.Green]

  -- The scope axis, and the mirror image of Upwelling's symmetry test above:
  -- alice controls the Omnath and BOB's green still empties, because CR 613.11's
  -- carrier here is PlayerScope.You. An EachPlayer implementation passes the
  -- test above and fails this one.
  Spec.it s "CR 613.11 Omnath is You-scoped: an opponent's green mana still empties" $ do
    forest <- S.printingOf s registry "Forest"
    omnath <- S.printingOf s registry "Omnath, Locus of Mana"
    let (_, g1) = S.addPermanent omnath S.alice (Setup.emptyGame S.bothPlayers)
        (alicesForest, g2) = S.addPermanent forest S.alice g1
        (bobsForest, g3) = S.addPermanent forest S.bob g2
        floated = S.runPure S.identityAnswer (S.runPure S.identityAnswer g3 (S.tapForMana alicesForest)) (S.tapForMana bobsForest)
        ended = Mana.emptiedManaPools floated
    Spec.assertEqWith s "alice keeps hers" (poolSize S.alice ended) 1
    Spec.assertEqWith s "bob loses his" (poolSize S.bob ended) 0

  -- The gameplay-level proof (design.md section 4) that the two halves of the
  -- card are one card, end to end through Engine.runStep: the green mana Omnath
  -- keeps across CR 500.5's turn-based action is the same mana Omnath's own
  -- layer-7c pump is still counting on the other side of the boundary, while the
  -- blue that emptied stops counting for the payment that follows.
  Spec.it s "CR 500.5/613.4c whole card: the green Omnath keeps is the green that keeps it big" $ do
    forest <- S.printingOf s registry "Forest"
    island <- S.printingOf s registry "Island"
    omnath <- S.printingOf s registry "Omnath, Locus of Mana"
    let (omnathId, g1) = S.addPermanent omnath S.alice (Setup.emptyGame S.bothPlayers)
        (forestId, g2) = S.addPermanent forest S.alice g1
        (islandId, g3) = S.addPermanent island S.alice g2
        floated = S.runPure S.identityAnswer (S.runPure S.identityAnswer g3 (S.tapForMana forestId)) (S.tapForMana islandId)
        afterStep = S.runPure S.identityAnswer floated Engine.runStep
    Spec.assertEqWith s "before: two floating, and only the green pumps" (Projection.powerOf omnathId floated) (Just 2)
    Spec.assertEqWith s "the blue is gone" (poolSize S.alice afterStep) 1
    Spec.assertEqWith s "the green survived the step boundary" (Projection.powerOf omnathId afterStep) (Just 2)
    Spec.assertEqWith s "and the toughness with it" (Projection.toughnessOf omnathId afterStep) (Just 2)

  -- CR 605.3a's permission to activate a mana ability while casting is not
  -- rationed by what the cost needs, so a player may tap past it. Omnath is why
  -- they would: floating green is what makes it big, so an engine that stops the
  -- moment the cost is covered has made the creature smaller on its controller's
  -- behalf (#218).
  --
  -- Four Forests, a {1}{G} Blurred Mongoose, and the same board answered twice.
  -- Every number differs from every other, so no two readings of the rule land
  -- on the same one.
  Spec.it s "CR 605.3a a player may tap more sources than the cost needs, and Omnath reads the float" $ do
    forest <- S.printingOf s registry "Forest"
    omnath <- S.printingOf s registry "Omnath, Locus of Mana"
    mongoose <- S.printingOf s registry "Blurred Mongoose"
    let (omnathId, g1) = S.addPermanent omnath S.alice (Setup.emptyGame S.bothPlayers)
        withForests = List.foldl' (\g _ -> snd (S.addPermanent forest S.alice g)) g1 [1 .. 4 :: Int]
        (board, mongooseId) = S.handOne mongoose withForests
        castWith :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState
        castWith answer = S.runPure answer board (S.cast S.alice mongooseId)
        floats = castWith floatsEverything
        stops = castWith S.identityAnswer
    Spec.assertEqWith s "floating: all four Forests tapped" (S.tappedCount S.alice floats) 4
    Spec.assertEqWith s "floating: two green left over" (poolSize S.alice floats) 2
    Spec.assertEqWith s "floating: and Omnath counts them" (Projection.powerOf omnathId floats) (Just 3)
    Spec.assertEqWith s "declining the float: only what the cost needed" (S.tappedCount S.alice stops) 2
    Spec.assertEqWith s "declining the float: nothing left over" (poolSize S.alice stops) 0
    Spec.assertEqWith s "declining the float: so Omnath is its printed size" (Projection.powerOf omnathId stops) (Just 1)

-- Says yes to the float window as long as anything is left to tap, and
-- answers everything else as S.identityAnswer does -- so the only difference
-- between a game run under this and one run under S.identityAnswer is how much
-- mana was floated.
floatsEverything :: Prompt.Prompt r -> r
floatsEverything p = case p of
  Prompt.ChooseExtraManaSource _ _ candidates -> Just (NonEmpty.head candidates)
  _ -> S.identityAnswer p

-- CR 605.3a's FIRST window: "a player may activate an activated mana ability
-- whenever they have priority", with no cost in flight at all. Its other two
-- windows -- casting or activating something that needs a mana payment, and a
-- rule or effect asking for one -- are Cost.payMana's and are covered above; the
-- difference is only whether a payment is waiting on the mana.
--
-- Offered as Action.ActivateManaAbility and taken in Engine.playGame's priority
-- loop, so the whole proof here is at gameplay level. Omnath, Locus of Mana is
-- what makes the pool VISIBLE on a board: its layer-7c pump counts the unspent
-- green mana its controller has, so three Forests tapped for nothing at all read
-- off the creature as 4/4.
priorityWindowSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
priorityWindowSpec s registry = Spec.describe s "CR 605.3a the priority window" $ do
  -- The offer itself, and its one gate. Mana.manaSources is what
  -- Action.legalActions filters on, so a source already tapped for the turn
  -- drops off the menu -- which is also what stops the loop above from being
  -- infinite.
  Spec.it s "CR 605.3a the menu carries one activation per untapped source" $ do
    forest <- S.printingOf s registry "Forest"
    omnath <- S.printingOf s registry "Omnath, Locus of Mana"
    let (_, board) = priorityWindowBoard omnath forest
        tappedOne = case Mana.manaSources Cost.manaActivations S.alice board of
          oid : _ -> S.tapObject oid board
          [] -> board
        offers gs = filter isManaActivation (Action.legalActions S.alice gs)
    Spec.assertEqWith s "one per Forest, and none for the Omnath" (length (offers board)) 3
    Spec.assertEqWith s "tapping one takes it off the menu" (length (offers tappedOne)) 2

-- alice, active, in her precombat main phase with an empty hand and an empty
-- stack: one Omnath and three Forests, so the only mana that can ever reach her
-- pool is mana she activated a mana ability for while holding priority.
priorityWindowBoard :: Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, GameState.GameState)
priorityWindowBoard omnath forest =
  let (omnathId, g1) = S.addPermanent omnath S.alice (Setup.emptyGame S.bothPlayers)
      board = foldr (\_ gs -> snd (S.addPermanent forest S.alice gs)) g1 [1 :: Int, 2, 3]
   in (omnathId, board {GameState.phase = Phase.PrecombatMain, GameState.remaining = Seq.empty})

-- CR 605.3a's two windows, gated by the "activate only ..." rider CR 602.5 makes
-- a prohibition. Synthetic Ember Spring is the producer -- "{T}: Add {R}.
-- Activate only during your upkeep", CR 500.1's window under CR 102.1's scope --
-- and CR 605.1's own sentence is why a rider leaves it a mana ability: an
-- ability is one "regardless of ... what timing restrictions ... they may have".
--
-- SYNTHETIC because no printing reaches CR 500.1's window on the mana path, not
-- because none prints a rider there. Scryfall `o:"Activate only" o:"Add {"`,
-- 2026-08-21: Grinning Ignus is the one printing whose mana ability names a
-- STEP-or-phase window this vocabulary can say, and what it says is CR 307.5's
-- "activate only as a sorcery" rather than a step -- grinningIgnusSpec below is
-- where that card is exercised. Lavinia, Foil to Conspiracy is in the pool and
-- gates these same two windows, but through CR 102.1's turn axis alone
-- (laviniaTurnRiderSpec below), so she leaves the phase axis unexercised. Vivi
-- Ornitier and every other hit ride on "only once each turn" -- which is
-- ActivationRestriction.OnlyOnceEachTurn and names no window either
-- (data/scenarios/mana is where that arm is exercised on this road) -- or on
-- "only if <condition>", which is its OnlyIf arm and names no window either
-- (nimbusMazeSpec below is where that arm is exercised on this road), so neither
-- kind reaches the phase axis this pair is about.
--
-- The two cases below are the SAME board at two moments, and the phase is the
-- one thing that differs. That is what makes the pair a proof about the rider
-- rather than about the fixture -- and the phase, unlike CR 307.5's sorcery
-- window, cannot change under a caster's feet between the offer and the payment
-- (CR 500.12). SorcerySpeed is the arm that CAN, since CR 601.2a's and CR
-- 602.2a's move puts an object on the stack between the two; the gates ask
-- Pawl.Engine.ActivationRestriction.needsEmptyStack about that arm rather than
-- reading the stack of the wrong moment, and grinningIgnusSpec below is where
-- both roads are proved.
riderWindowSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
riderWindowSpec s registry = Spec.describe s "CR 605.3a a printed rider gates both windows" $ do
  -- CR 605.3a's other window: the same rider asked while a payment is in flight,
  -- reached through Cast.castSpell rather than through the action menu, so
  -- neither assertion here can be passing on the offer above.
  Spec.it s "CR 605.3a the payment window refuses the same source outside the rider's step" $ do
    spring <- S.printingOf s registry "Synthetic Ember Spring"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let (inUpkeep, boltId) = springBoard spring bolt
        inMain = inUpkeep {GameState.phase = Phase.PrecombatMain}
        afterCast gs = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice boltId))
        inHand gs = elem boltId (Game.zoneMembers Zone.Hand S.alice gs)
    Spec.assertBool s (not (inHand (afterCast inUpkeep))) "CR 601.2a inside the window the Bolt is cast, leaving her hand"
    Spec.assertBool s (inHand (afterCast inMain)) "CR 601.2h outside it the payment fails and the Bolt stays in her hand"
    Spec.assertEqWith s "the Spring paid inside the window" (S.tappedCount S.alice (afterCast inUpkeep)) 1
    Spec.assertEqWith s "and paid nothing outside it" (S.tappedCount S.alice (afterCast inMain)) 0
    Spec.assertEqWith s "nothing reached the stack outside it" (length (GameState.stack (afterCast inMain))) 0
    -- The OFFER, asked of the same two boards: a cast the payment window cannot
    -- pay for is one the gate does not offer either. Both windows read
    -- Cost.manaActivations, so a divergence here would be a divergence in one
    -- predicate.
    Spec.assertEqWith s "CR 118.3 the cast gate agrees with the payment at both moments" (fmap (S.castable S.alice boltId) [inUpkeep, inMain]) [True, False]

-- alice, active, with one Synthetic Ember Spring untapped and one Lightning Bolt
-- in hand, in her upkeep and going nowhere -- an empty `remaining` pins the
-- phase, so the loop above cannot wander into a step the rider admits.
springBoard :: Printing.Printing -> Printing.Printing -> (GameState.GameState, ObjectId.ObjectId)
springBoard spring bolt =
  let (gs, oid) = S.boltInHand spring bolt 1 (Phase.Beginning BeginningStep.Upkeep)
   in (gs {GameState.remaining = Seq.empty}, oid)

-- CR 102.1's axis with no CR 500.1 window beside it:
-- ActivationRestriction.DuringTurn, the arm a rider naming a turn and no phase
-- needs. Lavinia, Foil to Conspiracy is the producer -- "{T}: Add {C}{C}.
-- Activate only during an opponent's turn" -- and her ability being a MANA
-- ability is why this group sits beside riderWindowSpec above rather than in
-- Pawl.ActivateSpec: CR 605.3b keeps it off the stack, so both of CR 605.3a's
-- windows are where the rider is asked.
--
-- THREE SEATS, and both opponents exercised. At two seats TurnScope.OpponentsTurn
-- and "not the controller's turn" are the same predicate (CR 102.2); at three
-- they are still the same predicate but the board can now tell an enumeration of
-- ONE opponent from "every seat that is not yours" (CR 806.1), which is what
-- carol's turn asserts.
--
-- The Withered Wretch below is the payment window's door: "{1}: Exile target
-- card from a graveyard" is activatable whenever its controller has priority, so
-- the SAME activation is legal on all three turns and Lavinia's rider is the one
-- thing that changes whether it can be paid for. A spell would not do: CR 307.1
-- keeps a sorcery off an opponent's turn, and walking data/cards/ on 2026-08-21
-- for a single-faced Instant whose printed cost is generic-only -- the only
-- shape two colorless mana pay -- turned up none, Lightning Bolt's {R} being
-- the shape every instant in the corpus has. Any generic-only instant added
-- later would serve as the door instead.
laviniaTurnRiderSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
laviniaTurnRiderSpec s registry = Spec.describe s "CR 102.1 a rider naming a turn and no phase" $ do
  -- CR 605.3a's other window, reached through Activate.activateAbility rather
  -- than the action menu, so neither assertion here can be passing on the offer
  -- above. Only Lavinia can pay the Wretch's {1}, so the payment is her rider's
  -- answer and nothing else's.
  Spec.it s "CR 605.3a the payment window pays a Wretch's cost from her on either opponent's turn" $ do
    lavinia <- S.printingOf s registry "Lavinia, Foil to Conspiracy"
    wretch <- S.printingOf s registry "Withered Wretch"
    piker <- S.printingOf s registry "Goblin Piker"
    let ability = theAbility wretch
        activated active =
          let (wretchId, gs) = laviniaBoard lavinia wretch piker active
           in S.runPure S.identityAnswer gs (Activate.activateAbility S.alice wretchId ability)
        onStack active = length (GameState.stack (activated active))
        paid active = S.tappedCount S.alice (activated active)
        gate active =
          let (wretchId, gs) = laviniaBoard lavinia wretch piker active
           in Activatable.activatable S.alice wretchId ability gs
    -- The gameplay-level assertion: CR 602.2a's activation happened, which it
    -- cannot without CR 601.2h's payment.
    Spec.assertEqWith s "CR 602.2a the Wretch's ability reaches the stack on both opponents' turns and not on hers" (fmap onStack [S.bob, S.carol, S.alice]) [1, 1, 0]
    Spec.assertEqWith s "CR 601.2h and Lavinia is what paid for it" (fmap paid [S.bob, S.carol, S.alice]) [1, 1, 0]
    -- The gate the menu reads, on the same three boards: a payment window that
    -- disagreed with the offer would show up as these two lists disagreeing.
    Spec.assertEqWith s "CR 118.3 the offer gate agrees with the payment at all three seats" (fmap gate [S.bob, S.carol, S.alice]) [True, True, False]

-- alice, at three seats, controlling one Lavinia and one Withered Wretch, with a
-- Goblin Piker in bob's graveyard for the Wretch's ability to aim at. `active` is
-- whose turn it is and is the ONE thing the three boards differ in; an empty
-- `remaining` pins the phase, so a priority loop cannot wander out of the turn
-- the rider is being asked about. Both permanents arrive Settled and untapped
-- (S.addPermanent), which is the precondition CR 302.6 and CR 107.5 put on
-- Lavinia's {T}.
laviniaBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> PlayerId.PlayerId -> (ObjectId.ObjectId, GameState.GameState)
laviniaBoard lavinia wretch piker active =
  let (_, g1) = S.addPermanent lavinia S.alice S.threePlayerGame
      (wretchId, g2) = S.addPermanent wretch S.alice g1
      (_, g3) = S.addGraveyardCard piker S.bob g2
   in (wretchId, g3 {GameState.activePlayer = active, GameState.phase = Phase.PrecombatMain, GameState.remaining = Seq.empty})

-- CR 602.5's board condition on the MANA path, which nothing in `data/cards/`
-- reached before Nimbus Maze: "{T}: Add {W}. Activate only if you control an
-- Island", beside an unridden "{T}: Add {C}" and a second ridden route on the
-- same land. Barbarian Ring carries the arm on an ability CR 605.1a makes no
-- mana ability, so ActivationRestriction.OnlyIf was asked at CR 605.3a's two
-- windows by no card at all.
--
-- TWO BOARDS one SUBTYPE apart -- an Island against a Swamp, alice's and TAPPED
-- on both, so the Maze is the only untapped source either board has and the
-- refusal is the rider's rather than the supply's. A tapped land still answers
-- CR 602.5's condition, which asks what she CONTROLS. Luminesce's printed cost
-- is exactly {W}, so the Maze's own {C} route -- live on both boards -- pays
-- nothing here.
--
-- NEITHER condition is one CR 601.2a's move can change: a card leaving a hand
-- for the stack changes no permanent alice controls. data/scenarios/mana holds
-- the board where it does.
nimbusMazeSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
nimbusMazeSpec s registry = Spec.describe s "Nimbus Maze" $ do
  Spec.it s "CR 602.5 a board condition gates the ridden mana route at both of CR 605.3a's windows" $ do
    maze <- S.printingOf s registry "Nimbus Maze"
    luminesce <- S.printingOf s registry "Luminesce"
    island <- S.printingOf s registry "Island"
    swamp <- S.printingOf s registry "Swamp"
    let board other =
          let (otherId, withOther) = S.addPermanent other S.alice (S.landsInPlay maze 1)
           in S.handOne luminesce ((S.tapObject otherId withOther) {GameState.phase = Phase.PrecombatMain, GameState.remaining = Seq.empty})
        stillInHand other =
          let (gs, oid) = board other
           in elem oid (Game.zoneMembers Zone.Hand S.alice (snd (Engine.runGamePure paysWithWhite gs (S.cast S.alice oid))))
        gate other = let (gs, oid) = board other in S.castable S.alice oid gs
    -- The gameplay-level assertions: CR 601.2h's payment found the {W} on the one
    -- board whose rider admits the route.
    Spec.assertBool s (not (stillInHand island)) "CR 601.2a controlling an Island the rider admits the {W} and Luminesce leaves her hand"
    Spec.assertBool s (stillInHand swamp) "CR 602.5 with a Swamp there instead the payment finds no {W} and it stays"
    -- The offer gate on the same two boards: a window that disagreed with the
    -- payment would show up as this list disagreeing with the pair above.
    Spec.assertEqWith s "CR 118.3 the cast gate agrees with the payment on both boards" (fmap gate [island, swamp]) [True, False]

-- CR 601.2g's two questions, asked while Luminesce's {W} is being paid: take the
-- source offered -- the Maze is the board's only untapped one -- and tap it for
-- the white yield. S.identityAnswer DECLINES a Prompt.ChooseManaSource, which
-- makes every board fail to pay and the negative above pass vacuously.
-- S.optionYielding falls back to the head of the offer, so a board that offers no
-- {W} pays nothing rather than being repaired here.
paysWithWhite :: Prompt.Prompt r -> r
paysWithWhite p = case p of
  Prompt.ChooseManaSource _ _ candidates -> Just (NonEmpty.head candidates)
  Prompt.ChooseManaYield _ _ _ candidates ->
    S.optionYielding
      (Mana.Type.MkMana [ManaUnit.MkManaUnit {ManaUnit.manaType = ManaType.Colored Color.White, ManaUnit.tags = Set.empty, ManaUnit.retention = ManaRetention.Ordinary, ManaUnit.restriction = Nothing, ManaUnit.rider = Nothing, ManaUnit.spendTrigger = Nothing, ManaUnit.sourceChosenSubtype = Nothing, ManaUnit.sourceLastExiled = Nothing}])
      candidates
  _ -> S.identityAnswer p

isManaActivation :: Action.Type.Action -> Bool
isManaActivation action = case action of
  Action.Type.ActivateManaAbility _ -> True
  Action.Type.Activate _ _ -> False
  Action.Type.Cast {} -> False
  Action.Type.Play {} -> False
  Action.Type.TurnFaceUp {} -> False
  Action.Type.Unlock _ _ -> False
  Action.Type.DiscardFromHand _ -> False
  Action.Type.Plot {} -> False
  Action.Type.Foretell _ -> False
  Action.Type.Suspend _ -> False
  Action.Type.PutCompanionIntoHand -> False
  Action.Type.RollPlanarDie -> False
  Action.Type.Ignore _ _ -> False
  Action.Type.EndEffect _ -> False
  Action.Type.Pass -> False

-- Activates a mana ability whenever one is offered, and passes once none is.
tapEverything :: Prompt.Prompt r -> r
tapEverything p = case p of
  Prompt.ChooseAction _ _ actions -> case filter isManaActivation actions of
    h : _ -> h
    [] -> Action.Type.Pass
  _ -> S.identityAnswer p

-- CR 605.3b: one activation of one mana ability, adding TWO mana. Sol Ring ({1}
-- Artifact, "{T}: Add {C}{C}") is the pool's first source whose yield is not one
-- unit, and it is what separates "the types this source could produce" from "the
-- mana this source produces when it is tapped".
solRingSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
solRingSpec s registry = Spec.describe s "Sol Ring" $ do
  -- The unit fact. A mode holding two AddMana effects is ONE activation
  -- yielding two mana, not a choice between two singles.
  Spec.it s "CR 605 tapping Sol Ring adds two colorless mana, not one" $ do
    solRing <- S.printingOf s registry "Sol Ring"
    let (solRingId, gs) = S.addPermanent solRing S.alice (Setup.emptyGame S.bothPlayers)
    Spec.assertEqWith
      s
      "two units of {C}"
      (tappedFor S.identityAnswer solRingId gs)
      [ManaType.Colorless, ManaType.Colorless]

  -- The payability half, which reads the same yield through a different door:
  -- CR 118.3 counts an untapped source as the mana it could make, so one Sol
  -- Ring is two supplies and pays {2} by itself.
  Spec.it s "CR 118.3 a lone Sol Ring pays {2} by itself" $ do
    solRing <- S.printingOf s registry "Sol Ring"
    let (_, gs) = S.addPermanent solRing S.alice (Setup.emptyGame S.bothPlayers)
    Spec.assertBool s (Mana.canPay Cost.manaActivations S.alice (ManaCost.MkManaCost [ManaSymbol.Generic 2]) gs) "{2} is affordable"
    Spec.assertBool s (not (Mana.canPay Cost.manaActivations S.alice (ManaCost.MkManaCost [ManaSymbol.Generic 3]) gs)) "{3} is not"

  -- Both supplies a Sol Ring contributes are COLORLESS, so they swell the
  -- generic count and serve no typed demand. Discriminating against a supply
  -- model that merely counted a source twice without keeping its types: that
  -- one passes the first assertion and fails the second.
  Spec.it s "CR 118.3 a Sol Ring and a Mountain pay {2}{R}, but not {R}{R}" $ do
    solRing <- S.printingOf s registry "Sol Ring"
    mountain <- S.printingOf s registry "Mountain"
    let (_, g1) = S.addPermanent solRing S.alice (Setup.emptyGame S.bothPlayers)
        (_, gs) = S.addPermanent mountain S.alice g1
        red = ManaSymbol.OfType (ManaType.Colored Color.Red)
    Spec.assertBool s (Mana.canPay Cost.manaActivations S.alice (ManaCost.MkManaCost [ManaSymbol.Generic 2, red]) gs) "{2}{R} is affordable"
    Spec.assertBool s (not (Mana.canPay Cost.manaActivations S.alice (ManaCost.MkManaCost [red, red]) gs)) "{R}{R} is not"

  -- The elision side of the invariant: Sol Ring offers exactly one yield, so
  -- there is nothing to ask -- and NOT because its two mana are the same
  -- type, which would be the engine choosing. "CR 605 a single-yield source
  -- is not asked what to produce" above is the counterpart that keeps a real
  -- choice asked.
  Spec.it s "CR 605 Sol Ring is not asked what to produce" $ do
    solRing <- S.printingOf s registry "Sol Ring"
    let countingAnswer :: Prompt.Prompt r -> State.State Int r
        countingAnswer p = case p of
          Prompt.ChooseManaYield {} -> do
            State.modify' (+ 1)
            pure (S.identityAnswer p)
          _ -> pure (S.identityAnswer p)
        (solRingId, gs) = S.addPermanent solRing S.alice (Setup.emptyGame S.bothPlayers)
    Spec.assertEqWith s "nothing to ask" (State.execState (Engine.runGame countingAnswer gs (S.tapForMana solRingId)) 0) 0

-- CR 405.6c: "mana abilities resolve immediately. If a mana ability both
-- produces mana and has another effect, the mana is produced and the other
-- effect happens immediately." Ancient Tomb ("{T}: Add {C}{C}. This land deals
-- 2 damage to you") is the pool's first mana ability with a clause beyond its
-- mana, and CR 605.3b is what makes that clause the payment path's business
-- rather than a resolution's: the ability never goes on the stack, so nothing
-- above Pawl.Engine.Cost would run it. Pawl.Engine.Resolve.Effect.performManaAbility is
-- the executor the payment path is handed for it.
ancientTombSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
ancientTombSpec s registry = Spec.describe s "Ancient Tomb" $ do
  -- The IMMEDIATELY half of CR 405.6c, which is what rules out queueing the
  -- clause for a caller above the payment. At 2 life the damage empties alice
  -- before CR 601.2g's window asks again, and CR 119.4 then refuses Mana
  -- Confluence's "Pay 1 life", so the cost goes unpaid; one life more and the
  -- same board pays the same {3} the same way.
  Spec.it s "CR 405.6c the damage lands before the rest of the mana window" $ do
    ancientTomb <- S.printingOf s registry "Ancient Tomb"
    manaConfluence <- S.printingOf s registry "Mana Confluence"
    let (_, withConfluence) = S.addPermanent manaConfluence S.alice (Setup.emptyGame S.bothPlayers)
        (tombId, board) = S.addPermanent ancientTomb S.alice withConfluence
        cost = ManaCost.MkManaCost [ManaSymbol.Generic 3]
        attempt life = fst (S.runPureWith (prefersSource tombId) (atLife life board) (Cost.payMana S.manaPerformer PaymentSubject.ForNeither ManaSpending.AsProduced S.alice cost))
    Spec.assertBool s (not (attempt 2)) "at 2 life the Confluence can no longer be paid"
    Spec.assertBool s (attempt 3) "at 3 life the same board pays"

  -- CR 605.1a is unmoved by the clause: what the ability PRODUCES is still its
  -- mana additions alone (Pawl.Engine.ManaAbility.manaProduced), so Ancient Tomb
  -- is a mana source, its activation uses no stack, and the two colorless are
  -- the whole of what it adds.
  Spec.it s "CR 605.1a Ancient Tomb's damage leaves it a mana ability" $ do
    ancientTomb <- S.printingOf s registry "Ancient Tomb"
    let (tombId, board) = S.addPermanent ancientTomb S.alice (Setup.emptyGame S.bothPlayers)
        after = S.runPure S.identityAnswer board (S.tapForMana tombId)
    Spec.assertEqWith s "the yield is two colorless and nothing else" (tappedFor S.identityAnswer tombId board) [ManaType.Colorless, ManaType.Colorless]
    Spec.assertBool s (elem tombId (Mana.manaSources Cost.manaActivations S.alice board)) "and it is a mana source"
    Spec.assertEqWith s "the activation used no stack (CR 605.3b)" (length (GameState.stack after)) 0
    Spec.assertEqWith s "while the damage was still dealt" (S.lifeOf S.alice after) (Just 18)

-- CR 118.3 on a source offering SEVERAL yields, one of which adds more than one
-- mana. Ashaya, Soul of the Wild ("Each nontoken creature you control is a Forest
-- land in addition to its other types") turns a Palladium Myr ({3} Artifact
-- Creature -- Myr, "{T}: Add {C}{C}") into exactly that: CR 305.6 gives the
-- Forest an intrinsic "{T}: Add {G}", so one permanent offers {G} OR {C}{C}, and
-- nothing else.
--
-- The pool's first such source, and the falsifier for the supply model that
-- TRANSPOSED a source's yields (#450): position by position that reads the first
-- mana as green-or-colorless and the second as colorless, so one Myr looked able
-- to make {G} AND a second mana -- a mix no single activation of it produces.
-- Both of the model's over-counts are here at once, because a Palladium Myr is
-- credited with the LONGER yield's two mana while keeping the SHORTER yield's
-- green.
--
-- CR 107.5 is why one yield per source is the exact reading: both of the Myr's
-- mana abilities include {T} in their activation cost, and "a permanent that's
-- already tapped can't be tapped again to pay the cost", so an untapped source
-- is tapped for mana at most once and adds what exactly one activation adds.
palladiumMyrSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
palladiumMyrSpec s registry = Spec.describe s "Palladium Myr" $ do
  -- The fixture fact everything below rests on: two yields, and they differ in
  -- TYPE as well as in length. Without Ashaya the Myr is Sol Ring's shape (one
  -- yield of two mana) and the transpose was exact.
  Spec.it s "CR 305.6 an Ashaya'd Palladium Myr offers {G} or {C}{C}, and nothing between" $ do
    ashaya <- S.printingOf s registry "Ashaya, Soul of the Wild"
    palladiumMyr <- S.printingOf s registry "Palladium Myr"
    let (_, g1) = S.addPermanent ashaya S.alice (Setup.emptyGame S.bothPlayers)
        (myrId, gs) = S.addPermanent palladiumMyr S.alice g1
        -- CR 106.3: the Myr is an artifact whichever yield is taken, so both
        -- units carry ProductionTag.Artifact -- including the {G} Ashaya's land
        -- grant adds, which is a land that is still an artifact.
        artifactUnit t = ManaUnit.MkManaUnit {ManaUnit.manaType = t, ManaUnit.tags = Set.singleton ProductionTag.Artifact, ManaUnit.retention = ManaRetention.Ordinary, ManaUnit.restriction = Nothing, ManaUnit.rider = Nothing, ManaUnit.spendTrigger = Nothing, ManaUnit.sourceChosenSubtype = Nothing, ManaUnit.sourceLastExiled = Nothing}
        green = artifactUnit (ManaType.Colored Color.Green)
        colorless = artifactUnit ManaType.Colorless
    Spec.assertEqWith
      s
      "the Forest's {G} and the artifact's {C}{C}"
      (Mana.manaYieldsOf myrId gs)
      [Mana.Type.MkMana [green], Mana.Type.MkMana [colorless, colorless]]

  -- THE PROVING CASE. Ashaya taps for {G}; each Myr adds {G} or {C}{C}. Every
  -- green mana past the first therefore costs a Myr its colorless pair, so the
  -- board makes three mana all green, or four of which two are green, or five of
  -- which one is -- and never five with two green. Transposing said otherwise:
  -- five supplies, three of them able to be green, which passes both of
  -- payableResolutions' counting clauses.
  Spec.it s "CR 118.3 Ashaya and two Palladium Myrs cannot pay {3}{G}{G}" $ do
    ashaya <- S.printingOf s registry "Ashaya, Soul of the Wild"
    palladiumMyr <- S.printingOf s registry "Palladium Myr"
    let (_, g1) = S.addPermanent ashaya S.alice (Setup.emptyGame S.bothPlayers)
        (_, g2) = S.addPermanent palladiumMyr S.alice g1
        (_, gs) = S.addPermanent palladiumMyr S.alice g2
        green = ManaSymbol.OfType (ManaType.Colored Color.Green)
    Spec.assertBool
      s
      (not (Mana.canPay Cost.manaActivations S.alice (ManaCost.MkManaCost [ManaSymbol.Generic 3, green, green]) gs))
      "five mana with two green is out of reach"

  -- The control legs, on the SAME board: everything the board really can pay is
  -- still payable, and each leg needs a different yield out of the same Myr.
  Spec.it s "CR 118.3 the same board still pays {2}{G}{G}, {5} and {G}{G}{G}" $ do
    ashaya <- S.printingOf s registry "Ashaya, Soul of the Wild"
    palladiumMyr <- S.printingOf s registry "Palladium Myr"
    let (_, g1) = S.addPermanent ashaya S.alice (Setup.emptyGame S.bothPlayers)
        (_, g2) = S.addPermanent palladiumMyr S.alice g1
        (_, gs) = S.addPermanent palladiumMyr S.alice g2
        green = ManaSymbol.OfType (ManaType.Colored Color.Green)
        pays cost = Mana.canPay Cost.manaActivations S.alice (ManaCost.MkManaCost cost) gs
    -- One Myr on its Forest, one on its {C}{C}: {G}{G}{C}{C}.
    Spec.assertBool s (pays [ManaSymbol.Generic 2, green, green]) "{2}{G}{G}"
    -- Both Myrs on {C}{C}: {G}{C}{C}{C}{C}, the board's largest payment.
    Spec.assertBool s (pays [ManaSymbol.Generic 5]) "{5}"
    -- Both Myrs on their Forest: {G}{G}{G}, the board's greenest.
    Spec.assertBool s (pays [green, green, green]) "{G}{G}{G}"
    -- And each axis has a real ceiling: five mana, or three green, never both.
    Spec.assertBool s (not (pays [ManaSymbol.Generic 6])) "but not {6}"
    Spec.assertBool s (not (pays [green, green, green, green])) "and not {G}{G}{G}{G}"

  -- The gameplay-level proof (design.md section 4), through the door
  -- Action.legalActions opens: CR 118.3's payability is what decides whether a
  -- cast is OFFERED at all, so an over-counted supply side offers the player an
  -- action whose payment then fails and rolls back (CR 601.2h). Two real spells,
  -- one board, one generic symbol apart.
  Spec.it s "CR 118.3 Living Plane is offered off Ashaya and two Palladium Myrs, Meandering Towershell is not" $ do
    ashaya <- S.printingOf s registry "Ashaya, Soul of the Wild"
    palladiumMyr <- S.printingOf s registry "Palladium Myr"
    livingPlane <- S.printingOf s registry "Living Plane"
    towershell <- S.printingOf s registry "Meandering Towershell"
    let (_, g1) = S.addPermanent ashaya S.alice (Setup.emptyGame S.bothPlayers)
        (_, g2) = S.addPermanent palladiumMyr S.alice g1
        (_, g3) = S.addPermanent palladiumMyr S.alice g2
        (planeId, g4) = S.addHandCard livingPlane S.alice g3
        (towershellId, g5) = S.addHandCard towershell S.alice g4
        -- CR 303.1 (the enchantment) and CR 302.1 (the creature) name the same
        -- window -- a main phase of your own turn, stack empty -- so it has to be
        -- open for either to be offered at all.
        gs = g5 {GameState.phase = Phase.PrecombatMain, GameState.priority = Just S.alice}
        offered = Action.legalActions S.alice gs
    Spec.assertBool s (elem (Action.Type.Cast planeId (S.printingName livingPlane) Facing.FaceUp) offered) "{2}{G}{G} is offered"
    Spec.assertBool s (not (any (S.isCastOf towershellId) offered)) "{3}{G}{G} is not"

-- CR 700.2's SELECTION on a mana ability. Synthetic Prismatic Wellspring
-- (Land, "{T}: Choose two -- * Add {R}. * Add {G}. * Add {W}.") is the pool's
-- first mana ability whose selection is not "choose exactly one", and it is
-- what separates "which mode" from "which COMBINATION of modes" (#449). A
-- choose-two ability read one mode at a time under-counts its supply by half,
-- so a cost it can afford is refused.
--
-- SYNTHETIC, and legitimate on the rules: CR 700.2 makes an object modal by a
-- bulleted list plus an instruction to choose a NUMBER of those options, CR
-- 700.2a names activated abilities explicitly, CR 700.2d covers a selection of
-- more than one mode, and CR 605.1a admits any activated ability that doesn't
-- target, isn't a loyalty ability, and could add mana. Nothing there bounds a
-- mana ability's selection to one. Every component is printed separately -- a
-- modal activated ability (Bow of Nylea), "Choose two --" (Kozilek's Command),
-- a mana-adding mode inside a modal ability (Jeska's Will) -- and only the
-- composite is missing. A Scryfall search across paper, Arena, MTGO, playtest
-- and un-set printings finds no modal mana ability at all: a bulleted mode
-- beginning "Add" matches five cards, every one a spell or a non-mana triggered
-- ability, and a "{T}: Choose one/two/three" activated ability matches nine,
-- none of which adds mana.
wellspringSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
wellspringSpec s registry = Spec.describe s "SyntheticPrismaticWellspring" $ do
  -- THE case, and it is taken with the pool EMPTY and the ability unactivated,
  -- which is what makes it about the supply model rather than about the pool:
  -- CR 118.3 counts an untapped source as the mana it could make, and one
  -- activation of this one makes TWO. Activating first and asking afterwards
  -- would pass either way, since by then the mana is really in the pool.
  --
  -- {3} is the falsifier for a model that simply credited a modal source with
  -- all of its modes: three modes, but the selection demands two.
  Spec.it s "CR 118.3 a lone untapped Wellspring pays {2}, though it is not activated" $ do
    wellspring <- S.printingOf s registry "Synthetic Prismatic Wellspring"
    let gs = S.landsInPlay wellspring 1
    Spec.assertEqWith s "nothing is floating" (poolSize S.alice gs) 0
    Spec.assertBool s (Mana.canPay Cost.manaActivations S.alice (ManaCost.MkManaCost [ManaSymbol.Generic 2]) gs) "{2} is affordable"
    Spec.assertBool s (not (Mana.canPay Cost.manaActivations S.alice (ManaCost.MkManaCost [ManaSymbol.Generic 3]) gs)) "{3} is not"

  -- CR 700.2d: "If a player is allowed to choose more than one mode ... that
  -- player normally can't choose the same mode more than once." So the
  -- combinations are the size-two subsets and not the size-two sequences --
  -- {R}{G} is on offer, {R}{R} never is. This is what a model enumerating
  -- repetitions would fail, and it fails nothing else.
  Spec.it s "CR 700.2d two DIFFERENT modes, so a Wellspring pays {R}{G} but not {R}{R}" $ do
    wellspring <- S.printingOf s registry "Synthetic Prismatic Wellspring"
    let gs = S.landsInPlay wellspring 1
        red = ManaSymbol.OfType (ManaType.Colored Color.Red)
        green = ManaSymbol.OfType (ManaType.Colored Color.Green)
    Spec.assertBool s (Mana.canPay Cost.manaActivations S.alice (ManaCost.MkManaCost [red, green]) gs) "{R}{G} is affordable"
    Spec.assertBool s (not (Mana.canPay Cost.manaActivations S.alice (ManaCost.MkManaCost [red, red]) gs)) "{R}{R} is not"

  -- What one activation actually PUTS in the pool, which is the same
  -- enumeration read through the other door: CR 106.12's tap-for-mana adds a
  -- whole yield, and a chosen pair of modes is one yield of two mana (CR
  -- 608.2c orders them by mode). S.identityAnswer takes the first candidate,
  -- which is the first two modes.
  Spec.it s "CR 605.3b tapping a Wellspring adds two mana, one per chosen mode" $ do
    wellspring <- S.printingOf s registry "Synthetic Prismatic Wellspring"
    let (wellspringId, gs) = S.addPermanent wellspring S.alice (Setup.emptyGame S.bothPlayers)
    Spec.assertEqWith
      s
      "{R} and {G}, from one activation"
      (tappedFor S.identityAnswer wellspringId gs)
      [ManaType.Colored Color.Red, ManaType.Colored Color.Green]

-- alice casts a Coldsteel Heart off two Mountains and resolves it, naming
-- `wanted` at CR 614.1c's colour choice. Returns the board and the permanent
-- that entered.
--
-- Never white in a caller below: white is Replay.defaultAnswer's fallback, so an
-- assertion against it would pass on a game that never asked.
resolvedColdsteel :: Color.Color -> Printing.Printing -> Printing.Printing -> (GameState.GameState, Maybe ObjectId.ObjectId)
resolvedColdsteel wanted mountain coldsteel =
  let board = S.landsInPlay mountain 2
      (withCard, oid) = S.handOne coldsteel board
      answer :: Prompt.Prompt r -> r
      answer p = case p of
        Prompt.ChooseColor {} -> wanted
        _ -> S.identityAnswer p
      after = S.runPure answer (S.runPure answer withCard (S.cast S.alice oid)) Stack.resolveTop
      entered = case Set.toList (Set.difference (GameState.battlefield after) (GameState.battlefield board)) of
        o : _ -> Just o
        [] -> Nothing
   in (after, entered)

-- CR 607.2d: "If an object has an ability printed on it that causes a player to
-- 'choose a [value]' and an ability printed on it that refers to 'the chosen
-- [value]' . . . those abilities are linked." Coldsteel Heart ({2} Snow
-- Artifact, "As this artifact enters, choose a color." / "{T}: Add one mana of
-- the chosen color.") is the pool's producer, and ManaProduction.Chosen is how
-- the second ability reads what the first wrote.
chosenColorSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
chosenColorSpec s registry = Spec.describe s "Mana of the chosen color (CR 607.2d)" $ do
  -- ONE option and not five, which is what separates Chosen from AnyColor: the
  -- colour is already settled, so nothing is asked when the ability is used.
  Spec.it s "CR 607.2d a Coldsteel Heart that chose blue offers blue and nothing else" $ do
    mountain <- S.printingOf s registry "Mountain"
    coldsteel <- S.printingOf s registry "Coldsteel Heart"
    case resolvedColdsteel Color.Blue mountain coldsteel of
      (after, Just oid) -> do
        Spec.assertEqWith s "exactly the chosen colour" (Mana.manaTypesOf oid after) [ManaType.Colored Color.Blue]
        -- CR 107.5: the Heart entered tapped, so its {T} cannot be paid and the
        -- ability adds nothing at all -- the proof that Cost.tapForMana asks CR
        -- 118.3 before paying. Untapped, the same board shows WHICH mana it adds.
        Spec.assertEqWith s "tapped, it adds nothing" (tappedFor S.identityAnswer oid after) []
        Spec.assertEqWith s "untapped, that is what reaches the pool" (tappedFor S.identityAnswer oid (untapObject oid after)) [ManaType.Colored Color.Blue]
      _ -> Spec.assertFailure s "the Coldsteel Heart did not reach the battlefield"

  -- No colour chosen yields NO mana rather than a fallback colour (CR 106.5). A
  -- fixture write puts a Coldsteel Heart in that state here; in play, a
  -- permanent becoming a copy of one without entering (Mirrorweave, CR 707.2)
  -- has made no choice. The point is that the engine invents nothing.
  Spec.it s "CR 607.2d a Coldsteel Heart placed with no colour chosen produces nothing" $ do
    coldsteel <- S.printingOf s registry "Coldsteel Heart"
    let (oid, gs) = S.addPermanent coldsteel S.alice (Setup.emptyGame S.bothPlayers)
    Spec.assertEqWith s "chosenColors is unset" (fmap Object.chosenColors (Game.lookupObject oid gs)) (Just Set.empty)
    Spec.assertEqWith s "so it offers no mana" (Mana.manaTypesOf oid gs) []
    Spec.assertEqWith s "and tapping it adds none" (tappedFor S.identityAnswer oid gs) []
    Spec.assertBool s (not (Mana.canPay Cost.manaActivations S.alice (ManaCost.MkManaCost [ManaSymbol.OfType (ManaType.Colored Color.Blue)]) gs)) "and it pays for nothing"

-- CR 602.2b sends an activation through CR 601.2b-i, so tapping for mana pays the
-- ability's whole cost -- which is not always just {T}. Mana Confluence (Land,
-- "{T}, Pay 1 life: Add one mana of any color") is the pool's first mana ability
-- charging anything beyond the tap, and Cost.tapForMana is where it is charged.
manaConfluenceSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
manaConfluenceSpec s registry = Spec.describe s "Mana Confluence" $ do
  -- The unit fact: the mana arrives AND the life goes. An engine that taps the
  -- permanent and adds the yield without routing through the cost passes the
  -- first assertion and fails the second.
  Spec.it s "CR 602.2b tapping it adds a mana and pays the 1 life" $ do
    manaConfluence <- S.printingOf s registry "Mana Confluence"
    let (oid, gs) = S.addPermanent manaConfluence S.alice (Setup.emptyGame S.bothPlayers)
        after = S.runPure (prefersColor Color.Black) gs (S.tapForMana oid)
    Spec.assertEqWith s "the colour asked for" (tappedFor (prefersColor Color.Black) oid gs) [ManaType.Colored Color.Black]
    Spec.assertEqWith s "exactly 1 life" (S.lifeOf S.alice after) (Just 19)
    Spec.assertEqWith s "and the land is tapped, by the {T} of that same cost" (S.tappedCount S.alice after) 1

  -- The life is read off THIS ability's cost rather than charged per tap: a
  -- second Mana Confluence charges again, and a Forest tapped on the same board
  -- charges nothing. Discriminating against a fixed toll on tapping for mana,
  -- which passes the case above.
  Spec.it s "CR 602.2b each activation pays its own cost, and a Forest's is free" $ do
    manaConfluence <- S.printingOf s registry "Mana Confluence"
    forest <- S.printingOf s registry "Forest"
    let (firstId, g1) = S.addPermanent manaConfluence S.alice (Setup.emptyGame S.bothPlayers)
        (secondId, g2) = S.addPermanent manaConfluence S.alice g1
        (forestId, g3) = S.addPermanent forest S.alice g2
        tapEach = List.foldl' (\g oid -> S.runPure (prefersColor Color.Black) g (S.tapForMana oid)) g3
    Spec.assertEqWith s "one Confluence, 1 life" (S.lifeOf S.alice (tapEach [firstId])) (Just 19)
    Spec.assertEqWith s "both of them, 2 life" (S.lifeOf S.alice (tapEach [firstId, secondId])) (Just 18)
    Spec.assertEqWith s "and the Forest adds a third mana for nothing" (S.lifeOf S.alice (tapEach [firstId, secondId, forestId])) (Just 18)
    Spec.assertEqWith s "three mana in the pool" (poolSize S.alice (tapEach [firstId, secondId, forestId])) 3

  -- CR 118.3c: "Activating mana abilities is not mandatory, even if paying a cost
  -- is." Mana Confluence is what makes that observable rather than a formality --
  -- at 1 life, its "Pay 1 life" is a cost its controller does not survive, and CR
  -- 704.5a then takes the game.
  --
  -- The same board answered twice. Declining leaves alice alive with the land
  -- untapped and the Rats in hand, CR 601.2h having reversed the cast; taking the
  -- offer kills her. An engine that taps whatever the cost needs can only do the
  -- second, and it is not the player who decided (#218).
  --
  -- ONE candidate, deliberately: this is the case the old elision swallowed
  -- whole, since "which source" has no answer to give when there is only one.
  Spec.it s "CR 118.3c a player at 1 life may decline to tap their only Mana Confluence" $ do
    manaConfluence <- S.printingOf s registry "Mana Confluence"
    typhoidRats <- S.printingOf s registry "Typhoid Rats"
    let (confluenceId, g1) = S.addPermanent manaConfluence S.alice (Setup.emptyGame S.bothPlayers)
        (withRats, ratsId) = S.handOne typhoidRats g1
        board = atLife 1 withRats
        declines :: Prompt.Prompt r -> r
        declines p = case p of
          Prompt.ChooseManaSource {} -> Nothing
          _ -> prefersColor Color.Black p
        castWith :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState
        castWith answer = S.settleSba (S.runPure answer board (S.cast S.alice ratsId))
        refused = castWith declines
        accepted = castWith (prefersColor Color.Black)
        tapOf g = fmap Object.tapped (Game.lookupObject confluenceId g)
    Spec.assertEqWith s "declining: alice keeps her last life" (S.lifeOf S.alice refused) (Just 1)
    Spec.assertEqWith s "declining: the land is untapped" (tapOf refused) (Just TapState.Untapped)
    Spec.assertEqWith s "declining: and the Rats are still in hand" (Game.zoneMembers Zone.Hand S.alice refused) [ratsId]
    Spec.assertEqWith s "declining: so she is still playing" (fmap Player.status (Map.lookup S.alice (GameState.players refused))) (Just Status.Playing)
    Spec.assertEqWith s "accepting: the life is gone" (S.lifeOf S.alice accepted) (Just 0)
    Spec.assertEqWith s "accepting: the land is tapped" (tapOf accepted) (Just TapState.Tapped)
    Spec.assertEqWith s "accepting: and CR 704.5a takes the game" (fmap Player.status (Map.lookup S.alice (GameState.players accepted))) (Just (Status.Departed Departure.Type.Lost))

  -- Two of ONE permanent's mana abilities adding the SAME mana for different
  -- costs, which the pool reaches by putting Urborg beside Mana Confluence: the
  -- land is a Swamp, so CR 305.6's intrinsic "{T}: Add {B}" sits beside the
  -- printed "{T}, Pay 1 life: Add {B}". The yield cannot tell those two apart,
  -- and an engine that offers the yield alone answers "which cost" itself (#1117).
  --
  -- Asked at the PROMPT and answered at the board, since neither is the whole
  -- fact: the payload is where the two candidates are visible, and the life total
  -- is what proves the answer decides which of them is paid for.
  Spec.it s "CR 305.6/602.2b an Urborg'd Mana Confluence's black is free or costs a life" $ do
    manaConfluence <- S.printingOf s registry "Mana Confluence"
    urborg <- S.printingOf s registry "Urborg, Tomb of Yawgmoth"
    let (confluenceId, g1) = S.addPermanent manaConfluence S.alice (Setup.emptyGame S.bothPlayers)
        (_, gs) = S.addPermanent urborg S.alice g1
        black = Mana.Type.MkMana [ManaUnit.MkManaUnit {ManaUnit.manaType = ManaType.Colored Color.Black, ManaUnit.tags = Set.empty, ManaUnit.retention = ManaRetention.Ordinary, ManaUnit.restriction = Nothing, ManaUnit.rider = Nothing, ManaUnit.spendTrigger = Nothing, ManaUnit.sourceChosenSubtype = Nothing, ManaUnit.sourceLastExiled = Nothing}]
        -- The offered black option that is CR 305.6's free one, or the printed
        -- one that charges the life -- `printed` picks which.
        buysBlack :: Bool -> Prompt.Prompt r -> r
        buysBlack printed p = case p of
          Prompt.ChooseManaYield _ _ _ candidates ->
            let wanted option = Mana.yieldUnits option == Mana.unitsOf black && (ManaOption.cost option /= Mana.intrinsicManaCost) == printed
             in Maybe.fromMaybe (NonEmpty.head candidates) (List.find wanted (NonEmpty.toList candidates))
          _ -> S.identityAnswer p
        blacks = filter ((==) (Mana.unitsOf black) . Mana.yieldUnits) (optionsOffered confluenceId gs)
        free = S.runPure (buysBlack False) gs (S.tapForMana confluenceId)
        bought = S.runPure (buysBlack True) gs (S.tapForMana confluenceId)
    Spec.assertEqWith s "both ways of adding black are offered" (length blacks) 2
    Spec.assertEqWith s "charging two different costs" (Set.size (Set.fromList (fmap ManaOption.cost blacks))) 2
    Spec.assertEqWith s "the free one: black in the pool" (poolTypes S.alice free) [ManaType.Colored Color.Black]
    Spec.assertEqWith s "the free one: and all 20 life" (S.lifeOf S.alice free) (Just 20)
    Spec.assertEqWith s "the bought one: the same black" (poolTypes S.alice bought) [ManaType.Colored Color.Black]
    Spec.assertEqWith s "the bought one: for 1 life" (S.lifeOf S.alice bought) (Just 19)

-- Answers Prompt.ChooseManaYield with a yield of two black mana whenever it is
-- on offer, and defers everything else to S.identityAnswer. prefersColor's
-- two-unit sibling, which Phyrexian Tower's "Add {B}{B}" is the pool's only card
-- to need.
prefersDoubleBlack :: Prompt.Prompt r -> r
prefersDoubleBlack p = case p of
  Prompt.ChooseManaYield _ _ _ candidates ->
    let unit = ManaUnit.MkManaUnit {ManaUnit.manaType = ManaType.Colored Color.Black, ManaUnit.tags = Set.empty, ManaUnit.retention = ManaRetention.Ordinary, ManaUnit.restriction = Nothing, ManaUnit.rider = Nothing, ManaUnit.spendTrigger = Nothing, ManaUnit.sourceChosenSubtype = Nothing, ManaUnit.sourceLastExiled = Nothing}
     in S.optionYielding (Mana.Type.MkMana [unit, unit]) candidates
  _ -> S.identityAnswer p

-- The question as ASKED, rather than as read off the pool afterwards: one entry
-- per candidate Prompt.ChooseManaYield offered, and none at all where the prompt
-- was elided. Prompt-level because that is the only level some of it is visible
-- at: two candidates a player can tell apart can still reach the same board.
optionsOffered :: ObjectId.ObjectId -> GameState.GameState -> [ManaOption.ManaOption]
optionsOffered oid gs =
  let step :: Prompt.Prompt r -> State.State [ManaOption.ManaOption] r
      step p = case p of
        Prompt.ChooseManaYield _ _ _ candidates -> do
          State.modify' (<> NonEmpty.toList candidates)
          pure (NonEmpty.head candidates)
        _ -> pure (S.identityAnswer p)
   in State.execState (Engine.runGame step gs (S.tapForMana oid)) []

-- The colours of those candidates alone, for a caller that is asking what the
-- source could produce rather than what it charges.
yieldsOffered :: ObjectId.ObjectId -> GameState.GameState -> [[ManaType.ManaType]]
yieldsOffered oid gs = fmap (fmap ManaUnit.manaType . Mana.yieldUnits) (optionsOffered oid gs)

-- CR 118.3: "A player can't pay a cost without having the necessary resources to
-- pay it fully", and CR 602.2b makes a mana ability's activation cost one of
-- those costs. Phyrexian Tower (Legendary Land, "{T}: Add {C}." / "{T}, Sacrifice
-- a creature: Add {B}{B}.") is the pool's first mana ability whose cost can fail
-- on a board a player can reach: the tap is always payable and the sacrifice is
-- not. Mana Confluence's "Pay 1 life" fails only at 0 life, which CR 704.5a has
-- already ended the game at.
--
-- Goblin Piker is the creature throughout, and it produces no mana, so the only
-- thing it changes is whether the second ability has a cost to pay.
phyrexianTowerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
phyrexianTowerSpec s registry = Spec.describe s "Phyrexian Tower" $ do
  -- The unit fact, in both directions on one board. The permanent PRODUCES black
  -- either way (CR 106.7, manaTypesOf reads the card), so an engine that skipped
  -- CR 118.3 would hand out {B}{B} with nothing to sacrifice.
  Spec.it s "CR 118.3 the sacrifice option is offered only with a creature to give" $ do
    tower <- S.printingOf s registry "Phyrexian Tower"
    piker <- S.printingOf s registry "Goblin Piker"
    let (towerId, alone) = S.addPermanent tower S.alice (Setup.emptyGame S.bothPlayers)
        withPiker = snd (S.addPermanent piker S.alice alone)
    Spec.assertEqWith s "CR 106.7: it produces {C} and {B} with nothing to sacrifice" (Mana.manaTypesOf towerId alone) [ManaType.Colorless, ManaType.Colored Color.Black]
    Spec.assertEqWith s "with no creature, only the {C} can be paid for" (tappedFor prefersDoubleBlack towerId alone) [ManaType.Colorless]
    Spec.assertEqWith s "so there is no colour question to ask" (yieldsOffered towerId alone) []
    Spec.assertEqWith
      s
      "with one, both options are on offer"
      (yieldsOffered towerId withPiker)
      [[ManaType.Colorless], [ManaType.Colored Color.Black, ManaType.Colored Color.Black]]
    Spec.assertEqWith
      s
      "with one, the sacrifice option adds {B}{B}"
      (tappedFor prefersDoubleBlack towerId withPiker)
      [ManaType.Colored Color.Black, ManaType.Colored Color.Black]

  -- CR 601.2h: the cost is paid in full or not at all, so the creature goes when
  -- the mana arrives -- and stays when the other option is taken.
  Spec.it s "CR 602.2b taking that option pays the sacrifice" $ do
    tower <- S.printingOf s registry "Phyrexian Tower"
    piker <- S.printingOf s registry "Goblin Piker"
    let (towerId, board) = S.addPermanent tower S.alice (snd (S.addPermanent piker S.alice (Setup.emptyGame S.bothPlayers)))
        tapWith :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState
        tapWith answer = S.runPure answer board (S.tapForMana towerId)
    Spec.assertEqWith s "the Piker is gone" (S.creaturesInPlay S.alice (tapWith prefersDoubleBlack)) 0
    Spec.assertEqWith s "choosing {C} instead leaves it alive" (S.creaturesInPlay S.alice (tapWith S.identityAnswer)) 1

  -- CR 118.3 on the SUPPLY side: Mana.canPay counts an untapped source as the
  -- mana it could make, and an activation nobody can pay makes none. Without
  -- this the Tower alone reads as a {B}{B} source and the cast below would be
  -- offered and then fail.
  Spec.it s "CR 118.3 an unpayable activation is no supply either" $ do
    tower <- S.printingOf s registry "Phyrexian Tower"
    piker <- S.printingOf s registry "Goblin Piker"
    let alone = snd (S.addPermanent tower S.alice (Setup.emptyGame S.bothPlayers))
        withPiker = snd (S.addPermanent piker S.alice alone)
        black = ManaSymbol.OfType (ManaType.Colored Color.Black)
        doubleBlack = ManaCost.MkManaCost [black, black]
    Spec.assertBool s (Mana.canPay Cost.manaActivations S.alice doubleBlack withPiker) "{B}{B} with a creature to sacrifice"
    Spec.assertBool s (not (Mana.canPay Cost.manaActivations S.alice doubleBlack alone)) "and not without one"
    Spec.assertBool s (Mana.canPay Cost.manaActivations S.alice (ManaCost.MkManaCost [ManaSymbol.Generic 1]) alone) "though the Tower alone still pays {1}"

  -- The gameplay-level proof (design.md section 4): a real spell whose whole cost
  -- is the mana only that activation can make, cast end to end. Withered Wretch
  -- is {B}{B} and targets nothing as it is cast.
  Spec.it s "CR 601.2g Withered Wretch is cast off a Tower that eats a Piker" $ do
    tower <- S.printingOf s registry "Phyrexian Tower"
    piker <- S.printingOf s registry "Goblin Piker"
    witheredWretch <- S.printingOf s registry "Withered Wretch"
    let resolved = castOffBoard prefersDoubleBlack [tower, piker] witheredWretch
        without = castOffBoard prefersDoubleBlack [tower] witheredWretch
        countOf name = S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack name) S.alice
    Spec.assertEqWith s "the Wretch resolved" (countOf "Withered Wretch" resolved) 1
    Spec.assertEqWith s "the Piker paid for it" (countOf "Goblin Piker" resolved) 0
    -- CR 601.2h reverses the whole cast, so the Tower is left untapped too.
    Spec.assertEqWith s "with no Piker there is no {B}{B} and the cast fails" (countOf "Withered Wretch" without) 0
    Spec.assertEqWith s "and nothing was spent trying" (S.tappedCount S.alice without) 0

-- CR 106.12: to "tap [a permanent] for mana" is to activate a mana ability of
-- that permanent that includes {T} in its activation cost. Blood Pet ({B}
-- Creature -- Thrull, "Sacrifice this creature: Add {B}.") is the pool's first
-- mana ability that is NOT one, so it is the first source CR 107.5's already
-- tapped permanent and CR 302.6's summoning sickness have nothing to say about:
-- neither rule reads a cost without {T}.
--
-- Llanowar Elves is the control on the first two boards below -- also a one-mana
-- creature whose one ability adds one mana, but charging {T} -- and it is
-- refused exactly where the Pet is not. Both stand on ONE board each time, so
-- the two answers come out of a single sweep and cannot differ by fixture.
bloodPetSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
bloodPetSpec s registry = Spec.describe s "Blood Pet" $ do
  Spec.it s "CR 107.5 a TAPPED Blood Pet is still a mana source" $ do
    bloodPet <- S.printingOf s registry "Blood Pet"
    llanowarElves <- S.printingOf s registry "Llanowar Elves"
    let (petId, g1) = S.addPermanent bloodPet S.alice (Setup.emptyGame S.bothPlayers)
        (elfId, g2) = S.addPermanent llanowarElves S.alice g1
        tapped = S.tapObject elfId (S.tapObject petId g2)
        sources = Mana.manaSources Cost.manaActivations S.alice tapped
    Spec.assertBool s (elem petId (Mana.manaSources Cost.manaActivations S.alice g2)) "untapped, the Pet is a source"
    Spec.assertBool s (elem petId sources) "and tapping it changes nothing, since its cost holds no {T}"
    Spec.assertBool s (notElem elfId sources) "where the Elves' does, so CR 107.5 refuses a second tap"

  Spec.it s "CR 302.6 a summoning-sick Blood Pet is still a mana source" $ do
    bloodPet <- S.printingOf s registry "Blood Pet"
    llanowarElves <- S.printingOf s registry "Llanowar Elves"
    let (petId, g1) = S.addPermanent bloodPet S.alice (Setup.emptyGame S.bothPlayers)
        (elfId, g2) = S.addPermanent llanowarElves S.alice g1
        sick = foldr sicken g2 [petId, elfId]
        sources = Mana.manaSources Cost.manaActivations S.alice sick
    Spec.assertBool s (elem petId sources) "CR 302.6 gates a cost with {T} or {Q}, and sacrificing is neither"
    Spec.assertBool s (notElem elfId sources) "where the Elves are gated, as they always were"

-- CR 118.3 on the supply side, counted rather than merely gated. Ashnod's Altar
-- ({3} Artifact, "Sacrifice a creature: Add {C}{C}") is the pool's first mana
-- ability a payment can activate MORE THAN ONCE: its cost holds no {T} for CR
-- 107.5 to bar a second time and does not spend the Altar, so two creatures are
-- two activations and four mana. Counting it once read a cost only two
-- activations could pay as unpayable, so the cast was never offered (#1128).
--
-- Goblin Piker is the victim throughout, and it makes no mana: every mana on
-- these boards comes through the Altar, so the counts below cannot be met any
-- other way.
ashnodsAltarSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
ashnodsAltarSpec s registry = Spec.describe s "Ashnod's Altar" $ do
  -- ONE board for both halves, so what separates {4} from {5} is only how many
  -- activations the supply model counted: two, and not one and not three.
  Spec.it s "CR 118.3 an Altar beside two creatures supplies four mana" $ do
    altar <- S.printingOf s registry "Ashnod's Altar"
    piker <- S.printingOf s registry "Goblin Piker"
    let board = altarBoard altar piker 2
        pays n = Mana.canPay Cost.manaActivations S.alice (ManaCost.MkManaCost [ManaSymbol.Generic n]) board
    Spec.assertBool s (pays 4) "two activations pay {4}"
    Spec.assertBool s (not (pays 5)) "and there is no third creature, so not {5}"

  Spec.it s "CR 118.3 one creature is one activation" $ do
    altar <- S.printingOf s registry "Ashnod's Altar"
    piker <- S.printingOf s registry "Goblin Piker"
    let board = altarBoard altar piker 1
        pays n = Mana.canPay Cost.manaActivations S.alice (ManaCost.MkManaCost [ManaSymbol.Generic n]) board
    Spec.assertBool s (pays 2) "{2} is what one activation adds"
    Spec.assertBool s (not (pays 3)) "and nothing pays {3}"

-- The Altar and `victims` Pikers, all under alice's control.
altarBoard :: Printing.Printing -> Printing.Printing -> Int -> GameState.GameState
altarBoard altar piker victims =
  foldr (\p gs -> snd (S.addPermanent p S.alice gs)) (Setup.emptyGame S.bothPlayers) (altar : replicate victims piker)

-- CR 118.3's "fully" over a resource that is neither an object nor life: the
-- +1/+1 counters on the source itself. Workhorse ({6} Artifact Creature -- Horse
-- 0/0, Oracle text checked against Scryfall: "This creature enters with four
-- +1/+1 counters on it. Remove a +1/+1 counter from this creature: Add {C}.") is
-- the pool's first mana ability repeatable through counters -- no {T} for CR
-- 107.5 to bar a second activation, no object claim, and no CR 119.4 life -- so
-- counting the counters once read four mana as one and the cast was never
-- offered (#1280).
--
-- alice controls nothing else on any of these boards, so every mana here comes
-- through the Workhorse and no count below can be met another way.
workhorseSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
workhorseSpec s registry = Spec.describe s "Workhorse" $ do
  -- ONE board for both halves, so what separates {4} from {5} is only how many
  -- activations the supply model counted.
  --
  -- FOUR and not three, even though the fourth leaves a 0/0: CR 704.3 checks
  -- state-based actions when a player would receive priority, and CR 601.2g's
  -- window is not such a moment, so the last counter is spendable.
  Spec.it s "CR 118.3 four +1/+1 counters supply four mana" $ do
    horse <- S.printingOf s registry "Workhorse"
    let (_, board) = workhorseBoard horse 4
        pays n = Mana.canPay Cost.manaActivations S.alice (ManaCost.MkManaCost [ManaSymbol.Generic n]) board
    Spec.assertBool s (pays 4) "four activations pay {4}"
    Spec.assertBool s (not (pays 5)) "and there is no fifth counter, so not {5}"

  Spec.it s "CR 118.3 one counter is one activation" $ do
    horse <- S.printingOf s registry "Workhorse"
    let (_, board) = workhorseBoard horse 1
        pays n = Mana.canPay Cost.manaActivations S.alice (ManaCost.MkManaCost [ManaSymbol.Generic n]) board
    Spec.assertBool s (pays 1) "{1} is what one activation adds"
    Spec.assertBool s (not (pays 2)) "and nothing pays {2}"

  -- The gameplay-level proof (design.md section 4). Crucible of Worlds is {3},
  -- all generic, and targets nothing as it is cast, so the whole cast turns on
  -- the counters being counted three times.
  --
  -- THREE and not four, which is what keeps the Workhorse readable afterwards: a
  -- fourth activation would leave a 0/0 that CR 704.5f buries before any
  -- assertion here runs.
  --
  -- The two boards differ in the counters and in nothing else, so the short one
  -- fails for the counter rather than for want of anything else.
  Spec.it s "CR 605.3a Crucible of Worlds is cast off three activations of one Workhorse" $ do
    horse <- S.printingOf s registry "Workhorse"
    crucible <- S.printingOf s registry "Crucible of Worlds"
    let (horseId, board) = workhorseBoard horse 4
        (shortId, shortBoard) = workhorseBoard horse 2
        resolved = castFrom S.identityAnswer board crucible
        short = castFrom S.identityAnswer shortBoard crucible
        countOf name = S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack name) S.alice
    Spec.assertEqWith s "the Crucible resolved" (countOf "Crucible of Worlds" resolved) 1
    Spec.assertEqWith s "with two counters there is no {3} and the cast fails" (countOf "Crucible of Worlds" short) 0
    Spec.assertEqWith s "CR 122.1 three of the four counters paid for it" (S.counterOf CounterKind.PlusOnePlusOne horseId resolved) 1
    Spec.assertEqWith s "CR 122.1a so the 4/4 is a 1/1, and still there to be read" (S.powerToughnessOf horseId resolved) (Just (1, 1))
    Spec.assertEqWith s "and the failed cast spent none of the short board's counters" (S.counterOf CounterKind.PlusOnePlusOne shortId short) 2

-- One Workhorse under alice's control carrying `counters` +1/+1 counters, and
-- nothing else on the board. The counters are placed by hand rather than by CR
-- 614.1c so that the two counts a case wants differ in the counters ALONE; the
-- entry rewrite has its own case above.
workhorseBoard :: Printing.Printing -> Natural -> (ObjectId.ObjectId, GameState.GameState)
workhorseBoard horse counters =
  let (horseId, gs) = S.addPermanent horse S.alice (Setup.emptyGame S.bothPlayers)
   in (horseId, S.addCounter CounterKind.PlusOnePlusOne counters horseId gs)

-- CR 118.3's "fully" over untapped-ness -- the resource CR 601.2f's "tapping
-- permanents" spends, and neither an object leaving a zone nor life nor a
-- counter. Heritage Druid ({G} Creature -- Elf Druid, Oracle text checked
-- against Scryfall: "Tap three untapped Elves you control: Add {G}{G}{G}.") is
-- the pool's first repeatable mana ability whose cost taps OTHER permanents: no
-- {T} on the Druid for CR 107.5 to bar a second activation, so nine untapped
-- Elves are three activations and nine mana. `uncountedCeiling` capped the
-- component at 1, so nine Elves supplied three mana and a cast two further
-- activations could have paid for was never offered; see #2173.
--
-- Glistener Elf is the fuel throughout and makes no mana, so every mana on these
-- boards comes through the Druid and no count below can be met any other way.
-- The Druid is itself an untapped Elf, so it is one of the nine and may be one
-- of the three its own cost taps.
heritageDruidSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
heritageDruidSpec s registry = Spec.describe s "Heritage Druid" $ do
  -- ONE board for both halves, so what separates {9} from {10} is only how many
  -- activations the supply model counted: three, and not one and not two.
  Spec.it s "CR 118.3 nine untapped Elves supply nine mana" $ do
    board <- heritageDruidBoard s registry 9
    let pays n = Mana.canPay Cost.manaActivations S.alice (ManaCost.MkManaCost [ManaSymbol.Generic n]) board
    Spec.assertBool s (pays 9) "three activations pay {9}"
    Spec.assertBool s (not (pays 10)) "and three is all nine Elves buy, so not {10}"

  -- The DIVISION rather than the pool's size: six Elves are two activations, not
  -- the six a ceiling reading the pool itself would have counted.
  Spec.it s "CR 118.3 six untapped Elves are two activations" $ do
    board <- heritageDruidBoard s registry 6
    let pays n = Mana.canPay Cost.manaActivations S.alice (ManaCost.MkManaCost [ManaSymbol.Generic n]) board
    Spec.assertBool s (pays 6) "{6} is what two activations add"
    Spec.assertBool s (not (pays 7)) "and two thirds of six leave no third activation, so not {7}"

  Spec.it s "CR 118.3 three untapped Elves are one activation" $ do
    board <- heritageDruidBoard s registry 3
    let pays n = Mana.canPay Cost.manaActivations S.alice (ManaCost.MkManaCost [ManaSymbol.Generic n]) board
    Spec.assertBool s (pays 3) "{3} is what one activation adds"
    Spec.assertBool s (not (pays 4)) "and nothing pays {4}"

-- Alice's Heritage Druid and as many Glistener Elves as make `elves` untapped
-- Elves in all, the Druid counted among them, and nothing else on the board.
heritageDruidBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Int -> m GameState.GameState
heritageDruidBoard s registry elves = do
  druid <- S.printingOf s registry "Heritage Druid"
  elf <- S.printingOf s registry "Glistener Elf"
  pure (alicePermanents (druid : replicate (elves - 1) elf))

-- CR 118.3's "fully" over CR 107.14's energy, the resource a player holds rather
-- than a permanent. Synthetic Dynamo Conduit ({2} Artifact, "Pay {E}: Add one
-- mana of any color.") is the pool's first mana ability whose cost spends energy
-- and nothing CR 107.5 bars repeating: Scryfall `o:"{E}" o:/: add/`, 2026-09-30,
-- every "Pay {E}: Add" printing also charges {T}. Three energy is three
-- activations and three mana, and two Conduits share the three rather than each
-- taking them.
--
-- Nothing else on these boards makes mana, so every count below comes through
-- a Conduit.
dynamoConduitSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
dynamoConduitSpec s registry = Spec.describe s "Synthetic Dynamo Conduit" $ do
  Spec.it s "CR 107.14 three energy supply three mana" $ do
    board <- dynamoConduitBoard s registry 1 3
    let pays n = Mana.canPay Cost.manaActivations S.alice (ManaCost.MkManaCost [ManaSymbol.Generic n]) board
    Spec.assertBool s (pays 3) "three activations pay {3}"
    Spec.assertBool s (not (pays 4)) "and three energy buy no fourth, so not {4}"

  -- The JOINT count (Mana.payableResolutionsGiven): each Conduit asked alone
  -- answers three, and the board has three energy between them.
  Spec.it s "CR 107.14 two Conduits share one count of energy" $ do
    board <- dynamoConduitBoard s registry 2 3
    let pays n = Mana.canPay Cost.manaActivations S.alice (ManaCost.MkManaCost [ManaSymbol.Generic n]) board
    Spec.assertBool s (pays 3) "the three energy pay {3} across the pair"
    Spec.assertBool s (not (pays 4)) "and not {4}: the second Conduit spends the same counters"

  -- The gameplay-level proof (design.md section 4). Crucible of Worlds is {3},
  -- all generic, and targets nothing, so the cast turns on the one Conduit being
  -- activated three times. The boards differ in the energy alone.
  Spec.it s "CR 605.3a Crucible of Worlds is cast off three activations of one Conduit" $ do
    crucible <- S.printingOf s registry "Crucible of Worlds"
    three <- dynamoConduitBoard s registry 1 3
    two <- dynamoConduitBoard s registry 1 2
    let resolved = castFrom S.identityAnswer three crucible
        short = castFrom S.identityAnswer two crucible
        countOf = S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Crucible of Worlds")) S.alice
    Spec.assertEqWith s "the Crucible resolved" (countOf resolved) 1
    Spec.assertEqWith s "with two energy there is no {3} and the cast fails" (countOf short) 0
    Spec.assertEqWith s "CR 107.14 all three energy paid for it" (S.playerCounterOf PlayerCounterKind.Energy S.alice resolved) 0
    Spec.assertEqWith s "and the failed cast spent none of the short board's" (S.playerCounterOf PlayerCounterKind.Energy S.alice short) 2

-- Alice with `conduits` Synthetic Dynamo Conduits, `energy` energy counters and
-- nothing else.
dynamoConduitBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Int -> Natural -> m GameState.GameState
dynamoConduitBoard s registry conduits energy = do
  conduit <- S.printingOf s registry "Synthetic Dynamo Conduit"
  pure (S.addPlayerCounter PlayerCounterKind.Energy energy S.alice (alicePermanents (replicate conduits conduit)))

-- CR 118.3's "fully" over CR 701.68a's blight, which spends nothing that runs
-- out: the creature blighted stays on the battlefield through CR 601.2g's mana
-- window, since CR 704.3 checks no state-based action there, and is blighted
-- again. Synthetic Withering Font ({2} Artifact, "Blight 1, Pay 1 life: Add
-- {C}.") is the pool's first mana ability whose cost blights and that nothing
-- caps at one activation: MTGJSON 2026-08-23 and Scryfall
-- `o:/blight [0-9X][^.]*: add/ include:extras`, 2026-09-30, no printing. The
-- life is the bound, so the boards differ in life alone.
--
-- The gameplay-level proof (design.md section 4). Crucible of Worlds is {3},
-- all generic, and targets nothing, so the cast turns on the Font being
-- activated three times, blighting the one Goblin Piker (2/1) each time.
witheringFontSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
witheringFontSpec s registry =
  Spec.describe s "Synthetic Withering Font"
    . Spec.it s "CR 605.3a Crucible of Worlds is cast off three blights of one creature"
    $ do
      crucible <- S.printingOf s registry "Crucible of Worlds"
      (pikerId, board) <- witheringFontBoard s registry
      let resolved = castFrom S.identityAnswer (atLife 4 board) crucible
          short = castFrom S.identityAnswer (atLife 2 board) crucible
          countOf = S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Crucible of Worlds")) S.alice
      Spec.assertEqWith s "the Crucible resolved" (countOf resolved) 1
      Spec.assertEqWith s "with two life there is no {3} and the cast fails" (countOf short) 0
      Spec.assertEqWith s "CR 701.68a the one Piker was blighted three times" (S.counterOf CounterKind.MinusOneMinusOne pikerId resolved) 3
      Spec.assertEqWith s "and CR 119.4 three life paid for it" (S.lifeOf S.alice resolved) (Just 1)

-- Alice's Synthetic Withering Font and one Goblin Piker, whose id is answered.
witheringFontBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (ObjectId.ObjectId, GameState.GameState)
witheringFontBoard s registry = do
  font <- S.printingOf s registry "Synthetic Withering Font"
  piker <- S.printingOf s registry "Goblin Piker"
  let (pikerId, withPiker) = S.addPermanent piker S.alice (Setup.emptyGame S.bothPlayers)
  pure (pikerId, snd (S.addPermanent font S.alice withPiker))

-- A cost tapping creatures for a TOTAL POWER, a threshold on an aggregate rather
-- than a count. Synthetic Muster Dynamo ({2} Artifact, "Tap any number of
-- untapped creatures you control with total power 3 or greater: Add {C}.") is
-- the pool's first such mana ability that nothing caps at one activation:
-- MTGJSON 2026-08-23 and Scryfall `o:/total power [0-9] or (greater|more): add/
-- include:extras`, 2026-09-30, no printing. Glistener Elf (1/1) and Goblin
-- Piker (2/1) are the fuel and make no mana.
musterDynamoSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
musterDynamoSpec s registry = Spec.describe s "Synthetic Muster Dynamo" $ do
  -- Two Pikers and four Elves total 8, and 2 + 1 is the fewest reaching 3. So
  -- two activations, not the three a division of the six by that fewest counts:
  -- the third would need two Elves, and 1 + 1 is short.
  Spec.it s "CR 118.3 two Pikers and four Elves are two activations" $ do
    board <- musterDynamoBoard s registry [("Goblin Piker", 2), ("Glistener Elf", 4)]
    let pays n = Mana.canPay Cost.manaActivations S.alice (ManaCost.MkManaCost [ManaSymbol.Generic n]) board
    Spec.assertBool s (pays 2) "two activations pay {2}"
    Spec.assertBool s (not (pays 3)) "and the Elves left over total 2, so not {3}"

  -- The JOINT count (Mana.payableResolutionsGiven): one activation taps all
  -- three Elves, so the Drum's own Elf is one too many.
  Spec.it s "CR 118.3 a Springleaf Drum cannot tap an Elf the Dynamo needs" $ do
    drum <- S.printingOf s registry "Springleaf Drum"
    board <- musterDynamoBoard s registry [("Glistener Elf", 3)]
    let withDrum = snd (S.addPermanent drum S.alice board)
        pays n = Mana.canPay Cost.manaActivations S.alice (ManaCost.MkManaCost [ManaSymbol.Generic n]) withDrum
    Spec.assertBool s (pays 1) "either source pays {1}"
    Spec.assertBool s (not (pays 2)) "and three Elves are not four, so not {2}"

  -- Two Bloodbraid Elves (3/2) would be two activations alone, but Heritage
  -- Druid must tap three of its four Elves, a Bloodbraid among them, so the
  -- Dynamo is one activation beside it: {G}{G}{G} and {C}. Eon Hub is {5} and
  -- Jade Statue {4}, both all generic and neither targeting as it is cast.
  Spec.it s "CR 601.2g the Dynamo beside a Heritage Druid is one activation" $ do
    druid <- S.printingOf s registry "Heritage Druid"
    bloodbraid <- S.printingOf s registry "Bloodbraid Elf"
    hub <- S.printingOf s registry "Eon Hub"
    statue <- S.printingOf s registry "Jade Statue"
    board <- musterDynamoBoard s registry [("Glistener Elf", 1), ("Goblin Piker", 1)]
    let withElves = foldr (\p gs -> snd (S.addPermanent p S.alice gs)) board [druid, bloodbraid, bloodbraid]
        offered spell =
          let (withSpell, oid) = S.handOne spell withElves
           in any (S.isCastOf oid) (Action.legalActions S.alice withSpell)
    Spec.assertBool s (not (offered hub)) "no {5}: the Druid leaves one Bloodbraid, and the Dynamo one activation"
    Spec.assertBool s (offered statue) "and {4} is the Druid's {G}{G}{G} and one {C}"

  -- One Bloodbraid, and the Dynamo's one activation has to be WHICH creatures:
  -- the Druid taps all three Elves, the Bloodbraid among them, and leaves the
  -- Piker's 2, short of 3. A count of objects puts the Dynamo's one on the
  -- Piker and promised {4}. Phyrexian Altar is {3}, all generic and targeting
  -- nothing as it is cast.
  Spec.it s "CR 118.3 the Dynamo cannot reach 3 with the creature the Druid leaves" $ do
    druid <- S.printingOf s registry "Heritage Druid"
    bloodbraid <- S.printingOf s registry "Bloodbraid Elf"
    altar <- S.printingOf s registry "Phyrexian Altar"
    statue <- S.printingOf s registry "Jade Statue"
    board <- musterDynamoBoard s registry [("Glistener Elf", 1), ("Goblin Piker", 1)]
    let withElves = foldr (\p gs -> snd (S.addPermanent p S.alice gs)) board [druid, bloodbraid]
        offered spell =
          let (withSpell, oid) = S.handOne spell withElves
           in any (S.isCastOf oid) (Action.legalActions S.alice withSpell)
    Spec.assertBool s (not (offered statue)) "no {4}: the Piker alone does not reach 3"
    Spec.assertBool s (offered altar) "and {3} is the Druid's {G}{G}{G}"

  -- The board above and one Sneaky Homunculus (1/1 Homunculus Illusion, no
  -- mana ability): the Druid's three Elves leave the Piker's 2 and the
  -- Homunculus' 1, and only that MIXED selection reaches 3.
  Spec.it s "CR 118.3 the Dynamo reaches 3 with a Piker and a 1-power creature" $ do
    druid <- S.printingOf s registry "Heritage Druid"
    bloodbraid <- S.printingOf s registry "Bloodbraid Elf"
    homunculus <- S.printingOf s registry "Sneaky Homunculus"
    hub <- S.printingOf s registry "Eon Hub"
    statue <- S.printingOf s registry "Jade Statue"
    board <- musterDynamoBoard s registry [("Glistener Elf", 1), ("Goblin Piker", 1)]
    let withElves = foldr (\p gs -> snd (S.addPermanent p S.alice gs)) board [druid, bloodbraid, homunculus]
        offered spell =
          let (withSpell, oid) = S.handOne spell withElves
           in any (S.isCastOf oid) (Action.legalActions S.alice withSpell)
    Spec.assertBool s (offered statue) "{4} is the Druid's {G}{G}{G} and the Piker and Homunculus' {C}"
    Spec.assertBool s (not (offered hub)) "and not {5}"

  -- The gameplay-level proof (design.md section 4). Sapphire Medallion is {2},
  -- all generic, and targets nothing, so the cast turns on the Dynamo being
  -- activated twice. The boards differ in one Elf.
  Spec.it s "CR 605.3a Sapphire Medallion is cast off two activations of one Dynamo" $ do
    medallion <- S.printingOf s registry "Sapphire Medallion"
    six <- musterDynamoBoard s registry [("Glistener Elf", 6)]
    five <- musterDynamoBoard s registry [("Glistener Elf", 5)]
    let resolved = castFrom tapFirstElves six medallion
        short = castFrom tapFirstElves five medallion
        countOf = S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Sapphire Medallion")) S.alice
    Spec.assertEqWith s "the Medallion resolved" (countOf resolved) 1
    Spec.assertEqWith s "with five Elves there is no second activation and the cast fails" (countOf short) 0
    Spec.assertEqWith s "CR 601.2h all six Elves paid for it" (S.tappedCount S.alice resolved) 6
    Spec.assertEqWith s "and CR 601.2h left the short board's Elves untapped" (S.tappedCount S.alice short) 0

-- The first `threshold` Elves offered, each of power 1, where the default answer
-- taps every candidate and would spend all six on one activation. Test-local:
-- the harness has no vocabulary for ChooseTapsForTotalPower.
tapFirstElves :: Prompt.Prompt r -> r
tapFirstElves p = case p of
  Prompt.ChooseTapsForTotalPower _ _ _ candidates threshold -> Set.fromList (List.genericTake threshold candidates)
  _ -> S.identityAnswer p

-- Alice's Synthetic Muster Dynamo and, for each name, that many of the card.
musterDynamoBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> [(String, Int)] -> m GameState.GameState
musterDynamoBoard s registry fuel = do
  dynamo <- S.printingOf s registry "Synthetic Muster Dynamo"
  creatures <- traverse (\(name, n) -> fmap (replicate n) (S.printingOf s registry name)) fuel
  pure (alicePermanents (dynamo : concat creatures))

-- Cryptex ({2} Artifact, "{T}, Collect evidence 3: Add one mana of any color.
-- Put an unlock counter on this artifact.") beside two Islands, paying for
-- Headless Skaab ({2}{U}, "As an additional cost to cast this spell, exile a
-- creature card from your graveyard"); Oracle checked against Scryfall
-- 2026-10-01. alice's graveyard holds Bloodbraid Elf (a creature card, mana
-- value 4) and one noncreature card, and the boards differ only in that card:
-- Lightning Bolt (mana value 1) or Acidic Soil (3). Beside the Bolt the
-- Cryptex reaches 3 only with the Elf, which the Skaab's cost must exile --
-- one card each, two cards, and a count of cards could not tell.
cryptexSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
cryptexSpec s registry = Spec.describe s "Cryptex" $ do
  Spec.it s "CR 118.3 Cryptex cannot collect the creature card Headless Skaab exiles" $ do
    (short, shortSkaab, _) <- cryptexBoard s registry "Lightning Bolt"
    (enough, skaab, soil) <- cryptexBoard s registry "Acidic Soil"
    let offered oid gs = any (S.isCastOf oid) (Action.legalActions S.alice gs)
        resolved = S.runPure (collectOnly soil) (S.runPure (collectOnly soil) enough (S.cast S.alice skaab)) Stack.resolveTop
        countOf = S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Headless Skaab")) S.alice
    Spec.assertBool s (not (offered shortSkaab short)) "beside the Bolt the Elf pays one cost or the other, so the Skaab is not offered"
    Spec.assertBool s (offered skaab enough) "beside the Soil the Cryptex collects it, so the Skaab is"
    Spec.assertEqWith s "CR 605.3a and the Skaab resolved off the Cryptex's mana" (countOf resolved) 1

-- That one card, wherever collect evidence asks: the default answer takes
-- every candidate, the Elf the Skaab needs included. Test-local: the harness
-- has no vocabulary for ChooseCollectEvidence.
collectOnly :: ObjectId.ObjectId -> Prompt.Prompt r -> r
collectOnly oid p = case p of
  Prompt.ChooseCollectEvidence _ _ _ candidates _ -> Set.fromList (filter (== oid) candidates)
  _ -> S.identityAnswer p

-- Alice's Cryptex and two Islands, Bloodbraid Elf and the named card in her
-- graveyard, and Headless Skaab in her hand. Answers the board, the Skaab and
-- the named card.
cryptexBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> String -> m (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId)
cryptexBoard s registry other = do
  cryptex <- S.printingOf s registry "Cryptex"
  island <- S.printingOf s registry "Island"
  bloodbraid <- S.printingOf s registry "Bloodbraid Elf"
  card <- S.printingOf s registry other
  skaab <- S.printingOf s registry "Headless Skaab"
  let (otherId, graveyard) = S.addGraveyardCard card S.alice (snd (S.addGraveyardCard bloodbraid S.alice (alicePermanents [cryptex, island, island])))
      (withSkaab, skaabId) = S.handOne skaab graveyard
  pure (withSkaab, skaabId, otherId)

-- The half #1128 gave up: WHICH mana each of a repeatable source's activations
-- makes. Phyrexian Altar ({3} Artifact, "Sacrifice a creature: Add one mana of
-- any color") is the pool's first mana ability that is both repeatable and offers
-- a choice, so two creatures buy two mana of any two colours -- where one option
-- per yield offered "n of one colour" and read {R}{G} as unpayable (#1131).
--
-- Goblin Piker is the victim throughout and makes no mana, so every mana on these
-- boards comes through the Altar.
phyrexianAltarSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
phyrexianAltarSpec s registry = Spec.describe s "Phyrexian Altar" $ do
  -- ONE board for all three, so what separates {R}{R} from {R}{G} is only whether
  -- the two activations could pick different colours -- not how many there are.
  Spec.it s "CR 118.3 two activations of one Altar make two different colors" $ do
    board <- phyrexianAltarBoard s registry 2
    Spec.assertBool s (paysColors [Color.Red, Color.Red] board) "two of one colour, which never wanted a mix"
    Spec.assertBool s (paysColors [Color.Red, Color.Green] board) "and one of each, which does"
    Spec.assertBool s (not (paysColors [Color.Red, Color.Green, Color.Blue] board)) "and there is no third creature, so not three"

  Spec.it s "CR 118.3 a third creature buys a third color" $ do
    board <- phyrexianAltarBoard s registry 3
    Spec.assertBool s (paysColors [Color.Red, Color.Green, Color.Blue] board) "three activations, three colours"
    Spec.assertBool s (not (paysColors [Color.Red, Color.Green, Color.Blue, Color.White] board)) "and not a fourth"

  Spec.it s "CR 118.3 one creature is one mana of one color" $ do
    board <- phyrexianAltarBoard s registry 1
    Spec.assertBool s (paysColors [Color.Red] board) "the one activation's colour is the player's"
    Spec.assertBool s (not (paysColors [Color.Red, Color.Green] board)) "and one activation is one mana"

  -- The gameplay-level offer (design.md section 4). Zhur-Taa Goblin is {R}{G} and
  -- targets nothing as it is cast, so the whole cast turns on the two activations
  -- being allowed different colours.
  Spec.it s "CR 601.2g Zhur-Taa Goblin is offered off two activations" $ do
    goblin <- S.printingOf s registry "Zhur-Taa Goblin"
    one <- phyrexianAltarBoard s registry 1
    two <- phyrexianAltarBoard s registry 2
    let offered board =
          let (withSpell, oid) = S.handOne goblin board
           in any (S.isCastOf oid) (Action.legalActions S.alice withSpell)
    Spec.assertBool s (not (offered one)) "one creature cannot pay {R}{G}"
    Spec.assertBool s (offered two) "two creatures can"

-- Alice's Phyrexian Altar and `victims` Goblin Pikers.
phyrexianAltarBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Int -> m GameState.GameState
phyrexianAltarBoard s registry victims = do
  altar <- S.printingOf s registry "Phyrexian Altar"
  piker <- S.printingOf s registry "Goblin Piker"
  pure (foldr (\p gs -> snd (S.addPermanent p S.alice gs)) (Setup.emptyGame S.bothPlayers) (altar : replicate victims piker))

-- Whether alice could pay one mana of each of these colors off this board.
paysColors :: [Color.Color] -> GameState.GameState -> Bool
paysColors colors =
  Mana.canPay Cost.manaActivations S.alice (ManaCost.MkManaCost (fmap (ManaSymbol.OfType . ManaType.Colored) colors))

-- CR 601.2g before CR 601.2h, on a mana ability whose activation cost holds
-- MANA: Transmogrant Altar, "{B}, {T}, Sacrifice a creature: Add
-- {C}{C}{C}" ({3} Artifact). CR 602.2b routes an activation cost through rule
-- 601.2b-i, so the mana window opens BEFORE the cost is paid, and the creature
-- the cost eats is still there to be tapped for mana first.
--
-- The board is what makes the two orders differ, and it is built to leave no
-- other way through: Birds of Paradise is at once the only source of the {B} and
-- the only legal sacrifice, and alice controls no land. A Swamp, a second Birds
-- or a second creature would let BOTH orders succeed and prove nothing.
transmograntAltarSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
transmograntAltarSpec s registry = Spec.describe s "Transmogrant Altar" $ do
  Spec.it s "CR 601.2g the mana window opens before CR 601.2h spends the source it needs" $ do
    altar <- S.printingOf s registry "Transmogrant Altar"
    birds <- S.printingOf s registry "Birds of Paradise"
    let (altarId, g1) = S.addPermanent altar S.alice (Setup.emptyGame S.bothPlayers)
        (birdsId, g2) = S.addPermanent birds S.alice g1
        board = g2 {GameState.phase = Phase.PrecombatMain, GameState.remaining = Seq.empty}
        after = snd (State.evalState (Engine.runGame (takesAltarOnce altarId birdsId) board Engine.priorityLoop) (0 :: Int))
    Spec.assertEqWith s "CR 601.2g the Birds pays the {B} first, so the Altar's three colorless reach her pool" (poolTypes S.alice after) [ManaType.Colorless, ManaType.Colorless, ManaType.Colorless]
    Spec.assertEqWith s "CR 601.2h and the same Birds is then what the sacrifice took" (S.creaturesInPlay S.alice after) 0
    Spec.assertEqWith s "leaving the Altar itself as the one tapped permanent she still controls" (S.tappedCount S.alice after) 1

  -- CR 605.3a's in-payment window in full: a mana ability whose OWN cost holds
  -- mana may be activated inside it. Chromatic Star ("{1}, {T}, Sacrifice this
  -- artifact: Add one mana of any color") is that ability, the Plains pays its
  -- {1}, and the colour it mints is the Altar's {B} -- a chain no board could
  -- reach while the window offered only mana-free routes.
  --
  -- CR 605.3c is what still bounds it: the Altar's mana ability is
  -- mid-activation, so its own window and every window nested inside it are
  -- closed to that ability -- and it is the Altar's only one, so to the Altar.
  --
  -- The pool assertion comes FIRST and is the gameplay one. Under the narrowed
  -- window the payment simply fails and CR 601.2h leaves the pool empty; the
  -- library is stocked because the Star's death trigger draws (CR 104.3c).
  Spec.it s "CR 605.3a a mana ability whose cost holds mana may be activated inside the window" $ do
    altar <- S.printingOf s registry "Transmogrant Altar"
    star <- S.printingOf s registry "Chromatic Star"
    piker <- S.printingOf s registry "Goblin Piker"
    plains <- S.printingOf s registry "Plains"
    let (altarId, g1) = S.addPermanent altar S.alice (Setup.emptyGame S.bothPlayers)
        (starId, g2) = S.addPermanent star S.alice g1
        (_, g3) = S.addPermanent piker S.alice g2
        (plainsId, g4) = S.addPermanent plains S.alice g3
        (_, g5) = S.addLibraryCard piker S.alice g4
        board = g5 {GameState.phase = Phase.PrecombatMain, GameState.remaining = Seq.empty}
        after = snd (State.evalState (Engine.runGame (takesAltarOnce altarId starId) board Engine.priorityLoop) (0 :: Int))
        asked = snd (State.execState (Engine.runGame (recordsSources altarId starId) board Engine.priorityLoop) (0 :: Int, []))
    Spec.assertEqWith s "CR 602.2b the Star's any-colour mana pays the {B}, so the Altar's three colorless reach her pool" (poolTypes S.alice after) [ManaType.Colorless, ManaType.Colorless, ManaType.Colorless]
    Spec.assertEqWith s "CR 605.3a the Altar's window offers the Star, and the Star's own window then offers the Plains" asked [[starId, plainsId], [plainsId]]

  -- The window's own candidate list where nothing nested is on offer. CR 605.3c
  -- is the whole of the narrowing now: the Birds is mana-free and the Altar's
  -- only mana ability is mid-activation, so the Birds is the only candidate.
  -- Recorded rather than inferred, since the pool is the same whichever source
  -- the answerer would have declined.
  Spec.it s "CR 605.3a the Altar's own window offers the Birds and not the Altar" $ do
    altar <- S.printingOf s registry "Transmogrant Altar"
    birds <- S.printingOf s registry "Birds of Paradise"
    let (altarId, g1) = S.addPermanent altar S.alice (Setup.emptyGame S.bothPlayers)
        (birdsId, g2) = S.addPermanent birds S.alice g1
        board = g2 {GameState.phase = Phase.PrecombatMain, GameState.remaining = Seq.empty}
        asked = snd (State.execState (Engine.runGame (recordsSources altarId birdsId) board Engine.priorityLoop) (0 :: Int, []))
    Spec.assertEqWith s "one window, and the Birds is the only source its {B} may come from" asked [[birdsId]]

  -- CR 118.3 at the OFFER. Two boards differing in ONE thing -- the Swamp -- so
  -- the refusal cannot be about the sacrifice, the tap or the sickness rules,
  -- each of which is satisfied on both. NOT CR 118.6, whose "unpayable cost" is
  -- an object with no mana cost at all and whose activation is a legal action.
  Spec.it s "CR 118.3 the activation is offered only where its own mana part is payable" $ do
    altar <- S.printingOf s registry "Transmogrant Altar"
    piker <- S.printingOf s registry "Goblin Piker"
    swamp <- S.printingOf s registry "Swamp"
    let (altarId, withSwamp, _) = altarSupplyBoard altar piker (Just swamp) Nothing
        (_, noSwamp, _) = altarSupplyBoard altar piker Nothing Nothing
        offers gs = filter (== Action.Type.ActivateManaAbility altarId) (Action.legalActions S.alice gs)
    Spec.assertEqWith s "with a Swamp to pay the {B}, CR 605.3a offers the Altar" (length (offers withSwamp)) 1
    Spec.assertEqWith s "with none, CR 118.3 leaves nothing to offer" (length (offers noSwamp)) 0

  -- The SUPPLY half. Mana.supplyCapacity counts the Altar's {C}{C}{C} as supply
  -- AND its {B} as a demand the same board must serve, so the net is two mana.
  --
  -- A VECTOR and not one assertion, because three implementations agree on any
  -- single generic cost. The board's supply is the Swamp's {B} plus the Altar's
  -- three colorless and its demand is the Altar's own {B}: today's zero-supply
  -- reading stops at {1}, the net reading reaches {3}, and a gross reading that
  -- counted the yield without the {B} would reach {4}. So the {3} separates this
  -- from the understatement and the refused {4} from the overstatement.
  Spec.it s "CR 601.2g the Altar's yield is supply the cast gate counts, net of the {B} it eats" $ do
    altar <- S.printingOf s registry "Transmogrant Altar"
    piker <- S.printingOf s registry "Goblin Piker"
    swamp <- S.printingOf s registry "Swamp"
    splitter <- S.printingOf s registry "Bonesplitter"
    crucible <- S.printingOf s registry "Crucible of Worlds"
    statue <- S.printingOf s registry "Jade Statue"
    ancestor <- S.printingOf s registry "Disowned Ancestor"
    let holding printing = case altarSupplyBoard altar piker (Just swamp) (Just printing) of
          (_, board, Just oid) -> S.castable S.alice oid board
          (_, _, Nothing) -> False
    Spec.assertBool s (holding crucible) "CR 602.2b the Swamp pays the Altar's {B} and the {C}{C}{C} it adds pays a {3}"
    Spec.assertBool s (not (holding statue)) "CR 118.3 and not a {4}: the {B} the Altar eats is a demand the same board must serve"
    -- The {B} is the DECLINED half: the one Swamp cannot both pay the Altar and
    -- pay this spell, so the only board that casts it is the one that never
    -- activates the Altar at all. CR 605.3a offers the window; it does not
    -- oblige anyone to use it.
    Spec.assertBool s (holding ancestor) "CR 605.3a and a {B} she casts by declining the Altar, the one Swamp being both payments"
    Spec.assertBool s (holding splitter) "and a {1} the Swamp alone covers is still castable"

  -- A CHAIN of mana-eating routes. CR 605.3c orders them -- an ability cannot be
  -- activated again before it has resolved -- so one activation's yield is in the
  -- pool (CR 106.4) before the next one's cost is paid (CR 601.2g, CR 601.2h),
  -- and the Swamp's {B} buys the Altar's three colorless, which buy Coal Golem's
  -- ("{3}, Sacrifice this creature: Add {R}{R}{R}") three red. Neither route
  -- carries {T}, so CR 302.6 gates nothing and the Golem is spendable the turn it
  -- arrives.
  --
  -- A VECTOR of three spells, because any single generic cost leaves the chained
  -- reading and the one-step reading agreeing. The one-step board reaches the
  -- Altar's {C}{C}{C} and no red at all; the chain reaches {R}{R}{R}; and both
  -- stop at three mana. So the red spell separates the two, the {3} says the
  -- board is not simply broken, and the {4} catches a reading that forgot to
  -- charge the links their own costs.
  Spec.it s "CR 605.3c one mana ability's yield pays the next one's cost" $ do
    altar <- S.printingOf s registry "Transmogrant Altar"
    golem <- S.printingOf s registry "Coal Golem"
    piker <- S.printingOf s registry "Goblin Piker"
    swamp <- S.printingOf s registry "Swamp"
    wall <- S.printingOf s registry "Wall of Stone"
    crucible <- S.printingOf s registry "Crucible of Worlds"
    statue <- S.printingOf s registry "Jade Statue"
    let board = altarChainBoard altar golem piker swamp
        casts printing =
          let (oid, held) = S.addHandCard printing S.alice board
           in S.castable S.alice oid held
    Spec.assertBool s (casts wall) "CR 605.3c the {B} buys {C}{C}{C}, which buy {R}{R}{R}, which pay Wall of Stone's {1}{R}{R}"
    Spec.assertBool s (casts crucible) "and the same board still pays a plain {3}"
    Spec.assertBool s (not (casts statue)) "CR 118.3 but not a {4}: every link charges its own cost, so three mana is the ceiling"

-- The board Pawl.Benchmark's payable group times: every shape that keeps a source
-- plural in payableResolutions' search at once. Treasonous Ogre pays life,
-- Springleaf Drum's tap contends with every Llanowar Elves, and Transmogrant
-- Altar and Chromatic Star eat mana. Each assertion sits on a boundary of the
-- relaxation that refuses a cost before the search, so a relaxation tighter
-- than the boards reddens the payable half of a pair.
pluralBoardSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
pluralBoardSpec s registry = Spec.describe s "plural sources" $ do
  -- CR 119.4 holds the two Ogres to one life total: six activations between
  -- them at 20 life, where each alone could take six.
  Spec.it s "CR 119.4 two Treasonous Ogres share one life total" $ do
    ogre <- S.printingOf s registry "Treasonous Ogre"
    let (_, g1) = S.addPermanent ogre S.alice (Setup.emptyGame S.bothPlayers)
        (_, gs) = S.addPermanent ogre S.alice g1
        reds n = ManaCost.MkManaCost (replicate n (ManaSymbol.OfType (ManaType.Colored Color.Red)))
    Spec.assertBool s (Mana.canPay Cost.manaActivations S.alice (reds 6) gs) "eighteen life buys {R}{R}{R}{R}{R}{R}"
    Spec.assertBool s (not (Mana.canPay Cost.manaActivations S.alice (reds 7) gs)) "and a seventh would cost 21"

  -- Six red from the Ogres, four green from the Elves, one from the Drum tapping
  -- an Ogre, and the Altar's {C}{C}{C} net of its {B} (CR 602.2b); the Star's
  -- {1} buys back exactly the one mana it makes. The Drum and the Star are the
  -- only blue.
  Spec.it s "CR 118.3 the plural board pays thirteen and two blue, and no more" $ do
    ogre <- S.printingOf s registry "Treasonous Ogre"
    drum <- S.printingOf s registry "Springleaf Drum"
    elves <- S.printingOf s registry "Llanowar Elves"
    altar <- S.printingOf s registry "Transmogrant Altar"
    star <- S.printingOf s registry "Chromatic Star"
    let gs = Foldable.foldl' (\board printing -> snd (S.addPermanent printing S.alice board)) (Setup.emptyGame S.bothPlayers) ([ogre, ogre, drum, altar, star] <> replicate 4 elves)
        pays = (\cost -> Mana.canPay Cost.manaActivations S.alice cost gs) . ManaCost.MkManaCost
        blues n = replicate n (ManaSymbol.OfType (ManaType.Colored Color.Blue))
    Spec.assertBool s (pays [ManaSymbol.Generic 13]) "CR 602.2b a {13}, every source at its best"
    Spec.assertBool s (not (pays [ManaSymbol.Generic 14])) "CR 118.3 but not a {14}"
    Spec.assertBool s (pays (blues 2)) "CR 106.1a the Drum and the Star make {U}{U}"
    Spec.assertBool s (not (pays (blues 3))) "CR 118.3 and nothing makes a third {U}"

-- alice, active, in her precombat main phase: one Transmogrant Altar, one Goblin
-- Piker for the sacrifice to take, and optionally a Swamp to pay the {B} and a
-- pair of spells in her hand. Returns the Altar and whichever spells were dealt.
altarSupplyBoard :: Printing.Printing -> Printing.Printing -> Maybe Printing.Printing -> Maybe Printing.Printing -> (ObjectId.ObjectId, GameState.GameState, Maybe ObjectId.ObjectId)
altarSupplyBoard altar piker swamp spell =
  let (altarId, g1) = S.addPermanent altar S.alice (Setup.emptyGame S.bothPlayers)
      (_, g2) = S.addPermanent piker S.alice g1
      g3 = case swamp of
        Nothing -> g2
        Just printing -> snd (S.addPermanent printing S.alice g2)
      -- S.handOne REPLACES the hand, so one spell per board and the pair of
      -- boards below differ in that one card alone.
      (g4, held) = case spell of
        Nothing -> (g3, Nothing)
        Just printing -> let (g3a, oid) = S.handOne printing g3 in (g3a, Just oid)
   in (altarId, g4 {GameState.phase = Phase.PrecombatMain, GameState.remaining = Seq.empty}, held)

-- alice, active, in her precombat main phase, with an empty pool: one Swamp, one
-- Transmogrant Altar, one Coal Golem and one Goblin Piker. Two mana-eating routes
-- and one creature each may claim -- the Altar takes the Piker and the Golem
-- takes itself -- so CR 118.3's joint payability admits both at once.
altarChainBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> GameState.GameState
altarChainBoard altar golem piker swamp =
  let (_, g1) = S.addPermanent altar S.alice (Setup.emptyGame S.bothPlayers)
      (_, g2) = S.addPermanent golem S.alice g1
      (_, g3) = S.addPermanent piker S.alice g2
      (_, g4) = S.addPermanent swamp S.alice g3
   in g4 {GameState.phase = Phase.PrecombatMain, GameState.remaining = Seq.empty}

-- Activates the ALTAR and nothing else, pays its {B} off the Birds, and asks the
-- Birds for black. Never the Birds at priority: floating the {B} before the
-- ability is announced would let both payment orders pay out of the pool, which
-- is the collapse this case is built to avoid. Never the Altar's second ability
-- either -- that one makes a token and is no mana ability (CR 605.1a).
takesAltar :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
takesAltar altarId birdsId p = case p of
  Prompt.ChooseAction _ _ actions -> case filter (== Action.Type.ActivateManaAbility altarId) actions of
    h : _ -> h
    [] -> Action.Type.Pass
  Prompt.ChooseManaSource _ _ candidates -> Just (if elem birdsId (NonEmpty.toList candidates) then birdsId else NonEmpty.head candidates)
  Prompt.ChooseExtraManaSource {} -> Nothing
  _ -> prefersColor Color.Black p

-- takesAltar recording every in-payment source prompt, and taking the Altar only
-- until one has been asked -- the once-guard takesAltarOnce spells with a count.
recordsSources :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> State.State (Int, [[ObjectId.ObjectId]]) r
recordsSources altarId birdsId p = case p of
  Prompt.ChooseAction {} -> do
    (taken, _) <- State.get
    State.modify' (\(n, seen) -> (n + 1, seen))
    pure (if taken == 0 then takesAltar altarId birdsId p else Action.Type.Pass)
  Prompt.ChooseManaSource _ _ candidates -> do
    State.modify' (\(n, seen) -> (n, seen <> [NonEmpty.toList candidates]))
    pure (takesAltar altarId birdsId p)
  _ -> pure (takesAltar altarId birdsId p)

-- takesAltar allowed at ONE priority prompt. An activation that fails leaves the
-- board exactly as it was, so a greedy answerer would be offered it again
-- forever -- and the mutation this case exists to catch (components before mana)
-- is exactly that shape, which would hang rather than fail.
takesAltarOnce :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> State.State Int r
takesAltarOnce altarId birdsId p = case p of
  Prompt.ChooseAction {} -> do
    taken <- State.get
    State.modify' (+ 1)
    pure (if taken == 0 then takesAltar altarId birdsId p else Action.Type.Pass)
  _ -> pure (takesAltar altarId birdsId p)

-- CR 118.1 as a cost that RETURNS the object it is on: Grinning Ignus, "{R},
-- Return this creature to its owner's hand: Add {C}{C}{R}. Activate only as a
-- sorcery." CR 601.2f's list of what a cost may include ends in "and so on" and
-- never names returning, so this is a cost by CR 118.1's general reading -- "an
-- action or payment necessary to take another action" -- exactly as
-- CostComponent.ExileThisFromGraveyard is.
--
-- The board is one Ignus and one Mountain and nothing else. The Mountain is the
-- only source of the ability's own {R}, so the mana window has exactly one
-- payer; a second red source would make the source prompt ambiguous and prove
-- nothing more. PrecombatMain with an empty stack and alice active is CR 307.5's
-- window, which the printed "activate only as a sorcery" rider requires.
--
-- The DESTINATION is the whole discriminator. A payment copied from
-- CostComponent.SacrificeThis reaches the same pool and the same empty
-- battlefield; only the zone the Ignus lands in tells the two apart, so the hand
-- count is asserted before either.
grinningIgnusSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
grinningIgnusSpec s registry = Spec.describe s "Grinning Ignus" $ do
  Spec.it s "CR 118.1 the cost returns the source to its owner's hand, and the ability still adds its mana" $ do
    ignus <- S.printingOf s registry "Grinning Ignus"
    mountain <- S.printingOf s registry "Mountain"
    let (ignusId, board) = ignusBoard ignus mountain
        after = snd (State.evalState (Engine.runGame (takesIgnusOnce ignusId) board Engine.priorityLoop) (0 :: Int))
    Spec.assertEqWith s "CR 118.1 the cost put the Ignus in its owner's hand" (S.handSize S.alice after) 1
    Spec.assertEqWith s "CR 400.7 through Event.changeZone, so the move is a recorded event bound for the hand" (fmap ZoneChange.to (filter ((== ignusId) . ZoneChange.departed) (S.zoneChangesOf after))) [Zone.Hand]
    Spec.assertEqWith s "and off the battlefield it left" (S.creaturesInPlay S.alice after) 0
    Spec.assertEqWith s "CR 602.2b the activation still resolved and added {C}{C}{R}" (poolTypes S.alice after) [ManaType.Colorless, ManaType.Colorless, ManaType.Colored Color.Red]

  -- CR 307.5's rider on the MANA path. riderWindowSpec above reaches
  -- ActivationRestriction.DuringPhase there with a synthetic, and SorcerySpeed
  -- reached the mana path with no card at all -- Bonesplitter declares it on an
  -- equip ability, which CR 605.1a makes no mana ability. Two boards differing in
  -- the PHASE alone, so the refusal cannot be about the {R}, the return or the
  -- sickness rules.
  Spec.it s "CR 307.5 the sorcery-speed rider offers the ability only in her main phase" $ do
    ignus <- S.printingOf s registry "Grinning Ignus"
    mountain <- S.printingOf s registry "Mountain"
    let (ignusId, inMain) = ignusBoard ignus mountain
        inUpkeep = inMain {GameState.phase = Phase.Beginning BeginningStep.Upkeep}
        offers gs = filter (== Action.Type.ActivateManaAbility ignusId) (Action.legalActions S.alice gs)
    Spec.assertEqWith s "in her precombat main phase CR 605.3a offers it" (length (offers inMain)) 1
    Spec.assertEqWith s "in her upkeep CR 307.5 leaves nothing to offer" (length (offers inUpkeep)) 0

  -- The supply walk's ACYCLICITY guard, and the Ignus is the pool's one board
  -- that can show it: its "{R}, Return this creature to its owner's hand: Add
  -- {C}{C}{R}" yields the very type its own activation cost eats. A supply model
  -- that let a route's yield serve the route's own demand would read one Ignus on
  -- an empty board as two spare colorless -- a {1} castable off no land at all.
  --
  -- CR 106.4 is why it may not: the mana an ability adds reaches the pool when
  -- the ability RESOLVES, and CR 601.2g's window is before CR 601.2h's payment,
  -- so the {R} is not there to pay the {R}. CR 605.3c says the same thing from
  -- the other side -- the ability cannot be activated again before it resolves.
  --
  -- The MOUNTAIN is the one difference between the two boards, so the refusal
  -- cannot be about the phase, the return, the rider or the sickness rules, each
  -- of which is satisfied on both. It has to be built with NO land: with one the
  -- two readings agree and the case proves nothing.
  Spec.it s "CR 106.4 the Ignus's own yield is no supply for the {R} its activation eats" $ do
    ignus <- S.printingOf s registry "Grinning Ignus"
    mountain <- S.printingOf s registry "Mountain"
    splitter <- S.printingOf s registry "Bonesplitter"
    let (_, alone) = S.addPermanent ignus S.alice (Setup.emptyGame S.bothPlayers)
        withLand = snd (S.addPermanent mountain S.alice alone)
        holding gs =
          let (board, oid) = S.handOne splitter (gs {GameState.phase = Phase.PrecombatMain, GameState.remaining = Seq.empty})
           in S.castable S.alice oid board
    Spec.assertBool s (not (holding alone)) "CR 605.3c the {R} it would add is not there to pay the {R} it costs"
    Spec.assertBool s (holding withLand) "and a Mountain is what makes the same {1} castable"

  -- CR 602.2a is CR 601.2a's rule for an ACTIVATION -- the ability goes on the
  -- stack, and only then does CR 602.2b send the cost through CR 601.2f-h -- so
  -- the same gate, reached by the other road, must count the Ignus the same way.
  -- Maskwood Nexus's "{3}, {T}: Create a 2/2 blue Shapeshifter creature token
  -- with changeling" is the producer: an ARTIFACT, so its {T} meets no CR 302.6
  -- gate, and three generic mana is the same Mountain-plus-Ignus total again.
  Spec.it s "CR 602.2a nor is an activation, the ability going on the stack before CR 602.2b pays for it" $ do
    ignus <- S.printingOf s registry "Grinning Ignus"
    mountain <- S.printingOf s registry "Mountain"
    nexus <- S.printingOf s registry "Maskwood Nexus"
    let (ignusId, board) = ignusBoard ignus mountain
        (nexusId, held) = S.addPermanent nexus S.alice board
        floated = snd (State.evalState (Engine.runGame (takesIgnusOnce ignusId) held Engine.priorityLoop) (0 :: Int))
        offers gs = filter (isActivationOf nexusId) (Action.legalActions S.alice gs)
    Spec.assertEqWith s "CR 602.2a the Ignus is no supply for an activation either" (length (offers held)) 0
    Spec.assertEqWith s "CR 605.3a and the same ability is offered once that mana is floated" (length (offers floated)) 1

-- Is this action an activation of that object's ability? The activation half of
-- Pawl.Support.isCastOf, local because CR 605.3b's mana abilities ride a
-- different constructor and no other spec wants the distinction.
isActivationOf :: ObjectId.ObjectId -> Action.Type.Action -> Bool
isActivationOf oid action = case action of
  Action.Type.Activate o _ -> o == oid
  _ -> False

-- alice, active, in her precombat main phase and going nowhere: one Grinning
-- Ignus and one Mountain, and nothing else on the board or in her hand.
ignusBoard :: Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, GameState.GameState)
ignusBoard ignus mountain =
  let (ignusId, g1) = S.addPermanent ignus S.alice (Setup.emptyGame S.bothPlayers)
      (_, g2) = S.addPermanent mountain S.alice g1
   in (ignusId, g2 {GameState.phase = Phase.PrecombatMain, GameState.remaining = Seq.empty})

-- CR 118.13a on a MANA ability's own activation cost. A mana ability is an
-- activated ability (CR 605.1a) and CR 602.2b sends its activation cost through
-- CR 601.2b, so a symbol payable in several ways is the PLAYER's to announce as
-- the ability is activated -- not the fixed order Mana.resolutions would
-- otherwise settle it in.
--
-- Mystic Gate is the producer: "{T}: Add {C}" and "{W/U}, {T}: Add {W}{W},
-- {W}{U}, or {U}{U}" (Land). Both halves of the {W/U} are payable out of the
-- seeded pool and they leave DIFFERENT pools behind, which is what makes the
-- announcement observable at all.
--
-- ONE board, two runs differing only in the answer to CR 601.2b's question, so
-- neither the seeded pool nor the yield can be what separates them. The Gate is
-- the only permanent, and CR 605.3c takes only the {W/U} ability itself off CR
-- 601.2g's window inside the payment -- the Gate's "{T}: Add {C}" IS offered
-- there, and this fixture declines it (S.identityAnswer), so the {W/U} is paid
-- out of what is floating.
mysticGateSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
mysticGateSpec s registry = Spec.describe s "Mystic Gate" $ do
  Spec.it s "CR 118.13a the player announces the {W/U} in a mana ability's own activation cost" $ do
    gate <- S.printingOf s registry "Mystic Gate"
    let board = gateBoard gate
        run half = S.runPureWith (takesGate [whiteType, blueType] (Just half)) (snd board) (S.tapForMana (fst board))
        (paidWhite, afterWhite) = run whiteType
        (paidBlue, afterBlue) = run blueType
    Spec.assertEqWith s "the white half announced, so the blue unit is what is still floating beside the {W}{U}" (poolTypes S.alice afterWhite) [blueType, whiteType, blueType]
    Spec.assertEqWith s "the blue half announced, and the white unit is left instead" (poolTypes S.alice afterBlue) [whiteType, whiteType, blueType]
    Spec.assertEqWith s "CR 602.2b both activations really paid" (paidWhite, paidBlue) (True, True)

  -- The elision side of the same invariant, on the SAME card: the Gate's other
  -- ability is "{T}: Add {C}", whose cost holds no symbol payable two ways, so CR
  -- 118.13a leaves nothing to ask. Two runs off one board again, differing only
  -- in which yield is taken.
  Spec.it s "CR 118.13a and asks nothing of a mana ability whose cost prints no such symbol" $ do
    gate <- S.printingOf s registry "Mystic Gate"
    let board = gateBoard gate
        counting :: [ManaType.ManaType] -> Prompt.Prompt r -> State.State Int r
        counting wanted p = case p of
          Prompt.AnnounceHybridHalf {} -> do
            State.modify' (+ 1)
            pure (takesGate wanted (Just whiteType) p)
          _ -> pure (takesGate wanted (Just whiteType) p)
        asked wanted = State.execState (Engine.runGame (counting wanted) (snd board) (S.tapForMana (fst board))) (0 :: Int)
    Spec.assertEqWith s "CR 118.13a the {C} route's cost prints no symbol payable two ways, so nothing is announced" (asked [ManaType.Colorless]) 0
    Spec.assertEqWith s "and the {W/U} route on the same board is asked once" (asked [whiteType, blueType]) 1

-- CR 605.3c narrows the ABILITY and not the permanent: "once a player begins to
-- activate a mana ability, THAT ABILITY can't be activated again until it has
-- resolved". Skyshroud Elf is the printing that tells the two readings apart --
-- "{T}: Add {G}." beside "{1}: Add {R} or {W}.", the second eating mana and not
-- tapping -- so the Elf's own {G} is what pays its {1}, and the whole point of
-- the card is a line a permanent-wide exclusion refuses.
--
-- ONE board and one activation, through Cost.tapForMana: CR 605.3b gives the
-- ability no stack object, so this is the narrowest path to both halves.
skyshroudElfSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
skyshroudElfSpec s registry = Spec.describe s "Skyshroud Elf" $ do
  Spec.it s "CR 605.3c the Elf's other mana ability pays for this one, the rule excluding the ability and not the permanent" $ do
    elf <- S.printingOf s registry "Skyshroud Elf"
    let (elfId, board) = S.addPermanent elf S.alice (Setup.emptyGame S.bothPlayers)
        ((paid, after), offers) = State.runState (Engine.runGame (takesElfRoute elfId) board (S.tapForMana elfId)) []
    -- The gameplay assertion: the {1} was paid by the Elf's OWN {T}, so the {G}
    -- is gone and the {R} is what is floating. An exclusion keyed to the
    -- permanent leaves the window nothing to offer and the pool empty.
    Spec.assertEqWith s "CR 605.3a the Elf's {G} paid its own {1}, and the {R} is what that turned into" (poolTypes S.alice after) [ManaType.Colored Color.Red]
    Spec.assertEqWith s "CR 602.2b the activation really paid" paid True
    -- The other half of the same rule, and the only level it is visible at: the
    -- ability BEING PAID FOR is off its own window, so the one route left to
    -- offer inside it is the {T}, and a single option is no question at all
    -- (Cost.chooseManaYield). A second entry here would be the {1} ability
    -- offered to pay for itself.
    Spec.assertEqWith s "CR 605.3c only the outer choice is asked: inside the payment the {1} ability is off its own window, leaving the {T} alone" offers [[[ManaType.Colored Color.Green], [ManaType.Colored Color.Red], [ManaType.Colored Color.White]]]

-- Takes the Elf's "{1}: Add {R}" at the FIRST yield question and its "{T}: Add
-- {G}" at every later one, recording the candidates of each. PINNED BY INDEX
-- rather than by what is payable: an answerer that hunted for a payable route
-- would find the {T} again after a mutation and repair the very choice this
-- proves. FILTERED, NOT BUILT, for Mana.optionYielding's reason.
takesElfRoute :: ObjectId.ObjectId -> Prompt.Prompt r -> State.State [[[ManaType.ManaType]]] r
takesElfRoute elfId p = case p of
  Prompt.ChooseManaYield _ _ _ candidates -> do
    seen <- State.get
    State.modify' (<> [fmap yieldTypes (NonEmpty.toList candidates)])
    let wanted = if null seen then [ManaType.Colored Color.Red] else [ManaType.Colored Color.Green]
    pure (Maybe.fromMaybe (NonEmpty.head candidates) (List.find ((==) wanted . yieldTypes) (NonEmpty.toList candidates)))
  -- The Elf is the only source the window has, and taking it is the line under
  -- test; an unrecognised id would read as declining (Cost.chooseSource).
  Prompt.ChooseManaSource _ _ candidates -> pure (List.find (elfId ==) (NonEmpty.toList candidates))
  _ -> pure (S.identityAnswer p)

-- The colours one offered route would add, in printed order.
yieldTypes :: ManaOption.ManaOption -> [ManaType.ManaType]
yieldTypes = fmap ManaUnit.manaType . Mana.yieldUnits

-- alice with one Mystic Gate and one white and one blue mana floating, and
-- nothing else anywhere. The pool is SEEDED rather than tapped for, so the {W/U}
-- is paid without CR 601.2g's window choosing a source -- what is under test is
-- the announcement, and a second land would make the source prompt the thing that
-- decided which colour went.
gateBoard :: Printing.Printing -> (ObjectId.ObjectId, GameState.GameState)
gateBoard gate =
  let (gateId, g1) = S.addPermanent gate S.alice (Setup.emptyGame S.bothPlayers)
      unit manaType = ManaUnit.MkManaUnit {ManaUnit.manaType = manaType, ManaUnit.tags = Set.empty, ManaUnit.retention = ManaRetention.Ordinary, ManaUnit.restriction = Nothing, ManaUnit.rider = Nothing, ManaUnit.spendTrigger = Nothing, ManaUnit.sourceChosenSubtype = Nothing, ManaUnit.sourceLastExiled = Nothing}
   in (gateId, Mana.addMana S.alice [unit whiteType, unit blueType] g1)

whiteType :: ManaType.ManaType
whiteType = ManaType.Colored Color.White

blueType :: ManaType.ManaType
blueType = ManaType.Colored Color.Blue

-- Takes the Gate's route whose YIELD is `wanted` and announces `half` for its
-- {W/U}. FILTERED, NOT BUILT: the yield is picked out of the offered options, so
-- an answer the source does not offer cannot mint mana, and the announcement is
-- pinned to one half rather than searched for -- an answerer that looked for a
-- payable half would repair the very choice this proves.
takesGate :: [ManaType.ManaType] -> Maybe ManaType.ManaType -> Prompt.Prompt r -> r
takesGate wanted half p = case p of
  Prompt.ChooseManaYield _ _ _ candidates ->
    let typesOf = fmap ManaUnit.manaType . Mana.yieldUnits
     in Maybe.fromMaybe (NonEmpty.head candidates) (List.find ((==) wanted . typesOf) (NonEmpty.toList candidates))
  Prompt.AnnounceHybridHalf _ _ _ _ offers -> Maybe.fromMaybe (NonEmpty.head offers) (List.find (\o -> Just o == half) (NonEmpty.toList offers))
  _ -> S.identityAnswer p

-- Takes the Ignus's mana ability at the FIRST priority prompt and passes at every
-- later one, takesAltarOnce's guard and for its reason: an activation that failed
-- would leave the board untouched and be offered again forever.
takesIgnusOnce :: ObjectId.ObjectId -> Prompt.Prompt r -> State.State Int r
takesIgnusOnce ignusId p = case p of
  Prompt.ChooseAction _ _ actions -> do
    taken <- State.get
    State.modify' (+ 1)
    pure $
      if taken == 0
        then case filter (== Action.Type.ActivateManaAbility ignusId) actions of
          h : _ -> h
          [] -> Action.Type.Pass
        else Action.Type.Pass
  _ -> pure (prefersColor Color.Red p)

-- CR 601.2f, reached by CR 602.2b: an ACTIVATION cost is adjusted like a spell's
-- mana cost, and CR 605.3b gives a mana ability no stack window for
-- Pawl.Engine.Activate to do it in -- so Cost.manaActivations and
-- Cost.tapForManaWith gather the adjustments themselves.
--
-- BOTH halves of CR 601.2f, one producer each, both pairings already printed:
--
--   * the REDUCTION -- Heartstone ("Activated abilities of creatures cost {1}
--     less to activate. This effect can't reduce the mana in that cost to less
--     than one mana") on Coal Golem ({5} Artifact Creature -- Golem, "{3},
--     Sacrifice this creature: Add {R}{R}{R}"). The Golem is a creature, so the
--     {3} is a {2}, and the floor at one mana is not reached.
--
--   * the INCREASE -- Suppression Field ("Activated abilities cost {2} more to
--     activate unless they're mana abilities"), whose "unless" is CR 605.1a's
--     classification and reaches this seam by NOT applying here at all. Zirda,
--     the Dawnwaker prints the same rider on the reduction side, and is read the
--     same way: its {2} spares the Golem's mana ability and reaches the
--     Brothers'.
--
--   * the ADDITIONAL COST -- Drought ("Activated abilities cost an additional
--     \"Sacrifice a Swamp\" to activate for each black mana symbol in their
--     activation costs") on Transmogrant Altar's "{B}, {T}, Sacrifice a
--     creature", whose one {B} buys one Swamp.
--
-- INDEPENDENT of the CR 601.2g/h order this path also carries (#1120): the
-- Golem's only component sacrifices ITSELF, which is a mana source for nothing
-- but the ability being activated, so both payment orders reach the same board.
activationAdjustmentSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
activationAdjustmentSpec s registry = Spec.describe s "CR 601.2f a mana ability's own activation cost" $ do
  -- TWO boards differing in the Heartstone alone. Exactly two Mountains is what
  -- makes the pair discriminate: {3} is one more than alice can pay and {2} is
  -- exactly what she can, so the reduction is the whole of the difference.
  Spec.it s "CR 601.2f a reduction reaches it, so Coal Golem activates for {2}" $ do
    golem <- S.printingOf s registry "Coal Golem"
    heartstone <- S.printingOf s registry "Heartstone"
    mountain <- S.printingOf s registry "Mountain"
    let (golemId, reduced) = golemBoard golem mountain (Just heartstone)
        (_, printed) = golemBoard golem mountain Nothing
        run gs = snd (State.evalState (Engine.runGame (takesSourceOnce golemId) gs Engine.priorityLoop) (0 :: Int))
        after = run reduced
        control = run printed
    Spec.assertEqWith s "CR 601.2f Heartstone makes the {3} a {2}, and two Mountains pay it" (poolTypes S.alice after) [ManaType.Colored Color.Red, ManaType.Colored Color.Red, ManaType.Colored Color.Red]
    Spec.assertEqWith s "CR 601.2h and the Golem sacrificed itself to do it" (S.creaturesInPlay S.alice after) 0
    Spec.assertEqWith s "both Mountains went" (S.tappedCount S.alice after) 2
    Spec.assertEqWith s "without the Heartstone the same board pays nothing" (poolTypes S.alice control) []
    Spec.assertEqWith s "so the Golem is still there" (S.creaturesInPlay S.alice control) 1
    Spec.assertEqWith s "and nothing was tapped in its name" (S.tappedCount S.alice control) 0

  -- The OFFER, asked of the same two boards: CR 605.3a offers an activation the
  -- payment can pay for and no other, so the gate and the payment have to read
  -- the same total (Cost.manaActivations and Cost.tapForManaWith, one gather).
  Spec.it s "CR 605.3a the offer is made against the reduced cost too" $ do
    golem <- S.printingOf s registry "Coal Golem"
    heartstone <- S.printingOf s registry "Heartstone"
    mountain <- S.printingOf s registry "Mountain"
    let (golemId, reduced) = golemBoard golem mountain (Just heartstone)
        (_, printed) = golemBoard golem mountain Nothing
        offers gs = length (filter (== Action.Type.ActivateManaAbility golemId) (Action.legalActions S.alice gs))
    Spec.assertEqWith s "reduced to {2}, the Golem is on the menu" (offers reduced) 1
    Spec.assertEqWith s "at its printed {3} it is not" (offers printed) 0

  -- CR 601.2f's OTHER half on the same seam. One Swamp does double duty -- it
  -- pays the {B} (CR 601.2g, before any cost is paid) and is then the Swamp the
  -- added component eats -- so what separates the two boards is the Drought and
  -- nothing else, and neither board can reach the other's answer by luck.
  Spec.it s "CR 601.2f an added component reaches it, so the Altar eats a Swamp" $ do
    altar <- S.printingOf s registry "Transmogrant Altar"
    piker <- S.printingOf s registry "Goblin Piker"
    swamp <- S.printingOf s registry "Swamp"
    drought <- S.printingOf s registry "Drought"
    let (altarId, taxed) = altarDroughtBoard altar piker swamp (Just drought)
        (_, untaxed) = altarDroughtBoard altar piker swamp Nothing
        run gs = snd (State.evalState (Engine.runGame (takesSourceOnce altarId) gs Engine.priorityLoop) (0 :: Int))
        after = run taxed
        control = run untaxed
        swampsLeft = S.countOnBattlefieldByName (S.printingName swamp) S.alice
    Spec.assertEqWith s "CR 601.2f the added \"Sacrifice a Swamp\" is paid, so the Swamp is gone" (swampsLeft after) 0
    Spec.assertEqWith s "and the activation still yielded its three colorless" (poolTypes S.alice after) [ManaType.Colorless, ManaType.Colorless, ManaType.Colorless]
    Spec.assertEqWith s "without the Drought the same Swamp survives" (swampsLeft control) 1
    Spec.assertEqWith s "having paid the same {B} for the same three colorless" (poolTypes S.alice control) [ManaType.Colorless, ManaType.Colorless, ManaType.Colorless]

  -- The GATE's half of the same addition, which the case above cannot separate:
  -- there a Swamp was on the board either way. Here the {B} comes off a Birds of
  -- Paradise and alice controls NO Swamp, so the added component is one CR 118.3
  -- leaves her unable to pay -- and the Drought is again the only difference
  -- between the two boards.
  Spec.it s "CR 118.3 an added component the board cannot pay takes the offer away" $ do
    altar <- S.printingOf s registry "Transmogrant Altar"
    piker <- S.printingOf s registry "Goblin Piker"
    birds <- S.printingOf s registry "Birds of Paradise"
    drought <- S.printingOf s registry "Drought"
    let (altarId, taxed) = altarDroughtBoard altar piker birds (Just drought)
        (_, untaxed) = altarDroughtBoard altar piker birds Nothing
        offers gs = length (filter (== Action.Type.ActivateManaAbility altarId) (Action.legalActions S.alice gs))
    Spec.assertEqWith s "with the Drought and no Swamp to give, the Altar is off the menu" (offers taxed) 0
    Spec.assertEqWith s "without it the same board offers the same activation" (offers untaxed) 1

  -- The SUPPLY walk's half of the same addition, which the offer case above
  -- cannot reach: that one asks Cost.activationManaSourcesGiven, which has always
  -- measured the printed cost, while a CAST is gated against what
  -- Mana.supplyCapacity counts. The two used to disagree, the supply walk having
  -- asked about a mana part it had emptied -- and an emptied mana part is what CR
  -- 601.2f's per-coloured-symbol scale counts its symbols in.
  --
  -- The Birds is the black source again, so alice controls no Swamp to give, and
  -- the Drought is once more the only difference between the two boards.
  Spec.it s "CR 601.2f the supply walk charges the added component too" $ do
    altar <- S.printingOf s registry "Transmogrant Altar"
    piker <- S.printingOf s registry "Goblin Piker"
    birds <- S.printingOf s registry "Birds of Paradise"
    drought <- S.printingOf s registry "Drought"
    crucible <- S.printingOf s registry "Crucible of Worlds"
    let (_, taxed) = altarDroughtBoard altar piker birds (Just drought)
        (_, untaxed) = altarDroughtBoard altar piker birds Nothing
        casts gs =
          let (crucibleId, held) = S.addHandCard crucible S.alice gs
           in S.castable S.alice crucibleId held
    Spec.assertBool s (not (casts taxed)) "CR 118.3 the Altar is no supply, so the Birds' one mana is the whole board and {3} is out of reach"
    Spec.assertBool s (casts untaxed) "without the Drought the Birds pays the {B} and the Altar's three colorless pay the {3}"

  -- CR 605.1a's rider on the INCREASE half, which the two halves above have no
  -- producer for: Suppression Field ("Activated abilities cost {2} more to
  -- activate unless they're mana abilities", checked against Scryfall) is the
  -- pool's, and its "unless" is a fact about the ABILITY rather than about the
  -- permanent the increase's Filter is asked of.
  --
  -- The MANA half is the proving one, and it is proved at the PAYMENT: the offer
  -- gate builds `plusComponents adjustments printedCost`, which adds only
  -- components, and manaPartPayable then short-circuits on a Mountain's empty
  -- mana part -- so the tap is on the menu whether the {2} reaches it or not, and
  -- a case enumerating legal actions would read the same either way. What tells
  -- the two readings apart is whether the tap PAYS: a lone {2} the Mountain has
  -- no way to produce leaves the pool empty.
  --
  -- Three Mountains and not one, so that the taxed reading has a board it could
  -- plausibly pay {2} on -- except that every Mountain is taxed the same {2}, so
  -- there is no mana anywhere on it.
  Spec.it s "CR 605.1a an increase that spares mana abilities does not reach one" $ do
    field <- S.printingOf s registry "Suppression Field"
    brothers <- S.printingOf s registry "Brothers of Fire"
    mountain <- S.printingOf s registry "Mountain"
    let (mountainId, _, taxed) = suppressionFieldBoard brothers mountain (Just field)
        (_, _, printed) = suppressionFieldBoard brothers mountain Nothing
        run gs = snd (State.evalState (Engine.runGame (takesSourceOnce mountainId) gs Engine.priorityLoop) (0 :: Int))
    Spec.assertEqWith s "CR 605.1a the Field's {2} does not reach the Mountain's mana ability, so its {R} floats" (poolTypes S.alice (run taxed)) [ManaType.Colored Color.Red]
    Spec.assertEqWith s "the same tap on the same board without the Field" (poolTypes S.alice (run printed)) [ManaType.Colored Color.Red]
    Spec.assertEqWith s "one Mountain paid for it, and the other two were not asked to" (S.tappedCount S.alice (run taxed)) 1

  -- The other side of the same sentence, and the reason the case above is not
  -- passing because the card was transcribed as a {0}: Brothers of Fire ("{1}{R}{R},
  -- {T}: Brothers of Fire deals 1 damage to any target") is no mana ability, so
  -- the same Field taxes it to {3}{R}{R} and three Mountains stop paying for it.
  -- Here in ManaSpec rather than in Pawl.PlayerEffectSpec because it is the half
  -- of ONE card the case above rests on.
  Spec.it s "CR 601.2f the same increase does reach an ability that is not one" $ do
    field <- S.printingOf s registry "Suppression Field"
    brothers <- S.printingOf s registry "Brothers of Fire"
    mountain <- S.printingOf s registry "Mountain"
    let (_, brothersId, taxed) = suppressionFieldBoard brothers mountain (Just field)
        (_, _, printed) = suppressionFieldBoard brothers mountain Nothing
        offers gs = length (filter (isActivationOf brothersId) (Action.legalActions S.alice gs))
    Spec.assertEqWith s "CR 601.2f three Mountains cannot pay the taxed {3}{R}{R}" (offers taxed) 0
    Spec.assertEqWith s "and pay the printed {1}{R}{R} on the same board without the Field" (offers printed) 1

  -- CR 605.1a's rider on the REDUCTION half, the sibling of the two cases above:
  -- Zirda, the Dawnwaker ("Abilities you activate that aren't mana abilities cost
  -- {2} less to activate. This effect can't reduce the mana in that cost to less
  -- than one mana", checked against Scryfall) prints it, and its "that aren't
  -- mana abilities" is a fact about the ABILITY rather than about the permanent
  -- the reduction's Filter is asked of. Scryfall o:"aren't mana abilities cost",
  -- 2026-08-29, one hit -- Zirda; a second such printing would join it here.
  --
  -- Zirda's other two clauses are transcribed too: the Companion condition, proved
  -- by Pawl.CompanionSpec, and "{1}, {T}: Target creature can't block this turn",
  -- proved by Pawl.CombatSpec's StoredBlockRestriction group. Neither bears on the
  -- sentence this case is about.
  --
  -- Proved at the PAYMENT and not at the offer, for the reason the Field's case
  -- is: Cost.tapForManaWith is what folds the gathered reduction into the mana
  -- part. Two Mountains against the Golem's {3} is what makes the two readings
  -- differ -- spared, the {3} stands and no pair of Mountains pays it; reached,
  -- the {2} takes it to Zirda's one-mana floor and a single Mountain buys three
  -- red.
  Spec.it s "CR 605.1a a reduction that spares mana abilities does not reach one" $ do
    golem <- S.printingOf s registry "Coal Golem"
    zirda <- S.printingOf s registry "Zirda, the Dawnwaker"
    mountain <- S.printingOf s registry "Mountain"
    let (golemId, reduced) = golemBoard golem mountain (Just zirda)
        (_, printed) = golemBoard golem mountain Nothing
        run gs = snd (State.evalState (Engine.runGame (takesSourceOnce golemId) gs Engine.priorityLoop) (0 :: Int))
        golemsLeft = S.countOnBattlefieldByName (S.printingName golem) S.alice
    Spec.assertEqWith s "CR 605.1a Zirda's {2} does not reach the Golem's mana ability, so two Mountains cannot pay its {3} and nothing floats" (poolTypes S.alice (run reduced)) []
    Spec.assertEqWith s "so the Golem was never sacrificed to pay for it" (golemsLeft (run reduced)) 1
    Spec.assertEqWith s "and neither Mountain was tapped in its name" (S.tappedCount S.alice (run reduced)) 0
    Spec.assertEqWith s "the same tap on the same board without Zirda" (poolTypes S.alice (run printed)) []
    Spec.assertEqWith s "with the same Golem still on the battlefield" (golemsLeft (run printed)) 1

  -- The other side of the same sentence, and the reason the case above is not
  -- passing because Zirda was transcribed as a {0}: Brothers of Fire ("{1}{R}{R},
  -- {T}: Brothers of Fire deals 1 damage to any target") is no mana ability, so
  -- the same Zirda takes its {1}{R}{R} to {R}{R} -- CR 118.7a, the {2} reaching
  -- the one generic symbol and no further, which leaves two mana and never tests
  -- the floor -- and two Mountains start paying for it.
  Spec.it s "CR 601.2f the same reduction does reach an ability that is not one" $ do
    brothers <- S.printingOf s registry "Brothers of Fire"
    zirda <- S.printingOf s registry "Zirda, the Dawnwaker"
    mountain <- S.printingOf s registry "Mountain"
    let (brothersId, reduced) = brothersBoard brothers mountain (Just zirda)
        (_, printed) = brothersBoard brothers mountain Nothing
        offers gs = length (filter (isActivationOf brothersId) (Action.legalActions S.alice gs))
    Spec.assertEqWith s "CR 118.7a reduced to {R}{R}, two Mountains pay for the Brothers" (offers reduced) 1
    Spec.assertEqWith s "at the printed {1}{R}{R} the same two do not" (offers printed) 0

-- alice, active, in her precombat main phase: one Coal Golem, two Mountains, and
-- one reducer or not -- the Heartstone, whose sentence reaches the Golem's mana
-- ability, or Zirda, whose sentence spares it. Returns the Golem.
golemBoard :: Printing.Printing -> Printing.Printing -> Maybe Printing.Printing -> (ObjectId.ObjectId, GameState.GameState)
golemBoard golem mountain reducer =
  let (golemId, g1) = S.addPermanent golem S.alice (Setup.emptyGame S.bothPlayers)
      g2 = foldr (\_ gs -> snd (S.addPermanent mountain S.alice gs)) g1 [1 :: Int, 2]
      g3 = case reducer of
        Nothing -> g2
        Just printing -> snd (S.addPermanent printing S.alice g2)
   in (golemId, g3 {GameState.phase = Phase.PrecombatMain, GameState.remaining = Seq.empty})

-- alice, active, in her precombat main phase: three Mountains, one Brothers of
-- Fire, and the Suppression Field or not. Returns the first Mountain and the
-- Brothers -- one mana ability and one ability that is not one.
suppressionFieldBoard :: Printing.Printing -> Printing.Printing -> Maybe Printing.Printing -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
suppressionFieldBoard brothers mountain field =
  let (mountainId, g1) = S.addPermanent mountain S.alice (Setup.emptyGame S.bothPlayers)
      g2 = foldr (\_ gs -> snd (S.addPermanent mountain S.alice gs)) g1 [1 :: Int, 2]
      (brothersId, g3) = S.addPermanent brothers S.alice g2
      g4 = case field of
        Nothing -> g3
        Just printing -> snd (S.addPermanent printing S.alice g3)
   in (mountainId, brothersId, g4 {GameState.phase = Phase.PrecombatMain, GameState.remaining = Seq.empty})

-- alice, active, in her precombat main phase: one Brothers of Fire, two
-- Mountains, and Zirda or not. Returns the Brothers -- one ability that is no
-- mana ability, against a board one mana short of its printed cost.
brothersBoard :: Printing.Printing -> Printing.Printing -> Maybe Printing.Printing -> (ObjectId.ObjectId, GameState.GameState)
brothersBoard brothers mountain zirda =
  let (brothersId, g1) = S.addPermanent brothers S.alice (Setup.emptyGame S.bothPlayers)
      g2 = foldr (\_ gs -> snd (S.addPermanent mountain S.alice gs)) g1 [1 :: Int, 2]
      g3 = case zirda of
        Nothing -> g2
        Just printing -> snd (S.addPermanent printing S.alice g2)
   in (brothersId, g3 {GameState.phase = Phase.PrecombatMain, GameState.remaining = Seq.empty})

-- alice, active, in her precombat main phase: one Transmogrant Altar, one Goblin
-- Piker for the printed sacrifice, one `blackSource` for the {B} -- a Swamp
-- where the Drought's added cost is to be payable and a Birds of Paradise where
-- it is not -- and the Drought or not. Returns the Altar.
altarDroughtBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Maybe Printing.Printing -> (ObjectId.ObjectId, GameState.GameState)
altarDroughtBoard altar piker blackSource drought =
  let (altarId, g1) = S.addPermanent altar S.alice (Setup.emptyGame S.bothPlayers)
      (_, g2) = S.addPermanent piker S.alice g1
      (_, g3) = S.addPermanent blackSource S.alice g2
      g4 = case drought of
        Nothing -> g3
        Just printing -> snd (S.addPermanent printing S.alice g3)
   in (altarId, g4 {GameState.phase = Phase.PrecombatMain, GameState.remaining = Seq.empty})

-- Activates ONE named source, at ONE priority prompt, and answers the payment
-- window with whatever it offers. Once-only for takesAltarOnce's reason: an
-- activation that fails leaves the board as it was, so a greedy answerer would
-- be offered it again forever and a mutation would hang rather than fail.
takesSourceOnce :: ObjectId.ObjectId -> Prompt.Prompt r -> State.State Int r
takesSourceOnce sourceId p = case p of
  Prompt.ChooseAction _ _ actions -> do
    taken <- State.get
    State.modify' (+ 1)
    pure $ case (taken :: Int, filter (== Action.Type.ActivateManaAbility sourceId) actions) of
      (0, offer : _) -> offer
      _ -> Action.Type.Pass
  Prompt.ChooseManaSource _ _ candidates -> pure (Just (NonEmpty.head candidates))
  Prompt.ChooseExtraManaSource {} -> pure Nothing
  _ -> pure (S.identityAnswer p)

-- These printings on the battlefield under alice's control, untapped and settled.
alicePermanents :: [Printing.Printing] -> GameState.GameState
alicePermanents = foldr (\p gs -> snd (S.addPermanent p S.alice gs)) (Setup.emptyGame S.bothPlayers)

-- S.identityAnswer, recording the candidates of every Prompt.ChooseManaSource --
-- the offers CR 601.2g's window made while the cost was still uncovered. A
-- Prompt.ChooseExtraManaSource is a different question and is not recorded.
recordingManaSources :: Prompt.Prompt r -> State.State [[ObjectId.ObjectId]] r
recordingManaSources p = case p of
  Prompt.ChooseManaSource _ _ candidates -> do
    State.modify' (<> [NonEmpty.toList candidates])
    pure (Replay.defaultAnswer p)
  _ -> pure (S.identityAnswer p)

-- CR 106.12a's "is tapped for mana" and CR 605.4a's off-stack resolution, on the
-- cheapest printing that needs both: Wild Growth, "{G} Enchantment -- Aura /
-- Enchant land / Whenever enchanted land is tapped for mana, its controller adds
-- an additional {G}".
--
-- The Aura is ALICE's and the land is BOB's, which is the board CR 605.1b's
-- recipient clause needs: "its controller" is the land's controller, and CR
-- 109.5's "you" would answer the Aura's. On one seat the two readings agree and
-- the test would prove nothing.
wildGrowthSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
wildGrowthSpec s registry = Spec.describe s "Wild Growth" $ do
  Spec.it s "CR 106.12a tapping the enchanted land adds the Aura's mana to the LAND's controller" $ do
    (aliceForest, bobForest, _, board) <- wildGrowthBoard s registry
    let after = S.runPure S.identityAnswer board (S.tapForMana bobForest)
        -- The same board differing in exactly one thing: which Forest was
        -- tapped. Alice's carries no Aura, so it must add one and only one.
        unenchanted = S.runPure S.identityAnswer board (S.tapForMana aliceForest)
        -- CR 605.4a from the other side: the next time the game settles for
        -- priority it scans the very event this tap recorded, and the ability
        -- must NOT be gathered onto the stack there -- it has already resolved.
        settled = resolveDown (S.runPure S.identityAnswer after Engine.settleForPriority)
    Spec.assertEqWith s "CR 106.12a bob's pool holds the land's {G} and the Aura's additional one" (poolTypes S.bob after) [ManaType.Colored Color.Green, ManaType.Colored Color.Green]
    Spec.assertEqWith s "CR 106.4 alice, who controls the Aura, is given none of it" (poolTypes S.alice after) []
    Spec.assertEqWith s "CR 605.4a and the triggered mana ability never reached the stack" (length (GameState.stack after)) 0
    Spec.assertEqWith s "CR 605.4a and settling for priority does not place a second copy of it to resolve" (poolTypes S.bob settled) [ManaType.Colored Color.Green, ManaType.Colored Color.Green]
    Spec.assertEqWith s "nor leave anything of it on the stack" (length (GameState.stack settled)) 0
    Spec.assertEqWith s "the control: the unenchanted Forest on the same board adds one" (poolTypes S.alice unenchanted) [ManaType.Colored Color.Green]
    Spec.assertEqWith s "and bob, whose land was not the one tapped, gets nothing there" (poolTypes S.bob unenchanted) []
  Spec.it s "CR 106.12 a tap that is not for mana leaves the Aura silent" $ do
    (_, bobForest, _, board) <- wildGrowthBoard s registry
    let after = S.runPure S.identityAnswer board (Event.tap bobForest >> Engine.settleForPriority)
    Spec.assertEqWith s "CR 106.12 nothing was tapped FOR MANA, so no mana was added" (poolTypes S.bob after) []
    Spec.assertEqWith s "nor did an ordinary triggered ability go on the stack" (length (GameState.stack after)) 0
    Spec.assertEqWith s "and the land really is tapped, so the boards differ in nothing else" (fmap Object.tapped (Game.lookupObject bobForest after)) (Just TapState.Tapped)

-- CR 106.12a read by a BYSTANDER rather than off an attachment link, on the
-- cheapest printing already in `data/cards/`: Autumn Willow, Harmony, "whenever
-- you tap a land creature for mana, add an additional {G}".
--
-- THREE permanents can be tapped for mana on the one board, and each is the
-- other two's control: alice's Dryad Arbor is a land creature she controls, her
-- Forest is a land that is no creature, and bob's Dryad Arbor is a land creature
-- that is not hers. A board of only the first could not tell the Filter or the
-- PlayerRelation from a condition that read neither.
autumnWillowSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
autumnWillowSpec s registry = Spec.describe s "Autumn Willow, Harmony" $ do
  Spec.it s "CR 106.12a tapping alice's own land creature for mana adds the Willow's additional {G}" $ do
    (aliceArbor, aliceForest, bobArbor, board) <- autumnWillowBoard s registry
    let arbor = S.runPure S.identityAnswer board (S.tapForMana aliceArbor)
        -- The same board differing in exactly one thing: which of alice's
        -- permanents was tapped. Her Forest is a land and no creature.
        forest = S.runPure S.identityAnswer board (S.tapForMana aliceForest)
        -- And differing in exactly one other thing: whose land creature it was.
        theirs = S.runPure S.identityAnswer board (S.tapForMana bobArbor)
        -- CR 605.4a from the other side, the Wild Growth group's reason.
        settled = resolveDown (S.runPure S.identityAnswer arbor Engine.settleForPriority)
    Spec.assertEqWith s "CR 106.12a alice's pool holds the Arbor's {G} and the Willow's additional one" (poolTypes S.alice arbor) [ManaType.Colored Color.Green, ManaType.Colored Color.Green]
    Spec.assertEqWith s "the Filter: her Forest is no creature, so tapping it adds one and only one" (poolTypes S.alice forest) [ManaType.Colored Color.Green]
    Spec.assertEqWith s "the PlayerRelation: bob's land creature is not one SHE tapped, so his pool holds only its own {G}" (poolTypes S.bob theirs) [ManaType.Colored Color.Green]
    Spec.assertEqWith s "and alice, whose Willow watched it, is given nothing there" (poolTypes S.alice theirs) []
    Spec.assertEqWith s "CR 605.4a and the triggered mana ability never reached the stack" (length (GameState.stack arbor)) 0
    Spec.assertEqWith s "CR 605.4a and settling for priority does not place a second copy of it to resolve" (poolTypes S.alice settled) [ManaType.Colored Color.Green, ManaType.Colored Color.Green]
    Spec.assertEqWith s "nor leave anything of it on the stack" (length (GameState.stack settled)) 0

-- alice holds a Forest, a Dryad Arbor and an Autumn Willow, Harmony; bob holds a
-- Dryad Arbor of his own. Returns alice's Arbor, her Forest, bob's Arbor and the
-- board.
--
-- Both Arbors are placed by S.addPermanent, which settles what it places, so CR
-- 302.6's summoning sickness does not stop a LAND CREATURE's {T} -- the
-- precondition this case rests on, and the reason the Willow's own enters
-- trigger is not used to make the token.
autumnWillowBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
autumnWillowBoard s registry = do
  forest <- S.printingOf s registry "Forest"
  arbor <- S.printingOf s registry "Dryad Arbor"
  willow <- S.printingOf s registry "Autumn Willow, Harmony"
  case Game.zoneMembers Zone.Battlefield S.alice (S.landsInPlay forest 1) of
    [aliceForest] ->
      let (aliceArbor, withHers) = S.addPermanent arbor S.alice (S.landsInPlay forest 1)
          (bobArbor, withHis) = S.addPermanent arbor S.bob withHers
          (_, withWillow) = S.addPermanent willow S.alice withHis
       in pure (aliceArbor, aliceForest, bobArbor, withWillow)
    _ -> Spec.assertFailure s "fixture should give alice exactly one Forest" >> pure (S.noSource, S.noSource, S.noSource, S.landsInPlay forest 1)

-- CR 106.12a's SECOND half, "or is tapped for mana of a specified type", which
-- the two groups above leave untouched: Gauntlet of Power ({5} Artifact, "As
-- this artifact enters, choose a color." / "Whenever a basic land is tapped for
-- mana of the chosen color, its controller adds an additional one mana of that
-- color."). Its narrowing is CR 607.2d's link, so the colour is the one its own
-- entry chose.
--
-- FOUR permanents can be tapped for mana on the one board, and each is the
-- others' control: alice's Forest is a basic land producing the chosen colour,
-- her Mountain is a basic land producing another, her Dryad Arbor produces the
-- chosen colour but carries no Basic supertype, and bob's Forest is the first
-- again under another seat. A board of only the first could tell the mana
-- specification from the Filter from the PlayerRelation not at all.
--
-- The printed "Creatures of the chosen color get +1/+1" is the card's other
-- reader of the same choice, and Pawl.ColorSpec's "CR 607.2d two Gauntlets of
-- Power each pump the creatures of their OWN chosen colour" is what proves it.
gauntletOfPowerSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
gauntletOfPowerSpec s registry = Spec.describe s "Gauntlet of Power" $ do
  Spec.it s "CR 106.12a a basic land tapped for the CHOSEN colour adds the Gauntlet's additional mana" $ do
    (aliceForest, aliceMountain, aliceArbor, bobForest, board) <- gauntletBoard s registry
    let forest = S.runPure S.identityAnswer board (S.tapForMana aliceForest)
        -- The same board differing in exactly one thing: the mana the tap
        -- produced. A Mountain is as basic a land as the Forest.
        mountain = S.runPure S.identityAnswer board (S.tapForMana aliceMountain)
        -- And in exactly one other: the Basic supertype. A Dryad Arbor is a land
        -- producing the same {G} (CR 305.6) with no supertype at all.
        arbor = S.runPure S.identityAnswer board (S.tapForMana aliceArbor)
        -- And in exactly one other again: whose land it was.
        theirs = S.runPure S.identityAnswer board (S.tapForMana bobForest)
        -- CR 605.4a from the other side, the Wild Growth group's reason.
        settled = resolveDown (S.runPure S.identityAnswer forest Engine.settleForPriority)
    Spec.assertEqWith s "CR 106.12a alice's pool holds the Forest's {G} and the Gauntlet's additional one" (poolTypes S.alice forest) [ManaType.Colored Color.Green, ManaType.Colored Color.Green]
    Spec.assertEqWith s "the specification: her Mountain produced red and not the chosen colour, so its {R} stands alone" (poolTypes S.alice mountain) [ManaType.Colored Color.Red]
    Spec.assertEqWith s "the Filter: her Dryad Arbor produced the chosen colour but is no BASIC land, so its {G} stands alone" (poolTypes S.alice arbor) [ManaType.Colored Color.Green]
    -- CR 106.4: "its controller", read through PlayerRef.ControllerOfBound off
    -- the tapped land, is bob -- not alice, who controls the Gauntlet.
    Spec.assertEqWith s "the PlayerRelation: bob's basic Forest fires it too, and the additional mana is HIS" (poolTypes S.bob theirs) [ManaType.Colored Color.Green, ManaType.Colored Color.Green]
    Spec.assertEqWith s "and alice, whose Gauntlet watched it, is given none of it" (poolTypes S.alice theirs) []
    Spec.assertEqWith s "CR 605.4a and the triggered mana ability never reached the stack" (length (GameState.stack forest)) 0
    Spec.assertEqWith s "CR 605.4a and settling for priority does not place a second copy of it to resolve" (poolTypes S.alice settled) [ManaType.Colored Color.Green, ManaType.Colored Color.Green]

-- alice CASTS a Gauntlet of Power off five Plains and names green at CR 614.1c's
-- choice, and the four permanents that will be tapped are added afterwards --
-- otherwise the {5} could pay itself with the Forest or the Mountain the
-- assertions rest on. Returns alice's Forest, her Mountain, her Dryad Arbor,
-- bob's Forest and the board.
--
-- Cast rather than placed, so the chosen colour is a player's answer travelling
-- CR 614.1c's entry rewrite rather than a fixture write: the case asserts the
-- Gauntlet reached the battlefield before it reads a pool.
gauntletBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
gauntletBoard s registry = do
  plains <- S.printingOf s registry "Plains"
  forest <- S.printingOf s registry "Forest"
  mountain <- S.printingOf s registry "Mountain"
  arbor <- S.printingOf s registry "Dryad Arbor"
  gauntlet <- S.printingOf s registry "Gauntlet of Power"
  let (withCard, cardId) = S.handOne gauntlet (S.landsInPlay plains 5)
      answer :: Prompt.Prompt r -> r
      answer p = case p of
        Prompt.ChooseColor {} -> Color.Green
        _ -> S.identityAnswer p
      resolved = S.runPure answer (S.runPure answer withCard (S.cast S.alice cardId)) Stack.resolveTop
      (aliceForest, withForest) = S.addPermanent forest S.alice resolved
      (aliceMountain, withMountain) = S.addPermanent mountain S.alice withForest
      (aliceArbor, withArbor) = S.addPermanent arbor S.alice withMountain
      (bobForest, withBob) = S.addPermanent forest S.bob withArbor
  Spec.assertEqWith s "the fixture: the Gauntlet resolved onto the battlefield" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Gauntlet of Power") S.alice withBob) 1
  pure (aliceForest, aliceMountain, aliceArbor, bobForest, withBob)

-- CR 605.1b's "mana being added to a player's mana pool", the trigger source the
-- three groups above leave untouched: Caged Sun ({6} Artifact, "As this
-- artifact enters, choose a color." / "Creatures you control of the chosen
-- color get +1/+1." / "Whenever a land's ability causes you to add one or more
-- mana of the chosen color, add an additional one mana of that color.").
--
-- The land is a TAPPED Blood Pet ("Sacrifice this creature: Add {B}.") made a
-- Forest land by Ashaya, Soul of the Wild: its ability adds mana with no {T} in
-- the cost, so CR 106.12a's "tapped for mana" is false of it and only the
-- mana-added event can fire Caged Sun. The same board without Ashaya is the
-- Filter's control -- a creature's ability, not a land's -- and bob's Swamp is
-- the relation's: mana HE adds is not mana alice adds.
--
-- The second case is CR 605.5a's half, off a Crumbling Vestige whose ability
-- adds its mana as it RESOLVES rather than as a mana ability.
cagedSunSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
cagedSunSpec s registry = Spec.describe s "Caged Sun" $ do
  Spec.it s "CR 605.1b a land's ability adding the chosen colour with no tap adds Caged Sun's additional mana" $ do
    (pet, bobSwamp, board, withAshaya) <- cagedSunBoard s registry
    let sacrificed = S.runPure S.identityAnswer withAshaya (S.tapForMana pet)
        -- The same board differing in exactly one thing: no Ashaya, so the Pet
        -- is no land.
        creature = S.runPure S.identityAnswer board (S.tapForMana pet)
        -- And in exactly one other: whose pool the mana went to.
        theirs = S.runPure S.identityAnswer withAshaya (S.tapForMana bobSwamp)
    Spec.assertEqWith s "CR 605.1b the Ashaya'd Pet is a land, so alice's pool holds its {B} and Caged Sun's additional one" (poolTypes S.alice sacrificed) [ManaType.Colored Color.Black, ManaType.Colored Color.Black]
    Spec.assertEqWith s "the Filter: without Ashaya the Pet is no land, so its {B} stands alone" (poolTypes S.alice creature) [ManaType.Colored Color.Black]
    Spec.assertEqWith s "the PlayerRelation: bob's Swamp added the {B} to HIS pool, so Caged Sun gives alice nothing" (poolTypes S.alice theirs) []
    Spec.assertEqWith s "and the Pet really was sacrificed for it" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Blood Pet") S.alice sacrificed) 0
  -- CR 605.5a's half of the same rule: a Crumbling Vestige ("This land enters
  -- tapped." / "When this land enters, add one mana of any color." / "{T}: Add
  -- {C}.") whose ETB trigger resolves off the stack adds mana too, and Caged
  -- Sun's trigger then is NOT a mana ability -- "triggers from an event other
  -- than activating a mana ability" -- so it uses the stack.
  Spec.it s "CR 605.5a mana a land's ability adds as it RESOLVES fires Caged Sun, whose trigger then uses the stack" $ do
    (_, _, board, _) <- cagedSunBoard s registry
    vestige <- S.printingOf s registry "Crumbling Vestige"
    let entered = enterVestige vestige board
        -- The Vestige's own ETB trigger, placed and resolved: it is the ability
        -- that adds the mana, and the colour it adds is a player's answer.
        afterEntry = S.runPure (colourAnswer Color.Black) (S.runPure (colourAnswer Color.Black) entered Engine.settleForPriority) Stack.resolveTop
        -- Caged Sun's trigger, gathered off that addition and placed like any
        -- other: CR 605.4a's inline road would have left the stack empty here.
        waiting = S.runPure S.identityAnswer afterEntry Engine.settleForPriority
        settled = resolveDown waiting
        -- The same board differing in exactly one thing: the colour the Vestige
        -- added, which is not the one Caged Sun named.
        other = resolveDown (S.runPure (colourAnswer Color.Green) (S.runPure (colourAnswer Color.Green) (S.runPure (colourAnswer Color.Green) entered Engine.settleForPriority) Stack.resolveTop) Engine.settleForPriority)
    Spec.assertEqWith s "CR 605.1b alice's pool holds the Vestige's black and Caged Sun's additional one" (poolTypes S.alice settled) [ManaType.Colored Color.Black, ManaType.Colored Color.Black]
    Spec.assertEqWith s "CR 605.5a Caged Sun's trigger went ON the stack, the resolving ability being no mana ability" (length (GameState.stack waiting)) 1
    Spec.assertEqWith s "the specification: a Vestige adding green, not the chosen colour, leaves its one mana alone" (poolTypes S.alice other) [ManaType.Colored Color.Green]
    Spec.assertEqWith s "the fixture: the Vestige is on the battlefield" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Crumbling Vestige") S.alice settled) 1

-- alice CASTS a Caged Sun off six Plains and names black, the Gauntlet of Power
-- group's reason; then a TAPPED Blood Pet of hers and a Swamp of bob's are
-- added. Returns the Pet, bob's Swamp, the board, and the board with alice's
-- Ashaya, Soul of the Wild added last.
cagedSunBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState, GameState.GameState)
cagedSunBoard s registry = do
  plains <- S.printingOf s registry "Plains"
  swamp <- S.printingOf s registry "Swamp"
  bloodPet <- S.printingOf s registry "Blood Pet"
  ashaya <- S.printingOf s registry "Ashaya, Soul of the Wild"
  cagedSun <- S.printingOf s registry "Caged Sun"
  let (withCard, cardId) = S.handOne cagedSun (S.landsInPlay plains 6)
      answer :: Prompt.Prompt r -> r
      answer p = case p of
        Prompt.ChooseColor {} -> Color.Black
        _ -> S.identityAnswer p
      resolved = S.runPure answer (S.runPure answer withCard (S.cast S.alice cardId)) Stack.resolveTop
      (pet, withPet) = S.addPermanent bloodPet S.alice resolved
      (bobSwamp, board) = S.addPermanent swamp S.bob (S.tapObject pet withPet)
      withAshaya = snd (S.addPermanent ashaya S.alice board)
  Spec.assertEqWith s "the fixture: Caged Sun resolved onto the battlefield" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Caged Sun") S.alice board) 1
  Spec.assertEqWith s "the fixture: the Pet is tapped, so no {T} can be what fires Caged Sun" (fmap Object.tapped (Game.lookupObject pet board)) (Just TapState.Tapped)
  pure (pet, bobSwamp, board, withAshaya)

-- A Crumbling Vestige of alice's, with CR 603.6a's event beside it so its ETB
-- trigger is pending, then tapped by a fixture write standing in for the
-- printed "This land enters tapped" -- S.entersWithTrigger applies no entry
-- replacement. Tapped for the Blood Pet's reason: with no {T} ever paid, CR
-- 106.12a's "tapped for mana" cannot be what fires Caged Sun, and the
-- mana-added event is the only road left.
enterVestige :: Printing.Printing -> GameState.GameState -> GameState.GameState
enterVestige printing gs =
  let (oid, withLand) = S.entersWithTrigger printing S.alice gs
   in S.tapObject oid withLand

-- CR 105.4's answer for "one mana of any color", pinned to one colour so the
-- pair of boards below differ in exactly it.
colourAnswer :: Color.Color -> Prompt.Prompt r -> r
colourAnswer colour p = case p of
  Prompt.ChooseManaType _ _ _ offered ->
    Maybe.fromMaybe (NonEmpty.head offered) (List.find (== ManaType.Colored colour) (NonEmpty.toList offered))
  _ -> S.identityAnswer p

-- Resolve the whole stack down, so a board that placed a triggered mana ability
-- CR 605.4a forbids the stack reads differently from one that placed nothing: a
-- reading taken with the trigger still waiting could not tell them apart at
-- gameplay level.
resolveDown :: GameState.GameState -> GameState.GameState
resolveDown =
  let go n gs =
        if n <= 0 || null (GameState.stack gs)
          then gs
          else go (n - 1) (S.runPure S.identityAnswer gs Stack.resolveTop)
   in go (8 :: Int)

-- alice and bob hold one Forest each; alice controls a Wild Growth enchanting
-- BOB's. Returns alice's Forest, bob's Forest, the Aura and the board.
wildGrowthBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
wildGrowthBoard s registry = do
  forest <- S.printingOf s registry "Forest"
  growth <- S.printingOf s registry "Wild Growth"
  let base = S.landsFor forest S.bob 1 (S.landsInPlay forest 1)
  case (Game.zoneMembers Zone.Battlefield S.alice base, Game.zoneMembers Zone.Battlefield S.bob base) of
    ([aliceForest], [bobForest]) ->
      let (auraId, withAura) = S.addPermanent growth S.alice base
       in -- ToObject and not S.attach's ToCreature: "Enchant land" is a
          -- Pool.Permanents slot narrowed by a Land filter, so a cast would leave
          -- this tag (Pawl.Support.attach says so of Convincing Mirage).
          pure (aliceForest, bobForest, auraId, S.attachTo auraId (Recipient.ToObject bobForest) withAura)
    _ -> Spec.assertFailure s "fixture should give each player exactly one Forest" >> pure (S.noSource, S.noSource, S.noSource, base)

-- A fixture write making one permanent summoning sick, the mirror of
-- S.tapObject: what an object that arrived this turn carries (CR 400.7).
sicken :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
sicken oid gs =
  gs
    { GameState.objects = Map.adjust (\o -> o {Object.sickness = Sickness.Sick}) oid (GameState.objects gs)
    }

-- CR 106.13's card. Drain Power is the only printing that moves mana from one
-- player's pool to another, and rule 106.13 says so in as many words, so this
-- group is the whole of what exercises Effect.MoveMana and
-- Effect.ActivateManaAbilities.
--
-- THREE SEATS, so "that player" and "you" cannot collapse and a third pool is
-- there to prove the transfer takes only the one the spell named.
drainPowerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
drainPowerSpec s registry = Spec.describe s "Drain Power" $ do
  -- The whole sentence at once: bob activates a mana ability of each of his
  -- three lands -- choosing Bayou's colour himself -- and then loses that mana
  -- to alice, units and all.
  Spec.it s "CR 106.13 the pool crosses whole, and its production tags with it" $ do
    drainPower <- S.printingOf s registry "Drain Power"
    island <- S.printingOf s registry "Island"
    bayou <- S.printingOf s registry "Bayou"
    snowCoveredMountain <- S.printingOf s registry "Snow-Covered Mountain"
    forest <- S.printingOf s registry "Forest"
    let (gs, spellId) = drainPowerBoard drainPower island bayou snowCoveredMountain forest
        ((_, cast), askedCasting) = State.runState (Engine.runGame (aimedAt S.bob Color.Black) gs (S.cast S.alice spellId)) []
        ((_, after), asked) = State.runState (Engine.runGame (aimedAt S.bob Color.Black) cast Stack.resolveTop) []
        ((_, green), _) = State.runState (Engine.runGame (aimedAt S.bob Color.Green) cast Stack.resolveTop) []
    -- The fixture, asserted rather than assumed: a cast that cannot be paid for
    -- fails silently, and every assertion below would then be about a board
    -- where nothing happened.
    Spec.assertEqWith s "the spell left alice's hand" (S.handSize S.alice cast) 0
    Spec.assertEqWith s "alice spent her {U}{U} paying for it" (poolTypes S.alice cast) []
    Spec.assertEqWith s "and nothing was asked of anyone but alice as she cast it" (filter (/= S.alice) askedCasting) []
    -- CR 605.3 and CR 106.13 together: bob's three lands produced, and every
    -- unit reached alice with the type bob's own answer settled.
    Spec.assertEqWith s "CR 106.13 alice holds what bob's lands made" (poolTypes S.alice after) [ManaType.Colored Color.Black, ManaType.Colored Color.Red, ManaType.Colored Color.Green]
    -- The discriminating half of that: the SAME board with the other answer to
    -- Bayou's prompt puts the other colour in alice's pool, so the colour is the
    -- answer's and not the engine's.
    Spec.assertEqWith s "CR 105.4 the other answer to Bayou sends green instead" (poolTypes S.alice green) [ManaType.Colored Color.Green, ManaType.Colored Color.Red, ManaType.Colored Color.Green]
    -- CR 106.13's second sentence, which is why whole ManaUnits cross: a
    -- transfer that re-added plain mana of the same types would leave alice
    -- holding red mana no snow permanent produced, and CR 107.4h's {S} reads
    -- exactly this.
    Spec.assertEqWith s "CR 106.13 the snow mana keeps its production tag" (fmap ManaUnit.tags (poolUnitsOf S.alice after)) [Set.empty, Set.singleton ProductionTag.Snow, Set.empty]
    Spec.assertEqWith s "CR 106.13 bob loses all of it" (poolTypes S.bob after) []
    Spec.assertEqWith s "and carol, whom the spell did not name, keeps hers" (poolTypes S.carol after) [ManaType.Colored Color.White]
    -- CR 602.2: WHICH mana ability of Bayou is bob's choice -- only an object's
    -- controller activates its activated ability -- so the one yield prompt
    -- the resolution raises is his. Bayou is the only one of the three lands that
    -- offers two, which is why the list is one long. (The resolution also asks
    -- bob CR 605.3a's ordering, which `aimedAt` does not record; the group below
    -- is where that is the subject.)
    Spec.assertEqWith s "CR 602.2 the choice of ability is the targeted player's" asked [S.bob]
  -- CR 106.13 names the move a LOSS ("causes one player to lose unspent mana"),
  -- and so does Drain Power's Oracle text, which is the word Yurlok of Scorch
  -- Thrash's middle line reads. A PAIR of boards differing only in carol's
  -- Yurlok: bob, who does not control it, is the one charged, and carol's own
  -- untouched {W} costs her nothing, so the charge is for what crossed.
  Spec.it s "CR 106.13 the targeted player loses life for the mana Drain Power takes" $ do
    drainPower <- S.printingOf s registry "Drain Power"
    island <- S.printingOf s registry "Island"
    bayou <- S.printingOf s registry "Bayou"
    snowCoveredMountain <- S.printingOf s registry "Snow-Covered Mountain"
    forest <- S.printingOf s registry "Forest"
    yurlok <- S.printingOf s registry "Yurlok of Scorch Thrash"
    let (gs, spellId) = drainPowerBoard drainPower island bayou snowCoveredMountain forest
        (_, withYurlok) = S.addPermanent yurlok S.carol gs
        resolved board =
          let ((_, cast), _) = State.runState (Engine.runGame (aimedAt S.bob Color.Black) board (S.cast S.alice spellId)) []
              ((_, after), _) = State.runState (Engine.runGame (aimedAt S.bob Color.Black) cast Stack.resolveTop) []
           in after
        charged = resolved withYurlok
        uncharged = resolved gs
        lives board = fmap (\pid -> S.lifeOf pid board) [S.alice, S.bob, S.carol]
    Spec.assertEqWith s "the fixture: the three crossed to alice" (poolTypes S.alice charged) [ManaType.Colored Color.Black, ManaType.Colored Color.Red, ManaType.Colored Color.Green]
    Spec.assertEqWith s "CR 119.3 bob pays 3 life for the three he lost, and nobody else pays" (lives charged) [Just 20, Just 17, Just 20]
    Spec.assertEqWith s "CR 103.4 and with no Yurlok the same move costs nobody anything" (lives uncharged) [Just 20, Just 20, Just 20]
  -- CR 605.3a: WHICH ORDER bob activates his two lands in is his, and it is
  -- observable -- Mystic Gate's "{W/U}, {T}: Add {W}{W}, {W}{U}, or {U}{U}" is
  -- paid for out of the mana the Plains put in his pool, which is there only if
  -- the Plains went first.
  --
  -- The Gate is added FIRST, so it holds the LOWER object id and the engine's
  -- own sweep (battlefieldMatching, APNAP then ascending) is the order that
  -- loses the {W}{W}.
  --
  -- The two runs differ in ONE answer, the ordering: `activatingInOrder` shuts
  -- CR 605.3a's payment window in both, so the Gate is paid for out of what the
  -- SWEEP already put in bob's pool or not at all. A bob who opens that window
  -- instead reaches {W}{W} from either order -- what the engine may not do is
  -- pick the order for him.
  Spec.it s "CR 605.3a the order the batch is activated in is the targeted player's" $ do
    drainPower <- S.printingOf s registry "Drain Power"
    island <- S.printingOf s registry "Island"
    mysticGate <- S.printingOf s registry "Mystic Gate"
    plains <- S.printingOf s registry "Plains"
    let (gs, spellId) = S.handOne drainPower (S.landsFor island S.alice 2 (S.landsFor plains S.bob 1 (S.landsFor mysticGate S.bob 1 S.threePlayerGame)))
        ((_, cast), _) = State.runState (Engine.runGame (activatingInOrder [1, 0]) gs (S.cast S.alice spellId)) []
        ((_, after), asked) = State.runState (Engine.runGame (activatingInOrder [1, 0]) cast Stack.resolveTop) []
        -- The SAME board with the other answer, which is the engine's own sweep
        -- order: one different answer is the only difference between the two.
        ((_, gateFirst), _) = State.runState (Engine.runGame (activatingInOrder [0, 1]) cast Stack.resolveTop) []
    -- The fixture, asserted rather than assumed (the group above's reason).
    Spec.assertEqWith s "the spell left alice's hand" (S.handSize S.alice cast) 0
    Spec.assertEqWith s "alice spent her {U}{U} paying for it" (poolTypes S.alice cast) []
    -- CR 605.3a and CR 106.13 together: the Plains paid for the Gate, so what
    -- crosses to alice is the Gate's {W}{W} and nothing else.
    Spec.assertEqWith s "CR 605.3a bob's order pays for the Gate with the Plains" (poolTypes S.alice after) [ManaType.Colored Color.White, ManaType.Colored Color.White]
    -- The discriminating half: the SAME board and the same answer to every other
    -- prompt, with the Gate taken first. Its {W/U} is unpayable out of an empty
    -- pool, and CR 601.2h (reached by CR 602.2b) allows no partial payment, so
    -- CR 609.3 leaves the Gate doing nothing and only the Plains' {W} crosses.
    Spec.assertEqWith s "CR 609.3 the Gate taken first adds nothing" (poolTypes S.alice gateFirst) [ManaType.Colored Color.White]
    -- CR 602.2: the order is the ACTIVATING player's, bob's, not the resolving
    -- controller's.
    Spec.assertEqWith s "CR 602.2 the ordering is asked of the targeted player" asked [S.bob]

-- alice holds Drain Power and two Islands to pay for it; bob controls the three
-- lands the spell will make him tap -- a Bayou (two mana abilities, so its colour
-- is a real choice), a Snow-Covered Mountain (a production tag alice cannot make
-- herself) and a Forest; carol holds a floating {W} nothing should touch.
--
-- The lands are added in that order, so bob's pool comes out in it: Bayou's
-- answer, then the snow red, then the green.
drainPowerBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> (GameState.GameState, ObjectId.ObjectId)
drainPowerBoard drainPower island bayou snowCoveredMountain forest =
  S.handOne
    drainPower
    ( Mana.addMana
        S.carol
        [unitOf (ManaType.Colored Color.White)]
        (S.landsFor island S.alice 2 (S.landsFor forest S.bob 1 (S.landsFor snowCoveredMountain S.bob 1 (S.landsFor bayou S.bob 1 S.threePlayerGame))))
    )

-- One ordinary unit of a type, for a pool a fixture floats mana into directly.
unitOf :: ManaType.ManaType -> ManaUnit.ManaUnit
unitOf manaType =
  ManaUnit.MkManaUnit
    { ManaUnit.manaType = manaType,
      ManaUnit.tags = Set.empty,
      ManaUnit.retention = ManaRetention.Ordinary,
      ManaUnit.restriction = Nothing,
      ManaUnit.rider = Nothing,
      ManaUnit.spendTrigger = Nothing,
      ManaUnit.sourceChosenSubtype = Nothing,
      ManaUnit.sourceLastExiled = Nothing
    }

-- Targets `victim`, takes `color` wherever a mana yield offers it, and records
-- the PLAYER each yield prompt was asked of.
--
-- Stateful rather than pure because the identity of the player asked is the
-- subject: a pure answerer could report which colour came back but never who
-- chose it.
aimedAt :: PlayerId.PlayerId -> Color.Color -> Prompt.Prompt r -> State.State [PlayerId.PlayerId] r
aimedAt victim color p = case p of
  Prompt.ChooseManaYield decider _ _ _ -> do
    State.modify' (<> [Decider.unwrap decider])
    pure (prefersColor color p)
  Prompt.ChooseTargets _ _ _ offered -> pure (S.preferring (== Recipient.ToPlayer victim) offered)
  _ -> pure (S.identityAnswer p)

-- Targets bob, hands `answer` to the one CR 605.3a ordering prompt the
-- resolution raises, takes the {W}{W} yield wherever the Gate offers it, opens
-- NO mana ability of its own to pay with, and records the player the ordering
-- was asked of.
--
-- The declined window is PINNED here rather than left to S.identityAnswer, which
-- would tap the first source offered: CR 605.3a's payment window is a second way
-- to reach the Plains' {W}, and a bob who uses it needs no ordering at all. What
-- is under test is the order the SWEEP takes, so the window is shut in both
-- runs and the ordering answer is the only difference between them. BOB's
-- windows only -- alice still opens hers to pay for the spell itself.
--
-- Stateful rather than pure because WHOSE the order is is half the subject; a
-- pure answerer could pick the permutation but never report who was asked.
activatingInOrder :: [Natural] -> Prompt.Prompt r -> State.State [PlayerId.PlayerId] r
activatingInOrder answer p = case p of
  Prompt.OrderManaActivations decider _ _ -> do
    State.modify' (<> [Decider.unwrap decider])
    pure answer
  Prompt.ChooseManaYield _ _ _ candidates -> pure (optionOfTypes [ManaType.Colored Color.White, ManaType.Colored Color.White] candidates)
  Prompt.ChooseManaSource decider _ _ | Decider.unwrap decider == S.bob -> pure Nothing
  Prompt.ChooseExtraManaSource decider _ _ | Decider.unwrap decider == S.bob -> pure Nothing
  Prompt.ChooseTargets _ _ _ offered -> pure (S.preferring (== Recipient.ToPlayer S.bob) offered)
  _ -> pure (S.identityAnswer p)

-- The units of any player's pool -- poolUnits' twin for the two pools a CR
-- 106.13 transfer has.
poolUnitsOf :: PlayerId.PlayerId -> GameState.GameState -> [ManaUnit.ManaUnit]
poolUnitsOf pid gs = case Game.poolOf pid gs of
  Mana.Type.MkMana units -> units

-- CR 605.1b's FIRST alternative, the trigger source the four groups above leave
-- untouched: Tyvar the Bellicose ({2}{B}{G} Legendary Creature -- Elf Warrior,
-- 5/4), whose second ability grants each creature you control "Whenever a mana
-- ability of this creature resolves, put a number of +1/+1 counters on it equal
-- to the amount of mana this creature produced. This ability triggers only once
-- each turn."
--
-- NOT a mana ability, CR 605.5a's third clause: the granted trigger could not
-- produce mana, so it uses the stack like any other -- which is what separates
-- this group from the Wild Growth one, where CR 605.4a keeps the trigger off it.
--
-- FOUR permanents can be tapped for mana on the one board, and each is the
-- others' control: alice's Palladium Myr ("{T}: Add {C}{C}") produces two mana,
-- her Llanowar Elves ("{T}: Add {G}") produces one, her Forest is a mana source
-- that is no creature and so was granted nothing, and bob's Llanowar Elves is
-- the first again under the other seat.
--
-- Tyvar's other ability, CR 508.3c's attack trigger, is Pawl.CardTriggerSpec's
-- "Tyvar the Bellicose, attacking".
tyvarSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
tyvarSpec s registry = Spec.describe s "Tyvar the Bellicose" $ do
  Spec.it s "CR 605.1b a creature's mana ability resolving puts a counter on it per mana it produced" $ do
    (myr, elves, forest, theirElves, board) <- tyvarBoard s registry
    let tapped = S.runPure S.identityAnswer board (S.tapForMana myr)
        -- CR 605.5a: the granted trigger could add no mana, so it waits on the
        -- stack rather than applying inline the way CR 605.4a's would.
        waiting = S.runPure S.identityAnswer tapped Engine.settleForPriority
        settled = resolveDown waiting
        -- The same board differing in exactly one thing: how much mana the
        -- activation produced. One Llanowar Elf's {G} against the Myr's {C}{C}.
        one = resolveDown (S.runPure S.identityAnswer (S.runPure S.identityAnswer board (S.tapForMana elves)) Engine.settleForPriority)
        -- And in exactly one other: a Forest is a mana source that is no
        -- creature, so Tyvar granted it nothing.
        land = resolveDown (S.runPure S.identityAnswer (S.runPure S.identityAnswer board (S.tapForMana forest)) Engine.settleForPriority)
        -- And in exactly one other again: whose creature it was.
        theirs = resolveDown (S.runPure S.identityAnswer (S.runPure S.identityAnswer board (S.tapForMana theirElves)) Engine.settleForPriority)
    Spec.assertEqWith s "CR 605.1b the Myr made two mana, so two +1/+1 counters went on it" (S.counterOf CounterKind.PlusOnePlusOne myr settled) 2
    Spec.assertEqWith s "the amount: one Llanowar Elf made one mana, so one counter" (S.counterOf CounterKind.PlusOnePlusOne elves one) 1
    Spec.assertEqWith s "the Filter: alice's Forest is no creature, so Tyvar granted it nothing" (S.counterOf CounterKind.PlusOnePlusOne forest land) 0
    Spec.assertEqWith s "the PlayerRelation: bob's Elf is no creature ALICE controls, so it gets nothing" (S.counterOf CounterKind.PlusOnePlusOne theirElves theirs) 0
    Spec.assertEqWith s "CR 605.5a the granted trigger went ON the stack, being no mana ability" (length (GameState.stack waiting)) 1
    Spec.assertEqWith s "and the Myr's own {C}{C} reached alice's pool" (poolTypes S.alice settled) [ManaType.Colorless, ManaType.Colorless]
  -- The printed rider, spent by Event.withinTriggerLimit over CR 603.3b's log.
  -- The Myr is untapped between the two activations through Event.untap, the
  -- road CR 502.3 takes, so the second one pays the same {T} the first did.
  Spec.it s "the rider: a second resolution the same turn triggers nothing more" $ do
    (myr, elves, _, _, board) <- tyvarBoard s registry
    let once = resolveDown (S.runPure S.identityAnswer (S.runPure S.identityAnswer board (S.tapForMana myr)) Engine.settleForPriority)
        untapped = S.runPure S.identityAnswer once (Event.untap myr)
        twice = resolveDown (S.runPure S.identityAnswer (S.runPure S.identityAnswer untapped (S.tapForMana myr)) Engine.settleForPriority)
        -- The same board differing in exactly one thing: which creature's mana
        -- ability resolved the second time. CR 113.7 gives each its own
        -- instance of the granted ability, so the Elf's limit is unspent.
        another = resolveDown (S.runPure S.identityAnswer (S.runPure S.identityAnswer once (S.tapForMana elves)) Engine.settleForPriority)
    Spec.assertEqWith s "the rider: the Myr's second resolution added no third counter" (S.counterOf CounterKind.PlusOnePlusOne myr twice) 2
    Spec.assertEqWith s "the fixture: the second activation really did make its mana" (poolTypes S.alice twice) [ManaType.Colorless, ManaType.Colorless, ManaType.Colorless, ManaType.Colorless]
    Spec.assertEqWith s "and the limit is per source: the Elf's own instance is unspent" (S.counterOf CounterKind.PlusOnePlusOne elves another) 1

-- alice controls a Tyvar the Bellicose, a Palladium Myr, a Llanowar Elves and a
-- Forest; bob controls a Llanowar Elves. Returns the Myr, alice's Elf, the
-- Forest, bob's Elf and the board.
--
-- Tyvar is placed FIRST, so the three permanents that follow are on the
-- battlefield beside a static ability already granting: CR 604.2's effect is
-- re-derived per projection, so the order is a readability choice rather than a
-- load-bearing one.
tyvarBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
tyvarBoard s registry = do
  tyvar <- S.printingOf s registry "Tyvar the Bellicose"
  myrPrinting <- S.printingOf s registry "Palladium Myr"
  elvesPrinting <- S.printingOf s registry "Llanowar Elves"
  forestPrinting <- S.printingOf s registry "Forest"
  let (_, withTyvar) = S.addPermanent tyvar S.alice (Setup.emptyGame S.bothPlayers)
      (myr, withMyr) = S.addPermanent myrPrinting S.alice withTyvar
      (elves, withElves) = S.addPermanent elvesPrinting S.alice withMyr
      (forest, withForest) = S.addPermanent forestPrinting S.alice withElves
      (theirElves, withBob) = S.addPermanent elvesPrinting S.bob withForest
  Spec.assertEqWith s "the fixture: Tyvar is on the battlefield" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Tyvar the Bellicose") S.alice withBob) 1
  pure (myr, elves, forest, theirElves, withBob)

-- Rhystic Cave, Land: "{T}: Choose a color. Add one mana of that color unless
-- any player pays {1}. Activate only as an instant." CR 118.12a's gate on a mana
-- ability's clause, which CR 605.3b gives no stack object -- the source stands in
-- (Resolve.Effect.performManaPayGate) -- and CR 602.5e's rider, which keeps the
-- ability to the priority window: its ruling forbids activating it while
-- casting a spell or activating an ability, so a paid gate never leaves a
-- payment short.
--
-- THREE SEATS, every one holding an untapped Island, so a decline is never CR
-- 118.3's "can't" and the "any player" reading is told apart from the "each
-- player" one.
rhysticCaveSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
rhysticCaveSpec s registry =
  let paysFor :: Maybe PlayerId.PlayerId -> Prompt.Prompt r -> r
      paysFor who p = case p of
        Prompt.ChooseToPay (Decider.MkDecider d) player _ _ _ _
          | Just d == who && Just player == who -> PaymentDecision.Pays
        _ -> S.identityAnswer p
      payResponses = filter (\r -> case r of Response.ChoseToPay _ -> True; _ -> False)
      caveBoard aliceIslands = do
        island <- S.printingOf s registry "Island"
        cave <- S.printingOf s registry "Rhystic Cave"
        let lands = S.landsFor island S.carol 1 (S.landsFor island S.bob 1 (S.landsFor island S.alice aliceIslands S.threePlayerGame))
            (caveId, gs) = S.addPermanent cave S.alice lands
        pure (caveId, gs {GameState.phase = Phase.PrecombatMain, GameState.priority = Just S.alice})
      -- CR 605.3a's priority window: the activation Engine.priorityLoop makes
      -- for Action.ActivateManaAbility.
      tapCave who = do
        (caveId, gs) <- caveBoard 1
        pure (caveId, gs, Replay.record (paysFor who) gs (S.tapForMana caveId))
   in Spec.describe s "CR 118.12a Rhystic Cave's unless any player pays" $ do
        Spec.it s "CR 118.12a bob pays {1}, so no mana is added" $ do
          (caveId, gs, ((_, after), transcript)) <- tapCave (Just S.bob)
          Spec.assertEqWith s "CR 118.12a: alice's pool is empty" (poolTypes S.alice after) []
          Spec.assertEqWith s "the Cave's {T} was paid" (Object.tapped <$> Game.lookupObject caveId after) (Just TapState.Tapped)
          Spec.assertEqWith s "bob's Island paid the {1}" (S.tappedCount S.bob after) 1
          Spec.assertEqWith s "setup: nothing was tapped before" (S.tappedCount S.bob gs) 0
          Spec.assertEqWith s "CR 101.4: alice declined, bob paid, carol declined" (payResponses transcript) [Response.ChoseToPay PaymentDecision.Declines, Response.ChoseToPay PaymentDecision.Pays, Response.ChoseToPay PaymentDecision.Declines]
        Spec.it s "CR 118.12a nobody pays, so one mana is added" $ do
          (_, _, ((_, after), transcript)) <- tapCave Nothing
          Spec.assertEqWith s "CR 118.12a: alice's pool holds one mana" (length (poolTypes S.alice after)) 1
          Spec.assertEqWith s "all three declined" (payResponses transcript) (replicate 3 (Response.ChoseToPay PaymentDecision.Declines))
        -- CR 602.5e: the same Cave while a spell is being cast. It is alice's
        -- only land, so Sol Ring's {1} needs it -- any colour pays, so the
        -- colour answer cannot decide the case -- and the ruling forbids it,
        -- where at priority the Cave is offered.
        Spec.it s "CR 602.5e the Cave is offered at priority and not inside a cast's payment" $ do
          (caveId, board) <- caveBoard 0
          ring <- S.printingOf s registry "Sol Ring"
          let (gs, ringId) = S.handOne ring board
              afterCast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice ringId))
          Spec.assertEqWith s "CR 602.5e: Sol Ring stays in alice's hand" (elem ringId (Game.zoneMembers Zone.Hand S.alice afterCast)) True
          Spec.assertEqWith s "and the Cave stays untapped" (Object.tapped <$> Game.lookupObject caveId afterCast) (Just TapState.Untapped)
          Spec.assertEqWith s "CR 118.3: the cast is not offered" (S.castable S.alice ringId gs) False
          Spec.assertEqWith s "CR 605.3a: at priority the Cave is offered" (elem (Action.Type.ActivateManaAbility caveId) (Action.legalActions S.alice gs)) True

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Mana" $ do
  manaSpec s registry
  anyColorSpec s registry
  rhysticCaveSpec s registry
  chosenColorSpec s registry
  solRingSpec s registry
  ancientTombSpec s registry
  palladiumMyrSpec s registry
  upwellingSpec s registry
  omnathSpec s registry
  priorityWindowSpec s registry
  riderWindowSpec s registry
  cabalCoffersSpec s registry
  laviniaTurnRiderSpec s registry
  nimbusMazeSpec s registry
  wellspringSpec s registry
  manaConfluenceSpec s registry
  phyrexianTowerSpec s registry
  bloodPetSpec s registry
  ashnodsAltarSpec s registry
  workhorseSpec s registry
  heritageDruidSpec s registry
  dynamoConduitSpec s registry
  witheringFontSpec s registry
  musterDynamoSpec s registry
  cryptexSpec s registry
  phyrexianAltarSpec s registry
  transmograntAltarSpec s registry
  pluralBoardSpec s registry
  grinningIgnusSpec s registry
  mysticGateSpec s registry
  skyshroudElfSpec s registry
  activationAdjustmentSpec s registry
  wildGrowthSpec s registry
  autumnWillowSpec s registry
  gauntletOfPowerSpec s registry
  cagedSunSpec s registry
  tyvarSpec s registry
  drainPowerSpec s registry
  yurlokSpec s registry
  almsEngineSpec s registry
  confluenceObeliskSpec s registry
  hickoryWoodlotSpec s registry
  manaCacheSpec s registry
  recipientsSpec s registry
  valleymakerSpec s registry
  spectralSearchlightSpec s registry

-- CR 605.3b's road has no ability object, so a mana addition excluding a
-- BINDING SLOT's players names nobody -- Mana.recipientsOf's own stated posture.
-- Function-level
-- because no card in data/cards/ pairs Pawl.Types.PlayerRef's EachPlayerExcept
-- with an AddMana, and adding one to buy a gameplay board would be a card no
-- printing reaches (Scryfall o:"each other player adds", 2026-09-08, no hit;
-- Yurlok of Scorch Thrash's "each player adds" is the nearest, and it names no
-- slot).
--
-- EachPlayerExcept is the one such arm that could answer otherwise:
-- Count.playersFor reads "a slot naming nobody excludes nobody" off a LIVE
-- SOURCE, so a context carrying one would answer every player here and
-- manaSuppliesGiven would count the excluded seat's share as the payer's supply.
-- The board holds a live object precisely so that the difference is reachable.
recipientsSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
recipientsSpec s registry = Spec.describe s "recipientsOf" $ do
  Spec.it s "CR 605.3b an off-stack mana ability's EachPlayerExcept names nobody" $ do
    forest <- S.printingOf s registry "Forest"
    let (_, board) = S.addPermanent forest S.alice S.threePlayerGame
        excepting = PlayerRef.EachPlayerExcept (SlotName.MkSlotName (Text.pack "x"))
    Spec.assertEqWith s "CR 605.3b the excluding reference resolves to no recipient at all" (Mana.recipientsOf S.alice Map.empty board excepting) []
    -- Behind the assertion above, and the guard against it passing because the
    -- board has no players to name: the slotless sibling names all three.
    Spec.assertEqWith s "CR 102.1 while the slotless EachPlayer beside it names the whole table" (Mana.recipientsOf S.alice Map.empty board PlayerRef.EachPlayer) [S.alice, S.bob, S.carol]

-- Valleymaker ({5}{R/G} Creature -- Giant Shaman 5/5, Shadowmoor; Oracle text
-- checked against Scryfall 2026-10-01): "{T}, Sacrifice a Forest: Choose a
-- player. That player adds {G}{G}{G}." A mana ability (its ruling: "It doesn't
-- target a player and it doesn't use the stack"), so the choice is made on CR
-- 605.3b's road, where no ability object holds the slot "that player" reads.
--
-- THREE seats, so the chosen player is told apart both from the activator and
-- from "each other player".
valleymakerSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
valleymakerSpec s registry = Spec.describe s "Valleymaker" $ do
  Spec.it s "CR 608.2c the player chosen as the mana ability resolves is the one who adds its mana" $ do
    valleymaker <- S.printingOf s registry "Valleymaker"
    forest <- S.printingOf s registry "Forest"
    let (valleymakerId, g1) = S.addPermanent valleymaker S.alice S.threePlayerGame
        (_, board) = S.addPermanent forest S.alice g1
        -- Pinned by seat out of the offered set, never built.
        choosesBob :: Prompt.Prompt r -> r
        choosesBob p = case p of
          Prompt.ChoosePlayer _ _ _ offered -> Maybe.fromMaybe (NonEmpty.head offered) (List.find (== S.bob) (NonEmpty.toList offered))
          _ -> S.identityAnswer p
        tapped = S.runPure choosesBob board (S.tapForMana valleymakerId)
    Spec.assertEqWith s "CR 608.2c bob, whom alice chose, adds {G}{G}{G}, and neither alice nor carol adds anything" (fmap (`poolTypes` tapped) [S.alice, S.bob, S.carol]) [[], replicate 3 (ManaType.Colored Color.Green), []]

-- Spectral Searchlight ({3} Artifact, Commander Masters; Oracle text checked
-- against Scryfall 2026-10-01): "{T}: Choose a player. That player adds one mana
-- of any color they choose." Its ruling: "You may choose yourself."
spectralSearchlightSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spectralSearchlightSpec s registry = Spec.describe s "Spectral Searchlight" $ do
  -- CR 106.3 / 608.2d: "they choose" is the CHOSEN player's colour. The answerer
  -- picks blue when carol is asked and red when anybody else is, so a colour
  -- asked of alice puts red in carol's pool. THREE seats for Valleymaker's
  -- reason.
  Spec.it s "CR 608.2d the chosen player picks the colour of the mana they add" $ do
    searchlight <- S.printingOf s registry "Spectral Searchlight"
    let (searchlightId, board) = S.addPermanent searchlight S.alice S.threePlayerGame
        tapped = S.runPure (searchlightAnswer S.carol (\asked -> if asked == S.carol then Color.Blue else Color.Red) Nothing) board (S.tapForMana searchlightId)
    Spec.assertEqWith s "CR 608.2d carol, whom alice chose, adds the blue she picked, and nobody else adds anything" (fmap (`poolTypes` tapped) [S.alice, S.bob, S.carol]) [[], [], [ManaType.Colored Color.Blue]]

  -- CR 601.2g / 605.3a: alice may choose herself, so the Searchlight is a source
  -- of any colour for her own payment. The pair differs in exactly the
  -- Searchlight: the same Llanowar Elves in hand, the same phase, no other mana.
  Spec.it s "CR 605.3a its activator may choose themself, so it pays for their own spell" $ do
    searchlight <- S.printingOf s registry "Spectral Searchlight"
    elves <- S.printingOf s registry "Llanowar Elves"
    let (elvesId, g1) = S.addHandCard elves S.alice S.threePlayerGame
        bare = g1 {GameState.phase = Phase.PrecombatMain, GameState.remaining = Seq.empty}
        (searchlightId, board) = S.addPermanent searchlight S.alice bare
        offers g = length (filter (S.isCastOf elvesId) (Action.legalActions S.alice g))
        cast = S.runPure (searchlightAnswer S.alice (const Color.Green) (Just searchlightId)) board (S.cast S.alice elvesId)
    Spec.assertEqWith s "CR 605.3a the Elves are castable with the Searchlight and not without it" (fmap offers [board, bare]) [1, 0]
    Spec.assertEqWith s "CR 601.2h and alice pays for them by choosing herself and green" (fmap (`Projection.controllerOf` cast) (GameState.stack cast)) [Just S.alice]

-- Chooses `chosen` when a player is asked for, answers a yield prompt with the
-- option adding the colour `colourFor` gives the seat ASKED, and taps `source`
-- for a payment. Each answer is pinned out of the offered set, never built.
searchlightAnswer :: PlayerId.PlayerId -> (PlayerId.PlayerId -> Color.Color) -> Maybe ObjectId.ObjectId -> Prompt.Prompt r -> r
searchlightAnswer chosen colourFor source p = case p of
  Prompt.ChoosePlayer _ _ _ offered -> Maybe.fromMaybe (NonEmpty.head offered) (List.find (== chosen) (NonEmpty.toList offered))
  Prompt.ChooseManaYield _ asked _ candidates -> Maybe.fromMaybe (NonEmpty.head candidates) (List.find (\option -> fmap ManaUnit.manaType (Mana.yieldUnits option) == [ManaType.Colored (colourFor asked)]) (NonEmpty.toList candidates))
  Prompt.ChooseManaSource _ _ candidates -> List.find (`elem` NonEmpty.toList candidates) source
  Prompt.ChooseExtraManaSource {} -> Nothing
  _ -> S.identityAnswer p

-- CR 106.4's other half, which no printing reaches: a mana ability whose mana
-- the ACTIVATOR can never get. "{T}: Each opponent adds {C}" -- one activated
-- mana ability, an addition naming Relative Opponent, nothing else. Legitimate
-- under CR 605.1a (a mana ability is one whose effect could add mana to "a
-- player's" pool -- not its controller's) and CR 106.4, with Yurlok of Scorch
-- Thrash as the printed sibling. Scryfall oracle:"opponent adds", 2026-09-07,
-- no hit; Spectral Searchlight and Valleymaker choose a player, so their
-- activator can name themself.
--
-- What only this shape can prove is the SUPPLY road: whether the offer counts a
-- route's mana as the payer's before the payment finds out it is not. Yurlok
-- cannot -- "each player" includes its controller, so its share is the whole
-- yield and both readings agree.
almsEngineSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
almsEngineSpec s registry = Spec.describe s "Synthetic Alms Engine" $ do
  -- THREE seats, so "each opponent" cannot collapse onto the one other player
  -- and a route naming a set is told apart from one naming a seat.
  --
  -- The negative is a PAIR of boards differing in exactly one thing: the same
  -- alice, the same Meekstone, the same phase, and one {C} of her own in the
  -- pool on the second. So the cast being unoffered on the first is about whose
  -- mana the Engine makes and not about timing, the stack or the board.
  Spec.it s "CR 118.3 a route whose mana goes to somebody else supplies its activator nothing" $ do
    engine <- S.printingOf s registry "Synthetic Alms Engine"
    meekstone <- S.printingOf s registry "Meekstone"
    let (engineId, g1) = S.addPermanent engine S.alice S.threePlayerGame
        (stoneId, g2) = S.addHandCard meekstone S.alice g1
        board = g2 {GameState.phase = Phase.PrecombatMain, GameState.remaining = Seq.empty}
        withOwn = Mana.addMana S.alice [unitOf ManaType.Colorless] board
        offers g = length (filter (S.isCastOf stoneId) (Action.legalActions S.alice g))
        tapped = S.runPure S.identityAnswer board (S.tapForMana engineId)
    Spec.assertEqWith s "CR 118.3 the Meekstone is castable only off a {C} of her own, the Engine's being no supply of hers" (fmap offers [board, withOwn]) [0, 1]
    Spec.assertEqWith s "CR 106.4 and the {C} the Engine does make reaches each opponent instead" (fmap (\pid -> poolTypes pid tapped) [S.alice, S.bob, S.carol]) [[], [ManaType.Colorless], [ManaType.Colorless]]

-- CR 608.2c: a mana ability's clause gated by a printed "if" adds its mana only
-- when the "if" holds on the board. Synthetic Confluence Obelisk ({3} Artifact,
-- "{T}: Add {C}. If you control a Forest, add {G}. If you control an Island,
-- add {U}.") is ONE ability with two independent gates: no printing adds mana
-- from a gated clause without "instead" (MTGJSON dump of 2026-08-23, mana
-- ability lines matching "Add ... . If ... add", every hit an "instead").
--
-- Three boards differing one permanent at a time, bob's Island on all three so
-- "you control" cannot be read off the battlefield at large.
confluenceObeliskSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
confluenceObeliskSpec s registry = Spec.describe s "Synthetic Confluence Obelisk" $ do
  Spec.it s "CR 608.2c each gated clause adds its mana only when its own condition holds" $ do
    obelisk <- S.printingOf s registry "Synthetic Confluence Obelisk"
    forest <- S.printingOf s registry "Forest"
    island <- S.printingOf s registry "Island"
    let (obeliskId, g1) = S.addPermanent obelisk S.alice (Setup.emptyGame S.bothPlayers)
        (_, bare) = S.addPermanent island S.bob g1
        (_, withForest) = S.addPermanent forest S.alice bare
        (_, withBoth) = S.addPermanent island S.alice withForest
        tapped g = List.sort (poolTypes S.alice (S.runPure S.identityAnswer g (S.tapForMana obeliskId)))
    Spec.assertEqWith
      s
      "CR 608.2c tapping the Obelisk adds {C}, then {G} with a Forest, then {U} too with an Island of alice's own"
      (fmap tapped [bare, withForest, withBoth])
      [ [ManaType.Colorless],
        List.sort [ManaType.Colorless, ManaType.Colored Color.Green],
        List.sort [ManaType.Colorless, ManaType.Colored Color.Green, ManaType.Colored Color.Blue]
      ]
    -- The OFFER reads the same gates before anything is paid (CR 106.7), which
    -- the pool above cannot show: the payment decides each clause again.
    Spec.assertEqWith
      s
      "CR 106.7 the Obelisk could produce only {C} without a Forest, and {C} or {G} with one"
      (fmap (List.sort . Mana.manaTypesOf obeliskId) [bare, withForest])
      [[ManaType.Colorless], List.sort [ManaType.Colorless, ManaType.Colored Color.Green]]

-- Hickory Woodlot (Land, Oracle text checked against Scryfall 2026-09-26):
-- "This land enters tapped with two depletion counters on it. {T}, Remove a
-- depletion counter from this land: Add {G}{G}. If there are no depletion
-- counters on this land, sacrifice it."
--
-- CR 608.2c / 602.2b: the "if" is read as the ability resolves, after its cost
-- removed a counter, so the activation that takes the LAST counter sacrifices
-- the land and the one before it does not. The pair differs only in how many
-- counters the land starts with.
hickoryWoodlotSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
hickoryWoodlotSpec s registry = Spec.describe s "Hickory Woodlot" $ do
  Spec.it s "CR 608.2c the sacrifice reads the counter its own cost removed" $ do
    woodlot <- S.printingOf s registry "Hickory Woodlot"
    let (woodlotId, base) = S.addPermanent woodlot S.alice (Setup.emptyGame S.bothPlayers)
        depletion = CounterKind.Named (CounterName.UnsafeMkCounterName (Text.pack "depletion"))
        tapWith n =
          let after = S.runPure S.identityAnswer (S.addCounter depletion n woodlotId base) (S.tapForMana woodlotId)
           in (poolTypes S.alice after, Set.member woodlotId (GameState.battlefield after))
    Spec.assertEqWith
      s
      "CR 608.2c {G}{G} either way, and the land is sacrificed only when the cost took its last counter"
      (fmap tapWith [2, 1])
      [ ([ManaType.Colored Color.Green, ManaType.Colored Color.Green], True),
        ([ManaType.Colored Color.Green, ManaType.Colored Color.Green], False)
      ]

-- CR 106.4: "adds that mana" says nothing about whose pool, and CR 106.3's
-- "instructs a player to add" is the sentence a card fills in. Yurlok of Scorch
-- Thrash ({1}{B}{R}{G} Legendary Creature -- Lizard Shaman, Commander Legends)
-- is the pool's first printing whose ACTIVATED mana ability fills a pool that is
-- not its controller's: "{1}, {T}: Each player adds {B}{R}{G}."
--
-- All three lines are transcribed. The middle one -- "A player losing unspent
-- mana causes that player to lose that much life" -- is the player-axis static
-- PlayerEffect.LoseLifeForUnspentMana, scoped to each player as printed, and the
-- case below is what proves it charges for the mana CR 500.5's sweep actually
-- took.
yurlokSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
yurlokSpec s registry = Spec.describe s "Yurlok of Scorch Thrash" $ do
  -- THREE seats, and each of them casts or holds something different. Two would
  -- collapse "each player" onto "your opponent", and a board where only the
  -- payer's own pool is read cannot tell "each player" from "you" at all: alice
  -- is one of "each player", so her share of the yield is the same under both
  -- readings and her cast succeeds either way.
  --
  -- Bob SPENDS his share rather than merely holding it, which is what the title
  -- of #1673 is about: mana added to somebody else's pool is that player's to
  -- pay with. Trumpet Blast is {2}{R} and an instant, so it is payable exactly
  -- once from {B}{R}{G} -- the {R} is the only red he has -- and legal to cast
  -- with alice's creature spell still on the stack.
  --
  -- The mana is added DURING A COST PAYMENT (CR 605.3a's second window), not at
  -- priority: alice announces Mayhem Devil and taps the Yurlok inside the
  -- window, whose own {1} the Forest pays in a window nested inside that one.
  Spec.it s "CR 106.4 a mana ability's named recipient may spend what it adds mid-payment" $ do
    yurlok <- S.printingOf s registry "Yurlok of Scorch Thrash"
    forest <- S.printingOf s registry "Forest"
    devil <- S.printingOf s registry "Mayhem Devil"
    blast <- S.printingOf s registry "Trumpet Blast"
    let (yurlokId, g1) = S.addPermanent yurlok S.alice S.threePlayerGame
        (forestId, g2) = S.addPermanent forest S.alice g1
        (devilId, g3) = S.addHandCard devil S.alice g2
        (blastId, g4) = S.addHandCard blast S.bob g3
        board = g4 {GameState.phase = Phase.PrecombatMain, GameState.remaining = Seq.empty}
        castByAlice = S.runPure (tapsYurlok yurlokId forestId) board (S.cast S.alice devilId)
        after = S.runPure (tapsYurlok yurlokId forestId) castByAlice (S.cast S.bob blastId)
    -- CR 112.2 off the stack objects rather than the two hand ids: CR 601.2a
    -- moves the card and pawl mints the spell its own object, so "the player who
    -- put it on the stack" is the question, and paying for it is what put it
    -- there.
    Spec.assertEqWith s "CR 106.4 bob pays for his own instant out of the {B}{R}{G} the Yurlok put in HIS pool while alice was paying for hers" (fmap (\oid -> Projection.controllerOf oid after) (GameState.stack after)) [Just S.bob, Just S.alice]
    Spec.assertEqWith s "CR 106.4 and carol, who spent none of hers, is still holding the same three" (poolTypes S.carol after) [ManaType.Colored Color.Black, ManaType.Colored Color.Red, ManaType.Colored Color.Green]
  -- CR 500.5 / 119.3, the middle line at GAMEPLAY level: Engine.runStep runs the
  -- whole upkeep step -- its priority round and CR 703.4q's end-of-step pool
  -- empty -- rather than the sweep being called, so the loss is charged where
  -- the rule puts it. Rule 500.5's other road (Engine.endPhase, a step whose
  -- phase ends with it) shares Mana.emptyManaPools and cannot answer differently.
  --
  -- A PAIR of boards differing in EXACTLY one thing, the Yurlok on the
  -- battlefield: the same three seats, the same three pools, the same step. THREE
  -- seats holding three DIFFERENT pools, so "that much life" cannot be read as a
  -- flat charge and "a player" cannot collapse onto the controller.
  --
  -- carol's three are one ordinary green and two retained until end of turn (CR
  -- 500.5a's unit-axis carrier), and she pays ONE: rule 106.4's "the player is
  -- said to lose this mana" is what the sentence charges for, not the pool's
  -- size. Retention on the UNIT rather than a player-axis Upwelling, which would
  -- keep every seat's mana and leave nothing for the sweep to take anywhere.
  Spec.it s "CR 500.5 each player loses life for the unspent mana the step's end takes, and for no more" $ do
    yurlok <- S.printingOf s registry "Yurlok of Scorch Thrash"
    let (_, withYurlok) = S.addPermanent yurlok S.alice S.threePlayerGame
        retainedGreen = (unitOf (ManaType.Colored Color.Green)) {ManaUnit.retention = ManaRetention.UntilEndOfTurn}
        floated gs =
          Mana.addMana S.carol [unitOf (ManaType.Colored Color.Green), retainedGreen, retainedGreen] $
            Mana.addMana S.alice [unitOf (ManaType.Colored Color.Black), unitOf (ManaType.Colored Color.Red), unitOf (ManaType.Colored Color.Green)] gs
        upkeep gs = (floated gs) {GameState.phase = Phase.Beginning BeginningStep.Upkeep, GameState.priority = Just (GameState.activePlayer gs)}
        ran gs = S.runPure S.identityAnswer (upkeep gs) Engine.runStep
        charged = ran withYurlok
        uncharged = ran S.threePlayerGame
        lives gs = fmap (\pid -> S.lifeOf pid gs) [S.alice, S.bob, S.carol]
    Spec.assertEqWith s "CR 119.3 alice pays 3 life for the three the sweep took, carol 1 for the one of hers it took, and bob nothing" (lives charged) [Just 17, Just 20, Just 19]
    Spec.assertEqWith s "CR 103.4 and with no Yurlok on the battlefield the same three pools cost nobody anything" (lives uncharged) [Just 20, Just 20, Just 20]
    Spec.assertEqWith s "CR 500.5a the two retained green are what carol keeps, on either board" (fmap (poolTypes S.carol) [charged, uncharged]) [[ManaType.Colored Color.Green, ManaType.Colored Color.Green], [ManaType.Colored Color.Green, ManaType.Colored Color.Green]]
    Spec.assertEqWith s "CR 703.4q and alice's three are gone" (poolTypes S.alice charged) []

-- Takes the Yurlok wherever it is offered and the Forest wherever it is not --
-- which is the two windows exactly: the Yurlok's only mana ability is
-- mid-activation inside its own (CR 605.3c), so the Forest is the only candidate
-- there.
--
-- PINNED, not searched. An answerer taking any legal source would spend the
-- Forest on alice's own cost at the outer window and never reach the Yurlok, and
-- an answerer that searched for a payable line would find one again after the
-- mutation. Declining where neither is offered, so a window that has nothing
-- left closes instead of looping.
tapsYurlok :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
tapsYurlok yurlokId forestId p = case p of
  Prompt.ChooseManaSource _ _ candidates
    | elem yurlokId (NonEmpty.toList candidates) -> Just yurlokId
    | elem forestId (NonEmpty.toList candidates) -> Just forestId
    | otherwise -> Nothing
  Prompt.ChooseExtraManaSource {} -> Nothing
  _ -> S.identityAnswer p

-- The units of Alice's pool, so a test can look at a mana's TAGS and not only at
-- its type -- which is the whole of what CR 107.4h reads.
poolUnits :: GameState.GameState -> [ManaUnit.ManaUnit]
poolUnits gs = case Game.poolOf S.alice gs of
  Mana.Type.MkMana units -> units

-- What is floating, as types in pool order -- poolSize's discriminating twin,
-- for a case where the SIZE is the same under either answer.
poolTypes :: PlayerId.PlayerId -> GameState.GameState -> [ManaType.ManaType]
poolTypes pid gs = case Game.poolOf pid gs of
  Mana.Type.MkMana units -> fmap ManaUnit.manaType units

-- alice at `n` life on a board that already has permanents on it, which aliceAt
-- cannot build.
atLife :: Integer -> GameState.GameState -> GameState.GameState
atLife n gs = gs {GameState.players = Map.adjust (\p -> p {Player.life = n}) S.alice (GameState.players gs)}

-- The single activated ability of a printing that has exactly one -- Moltensteel
-- Dragon's "{R/P}: This creature gets +1/+0 until end of turn." Total because
-- the spec needs a value; a printing with no ability would fail the assertions that
-- follow rather than this lookup.
theAbility :: Printing.Printing -> ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)
theAbility p = case Face.activatedAbilities (S.combinedFace p) of
  ab : _ -> ab
  [] -> ActivatedAbility.MkActivatedAbility (Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) []) [] 0 (singleModeAbility [] Map.empty) [] Activator.Controller Nothing Nothing Nothing

-- CR 106.3's count read off the BOARD rather than off the card: Cabal Coffers
-- (Torment) prints "{2}, {T}: Add {B} for each Swamp you control." Oracle text
-- checked against Scryfall 2026-09-15.
--
-- The Swamps are TAPPED, so the Coffers' own yield is the only black mana
-- anywhere: with untapped Swamps a spell with three black pips would be paid for
-- whatever the count answered, and the test would prove the fixture. Three
-- Forests are the only untapped mana on the board -- two pay the Coffers' {2}
-- and the third pays Bloodletter of Aclazotz's {1}, which leaves its
-- {B}{B}{B} to the Coffers and to nothing else.
--
-- THE PAIR is the two boards this builds, alike in every permanent, every
-- tap state and the card in hand, and differing only in how many Swamps alice
-- controls: three against one.
cabalCoffersBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Int -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
cabalCoffersBoard coffers swamp forest bloodletter swamps =
  let (coffersId, gs1) = S.addPermanent coffers S.alice (S.landsInPlay forest 3)
      tapNew gs =
        let (oid, gsN) = S.addPermanent swamp S.alice gs
         in S.tapObject oid gsN
      gs2 = List.foldl' (\gs _ -> tapNew gs) gs1 (replicate swamps ())
      (spell, gs3) = S.addHandCard bloodletter S.alice gs2
   in ( coffersId,
        spell,
        gs3
          { GameState.activePlayer = S.alice,
            GameState.phase = Phase.PrecombatMain,
            GameState.priority = Just S.alice,
            GameState.remaining = Seq.empty
          }
      )

cabalCoffersSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
cabalCoffersSpec s registry = Spec.describe s "Cabal Coffers" $ do
  Spec.it s "CR 106.3 an addition's count is read off the board, so three Swamps add three black mana and one adds one" $ do
    coffers <- S.printingOf s registry "Cabal Coffers"
    swamp <- S.printingOf s registry "Swamp"
    forest <- S.printingOf s registry "Forest"
    bloodletter <- S.printingOf s registry "Bloodletter of Aclazotz"
    let (threeCoffers, threeSpell, three) = cabalCoffersBoard coffers swamp forest bloodletter 3
        (oneCoffers, oneSpell, one) = cabalCoffersBoard coffers swamp forest bloodletter 1
        castOf spell gs = snd (Engine.runGamePure S.identityAnswer gs (do S.cast S.alice spell; Stack.resolveTop))
        arrived = S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Bloodletter of Aclazotz") S.alice
    -- The gameplay-level assertion: a real spell whose three black pips only the
    -- Coffers can pay, cast end to end off the board CR 106.3 measured.
    Spec.assertEqWith s "CR 106.3 the three-Swamp board pays {B}{B}{B} and the one-Swamp board cannot" (fmap arrived [castOf threeSpell three, castOf oneSpell one]) [1, 0]
    -- The count itself, off CR 605.3b's inline road: what one activation put in
    -- the pool once its own {2} was paid.
    Spec.assertEqWith s "CR 106.3 one activation adds one unit per Swamp" (fmap (uncurry (tappedFor S.identityAnswer)) [(threeCoffers, three), (oneCoffers, one)]) [replicate 3 (ManaType.Colored Color.Black), [ManaType.Colored Color.Black]]
    -- CR 118.3's offer, which reaches the same count through manaSuppliesGiven
    -- rather than through a payment: the two cannot disagree about what the
    -- board yields.
    Spec.assertEqWith s "CR 118.3 the cast gate agrees with the payment at both counts" (fmap (uncurry (S.castable S.alice)) [(threeSpell, three), (oneSpell, one)]) [True, False]

-- CR 602.1b on a MANA ability: Mana Cache ({1}{R}{R} Enchantment, Oracle text
-- checked against Scryfall 2026-09-29): "At the beginning of each player's end
-- step, put a charge counter on this enchantment for each untapped land that
-- player controls. Remove a charge counter from this enchantment: Add {C}. Any
-- player may activate this ability but only during their turn before the end
-- step."
--
-- THREE SEATS, alice controlling the Cache on every board, so "their turn" can
-- be told apart from its controller's turn and from any one opponent's. The
-- Workhorse board is the control: alice's again, the same "Remove a counter:
-- Add {C}" shape, and no "any player" clause -- so what bob cannot do there is
-- CR 602.2's default rather than a want of mana.
manaCacheSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
manaCacheSpec s registry = Spec.describe s "Mana Cache" $ do
  let charge = CounterKind.Named (CounterName.UnsafeMkCounterName (Text.pack "charge"))
      colorless n = replicate n ManaType.Colorless
      pools gs = fmap (`poolTypes` gs) [S.alice, S.bob, S.carol]
      floated active phase = do
        cache <- S.printingOf s registry "Mana Cache"
        let (cacheId, gs) = cacheBoard cache charge active phase
        pure (cacheId, S.runPure tapEverything gs Engine.priorityLoop)

  -- CR 605.3a's priority window. The gameplay-level assertion is whose POOL
  -- the {C} lands in (CR 109.4a, CR 113.8: the activator controls the
  -- ability), and whose counters paid for it (CR 602.1a).
  Spec.it s "CR 602.1b the player whose turn it is activates alice's Cache and the mana is theirs" $ do
    (cacheId, onBob) <- floated S.bob Phase.PrecombatMain
    (_, onCarol) <- floated S.carol Phase.PrecombatMain
    (_, onAlice) <- floated S.alice Phase.PrecombatMain
    Spec.assertEqWith s "CR 113.8 on bob's turn all three {C} are bob's" (pools onBob) [[], colorless 3, []]
    Spec.assertEqWith s "CR 102.1 on carol's turn they are carol's, so this is not one opponent" (pools onCarol) [[], [], colorless 3]
    Spec.assertEqWith s "CR 602.2 and on alice's own turn hers" (pools onAlice) [colorless 3, [], []]
    Spec.assertEqWith s "CR 602.1a bob's activations spent the counters on alice's permanent" (S.counterOf charge cacheId onBob) 0

  -- CR 605.3a's payment window, through Cast.castSpell rather than the menu.
  -- Crucible of Worlds is {3}, all generic, and bob controls nothing, so the
  -- Cache's three counters are the only way to pay it.
  Spec.it s "CR 605.3a bob pays for his spell from alice's Cache, and not from her Workhorse" $ do
    cache <- S.printingOf s registry "Mana Cache"
    horse <- S.printingOf s registry "Workhorse"
    crucible <- S.printingOf s registry "Crucible of Worlds"
    let (cacheId, cacheBoard_) = cacheBoard cache charge S.bob Phase.PrecombatMain
        (horseId, horseBoard_) = cacheBoard horse CounterKind.PlusOnePlusOne S.bob Phase.PrecombatMain
        castBy gs =
          let (spellId, withSpell) = S.addHandCard crucible S.bob gs
              afterCast = S.runPure S.identityAnswer withSpell (S.cast S.bob spellId)
           in (S.castable S.bob spellId withSpell, S.runPure S.identityAnswer afterCast Stack.resolveTop)
        crucibles = S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Crucible of Worlds")) S.bob
        (cacheCastable, viaCache) = castBy cacheBoard_
        (horseCastable, viaHorse) = castBy horseBoard_
    Spec.assertEqWith s "CR 602.1b the Crucible resolved under bob" (crucibles viaCache) 1
    Spec.assertEqWith s "CR 602.2 alice's Workhorse, printing no such clause, pays bob nothing" (crucibles viaHorse) 0
    Spec.assertEqWith s "CR 602.1a the Cache's three counters paid for it" (S.counterOf charge cacheId viaCache) 0
    Spec.assertEqWith s "and the Workhorse kept its counters" (S.counterOf CounterKind.PlusOnePlusOne horseId viaHorse) 3
    Spec.assertEqWith s "CR 118.3 the cast gate agrees on both boards" [cacheCastable, horseCastable] [True, False]

  -- The printed trigger: "each player's end step" and "that player controls"
  -- both name the ACTIVE player (CR 603.2), not the Cache's controller. Every
  -- seat's untapped-land count differs, and bob's tapped Forest is the one land
  -- the "untapped" word excludes.
  Spec.it s "CR 122.1 at bob's end step the Cache gets a counter per untapped land bob controls" $ do
    cache <- S.printingOf s registry "Mana Cache"
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    let (cacheId, base) = S.addPermanent cache S.alice S.threePlayerGame
        lands = S.landsFor forest S.carol 1 (S.landsFor forest S.alice 4 (S.landsFor forest S.bob 2 base))
        (tappedForest, withTapped) = S.addPermanent forest S.bob lands
        stocked = List.foldl' (\g pid -> List.foldl' (\g2 _ -> snd (S.addLibraryCard piker pid g2)) g [1 .. (5 :: Int)]) (S.tapObject tappedForest withTapped) [S.alice, S.bob, S.carol]
        endStep = Phase.Ending EndingStep.EndStep
        began = Event.recordEvent (GameEvent.StepBegan (StepBegan.MkStepBegan endStep S.bob)) (stocked {GameState.phase = endStep, GameState.activePlayer = S.bob, GameState.remaining = Seq.empty})
        settled = S.runPure S.identityAnswer began Engine.settleForPriority
        after = S.runPure S.identityAnswer settled Engine.priorityLoop
    Spec.assertEqWith s "two: bob's untapped Forests, not alice's four or carol's one" (S.counterOf charge cacheId after) 2

-- One `printing` under alice's control carrying three `kind` counters, at three
-- seats, on `active`'s turn in `phase`; an empty `remaining` pins the phase.
-- Nothing else is on the board, so every mana comes through this permanent.
cacheBoard :: Printing.Printing -> CounterKind.CounterKind Keyword.Keyword -> PlayerId.PlayerId -> Phase.Phase -> (ObjectId.ObjectId, GameState.GameState)
cacheBoard printing kind active phase =
  let (oid, gs) = S.addPermanent printing S.alice S.threePlayerGame
   in (oid, (S.addCounter kind 3 oid gs) {GameState.activePlayer = active, GameState.phase = phase, GameState.remaining = Seq.empty})
