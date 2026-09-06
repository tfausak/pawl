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
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Action as Action
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
import qualified Pawl.Types.Departure as Departure.Type
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Facing as Facing
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
import qualified Pawl.Types.PaymentMoment as PaymentMoment
import qualified Pawl.Types.PaymentSubject as PaymentSubject
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Pool as Pool
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Status as Status
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TargetSlot as TargetSlot
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChange as ZoneChange

-- Cast `creature` off `nLands` copies of `land`, then resolve it.
resolvedCreature :: Printing.Printing -> Printing.Printing -> Int -> GameState.GameState
resolvedCreature land creature nLands =
  let (base, oid) = S.handOne creature (S.landsInPlay land nLands)
      afterCast = snd (Engine.runGamePure S.identityAnswer base (S.cast S.alice oid))
   in snd (Engine.runGamePure S.identityAnswer afterCast Stack.resolveTop)

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

castabilitySpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
castabilitySpec s registry = Spec.describe s "Castability" $ do
  Spec.it s "War Mammoth is cast off four Forests and resolves onto the battlefield" $ do
    forest <- S.printingOf s registry "Forest"
    warMammoth <- S.printingOf s registry "War Mammoth"
    let gs = resolvedCreature forest warMammoth 4
    Spec.assertEqWith s "stack empty" (length (GameState.stack gs)) 0
    Spec.assertEqWith s "one creature in play" (S.creaturesInPlay S.alice gs) 1
    Spec.assertEqWith s "lands tapped" (S.tappedCount S.alice gs) 4

  Spec.it s "Typhoid Rats is cast off one Swamp and resolves onto the battlefield" $ do
    swamp <- S.printingOf s registry "Swamp"
    typhoidRats <- S.printingOf s registry "Typhoid Rats"
    let gs = resolvedCreature swamp typhoidRats 1
    Spec.assertEqWith s "stack empty" (length (GameState.stack gs)) 0
    Spec.assertEqWith s "one creature in play" (S.creaturesInPlay S.alice gs) 1
    Spec.assertEqWith s "lands tapped" (S.tappedCount S.alice gs) 1

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
oneUnit mt = Mana.Type.MkMana [ManaUnit.MkManaUnit {ManaUnit.manaType = mt, ManaUnit.tags = Set.empty, ManaUnit.retention = ManaRetention.Ordinary, ManaUnit.restriction = Nothing, ManaUnit.rider = Nothing}]

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
        let after = S.runPure S.identityAnswer gs (Cost.tapForMana S.manaPerformer oid)
        Spec.assertEqWith s "tapped" (S.tappedCount S.alice after) 1
        Spec.assertEqWith
          s
          "pool"
          (Game.poolOf S.alice after)
          (Mana.Type.MkMana [ManaUnit.MkManaUnit {ManaUnit.manaType = ManaType.Colored Color.Red, ManaUnit.tags = Set.empty, ManaUnit.retention = ManaRetention.Ordinary, ManaUnit.restriction = Nothing, ManaUnit.rider = Nothing}])

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
        Spec.assertEqWith s "emptied" (poolSize S.alice (Mana.emptyManaPools (S.runPure S.identityAnswer gs (Cost.tapForMana S.manaPerformer oid)))) 0

  -- CR 122.1 / CR 105.4: "{T}: Add {C}. If Gemstone Caverns has a luck counter on
  -- it, instead add one mana of any color." pawl carries the sentence as two
  -- gated abilities whose conditions are complements (ActivatedAbility.condition
  -- over Quantity.ObjectCounters), so exactly one exists at a time and "instead"
  -- falls out of the pair -- see Pawl.Engine.Mana.manaRoutesOfGiven for why a
  -- single ability with two conditional CLAUSES would offer both at once (#1924).
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
    Spec.assertEqWith s "pool" (Game.poolOf S.alice (S.runPure S.identityAnswer gs (Cost.tapForMana S.manaPerformer towerId))) (oneUnit ManaType.Colorless)
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
    Spec.assertEqWith s "pool" (Game.poolOf S.alice (S.runPure S.identityAnswer gs (Cost.tapForMana S.manaPerformer towerId))) (oneUnit ManaType.Colorless)
    Spec.assertBool s (Subtype.Mountain `notElem` Set.toList (Projection.subtypesOf towerId gs)) "and the layer-4 set did not happen either"

  -- The same board with the clause satisfied, which is what keeps the two cases
  -- above from passing on a gate wired SHUT: one conqueror counter arrives,
  -- Zhao's effect starts to apply, and the Tower is a Mountain that taps for red
  -- alone.
  Spec.it s "CR 604.2/305.7 a conqueror counter turns the clause on, and the Tower taps for {R}" $ do
    zhao <- S.printingOf s registry "Zhao, the Moon Slayer"
    reliquaryTower <- S.printingOf s registry "Reliquary Tower"
    let (towerId, gs) = zhaoBoard zhao reliquaryTower [conquerorCounter]
    Spec.assertEqWith s "pool" (Game.poolOf S.alice (S.runPure S.identityAnswer gs (Cost.tapForMana S.manaPerformer towerId))) (oneUnit (ManaType.Colored Color.Red))
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
              ActivatedAbility.modal = singleModeAbility [Effect.AddMana (ManaAddition.MkManaAddition (PlayerRef.Relative PlayerRelation.You) (ManaProduction.OfType (ManaType.Colored Color.Green)) 1 ManaRetention.Ordinary Nothing Nothing)] Map.empty,
              ActivatedAbility.maximumX = [],
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
                  [Effect.AddMana (ManaAddition.MkManaAddition (PlayerRef.Relative PlayerRelation.You) (ManaProduction.OfType (ManaType.Colored Color.Green)) 1 ManaRetention.Ordinary Nothing Nothing)]
                  (Map.singleton (SlotName.MkSlotName (Text.pack "x")) (TargetSlot.required Pool.AnyTarget Nothing)),
              ActivatedAbility.maximumX = [],
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
    Spec.assertEqWith s "asked to tap the Elf, it is tapped" (tappedElf (S.runPure (prefersSource elfId) gs (Cost.pay S.manaPerformer PaymentMoment.OutsideResolution PaymentSubject.ForNeither Nothing ManaSpending.AsProduced S.alice elfId cost))) (Just TapState.Tapped)
    Spec.assertEqWith s "asked to spare the Elf, it is untapped" (tappedElf (S.runPure (avoidsSource elfId) gs (Cost.pay S.manaPerformer PaymentMoment.OutsideResolution PaymentSubject.ForNeither Nothing ManaSpending.AsProduced S.alice elfId cost))) (Just TapState.Untapped)

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
        after = S.runPure S.identityAnswer gs0 (Cost.tapForMana S.manaPerformer oid)
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
-- One unit, because every candidate a colour choice reaches is one mana: a
-- source whose yield is longer offers it whole (Sol Ring), and no card in the
-- pool both chooses a colour and adds twice.
prefersColor :: Color.Color -> Prompt.Prompt r -> r
prefersColor wanted p = case p of
  Prompt.ChooseManaYield _ _ _ candidates ->
    S.optionYielding (Mana.Type.MkMana [ManaUnit.MkManaUnit {ManaUnit.manaType = ManaType.Colored wanted, ManaUnit.tags = Set.empty, ManaUnit.retention = ManaRetention.Ordinary, ManaUnit.restriction = Nothing, ManaUnit.rider = Nothing}]) candidates
  _ -> S.identityAnswer p

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
tappedFor answer oid gs = case Game.poolOf S.alice (S.runPure answer gs (Cost.tapForMana S.manaPerformer oid)) of
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

  -- The gameplay-level proof (design.md section 4): a real card, cast end to
  -- end off a source that produces no black mana until its controller says so.
  Spec.it s "CR 605.3b Typhoid Rats is cast off a lone Birds of Paradise that taps for black" $ do
    birds <- S.printingOf s registry "Birds of Paradise"
    typhoidRats <- S.printingOf s registry "Typhoid Rats"
    let resolved = castOffBoard (prefersColor Color.Black) [birds] typhoidRats
    Spec.assertEqWith s "stack empty" (length (GameState.stack resolved)) 0
    Spec.assertEqWith s "the Birds and the Rats" (S.creaturesInPlay S.alice resolved) 2
    Spec.assertEqWith s "the Birds is tapped" (S.tappedCount S.alice resolved) 1

  -- The discriminating half: identical board, identical spell, one different
  -- answer. If the engine picked the colour itself this would pass too.
  Spec.it s "the color is the player's: a Birds tapped for green does not pay {B}" $ do
    birds <- S.printingOf s registry "Birds of Paradise"
    typhoidRats <- S.printingOf s registry "Typhoid Rats"
    let resolved = castOffBoard (prefersColor Color.Green) [birds] typhoidRats
    Spec.assertEqWith s "the Rats never resolved" (S.creaturesInPlay S.alice resolved) 1
    -- CR 601.2h: partial payments are not allowed, so the failed payment is
    -- rolled back whole and the Birds is left untapped.
    Spec.assertEqWith s "payment rolled back" (S.tappedCount S.alice resolved) 0

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
           in State.execState (Engine.runGame countingAnswer gs (Cost.tapForMana S.manaPerformer oid)) 0
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
      g3 = S.runPure S.identityAnswer g2 (Cost.tapForMana S.manaPerformer aliceForest)
   in (extras, S.runPure S.identityAnswer g3 (Cost.tapForMana S.manaPerformer bobForest))

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
        ended = Mana.emptyManaPools floated
    Spec.assertEqWith s "alice floated one" (poolSize S.alice floated) 1
    Spec.assertEqWith s "bob floated one" (poolSize S.bob floated) 1
    Spec.assertEqWith s "alice lost it" (poolSize S.alice ended) 0
    Spec.assertEqWith s "bob lost it" (poolSize S.bob ended) 0

  Spec.it s "CR 613.11 Upwelling keeps its controller's unspent mana" $ do
    forest <- S.printingOf s registry "Forest"
    upwelling <- S.printingOf s registry "Upwelling"
    let (_, floated) = floatedPools [upwelling] forest
    Spec.assertEqWith s "alice kept it" (poolSize S.alice (Mana.emptyManaPools floated)) 1

  -- The discriminating half of the scope. Alice controls the Upwelling and
  -- bob keeps his mana anyway -- that is what PlayerScope.EachPlayer means,
  -- and a You-scoped implementation passes the test above and fails this one.
  Spec.it s "CR 613.11 Upwelling is symmetric: an opponent's unspent mana is kept too" $ do
    forest <- S.printingOf s registry "Forest"
    upwelling <- S.printingOf s registry "Upwelling"
    let (_, floated) = floatedPools [upwelling] forest
    Spec.assertEqWith s "bob kept it, though alice controls the Upwelling" (poolSize S.bob (Mana.emptyManaPools floated)) 1

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
        Spec.assertEqWith s "kept while it stands" (poolSize S.alice (Mana.emptyManaPools floated)) 1
        Spec.assertEqWith s "alice loses it once it is gone" (poolSize S.alice (Mana.emptyManaPools gone)) 0
        Spec.assertEqWith s "and so does bob" (poolSize S.bob (Mana.emptyManaPools gone)) 0

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
        floated = S.runPure S.identityAnswer (S.runPure S.identityAnswer g3 (Cost.tapForMana S.manaPerformer forestId)) (Cost.tapForMana S.manaPerformer islandId)
        ended = Mana.emptyManaPools floated
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
        floated = S.runPure S.identityAnswer (S.runPure S.identityAnswer g3 (Cost.tapForMana S.manaPerformer alicesForest)) (Cost.tapForMana S.manaPerformer bobsForest)
        ended = Mana.emptyManaPools floated
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
        floated = S.runPure S.identityAnswer (S.runPure S.identityAnswer g3 (Cost.tapForMana S.manaPerformer forestId)) (Cost.tapForMana S.manaPerformer islandId)
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
priorityWindowSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
priorityWindowSpec s registry = Spec.describe s "CR 605.3a the priority window" $ do
  Spec.it s "CR 605.3a a player with priority may fill their pool with nothing to pay for" $ do
    forest <- S.printingOf s registry "Forest"
    omnath <- S.printingOf s registry "Omnath, Locus of Mana"
    let (omnathId, board) = priorityWindowBoard omnath forest
        after = S.runPure tapEverything board Engine.priorityLoop
        -- The paired control: the same board, the same loop, and the one
        -- difference is that alice declines the action. Every assertion below
        -- reads the other way on it, so none of them can be passing on
        -- something the fixture would have done anyway.
        passed = S.runPure S.identityAnswer board Engine.priorityLoop
    Spec.assertEqWith s "three green floating, and nothing asked for them" (poolSize S.alice after) 3
    Spec.assertEqWith s "all three Forests are tapped" (S.tappedCount S.alice after) 3
    Spec.assertEqWith s "CR 605.3b nothing went on the stack" (length (GameState.stack after)) 0
    Spec.assertEqWith s "CR 613.4c Omnath counts all three" (Projection.powerOf omnathId after) (Just 4)
    Spec.assertEqWith s "and its toughness with them" (Projection.toughnessOf omnathId after) (Just 4)
    Spec.assertEqWith s "the control: passing floats nothing" (poolSize S.alice passed) 0
    Spec.assertEqWith s "and leaves Omnath its printed 1/1" (Projection.powerOf omnathId passed) (Just 1)

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

  -- CR 118.3 reaches this window too: the options are gated by whether the
  -- ability's own activation cost can be paid (CR 602.2b), the same predicate
  -- the payment window is gated by. Phyrexian Tower is the pool's one permanent
  -- pairing a mana ability whose cost can fail -- "{T}, Sacrifice a creature:
  -- Add {B}{B}" -- with an always-payable "{T}: Add {C}", so the permanent is on
  -- the menu either way and what changes is what taking it can yield.
  -- Transmogrant Altar's cost can fail too and has no such sibling, which is
  -- why transmograntAltarSpec asserts the whole permanent off the menu.
  Spec.it s "CR 118.3 what the activation may yield is gated by its own cost" $ do
    tower <- S.printingOf s registry "Phyrexian Tower"
    piker <- S.printingOf s registry "Goblin Piker"
    let alone = towerBoard tower Nothing
        withPiker = towerBoard tower (Just piker)
        run gs = S.runPure tapEverythingForBlack gs Engine.priorityLoop
    Spec.assertEqWith s "with a creature to give, the priority window floats {B}{B}" (poolTypes S.alice (run withPiker)) [ManaType.Colored Color.Black, ManaType.Colored Color.Black]
    Spec.assertEqWith s "CR 601.2h and the creature paid for it" (S.creaturesInPlay S.alice (run withPiker)) 0
    Spec.assertEqWith s "with none, the same window can only take the {C}" (poolTypes S.alice (run alone)) [ManaType.Colorless]

  -- CR 117.3c: "if a player has priority when they ... activate an ability ...
  -- that player receives priority afterward." Who is asked next is the only
  -- thing a game observes about who holds it, so the sequence is the assertion --
  -- SpecialActionSpec's shape for CR 116.3, and both halves of the arm ride on
  -- it. It is BOB's turn, so his pass is already standing when alice taps:
  -- retaining priority puts her second prompt before his second, and restarting
  -- CR 117.4's pass count is what makes him asked a second time at all.
  Spec.it s "CR 117.3c the activator receives priority again afterward" $ do
    forest <- S.printingOf s registry "Forest"
    let (_, withForest) = S.addPermanent forest S.alice (Setup.emptyGame S.bothPlayers)
        board = withForest {GameState.activePlayer = S.bob, GameState.phase = Phase.PrecombatMain, GameState.remaining = Seq.empty}
        asked = State.execState (Engine.runGame tapOnceThenPass board Engine.priorityLoop) []
    Spec.assertEqWith s "bob passes, alice taps, alice is asked again, and only then is bob asked again" asked [S.bob, S.alice, S.alice, S.bob]

-- alice, active, in her precombat main phase with an empty hand and an empty
-- stack: one Omnath and three Forests, so the only mana that can ever reach her
-- pool is mana she activated a mana ability for while holding priority.
priorityWindowBoard :: Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, GameState.GameState)
priorityWindowBoard omnath forest =
  let (omnathId, g1) = S.addPermanent omnath S.alice (Setup.emptyGame S.bothPlayers)
      board = foldr (\_ gs -> snd (S.addPermanent forest S.alice gs)) g1 [1 :: Int, 2, 3]
   in (omnathId, board {GameState.phase = Phase.PrecombatMain, GameState.remaining = Seq.empty})

-- alice, active, in her precombat main phase: one Phyrexian Tower, and a
-- creature to sacrifice or not.
towerBoard :: Printing.Printing -> Maybe Printing.Printing -> GameState.GameState
towerBoard tower victim =
  let g1 = snd (S.addPermanent tower S.alice (Setup.emptyGame S.bothPlayers))
      g2 = case victim of
        Nothing -> g1
        Just printing -> snd (S.addPermanent printing S.alice g1)
   in g2 {GameState.phase = Phase.PrecombatMain, GameState.remaining = Seq.empty}

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
-- Ornitier and every other hit ride on "only once each turn" -- which
-- Pawl.Types.ActivationRestriction still cannot say, its OnlyOnce arm being CR
-- 702.177a's per-GAME clause (#3306) -- or on "only if
-- <condition>", which is its OnlyIf arm and names no window, so neither kind
-- reaches the phase axis this pair is about.
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
riderWindowSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
riderWindowSpec s registry = Spec.describe s "CR 605.3a a printed rider gates both windows" $ do
  Spec.it s "CR 500.1 the priority window offers the source only inside the rider's step" $ do
    spring <- S.printingOf s registry "Synthetic Ember Spring"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let (inUpkeep, _) = springBoard spring bolt
        inMain = inUpkeep {GameState.phase = Phase.PrecombatMain}
        floated gs = poolTypes S.alice (S.runPure tapEverything gs Engine.priorityLoop)
        offers gs = filter isManaActivation (Action.legalActions S.alice gs)
    Spec.assertEqWith s "in her upkeep the rider admits it and {R} floats" (floated inUpkeep) [ManaType.Colored Color.Red]
    Spec.assertEqWith s "in her main phase the same board floats nothing" (floated inMain) []
    Spec.assertEqWith s "the menu it was taken from carries the one offer" (length (offers inUpkeep)) 1
    Spec.assertEqWith s "and carries none outside the window" (length (offers inMain)) 0

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
laviniaTurnRiderSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
laviniaTurnRiderSpec s registry = Spec.describe s "CR 102.1 a rider naming a turn and no phase" $ do
  Spec.it s "CR 605.3a the priority window offers her mana ability on either opponent's turn and not on hers" $ do
    lavinia <- S.printingOf s registry "Lavinia, Foil to Conspiracy"
    wretch <- S.printingOf s registry "Withered Wretch"
    piker <- S.printingOf s registry "Goblin Piker"
    let boardOn active = snd (laviniaBoard lavinia wretch piker active)
        floated active = poolTypes S.alice (S.runPure tapEverything (boardOn active) Engine.priorityLoop)
        offers active = length (filter isManaActivation (Action.legalActions S.alice (boardOn active)))
        colorless = [ManaType.Colorless, ManaType.Colorless]
    -- The gameplay-level assertion, and the whole point of three seats: the mana
    -- exists on BOTH opponents' turns and on neither reading of "an opponent" is
    -- alice's own turn one.
    Spec.assertEqWith s "CR 102.2 on bob's turn the rider admits it and {C}{C} floats" (floated S.bob) colorless
    Spec.assertEqWith s "CR 806.1 on carol's turn too, so this is not an enumeration of one opponent" (floated S.carol) colorless
    Spec.assertEqWith s "CR 109.5 on her own turn the same board floats nothing" (floated S.alice) []
    -- The menu those activations were taken from, as a supporting check.
    Spec.assertEqWith s "the offer follows the pool at all three seats" (fmap offers [S.bob, S.carol, S.alice]) [1, 1, 0]

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
           in Activate.activatable S.alice wretchId ability gs
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

isManaActivation :: Action.Type.Action -> Bool
isManaActivation action = case action of
  Action.Type.ActivateManaAbility _ -> True
  Action.Type.Activate _ _ -> False
  Action.Type.Cast {} -> False
  Action.Type.Play {} -> False
  Action.Type.TurnFaceUp {} -> False
  Action.Type.Unlock _ _ -> False
  Action.Type.DiscardFromHand _ -> False
  Action.Type.Plot _ -> False
  Action.Type.Foretell _ -> False
  Action.Type.PutCompanionIntoHand -> False
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

-- tapEverything's actions with prefersDoubleBlack's colour answer, so the one
-- thing separating the two Phyrexian Tower boards is whether {B}{B} was on offer.
tapEverythingForBlack :: Prompt.Prompt r -> r
tapEverythingForBlack p = case p of
  Prompt.ChooseAction {} -> tapEverything p
  _ -> prefersDoubleBlack p

-- tapEverything again, recording which player each priority prompt went to:
-- the record CR 117.3c is asserted on. Its board holds one source, so the
-- activation happens once and every later prompt passes.
tapOnceThenPass :: Prompt.Prompt r -> State.State [PlayerId.PlayerId] r
tapOnceThenPass p = case p of
  Prompt.ChooseAction _ pid _ -> do
    State.modify' (<> [pid])
    pure (tapEverything p)
  _ -> pure (S.identityAnswer p)

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

  -- The gameplay-level proof (design.md section 4): a real spell cast end to
  -- end off a single permanent, which no one-mana-per-source engine can do.
  Spec.it s "CR 601.2g Sapphire Medallion is cast off a lone Sol Ring" $ do
    solRing <- S.printingOf s registry "Sol Ring"
    sapphireMedallion <- S.printingOf s registry "Sapphire Medallion"
    let resolved = castOffBoard S.identityAnswer [solRing] sapphireMedallion
    Spec.assertEqWith s "stack empty" (length (GameState.stack resolved)) 0
    Spec.assertEqWith s "the Medallion resolved" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Sapphire Medallion") S.alice resolved) 1
    Spec.assertEqWith s "the Sol Ring is tapped" (S.tappedCount S.alice resolved) 1
    Spec.assertEqWith s "and both mana were spent" (poolSize S.alice resolved) 0

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
    Spec.assertEqWith s "nothing to ask" (State.execState (Engine.runGame countingAnswer gs (Cost.tapForMana S.manaPerformer solRingId)) 0) 0

-- Answers Prompt.ChooseManaYield with `wanted`'s LONGEST yield, and defers every
-- other source's prompt to S.identityAnswer, which takes the head. A payment off
-- several two-yield sources needs a different answer from each, and the prompt
-- carries the object it is about, so keying on that is what lets one answerer
-- send one Palladium Myr to its Forest and the other to its {C}{C}.
prefersLongYieldFrom :: ObjectId.ObjectId -> Prompt.Prompt r -> r
prefersLongYieldFrom wanted p = case p of
  Prompt.ChooseManaYield _ _ oid candidates
    | oid == wanted ->
        let size option = case ManaOption.yield option of
              Mana.Type.MkMana units -> length units
         in List.maximumBy (\a b -> compare (size a) (size b)) (NonEmpty.toList candidates)
  _ -> S.identityAnswer p

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
  Spec.it s "CR 405.6c Ancient Tomb charges its damage for the mana it makes" $ do
    ancientTomb <- S.printingOf s registry "Ancient Tomb"
    sapphireMedallion <- S.printingOf s registry "Sapphire Medallion"
    let resolved = castOffBoard S.identityAnswer [ancientTomb] sapphireMedallion
    Spec.assertEqWith s "alice took 2 for the mana" (S.lifeOf S.alice resolved) (Just 18)
    Spec.assertEqWith s "and only alice, CR 109.5's you being the activator" (S.lifeOf S.bob resolved) (Just 20)
    Spec.assertEqWith s "the Medallion was still cast off it" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Sapphire Medallion") S.alice resolved) 1

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
        after = S.runPure S.identityAnswer board (Cost.tapForMana S.manaPerformer tombId)
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
        green = ManaUnit.MkManaUnit {ManaUnit.manaType = ManaType.Colored Color.Green, ManaUnit.tags = Set.empty, ManaUnit.retention = ManaRetention.Ordinary, ManaUnit.restriction = Nothing, ManaUnit.rider = Nothing}
        colorless = ManaUnit.MkManaUnit {ManaUnit.manaType = ManaType.Colorless, ManaUnit.tags = Set.empty, ManaUnit.retention = ManaRetention.Ordinary, ManaUnit.restriction = Nothing, ManaUnit.rider = Nothing}
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

  -- And the offer is honoured: the same board casts Living Plane end to end,
  -- which it can only do by tapping one Myr for its Forest's {G} and the other
  -- for {C}{C} -- the mixed choice the transposing model could not represent.
  Spec.it s "CR 601.2g Living Plane is cast off Ashaya and two Palladium Myrs" $ do
    ashaya <- S.printingOf s registry "Ashaya, Soul of the Wild"
    palladiumMyr <- S.printingOf s registry "Palladium Myr"
    livingPlane <- S.printingOf s registry "Living Plane"
    let (_, g1) = S.addPermanent ashaya S.alice (Setup.emptyGame S.bothPlayers)
        (firstMyrId, g2) = S.addPermanent palladiumMyr S.alice g1
        (_, g3) = S.addPermanent palladiumMyr S.alice g2
        (withSpell, planeId) = S.handOne livingPlane g3
        cast = S.runPure (prefersLongYieldFrom firstMyrId) withSpell (S.cast S.alice planeId)
        resolved = S.runPure S.identityAnswer cast Stack.resolveTop
    Spec.assertEqWith s "stack empty" (length (GameState.stack resolved)) 0
    Spec.assertEqWith s "Living Plane resolved" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Living Plane") S.alice resolved) 1
    Spec.assertEqWith s "all three sources tapped" (S.tappedCount S.alice resolved) 3
    Spec.assertEqWith s "and nothing was left over" (poolSize S.alice resolved) 0

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

  -- The gameplay-level proof (design.md section 4): a real spell cast end to
  -- end off the one synthetic permanent. Liquimetal Coating is a plain {2}
  -- Artifact -- no colour in the cost and nothing to target -- so quantity is
  -- the only thing the payment can turn on.
  Spec.it s "CR 601.2g Liquimetal Coating is cast off a lone Wellspring" $ do
    wellspring <- S.printingOf s registry "Synthetic Prismatic Wellspring"
    liquimetalCoating <- S.printingOf s registry "Liquimetal Coating"
    let resolved = castOffBoard S.identityAnswer [wellspring] liquimetalCoating
    Spec.assertEqWith s "stack empty" (length (GameState.stack resolved)) 0
    Spec.assertEqWith s "the Coating resolved" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Liquimetal Coating") S.alice resolved) 1
    Spec.assertEqWith s "the Wellspring is tapped" (S.tappedCount S.alice resolved) 1
    Spec.assertEqWith s "and both mana were spent" (poolSize S.alice resolved) 0

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

  -- The discriminating half: same card, different answer, different mana. An
  -- engine picking the colour itself would give one of these the other's mana.
  Spec.it s "CR 607.2d the colour is the player's, not the engine's" $ do
    mountain <- S.printingOf s registry "Mountain"
    coldsteel <- S.printingOf s registry "Coldsteel Heart"
    let run wanted = case resolvedColdsteel wanted mountain coldsteel of
          (after, Just oid) -> Mana.manaTypesOf oid after
          _ -> []
    Spec.assertEqWith s "naming red" (run Color.Red) [ManaType.Colored Color.Red]
    Spec.assertEqWith s "naming green" (run Color.Green) [ManaType.Colored Color.Green]

  -- No colour chosen yields NO mana rather than a fallback colour. Unreachable
  -- through play -- CR 614.1c settles the choice as the permanent enters -- so a
  -- fixture write is what puts a Coldsteel Heart in that state, and the point is
  -- that the engine invents nothing when it finds one.
  Spec.it s "CR 607.2d a Coldsteel Heart placed with no colour chosen produces nothing" $ do
    coldsteel <- S.printingOf s registry "Coldsteel Heart"
    let (oid, gs) = S.addPermanent coldsteel S.alice (Setup.emptyGame S.bothPlayers)
    Spec.assertEqWith s "chosenColor is unset" (fmap Object.chosenColor (Game.lookupObject oid gs)) (Just Nothing)
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
        after = S.runPure (prefersColor Color.Black) gs (Cost.tapForMana S.manaPerformer oid)
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
        tapEach = List.foldl' (\g oid -> S.runPure (prefersColor Color.Black) g (Cost.tapForMana S.manaPerformer oid)) g3
    Spec.assertEqWith s "one Confluence, 1 life" (S.lifeOf S.alice (tapEach [firstId])) (Just 19)
    Spec.assertEqWith s "both of them, 2 life" (S.lifeOf S.alice (tapEach [firstId, secondId])) (Just 18)
    Spec.assertEqWith s "and the Forest adds a third mana for nothing" (S.lifeOf S.alice (tapEach [firstId, secondId, forestId])) (Just 18)
    Spec.assertEqWith s "three mana in the pool" (poolSize S.alice (tapEach [firstId, secondId, forestId])) 3

  -- The gameplay-level proof (design.md section 4): a real spell cast end to end
  -- off the card, with the life leaving the caster as part of paying for it.
  Spec.it s "CR 601.2g Typhoid Rats is cast off a lone Mana Confluence, for 1 life" $ do
    manaConfluence <- S.printingOf s registry "Mana Confluence"
    typhoidRats <- S.printingOf s registry "Typhoid Rats"
    let resolved = castOffBoard (prefersColor Color.Black) [manaConfluence] typhoidRats
    Spec.assertEqWith s "the Rats resolved" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Typhoid Rats") S.alice resolved) 1
    Spec.assertEqWith s "and the black mana cost her 1 life" (S.lifeOf S.alice resolved) (Just 19)

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
        black = Mana.Type.MkMana [ManaUnit.MkManaUnit {ManaUnit.manaType = ManaType.Colored Color.Black, ManaUnit.tags = Set.empty, ManaUnit.retention = ManaRetention.Ordinary, ManaUnit.restriction = Nothing, ManaUnit.rider = Nothing}]
        -- The offered black option that is CR 305.6's free one, or the printed
        -- one that charges the life -- `printed` picks which.
        buysBlack :: Bool -> Prompt.Prompt r -> r
        buysBlack printed p = case p of
          Prompt.ChooseManaYield _ _ _ candidates ->
            let wanted option = ManaOption.yield option == black && (ManaOption.cost option /= Mana.intrinsicManaCost) == printed
             in Maybe.fromMaybe (NonEmpty.head candidates) (List.find wanted (NonEmpty.toList candidates))
          _ -> S.identityAnswer p
        blacks = filter ((==) black . ManaOption.yield) (optionsOffered confluenceId gs)
        free = S.runPure (buysBlack False) gs (Cost.tapForMana S.manaPerformer confluenceId)
        bought = S.runPure (buysBlack True) gs (Cost.tapForMana S.manaPerformer confluenceId)
    Spec.assertEqWith s "both ways of adding black are offered" (length blacks) 2
    Spec.assertEqWith s "charging two different costs" (length (List.nub (fmap ManaOption.cost blacks))) 2
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
    let unit = ManaUnit.MkManaUnit {ManaUnit.manaType = ManaType.Colored Color.Black, ManaUnit.tags = Set.empty, ManaUnit.retention = ManaRetention.Ordinary, ManaUnit.restriction = Nothing, ManaUnit.rider = Nothing}
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
   in State.execState (Engine.runGame step gs (Cost.tapForMana S.manaPerformer oid)) []

-- The colours of those candidates alone, for a caller that is asking what the
-- source could produce rather than what it charges.
yieldsOffered :: ObjectId.ObjectId -> GameState.GameState -> [[ManaType.ManaType]]
yieldsOffered oid gs = fmap (fmap ManaUnit.manaType . Mana.unitsOf . ManaOption.yield) (optionsOffered oid gs)

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
        tapWith answer = S.runPure answer board (Cost.tapForMana S.manaPerformer towerId)
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

  -- The gameplay-level proof (design.md section 4). Typhoid Rats is {B} and
  -- targets nothing as it is cast, so the whole cast turns on whether the Pet
  -- could be activated -- on a board where nothing else makes mana and the Pet
  -- is both tapped and sick, which is to say refused twice over before this.
  Spec.it s "CR 605.3a a tapped, sick Blood Pet pays for Typhoid Rats" $ do
    bloodPet <- S.printingOf s registry "Blood Pet"
    typhoidRats <- S.printingOf s registry "Typhoid Rats"
    let (petId, g1) = S.addPermanent bloodPet S.alice (Setup.emptyGame S.bothPlayers)
        board = S.tapObject petId (sicken petId g1)
        (withSpell, ratsId) = S.handOne typhoidRats board
        resolved = S.runPure S.identityAnswer (S.runPure S.identityAnswer withSpell (S.cast S.alice ratsId)) Stack.resolveTop
        countOf name = S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack name) S.alice
    Spec.assertBool s (Mana.canPay Cost.manaActivations S.alice (ManaCost.MkManaCost [ManaSymbol.OfType (ManaType.Colored Color.Black)]) board) "the Pet is a {B} supply while tapped and sick"
    Spec.assertEqWith s "the Rats resolved" (countOf "Typhoid Rats" resolved) 1
    Spec.assertEqWith s "and the Pet paid for them" (countOf "Blood Pet" resolved) 0

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

  -- The gameplay-level proof (design.md section 4). Silent Arbiter is {4} and
  -- targets nothing as it is cast, so the whole cast turns on the Altar being
  -- counted twice -- and both Pikers in the graveyard afterwards is what says
  -- two activations happened rather than one large one.
  Spec.it s "CR 605.3a Silent Arbiter is cast off two activations of one Altar" $ do
    altar <- S.printingOf s registry "Ashnod's Altar"
    piker <- S.printingOf s registry "Goblin Piker"
    arbiter <- S.printingOf s registry "Silent Arbiter"
    let resolved = castOffBoard S.identityAnswer (altar : replicate 2 piker) arbiter
        short = castOffBoard S.identityAnswer [altar, piker] arbiter
        countOf name = S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack name) S.alice
    Spec.assertEqWith s "the Arbiter resolved" (countOf "Silent Arbiter" resolved) 1
    Spec.assertEqWith s "both Pikers paid for it" (countOf "Goblin Piker" resolved) 0
    Spec.assertEqWith s "with one Piker there is no {4} and the cast fails" (countOf "Silent Arbiter" short) 0
    Spec.assertEqWith s "and CR 601.2h left the Piker alive" (countOf "Goblin Piker" short) 1

  -- The prompt-level half, since a board cannot say whether the window CLOSED
  -- early: the Altar has to be offered a second time, with the cost still
  -- uncovered, for the second activation to happen at all.
  Spec.it s "CR 601.2g the mana window offers the Altar twice" $ do
    altar <- S.printingOf s registry "Ashnod's Altar"
    piker <- S.printingOf s registry "Goblin Piker"
    arbiter <- S.printingOf s registry "Silent Arbiter"
    let (withSpell, oid) = S.handOne arbiter (altarBoard altar piker 2)
        offers = State.execState (Engine.runGame recordingManaSources withSpell (S.cast S.alice oid)) []
    Spec.assertEqWith s "asked for a source twice, the Altar the only candidate each time" (fmap length offers) [1, 1]

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

  -- The card's OTHER printed line, which the boards above set by hand: cast the
  -- Workhorse and it arrives already carrying the four counters (CR 614.1c
  -- through EntryRewrite.WithCounters).
  Spec.it s "CR 614.1c Workhorse enters with four +1/+1 counters, so it arrives a 4/4" $ do
    horse <- S.printingOf s registry "Workhorse"
    forest <- S.printingOf s registry "Forest"
    let (withSpell, spellId) = S.handOne horse (S.landsInPlay forest 6)
        cast = S.runPure S.identityAnswer withSpell (S.cast S.alice spellId)
        resolved = S.runPure S.identityAnswer cast Stack.resolveTop
        entered = Set.toList (Set.difference (GameState.battlefield resolved) (GameState.battlefield withSpell))
    case entered of
      [horseId] -> do
        Spec.assertEqWith s "CR 122.1a a 4/4 on arrival, not the printed 0/0" (S.powerToughnessOf horseId resolved) (Just (4, 4))
        Spec.assertEqWith s "four +1/+1 counters" (S.counterOf CounterKind.PlusOnePlusOne horseId resolved) 4
      _ -> Spec.assertFailure s "Workhorse should have resolved onto the battlefield"

-- One Workhorse under alice's control carrying `counters` +1/+1 counters, and
-- nothing else on the board. The counters are placed by hand rather than by CR
-- 614.1c so that the two counts a case wants differ in the counters ALONE; the
-- entry rewrite has its own case above.
workhorseBoard :: Printing.Printing -> Natural -> (ObjectId.ObjectId, GameState.GameState)
workhorseBoard horse counters =
  let (horseId, gs) = S.addPermanent horse S.alice (Setup.emptyGame S.bothPlayers)
   in (horseId, S.addCounter CounterKind.PlusOnePlusOne counters horseId gs)

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

  -- And the payment carries it out, which the offer alone cannot say: the colours
  -- are SCRIPTED, one per activation, so the second half is the same board and the
  -- same spell with one answer changed. If the engine picked the colours itself
  -- both halves would pass.
  Spec.it s "CR 605.3a the Goblin is cast off a red activation and a green one" $ do
    goblin <- S.printingOf s registry "Zhur-Taa Goblin"
    board <- phyrexianAltarBoard s registry 2
    let countOf name = S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack name) S.alice
        mixed = castScripted goblin board [Color.Red, Color.Green]
        monochrome = castScripted goblin board [Color.Red, Color.Red]
    Spec.assertEqWith s "the Goblin resolved" (countOf "Zhur-Taa Goblin" mixed) 1
    Spec.assertEqWith s "both Pikers paid for it" (countOf "Goblin Piker" mixed) 0
    Spec.assertEqWith s "two red activations pay no {G}, so the cast fails" (countOf "Zhur-Taa Goblin" monochrome) 0
    Spec.assertEqWith s "and CR 601.2h rolled the sacrifices back" (countOf "Goblin Piker" monochrome) 2

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

-- `spell` cast off `board` and resolved, with the mana-yield choices answered off
-- `script` in order -- the fixture a repeatable source needs, since one answer
-- has to serve two activations of one permanent.
castScripted :: Printing.Printing -> GameState.GameState -> [Color.Color] -> GameState.GameState
castScripted spell board script =
  let (withSpell, oid) = S.handOne spell board
      afterCast = snd (State.evalState (Engine.runGame nextColor withSpell (S.cast S.alice oid)) script)
   in S.runPure S.identityAnswer afterCast Stack.resolveTop

-- The next colour in the script, or the default answer once it runs out.
nextColor :: Prompt.Prompt r -> State.State [Color.Color] r
nextColor p = case p of
  Prompt.ChooseManaYield _ _ _ candidates -> do
    scripted <- State.gets Maybe.listToMaybe
    State.modify' (drop 1)
    pure $ case scripted of
      Nothing -> S.identityAnswer p
      Just color ->
        S.optionYielding
          (Mana.Type.MkMana [ManaUnit.MkManaUnit {ManaUnit.manaType = ManaType.Colored color, ManaUnit.tags = Set.empty, ManaUnit.retention = ManaRetention.Ordinary, ManaUnit.restriction = Nothing, ManaUnit.rider = Nothing}])
          candidates
  _ -> pure (S.identityAnswer p)

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
  -- CR 605.3c is what still bounds it: the Altar is mid-activation, so its own
  -- window and every window nested inside it are closed to it.
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
  -- is the whole of the narrowing now: the Birds is mana-free and the Altar is
  -- mid-activation, so the Birds is the only candidate. Recorded rather than
  -- inferred, since the pool is the same whichever source the answerer would have
  -- declined.
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

  -- CR 601.2a against CR 307.5, which is the one rider whose window CLOSES
  -- between the gate and the payment: CR 601.2a puts the spell on the stack
  -- BEFORE CR 601.2f-h totals and pays the cost, so no cast's payment ever has
  -- the empty stack CR 307.5 requires. A gate counting the Ignus's {C}{C}{R}
  -- therefore offered a cast whose own payment could not reach that mana, and CR
  -- 601.2 then rewound it (#2005).
  --
  -- The BOARD is ignusBoard plus a Boggart Brute in hand: {2}{R} against a
  -- Mountain and an Ignus, whose three mana are exactly the Mountain's one and
  -- the Ignus's net two -- so nothing pays the Brute without the Ignus, and the
  -- Mountain alone is not the reason either way.
  --
  -- The two boards are ONE ACTIVATION apart and nothing else -- same phase, same
  -- Brute, same Mountain. Floating the mana first, with the stack still empty, is
  -- what the rules leave the player (CR 605.3a's priority window, which
  -- Action.legalActions goes on offering), and the same Brute is castable there.
  -- So the refusal is about the window and not about the {2}{R}.
  Spec.it s "CR 601.2a no cast is offered off the sorcery-speed source, the proposal itself closing CR 307.5's window" $ do
    ignus <- S.printingOf s registry "Grinning Ignus"
    mountain <- S.printingOf s registry "Mountain"
    brute <- S.printingOf s registry "Boggart Brute"
    let (ignusId, board) = ignusBoard ignus mountain
        (held, bruteId) = S.handOne brute board
        floated = snd (State.evalState (Engine.runGame (takesIgnusOnce ignusId) held Engine.priorityLoop) (0 :: Int))
        offers gs = filter (S.isCastOf bruteId) (Action.legalActions S.alice gs)
    Spec.assertEqWith s "CR 601.2a the Ignus is no supply for a cast, whose payment the proposal has already put a spell on the stack for" (length (offers held)) 0
    Spec.assertEqWith s "CR 605.3a the same Brute off the same board is castable once that mana is floated with the stack still empty" (length (offers floated)) 1
    Spec.assertEqWith s "and the float is the Ignus's own {C}{C}{R}" (poolTypes S.alice floated) [ManaType.Colorless, ManaType.Colorless, ManaType.Colored Color.Red]
    Spec.assertEqWith s "CR 307.1 both boards are the same sorcery-speed window, so the phase decides neither" (GameState.phase floated) Phase.PrecombatMain

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
-- the only permanent, so CR 601.2g's window inside the payment has nothing to
-- offer (CR 605.3c takes the Gate itself off it) and the {W/U} is paid out of
-- what is floating.
mysticGateSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
mysticGateSpec s registry = Spec.describe s "Mystic Gate" $ do
  Spec.it s "CR 118.13a the player announces the {W/U} in a mana ability's own activation cost" $ do
    gate <- S.printingOf s registry "Mystic Gate"
    let board = gateBoard gate
        run half = S.runPureWith (takesGate [whiteType, blueType] (Just half)) (snd board) (Cost.tapForMana S.manaPerformer (fst board))
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
        asked wanted = State.execState (Engine.runGame (counting wanted) (snd board) (Cost.tapForMana S.manaPerformer (fst board))) (0 :: Int)
    Spec.assertEqWith s "CR 118.13a the {C} route's cost prints no symbol payable two ways, so nothing is announced" (asked [ManaType.Colorless]) 0
    Spec.assertEqWith s "and the {W/U} route on the same board is asked once" (asked [whiteType, blueType]) 1

-- alice with one Mystic Gate and one white and one blue mana floating, and
-- nothing else anywhere. The pool is SEEDED rather than tapped for, so the {W/U}
-- is paid without CR 601.2g's window choosing a source -- what is under test is
-- the announcement, and a second land would make the source prompt the thing that
-- decided which colour went.
gateBoard :: Printing.Printing -> (ObjectId.ObjectId, GameState.GameState)
gateBoard gate =
  let (gateId, g1) = S.addPermanent gate S.alice (Setup.emptyGame S.bothPlayers)
      unit manaType = ManaUnit.MkManaUnit {ManaUnit.manaType = manaType, ManaUnit.tags = Set.empty, ManaUnit.retention = ManaRetention.Ordinary, ManaUnit.restriction = Nothing, ManaUnit.rider = Nothing}
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
    let typesOf option = case ManaOption.yield option of
          Mana.Type.MkMana units -> fmap ManaUnit.manaType units
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
    let after = S.runPure S.identityAnswer board (Cost.tapForMana S.manaPerformer bobForest)
        -- The same board differing in exactly one thing: which Forest was
        -- tapped. Alice's carries no Aura, so it must add one and only one.
        unenchanted = S.runPure S.identityAnswer board (Cost.tapForMana S.manaPerformer aliceForest)
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

-- Resolve the whole stack down, so a board that placed a triggered mana ability
-- CR 605.4a forbids the stack reads differently from one that placed nothing: a
-- reading taken with the trigger still waiting could not tell them apart at
-- gameplay level.
resolveDown :: GameState.GameState -> GameState.GameState
resolveDown = go (8 :: Int)
  where
    go n gs =
      if n <= 0 || null (GameState.stack gs)
        then gs
        else go (n - 1) (S.runPure S.identityAnswer gs Stack.resolveTop)

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

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Mana" $ do
  manaSpec s registry
  castabilitySpec s registry
  anyColorSpec s registry
  chosenColorSpec s registry
  solRingSpec s registry
  ancientTombSpec s registry
  palladiumMyrSpec s registry
  upwellingSpec s registry
  omnathSpec s registry
  priorityWindowSpec s registry
  riderWindowSpec s registry
  laviniaTurnRiderSpec s registry
  wellspringSpec s registry
  manaConfluenceSpec s registry
  phyrexianTowerSpec s registry
  bloodPetSpec s registry
  ashnodsAltarSpec s registry
  workhorseSpec s registry
  phyrexianAltarSpec s registry
  transmograntAltarSpec s registry
  grinningIgnusSpec s registry
  mysticGateSpec s registry
  activationAdjustmentSpec s registry
  wildGrowthSpec s registry

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
  [] -> ActivatedAbility.MkActivatedAbility (Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) []) [] (singleModeAbility [] Map.empty) [] Activator.Controller Nothing Nothing Nothing
