{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers Pawl.Engine.Mana: mana payment and castability. CR 118.13a's announcement lives
-- here too (Mana.announcePhyrexian), so the cases that reach it through
-- Cast.castSpell and Activate.activateAbility are in this spec rather than in
-- CastSpec or ActivateSpec -- the module under test is this one, and the two entry
-- points are how the rule is reached.
module Pawl.ManaSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine.Action as Action
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Cost as Cost
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Mana as Mana
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Replay as Replay
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as Action.Type
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.ActivationTiming as ActivationTiming
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Mana as Mana.Type
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaProduction as ManaProduction
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
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PhyrexianPayment as PhyrexianPayment
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Pool as Pool
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.ProductionTag as ProductionTag
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.Response as Response
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TargetSpec as TargetSpec
import qualified Pawl.Types.Zone as Zone

-- Cast `creature` off `nLands` copies of `land`, then resolve it.
resolvedCreature :: Printing.Printing -> Printing.Printing -> Int -> GameState.GameState
resolvedCreature land creature nLands =
  let (base, oid) = S.handOne creature (S.landsInPlay land nLands)
      afterCast = snd (Engine.runGamePure S.identityAnswer base (S.cast S.alice oid))
   in snd (Engine.runGamePure S.identityAnswer afterCast Stack.resolveTop)

-- A single forced mode (ChooseExactly 1, M4g's non-modal shape) wrapping one
-- ability's effects and target specs -- the fixture shape every pre-M4h
-- single-mode ActivatedAbility now takes.
singleModeAbility :: [Effect.Effect card] -> Map.Map SlotName.SlotName TargetSpec.TargetSpec -> Modal.Modal card
singleModeAbility effects specs =
  Modal.MkModal (Seq.singleton (Mode.MkMode (Seq.fromList effects) specs Optionality.Mandatory)) (ModeSelection.ChooseExactly 1)

-- Answers Prompt.ChooseManaSource with `wanted` whenever it is on offer, and
-- defers everything else to S.identityAnswer. Its sibling avoids that source
-- instead: between them they prove the ANSWER is what decides, rather than the
-- order Mana.manaSources happens to return (#12).
prefersSource :: ObjectId.ObjectId -> Prompt.Prompt r -> r
prefersSource wanted p = case p of
  Prompt.ChooseManaSource _ _ candidates ->
    if elem wanted (NonEmpty.toList candidates) then wanted else NonEmpty.head candidates
  _ -> S.identityAnswer p

avoidsSource :: ObjectId.ObjectId -> Prompt.Prompt r -> r
avoidsSource unwanted p = case p of
  Prompt.ChooseManaSource _ _ candidates -> case filter (/= unwanted) (NonEmpty.toList candidates) of
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

pikerCost :: ManaCost.ManaCost
pikerCost = ManaCost.MkManaCost [ManaSymbol.Generic 1, ManaSymbol.OfType (ManaType.Colored Color.Red)]

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
      (Mana.subtypeMana Subtype.Mountain)
      (Just (ManaType.Colored Color.Red))

  Spec.it s "a Goblin grants no mana ability" $
    Spec.assertEqWith s "none" (Mana.subtypeMana Subtype.Goblin) Nothing

  Spec.it s "CR 305.6 Island taps blue, Plains taps white" $ do
    Spec.assertEqWith s "island" (Mana.subtypeMana Subtype.Island) (Just (ManaType.Colored Color.Blue))
    Spec.assertEqWith s "plains" (Mana.subtypeMana Subtype.Plains) (Just (ManaType.Colored Color.White))

  Spec.it s "CR 205.3h: Aura is an enchantment type, so it has no CR 305.6 intrinsic mana" $
    Spec.assertEqWith s "no mana" (Mana.subtypeMana Subtype.Aura) Nothing

  -- CR 305.6 grants its intrinsic ability to "an object with the land card
  -- type and A BASIC LAND TYPE", and CR 205.3i lists which of the land types
  -- those are: "Of that list, Forest, Island, Mountain, Plains, and Swamp are
  -- the basic land types." So a Desert is a land type with no mana of its own
  -- -- the one constructor where this answer and Pawl.Engine.Subtype.isLandType's
  -- (asserted in Pawl.ProjectionSpec) come apart, and the reason they are two
  -- functions.
  Spec.it s "CR 305.6 Desert is a land type but not a BASIC one, so it grants no mana" $
    Spec.assertEqWith s "no mana" (Mana.subtypeMana Subtype.Desert) Nothing

  Spec.it s "an empty pool starts empty" $ do
    mountain <- S.printingOf s registry "Mountain"
    Spec.assertEqWith s "empty" (poolSize S.alice (S.landsInPlay mountain 2)) 0

  Spec.it s "tapping a Mountain taps it and adds one red unit" $ do
    mountain <- S.printingOf s registry "Mountain"
    let gs = S.landsInPlay mountain 1
    case Game.zoneMembers Zone.Battlefield S.alice gs of
      [] -> Spec.assertFailure s "fixture should have one Mountain"
      oid : _ -> do
        let after = S.runPure S.identityAnswer gs (Mana.tapForMana oid)
        Spec.assertEqWith s "tapped" (S.tappedCount S.alice after) 1
        Spec.assertEqWith
          s
          "pool"
          (Game.poolOf S.alice after)
          (Mana.Type.MkMana [ManaUnit.MkManaUnit {ManaUnit.manaType = ManaType.Colored Color.Red, ManaUnit.tags = Set.empty}])

  Spec.it s "two Mountains can pay {1}{R}" $ do
    mountain <- S.printingOf s registry "Mountain"
    Spec.assertBool s (Mana.canPay S.alice pikerCost (S.landsInPlay mountain 2)) "affordable"

  Spec.it s "one Mountain cannot pay {1}{R}" $ do
    mountain <- S.printingOf s registry "Mountain"
    Spec.assertBool s (not (Mana.canPay S.alice pikerCost (S.landsInPlay mountain 1))) "unaffordable"

  Spec.it s "no Mountains cannot pay {1}{R}" $ do
    mountain <- S.printingOf s registry "Mountain"
    Spec.assertBool s (not (Mana.canPay S.alice pikerCost (S.landsInPlay mountain 0))) "unaffordable"

  -- Three identical Mountains: every candidate is a copy of the same card, so
  -- the choice is genuinely indistinguishable and payCost must NOT ask (#12).
  -- S.identityAnswer would answer anyway; what this pins is the tap count.
  Spec.it s "paying {1}{R} taps exactly two of three Mountains and leaves no float" $ do
    mountain <- S.printingOf s registry "Mountain"
    let (paid, after) = S.runPureWith S.identityAnswer (S.landsInPlay mountain 3) (Mana.payCost S.alice pikerCost)
    Spec.assertBool s paid "three Mountains should pay {1}{R}"
    Spec.assertEqWith s "tapped" (S.tappedCount S.alice after) 2
    Spec.assertEqWith s "no float" (poolSize S.alice after) 0

  Spec.it s "CR 500.5 mana pools empty" $ do
    mountain <- S.printingOf s registry "Mountain"
    let gs = S.landsInPlay mountain 1
    case Game.zoneMembers Zone.Battlefield S.alice gs of
      [] -> Spec.assertFailure s "fixture should have one Mountain"
      oid : _ ->
        Spec.assertEqWith s "emptied" (poolSize S.alice (Mana.emptyManaPools (S.runPure S.identityAnswer gs (Mana.tapForMana oid)))) 0

  Spec.it s "CR 305.6/305.7 an Urborg'd Mountain taps for black too" $ do
    mountain <- S.printingOf s registry "Mountain"
    urborg <- S.printingOf s registry "Urborg, Tomb of Yawgmoth"
    let base = Setup.emptyGame S.bothPlayers
        (mountainId, g1) = S.addCreature mountain S.alice base
        (_, gs) = S.addCreature urborg S.alice g1
    -- Urborg adds Swamp to all lands, so the Mountain taps for black too.
    Spec.assertBool s (ManaType.Colored Color.Black `elem` Mana.manaTypesOf mountainId gs) "black available"
    Spec.assertBool s (ManaType.Colored Color.Red `elem` Mana.manaTypesOf mountainId gs) "red still available"

  Spec.it s "CR 305.6/305.7 a Blood Moon'd Urborg taps for red only" $ do
    urborg <- S.printingOf s registry "Urborg, Tomb of Yawgmoth"
    bloodMoon <- S.printingOf s registry "Blood Moon"
    let base = Setup.emptyGame S.bothPlayers
        (urborgId, g1) = S.addCreature urborg S.alice base
        (_, gs) = S.addCreature bloodMoon S.alice g1
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
        (towerId, g1) = S.addCreature reliquaryTower S.alice base
        (_, gs) = S.addCreature bloodMoon S.alice g1
    Spec.assertBool s (ManaType.Colored Color.Red `elem` Mana.manaTypesOf towerId gs) "red available (CR 305.6, from the new Mountain type)"
    Spec.assertBool s (ManaType.Colorless `notElem` Mana.manaTypesOf towerId gs) "colorless gone (the printed {T}: Add {C} was stripped)"

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
        (towerId, g1) = S.addCreature reliquaryTower S.alice base
        (mirageId, g2) = S.addCreature convincingMirage S.alice g1
        gs = S.withChosenSubtype Subtype.Plains mirageId (S.attach mirageId towerId g2)
    Spec.assertBool s (ManaType.Colored Color.White `elem` Mana.manaTypesOf towerId gs) "white available (CR 305.6, from the chosen Plains)"
    Spec.assertBool s (ManaType.Colorless `notElem` Mana.manaTypesOf towerId gs) "colorless gone (the printed {T}: Add {C} was stripped)"

  -- The same strip, on a land whose rules text is not a mana ability at all.
  Spec.it s "CR 305.7 a Blood Moon'd Evolving Wilds has no activated ability left" $ do
    evolvingWilds <- S.printingOf s registry "Evolving Wilds"
    bloodMoon <- S.printingOf s registry "Blood Moon"
    let base = Setup.emptyGame S.bothPlayers
        (wildsId, g1) = S.addCreature evolvingWilds S.alice base
        (_, gs) = S.addCreature bloodMoon S.alice g1
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
        (pikerId, g1) = S.addCreature piker S.alice base
        (_, gs) = S.addCreature ashaya S.alice g1
    Spec.assertBool s (ManaType.Colored Color.Green `elem` Mana.manaTypesOf pikerId gs) "green available"
    Spec.assertBool s (pikerId `elem` Mana.manaSources S.alice gs) "and it is a mana source"

  Spec.it s "CR 305.7 Blood Moon turns that same creature-land red" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    ashaya <- S.printingOf s registry "Ashaya, Soul of the Wild"
    bloodMoon <- S.printingOf s registry "Blood Moon"
    let base = Setup.emptyGame S.bothPlayers
        (pikerId, g1) = S.addCreature piker S.alice base
        (_, g2) = S.addCreature bloodMoon S.alice g1
        (_, gs) = S.addCreature ashaya S.alice g2
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
        (pikerId, g1) = S.addCreature piker S.alice base
        (_, g2) = S.addCreature ashaya S.alice g1
        sick = g2 {GameState.objects = Map.adjust (\o -> o {Object.sickness = Sickness.Sick}) pikerId (GameState.objects g2)}
    Spec.assertBool s (Set.member CardType.Land (Projection.cardTypesOf pikerId sick)) "it is a land now"
    Spec.assertBool s (Projection.isCreatureOf pikerId sick) "and still a creature"
    Spec.assertBool s (pikerId `notElem` Mana.manaSources S.alice sick) "so the sick creature is no mana source"

  Spec.it s "CR 605.1a a {T}: Add {G} ability is a mana ability" $
    let ab =
          ActivatedAbility.MkActivatedAbility
            { ActivatedAbility.cost = Cost.Type.MkCost {Cost.Type.mana = Just (ManaCost.MkManaCost []), Cost.Type.components = []},
              ActivatedAbility.modal = singleModeAbility [Effect.AddMana (ManaProduction.OfType (ManaType.Colored Color.Green))] Map.empty,
              ActivatedAbility.timing = ActivationTiming.AnyTime
            }
     in Spec.assertBool s (Mana.isManaAbility ab) "mana ability"

  Spec.it s "CR 605.1a an ability that targets is NOT a mana ability" $
    let ab =
          ActivatedAbility.MkActivatedAbility
            { ActivatedAbility.cost = Cost.Type.MkCost {Cost.Type.mana = Just (ManaCost.MkManaCost []), Cost.Type.components = []},
              ActivatedAbility.modal =
                singleModeAbility
                  [Effect.AddMana (ManaProduction.OfType (ManaType.Colored Color.Green))]
                  (Map.singleton (SlotName.MkSlotName (Text.pack "x")) (TargetSpec.MkTargetSpec Pool.AnyTarget Nothing)),
              ActivatedAbility.timing = ActivationTiming.AnyTime
            }
     in Spec.assertBool s (not (Mana.isManaAbility ab)) "targets -> not mana"

  Spec.it s "CR 605.1a a damage ability is NOT a mana ability" $
    let ab =
          ActivatedAbility.MkActivatedAbility
            { ActivatedAbility.cost = Cost.Type.MkCost {Cost.Type.mana = Just (ManaCost.MkManaCost []), Cost.Type.components = []},
              ActivatedAbility.modal =
                singleModeAbility
                  [Effect.DealDamage (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "x"))) (Quantity.Literal 1)]
                  (Map.singleton (SlotName.MkSlotName (Text.pack "x")) (TargetSpec.MkTargetSpec Pool.AnyTarget Nothing)),
              ActivatedAbility.timing = ActivationTiming.AnyTime
            }
     in Spec.assertBool s (not (Mana.isManaAbility ab)) "no mana produced -> not mana"

  Spec.it s "CR 605 a settled Llanowar Elves is a green mana source" $ do
    llanowarElves <- S.printingOf s registry "Llanowar Elves"
    let (elfId, gs) = S.addCreature llanowarElves S.alice (Setup.emptyGame S.bothPlayers)
    Spec.assertBool s (elem (ManaType.Colored Color.Green) (Mana.manaTypesOf elfId gs)) "taps green"
    Spec.assertBool s (elem elfId (Mana.manaSources S.alice gs)) "is a mana source"

  Spec.it s "CR 302.6 a summoning-sick Llanowar Elves is NOT a mana source" $ do
    llanowarElves <- S.printingOf s registry "Llanowar Elves"
    let (elfId, g0) = S.addCreature llanowarElves S.alice (Setup.emptyGame S.bothPlayers)
        sick = g0 {GameState.objects = Map.adjust (\o -> o {Object.sickness = Sickness.Sick}) elfId (GameState.objects g0)}
    Spec.assertBool s (notElem elfId (Mana.manaSources S.alice sick)) "sick elf excluded"

  -- CR 302.6's other half, and the same trap #198 sprang on attacking: bob's
  -- Elves settled under BOB, so the settle it carries says nothing about
  -- alice. Stealing it does not hand her a mana source this turn.
  Spec.it s "CR 302.6 a stolen Llanowar Elves is not a mana source for the thief" $ do
    llanowarElves <- S.printingOf s registry "Llanowar Elves"
    controlMagic <- S.printingOf s registry "Control Magic"
    let (elfId, g0) = S.addCreature llanowarElves S.bob (Setup.emptyGame S.bothPlayers)
        settled = S.runPure S.identityAnswer g0 (Engine.settleAll S.bob)
        (aura, withAura) = S.addCreature controlMagic S.alice settled
        stolen = S.attach aura elfId withAura
    Spec.assertBool s (elem elfId (Mana.manaSources S.bob settled)) "bob could tap it"
    Spec.assertBool s (elem elfId (Projection.controls S.alice stolen)) "alice controls it now"
    Spec.assertBool s (notElem elfId (Mana.manaSources S.alice stolen)) "but it is sick for her, so it is not her mana source"

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
        (elfId, base1) = S.addCreature llanowarElves S.bob base0
        base = S.runPure S.identityAnswer base1 (Engine.settleAll S.bob)
        (withSpell, spellId) = S.handOne actOfTreason base
        cast = snd (Engine.runGamePure S.identityAnswer withSpell (S.cast S.alice spellId))
        resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "alice controls the Elves" (Projection.controllerOf elfId resolved) (Just S.alice)
    Spec.assertBool s (Projection.hasKeyword Keyword.Haste elfId resolved) "it has haste"
    Spec.assertBool s (elem elfId (Mana.manaSources S.alice resolved)) "so she may tap it for mana this turn"

  -- CR 601.2g / 602.1: WHICH sources to activate is the player's choice, and
  -- pawl's second invariant is that the engine never makes one. A Forest and a
  -- Llanowar Elves both pay {G}, but they are not interchangeable -- tapping
  -- the Elf spends a creature that could otherwise block -- so the choice must
  -- be asked, and the answer must be honoured (#12).
  Spec.it s "CR 601.2g paying {G} with a Forest AND a Llanowar Elves asks which to tap" $ do
    forest <- S.printingOf s registry "Forest"
    llanowarElves <- S.printingOf s registry "Llanowar Elves"
    let base0 = S.landsInPlay forest 1
        (elfId, base1) = S.addCreature llanowarElves S.alice base0
        gs = S.runPure S.identityAnswer base1 (Engine.settleAll S.alice)
        green = ManaCost.MkManaCost [ManaSymbol.OfType (ManaType.Colored Color.Green)]
        cost = Cost.Type.MkCost (Just green) []
        tappedElf g = fmap Object.tapped (Game.lookupObject elfId g)
    Spec.assertEqWith s "asked to tap the Elf, it is tapped" (tappedElf (S.runPure (prefersSource elfId) gs (Cost.pay S.alice elfId cost))) (Just TapState.Tapped)
    Spec.assertEqWith s "asked to spare the Elf, it is untapped" (tappedElf (S.runPure (avoidsSource elfId) gs (Cost.pay S.alice elfId cost))) (Just TapState.Untapped)

  -- The other half of the invariant: the elision is exactly "there is only one
  -- candidate, so there is nothing to decide", and nothing broader. Counting
  -- prompts is the direct assertion -- without it, an implementation that
  -- never asks would still pass the test above's first half.
  --
  -- Three Forests DO ask. They are one card, but sameness of card is not
  -- sameness of object (#217), so the engine does not presume to skip it.
  Spec.it s "CR 601.2g one candidate asks nothing; more than one always asks" $ do
    forest <- S.printingOf s registry "Forest"
    let green = ManaCost.MkManaCost [ManaSymbol.OfType (ManaType.Colored Color.Green)]
        countingAnswer :: Prompt.Prompt r -> State.State Int r
        countingAnswer p = case p of
          Prompt.ChooseManaSource _ _ candidates -> do
            State.modify' (+ 1)
            pure (NonEmpty.head candidates)
          _ -> pure (S.identityAnswer p)
        promptsFor g = State.execState (Engine.runGame countingAnswer g (Mana.payCost S.alice green)) 0
    Spec.assertEqWith s "a lone Forest: nothing to ask" (promptsFor (S.landsInPlay forest 1)) 0
    Spec.assertEqWith s "three Forests: one real decision" (promptsFor (S.landsInPlay forest 3)) 1

  -- FILTERED, NOT TRUSTED: an interpreter naming a source that was not offered
  -- must not be honoured. Beyond hygiene, tapForMana is a no-op on an unknown
  -- id, so obeying the answer would leave the state unchanged and loop forever.
  Spec.it s "CR 601.2g an answer outside the offered set is rejected, not obeyed" $ do
    forest <- S.printingOf s registry "Forest"
    let green = ManaCost.MkManaCost [ManaSymbol.OfType (ManaType.Colored Color.Green)]
        bogus = ObjectId.MkObjectId 9999
        liar p = case p of
          Prompt.ChooseManaSource {} -> bogus
          _ -> S.identityAnswer p
        gs = S.landsInPlay forest 3
        (paid, after) = S.runPureWith liar gs (Mana.payCost S.alice green)
    Spec.assertBool s paid "the cost is still paid"
    Spec.assertEqWith s "from a real Forest, not the invented id" (S.tappedCount S.alice after) 1

  Spec.it s "mana from a controlled permanent goes to its controller, not owner" $ do
    llanowarElves <- S.printingOf s registry "Llanowar Elves"
    let (oid, base) = S.addCreature llanowarElves S.bob (Setup.emptyGame S.bothPlayers)
        gs0 = S.giveControl oid S.alice base
        after = S.runPure S.identityAnswer gs0 (Mana.tapForMana oid)
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
    let yield = Mana.Type.MkMana [ManaUnit.MkManaUnit {ManaUnit.manaType = ManaType.Colored wanted, ManaUnit.tags = Set.empty}]
     in if elem yield (NonEmpty.toList candidates)
          then yield
          else NonEmpty.head candidates
  _ -> S.identityAnswer p

-- Alice controls `permanents` and holds `spell`; she casts it and resolves it,
-- with every prompt answered by `answer`.
castOffBoard :: (forall r. Prompt.Prompt r -> r) -> [Printing.Printing] -> Printing.Printing -> GameState.GameState
castOffBoard answer permanents spell =
  let board = foldr (\p gs -> snd (S.addCreature p S.alice gs)) (Setup.emptyGame S.bothPlayers) permanents
      (withSpell, oid) = S.handOne spell board
      afterCast = S.runPure answer withSpell (S.cast S.alice oid)
   in S.runPure answer afterCast Stack.resolveTop

-- The mana Alice's pool holds after tapping `oid` with every prompt answered by
-- `answer` -- the observable that says WHAT a source produced: which type, where
-- it offers several, and how much, where one activation adds more than one.
tappedFor :: (forall r. Prompt.Prompt r -> r) -> ObjectId.ObjectId -> GameState.GameState -> [ManaType.ManaType]
tappedFor answer oid gs = case Game.poolOf S.alice (S.runPure answer gs (Mana.tapForMana oid)) of
  Mana.Type.MkMana units -> fmap ManaUnit.manaType units

anyColorSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
anyColorSpec s registry = Spec.describe s "Mana of any color" $ do
  -- CR 105.4: "If a player is asked to choose a color, they must choose one of
  -- the five colors. 'Multicolored' is not a color. Neither is 'colorless.'"
  -- So AnyColor offers exactly five options, and {C} is not among them.
  Spec.it s "CR 105.4 Birds of Paradise offers the five colors and not colorless" $ do
    birds <- S.printingOf s registry "Birds of Paradise"
    let (birdsId, gs) = S.addCreature birds S.alice (Setup.emptyGame S.bothPlayers)
    Spec.assertEqWith
      s
      "exactly the five colors"
      (Mana.manaTypesOf birdsId gs)
      (fmap ManaType.Colored [Color.White, Color.Blue, Color.Black, Color.Red, Color.Green])
    Spec.assertBool s (elem birdsId (Mana.manaSources S.alice gs)) "it is a mana source"

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
    let (_, g1) = S.addCreature forest S.alice (Setup.emptyGame S.bothPlayers)
        (_, gs) = S.addCreature birds S.alice g1
        cost = ManaCost.MkManaCost [ManaSymbol.OfType (ManaType.Colored Color.Green), ManaSymbol.OfType (ManaType.Colored Color.Black)]
    Spec.assertBool s (Mana.canPay S.alice cost gs) "affordable"

  Spec.it s "CR 118.3 two Birds of Paradise can pay {B}{B}, one cannot" $ do
    birds <- S.printingOf s registry "Birds of Paradise"
    let (_, one) = S.addCreature birds S.alice (Setup.emptyGame S.bothPlayers)
        (_, two) = S.addCreature birds S.alice one
        black = ManaSymbol.OfType (ManaType.Colored Color.Black)
        cost = ManaCost.MkManaCost [black, black]
    Spec.assertBool s (Mana.canPay S.alice cost two) "two suffice"
    Spec.assertBool s (not (Mana.canPay S.alice cost one)) "one does not"

  -- The OTHER way to get this wrong, which Hall's condition also rules out:
  -- checking each symbol independently ("is there a source that could make
  -- white?") passes both {W} symbols, because the same Birds answers each
  -- one. Only weighing the whole demand set against the supplies that could
  -- serve it catches that one source cannot make two mana. Two demands, three
  -- sources, plenty of mana, still unpayable.
  Spec.it s "CR 118.3 a Birds and two Forests cannot pay {W}{W}" $ do
    birds <- S.printingOf s registry "Birds of Paradise"
    forest <- S.printingOf s registry "Forest"
    let (_, g1) = S.addCreature birds S.alice (Setup.emptyGame S.bothPlayers)
        (_, g2) = S.addCreature forest S.alice g1
        (_, gs) = S.addCreature forest S.alice g2
        white = ManaSymbol.OfType (ManaType.Colored Color.White)
    Spec.assertBool s (not (Mana.canPay S.alice (ManaCost.MkManaCost [white, white]) gs)) "only one white source"
    Spec.assertBool s (Mana.canPay S.alice (ManaCost.MkManaCost [white, ManaSymbol.Generic 2]) gs) "but one {W} plus {2} is fine"

  -- Not only "any color": a source with two BASIC LAND TYPES has been a real
  -- choice in this pool since Urborg landed, and tapForMana was silently
  -- taking the first. Both directions, so the answer is proven to decide.
  Spec.it s "CR 305.6/305.7 an Urborg'd Mountain's controller chooses red or black" $ do
    mountain <- S.printingOf s registry "Mountain"
    urborg <- S.printingOf s registry "Urborg, Tomb of Yawgmoth"
    let base = Setup.emptyGame S.bothPlayers
        (mountainId, g1) = S.addCreature mountain S.alice base
        (_, gs) = S.addCreature urborg S.alice g1
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
          let (oid, gs) = S.addCreature printing S.alice (Setup.emptyGame S.bothPlayers)
           in State.execState (Engine.runGame countingAnswer gs (Mana.tapForMana oid)) 0
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
        let (oid, gs1) = S.addCreature printing S.alice gs
         in (ids <> [oid], gs1)
      (extras, withAlices) = List.foldl' addOne ([], Setup.emptyGame S.bothPlayers) alices
      (aliceForest, g1) = S.addCreature forest S.alice withAlices
      (bobForest, g2) = S.addCreature forest S.bob g1
      g3 = S.runPure S.identityAnswer g2 (Mana.tapForMana aliceForest)
   in (extras, S.runPure S.identityAnswer g3 (Mana.tapForMana bobForest))

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
        (_, g1) = S.addCreature upwelling S.alice base
        (birdsId, g2) = S.addCreature birds S.alice g1
        (_, g3) = S.addCreature forest S.alice g2
        (withElves, elvesId) = S.handOne llanowarElves g3
        (unsummonId, board) = S.addHandCard unsummon S.alice withElves
        -- Prefer the Birds and take blue from it: it cannot pay {G}, so the
        -- Forest is tapped next and the {U} is left over.
        floatBlue :: Prompt.Prompt r -> r
        floatBlue p = case p of
          Prompt.ChooseManaSource _ _ candidates ->
            if elem birdsId (NonEmpty.toList candidates) then birdsId else NonEmpty.head candidates
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
    let (_, g1) = S.addCreature omnath S.alice (Setup.emptyGame S.bothPlayers)
        (forestId, g2) = S.addCreature forest S.alice g1
        (islandId, g3) = S.addCreature island S.alice g2
        floated = S.runPure S.identityAnswer (S.runPure S.identityAnswer g3 (Mana.tapForMana forestId)) (Mana.tapForMana islandId)
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
    let (_, g1) = S.addCreature omnath S.alice (Setup.emptyGame S.bothPlayers)
        (alicesForest, g2) = S.addCreature forest S.alice g1
        (bobsForest, g3) = S.addCreature forest S.bob g2
        floated = S.runPure S.identityAnswer (S.runPure S.identityAnswer g3 (Mana.tapForMana alicesForest)) (Mana.tapForMana bobsForest)
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
    let (omnathId, g1) = S.addCreature omnath S.alice (Setup.emptyGame S.bothPlayers)
        (forestId, g2) = S.addCreature forest S.alice g1
        (islandId, g3) = S.addCreature island S.alice g2
        floated = S.runPure S.identityAnswer (S.runPure S.identityAnswer g3 (Mana.tapForMana forestId)) (Mana.tapForMana islandId)
        afterStep = S.runPure S.identityAnswer floated Engine.runStep
    Spec.assertEqWith s "before: two floating, and only the green pumps" (Projection.powerOf omnathId floated) (Just 2)
    Spec.assertEqWith s "the blue is gone" (poolSize S.alice afterStep) 1
    Spec.assertEqWith s "the green survived the step boundary" (Projection.powerOf omnathId afterStep) (Just 2)
    Spec.assertEqWith s "and the toughness with it" (Projection.toughnessOf omnathId afterStep) (Just 2)

-- CR 605.3b: one activation of one mana ability, adding TWO mana. Sol Ring ({1}
-- Artifact, "{T}: Add {C}{C}") is the pool's first source whose yield is not one
-- unit, and it is what separates "the types this source could produce" from "the
-- mana this source produces when it is tapped" (#238).
solRingSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
solRingSpec s registry = Spec.describe s "Sol Ring" $ do
  -- The unit fact. A mode holding two AddMana effects is ONE activation
  -- yielding two mana, not a choice between two singles.
  Spec.it s "CR 605 tapping Sol Ring adds two colorless mana, not one" $ do
    solRing <- S.printingOf s registry "Sol Ring"
    let (solRingId, gs) = S.addCreature solRing S.alice (Setup.emptyGame S.bothPlayers)
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
    let (_, gs) = S.addCreature solRing S.alice (Setup.emptyGame S.bothPlayers)
    Spec.assertBool s (Mana.canPay S.alice (ManaCost.MkManaCost [ManaSymbol.Generic 2]) gs) "{2} is affordable"
    Spec.assertBool s (not (Mana.canPay S.alice (ManaCost.MkManaCost [ManaSymbol.Generic 3]) gs)) "{3} is not"

  -- Both supplies a Sol Ring contributes are COLORLESS, so they swell the
  -- generic count and serve no typed demand. Discriminating against a supply
  -- model that merely counted a source twice without keeping its types: that
  -- one passes the first assertion and fails the second.
  Spec.it s "CR 118.3 a Sol Ring and a Mountain pay {2}{R}, but not {R}{R}" $ do
    solRing <- S.printingOf s registry "Sol Ring"
    mountain <- S.printingOf s registry "Mountain"
    let (_, g1) = S.addCreature solRing S.alice (Setup.emptyGame S.bothPlayers)
        (_, gs) = S.addCreature mountain S.alice g1
        red = ManaSymbol.OfType (ManaType.Colored Color.Red)
    Spec.assertBool s (Mana.canPay S.alice (ManaCost.MkManaCost [ManaSymbol.Generic 2, red]) gs) "{2}{R} is affordable"
    Spec.assertBool s (not (Mana.canPay S.alice (ManaCost.MkManaCost [red, red]) gs)) "{R}{R} is not"

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
        (solRingId, gs) = S.addCreature solRing S.alice (Setup.emptyGame S.bothPlayers)
    Spec.assertEqWith s "nothing to ask" (State.execState (Engine.runGame countingAnswer gs (Mana.tapForMana solRingId)) 0) 0

-- Answers Prompt.ChooseManaYield with `wanted`'s LONGEST yield, and defers every
-- other source's prompt to S.identityAnswer, which takes the head. A payment off
-- several two-yield sources needs a different answer from each, and the prompt
-- carries the object it is about, so keying on that is what lets one answerer
-- send one Palladium Myr to its Forest and the other to its {C}{C}.
prefersLongYieldFrom :: ObjectId.ObjectId -> Prompt.Prompt r -> r
prefersLongYieldFrom wanted p = case p of
  Prompt.ChooseManaYield _ _ oid candidates
    | oid == wanted ->
        let size yield = case yield of
              Mana.Type.MkMana units -> length units
         in List.maximumBy (\a b -> compare (size a) (size b)) (NonEmpty.toList candidates)
  _ -> S.identityAnswer p

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
    let (_, g1) = S.addCreature ashaya S.alice (Setup.emptyGame S.bothPlayers)
        (myrId, gs) = S.addCreature palladiumMyr S.alice g1
        green = ManaUnit.MkManaUnit {ManaUnit.manaType = ManaType.Colored Color.Green, ManaUnit.tags = Set.empty}
        colorless = ManaUnit.MkManaUnit {ManaUnit.manaType = ManaType.Colorless, ManaUnit.tags = Set.empty}
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
    let (_, g1) = S.addCreature ashaya S.alice (Setup.emptyGame S.bothPlayers)
        (_, g2) = S.addCreature palladiumMyr S.alice g1
        (_, gs) = S.addCreature palladiumMyr S.alice g2
        green = ManaSymbol.OfType (ManaType.Colored Color.Green)
    Spec.assertBool
      s
      (not (Mana.canPay S.alice (ManaCost.MkManaCost [ManaSymbol.Generic 3, green, green]) gs))
      "five mana with two green is out of reach"

  -- The control legs, on the SAME board: everything the board really can pay is
  -- still payable, and each leg needs a different yield out of the same Myr.
  Spec.it s "CR 118.3 the same board still pays {2}{G}{G}, {5} and {G}{G}{G}" $ do
    ashaya <- S.printingOf s registry "Ashaya, Soul of the Wild"
    palladiumMyr <- S.printingOf s registry "Palladium Myr"
    let (_, g1) = S.addCreature ashaya S.alice (Setup.emptyGame S.bothPlayers)
        (_, g2) = S.addCreature palladiumMyr S.alice g1
        (_, gs) = S.addCreature palladiumMyr S.alice g2
        green = ManaSymbol.OfType (ManaType.Colored Color.Green)
        pays cost = Mana.canPay S.alice (ManaCost.MkManaCost cost) gs
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
    let (_, g1) = S.addCreature ashaya S.alice (Setup.emptyGame S.bothPlayers)
        (_, g2) = S.addCreature palladiumMyr S.alice g1
        (_, g3) = S.addCreature palladiumMyr S.alice g2
        (planeId, g4) = S.addHandCard livingPlane S.alice g3
        (towershellId, g5) = S.addHandCard towershell S.alice g4
        -- CR 303.1 (the enchantment) and CR 302.1 (the creature) name the same
        -- window -- a main phase of your own turn, stack empty -- so it has to be
        -- open for either to be offered at all.
        gs = g5 {GameState.phase = Phase.PrecombatMain, GameState.priority = Just S.alice}
        offered = Action.legalActions S.alice gs
    Spec.assertBool s (elem (Action.Type.Cast planeId (S.printingName livingPlane)) offered) "{2}{G}{G} is offered"
    Spec.assertBool s (not (any (S.isCastOf towershellId) offered)) "{3}{G}{G} is not"

  -- And the offer is honoured: the same board casts Living Plane end to end,
  -- which it can only do by tapping one Myr for its Forest's {G} and the other
  -- for {C}{C} -- the mixed choice the transposing model could not represent.
  Spec.it s "CR 601.2g Living Plane is cast off Ashaya and two Palladium Myrs" $ do
    ashaya <- S.printingOf s registry "Ashaya, Soul of the Wild"
    palladiumMyr <- S.printingOf s registry "Palladium Myr"
    livingPlane <- S.printingOf s registry "Living Plane"
    let (_, g1) = S.addCreature ashaya S.alice (Setup.emptyGame S.bothPlayers)
        (firstMyrId, g2) = S.addCreature palladiumMyr S.alice g1
        (_, g3) = S.addCreature palladiumMyr S.alice g2
        (withSpell, planeId) = S.handOne livingPlane g3
        cast = S.runPure (prefersLongYieldFrom firstMyrId) withSpell (S.cast S.alice planeId)
        resolved = S.runPure S.identityAnswer cast Stack.resolveTop
    Spec.assertEqWith s "stack empty" (length (GameState.stack resolved)) 0
    Spec.assertEqWith s "Living Plane resolved" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Living Plane") S.alice resolved) 1
    Spec.assertEqWith s "all three sources tapped" (S.tappedCount S.alice resolved) 3
    Spec.assertEqWith s "and nothing was left over" (poolSize S.alice resolved) 0

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Mana" $ do
  manaSpec s registry
  castabilitySpec s registry
  anyColorSpec s registry
  solRingSpec s registry
  palladiumMyrSpec s registry
  hybridSpec s registry
  monocoloredHybridSpec s registry
  phyrexianSpec s registry
  totalCostSpec s registry
  dismemberSpec s registry
  moltensteelSpec s registry
  upwellingSpec s registry
  omnathSpec s registry
  snowSpec s registry

-- Icehide Golem's whole printed cost. Restated rather than read off the card,
-- for the reason javelinCost gives; Pawl.CardsSpec pins it against
-- data/cards/icehide-golem.json.
snowCost :: ManaCost.ManaCost
snowCost = ManaCost.MkManaCost [ManaSymbol.Snow]

-- The units of Alice's pool, so a test can look at a mana's TAGS and not only at
-- its type -- which is the whole of what CR 107.4h reads.
poolUnits :: GameState.GameState -> [ManaUnit.ManaUnit]
poolUnits gs = case Game.poolOf S.alice gs of
  Mana.Type.MkMana units -> units

-- One red mana, produced by a snow source and by a nonsnow one. These are what
-- tapping a Snow-Covered Mountain and a Mountain really put in a pool, which is
-- itself one of the assertions below; the payment tests then build a pool out of
-- them directly, because `Mana.spend` is a pure function of one.
snowRed :: ManaUnit.ManaUnit
snowRed =
  ManaUnit.MkManaUnit
    { ManaUnit.manaType = ManaType.Colored Color.Red,
      ManaUnit.tags = Set.singleton ProductionTag.Snow
    }

plainRed :: ManaUnit.ManaUnit
plainRed = ManaUnit.MkManaUnit {ManaUnit.manaType = ManaType.Colored Color.Red, ManaUnit.tags = Set.empty}

-- CR 107.4h: "When used in a cost, the snow mana symbol {S} represents a cost
-- that can be paid with one mana of any type produced by a snow source (see rule
-- 106.3). Effects that reduce the amount of generic mana you pay don't affect
-- {S} costs. ... Snow is neither a color nor a type of mana."
--
-- Icehide Golem's entire content is that cost -- its oracle text is nothing but
-- the reminder text for it -- so every test here is about the symbol. The pair
-- that carries the rule is the first two: the same card cast off the same
-- {R}-producing Mountain, differing only in CR 205.4g's supertype.
snowSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
snowSpec s registry = Spec.describe s "Snow" $ do
  Spec.it s "CR 107.4h a Snow-Covered Mountain's mana pays {S}, and Icehide Golem resolves" $ do
    snowMountain <- S.printingOf s registry "Snow-Covered Mountain"
    icehideGolem <- S.printingOf s registry "Icehide Golem"
    let after = resolvedCreature snowMountain icehideGolem 1
    Spec.assertEqWith s "the Golem is on the battlefield" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Icehide Golem") S.alice after) 1
    Spec.assertEqWith s "the Snow-Covered Mountain paid for it" (S.tappedCount S.alice after) 1
    Spec.assertEqWith s "nothing left floating" (poolSize S.alice after) 0

  -- The negative half, and it must fail for the RIGHT reason: the board is a
  -- mana source, it is untapped, and it produces exactly the red mana the snow
  -- one does. CR 205.4g's supertype is the only difference, and CR 107.4h asks
  -- for nothing else.
  Spec.it s "CR 107.4h an ordinary Mountain's mana does not pay {S}" $ do
    mountain <- S.printingOf s registry "Mountain"
    icehideGolem <- S.printingOf s registry "Icehide Golem"
    let board = S.landsInPlay mountain 1
        (g, spellId) = S.handOne icehideGolem board
    Spec.assertBool s (not (null (Mana.manaSources S.alice board))) "the Mountain IS a mana source"
    Spec.assertBool s (Mana.canPay S.alice (ManaCost.MkManaCost [redSymbol]) board) "and it pays {R}"
    Spec.assertBool s (not (Mana.canPay S.alice snowCost board)) "but it does not pay {S}"
    Spec.assertBool s (not (S.castable S.alice spellId g)) "so the Golem cannot be cast"

  -- CR 107.4h's second sentence, from the other end: "Effects that reduce the
  -- amount of generic mana you pay don't affect {S} costs." An {S} that were
  -- Generic 1 would be paid by any one mana, so a board that pays {1} six times
  -- over and still cannot pay {S} is what says the two are different symbols.
  Spec.it s "CR 107.4h {S} is not generic: no number of nonsnow Mountains pays it" $ do
    mountain <- S.printingOf s registry "Mountain"
    let board = S.landsInPlay mountain 6
    Spec.assertBool s (Mana.canPay S.alice (ManaCost.MkManaCost [ManaSymbol.Generic 6]) board) "six Mountains pay {6}"
    Spec.assertBool s (not (Mana.canPay S.alice snowCost board)) "and none of them pays {S}"

  -- The tag narrows nothing ELSE. CR 107.4h's last sentence -- "Snow is neither
  -- a color nor a type of mana" -- cuts both ways: a Snow-Covered Mountain's
  -- mana is red mana, so Skred's {R} is paid by it exactly as a Mountain's is.
  Spec.it s "CR 107.4h a snow source's mana is still its own type, and pays {R}" $ do
    snowMountain <- S.printingOf s registry "Snow-Covered Mountain"
    Spec.assertBool
      s
      (Mana.canPay S.alice (ManaCost.MkManaCost [redSymbol]) (S.landsInPlay snowMountain 1))
      "a Snow-Covered Mountain pays {R}"

  -- CR 106.3: "If mana is produced by an ability, the source of that mana is the
  -- source of that ability." The tag is on the MANA, put there when it was
  -- produced -- which is why this is asked of the pool and not of the land.
  Spec.it s "CR 106.3 the mana a Snow-Covered Mountain adds is tagged snow; a Mountain's is not" $ do
    snowMountain <- S.printingOf s registry "Snow-Covered Mountain"
    mountain <- S.printingOf s registry "Mountain"
    let tapFirst land =
          let board = S.landsInPlay land 1
           in case Game.zoneMembers Zone.Battlefield S.alice board of
                [] -> []
                oid : _ -> poolUnits (S.runPure S.identityAnswer board (Mana.tapForMana oid))
    Spec.assertEqWith s "the snow one" (tapFirst snowMountain) [snowRed]
    Spec.assertEqWith s "the plain one" (tapFirst mountain) [plainRed]

  -- The assignment, not merely the count. Both units are red and only one is
  -- snow, so a payment that took the head of the pool would be right half the
  -- time -- hence both orders.
  Spec.it s "CR 107.4h payment spends the snow mana out of a mixed pool, whichever end it is at" $ do
    Spec.assertEqWith
      s
      "snow first"
      (Mana.spend 0 snowCost (Mana.Type.MkMana [snowRed, plainRed]))
      (Just (Mana.Type.MkMana [plainRed], 0))
    Spec.assertEqWith
      s
      "snow last"
      (Mana.spend 0 snowCost (Mana.Type.MkMana [plainRed, snowRed]))
      (Just (Mana.Type.MkMana [plainRed], 0))

  -- CR 202.2d's colour-granting list names the hybrid and Phyrexian symbols and
  -- not this one, because of CR 107.4h's last sentence: "Snow is neither a color
  -- nor a type of mana." So a card whose whole mana cost is {S} is colorless (CR
  -- 202.2b) -- the sibling of monocoloredHybridSpec's "CR 107.4e a monocolored
  -- hybrid symbol is its coloured half, and only that", which is the same read
  -- taken of a symbol that DOES grant one.
  Spec.it s "CR 202.2b Icehide Golem is colorless: its {S} grants no colour" $ do
    icehideGolem <- S.printingOf s registry "Icehide Golem"
    let (oid, gs) = S.addCreature icehideGolem S.alice (Setup.emptyGame S.bothPlayers)
    Spec.assertEqWith s "colorless" (Projection.colorsOf oid gs) Set.empty

-- alice controls `n` copies of `first` and `m` copies of `second`, and nothing
-- else. Both are lands in every caller, but nothing here requires it.
mixedLands :: Printing.Printing -> Printing.Printing -> Int -> Int -> GameState.GameState
mixedLands first second n m =
  let base = S.landsInPlay first n
   in List.foldl' (\g _ -> snd (S.addCreature second S.alice g)) base [1 .. m]

redGreen :: ManaSymbol.ManaSymbol
redGreen = ManaSymbol.Hybrid (ManaType.Colored Color.Red) (ManaType.Colored Color.Green)

redSymbol :: ManaSymbol.ManaSymbol
redSymbol = ManaSymbol.OfType (ManaType.Colored Color.Red)

-- CR 107.4e: "A hybrid symbol such as {W/U} can be paid with either white or blue
-- mana." Its example is exactly this shape: "{G/W}{G/W} can be paid by spending
-- {G}{G}, {G}{W}, or {W}{W}."
hybridSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
hybridSpec s registry = Spec.describe s "Hybrid" $ do
  Spec.it s "CR 107.4e one {R/G} is payable from either half, and from neither otherwise" $ do
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    island <- S.printingOf s registry "Island"
    let cost = ManaCost.MkManaCost [redGreen]
    Spec.assertBool s (Mana.canPay S.alice cost (S.landsInPlay mountain 1)) "a Mountain pays it"
    Spec.assertBool s (Mana.canPay S.alice cost (mixedLands mountain forest 0 1)) "a Forest pays it"
    Spec.assertBool s (not (Mana.canPay S.alice cost (S.landsInPlay island 1))) "an Island does not"
    Spec.assertBool s (not (Mana.canPay S.alice cost (S.landsInPlay mountain 0))) "and nothing does not"

  -- THE case a greedy left-to-right match gets wrong, and the reason
  -- Mana.spendDemands searches instead of folding. One Mountain and one
  -- Forest pay {R/G}{R} only if the hybrid takes the GREEN; handing it the
  -- red first strands the {R} with a Forest still untapped.
  Spec.it s "CR 107.4e {R/G}{R} off one Mountain and one Forest: the hybrid must take the GREEN" $ do
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    let cost = ManaCost.MkManaCost [redGreen, redSymbol]
        gs = mixedLands mountain forest 1 1
    Spec.assertBool s (Mana.canPay S.alice cost gs) "canPay says yes"
    let (paid, after) = S.runPureWith S.identityAnswer gs (Mana.payCost S.alice cost)
    Spec.assertBool s paid "and it really is paid"
    Spec.assertEqWith s "both lands tapped" (S.tappedCount S.alice after) 2
    Spec.assertEqWith s "nothing left floating" (poolSize S.alice after) 0

  -- The twin: the same cost with no red anywhere is unpayable, so the case
  -- above is not "hybrids always succeed".
  Spec.it s "CR 107.4e {R/G}{R} off two Forests is unpayable -- the {R} has no source" $ do
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    let cost = ManaCost.MkManaCost [redGreen, redSymbol]
        gs = mixedLands mountain forest 0 2
    Spec.assertBool s (not (Mana.canPay S.alice cost gs)) "canPay says no"
    Spec.assertBool s (not (fst (S.runPureWith S.identityAnswer gs (Mana.payCost S.alice cost)))) "and paying fails"
    Spec.assertEqWith s "two {R/G} alone WOULD be payable from them" (Mana.canPay S.alice (ManaCost.MkManaCost [redGreen, redGreen]) gs) True

  Spec.it s "CR 107.4e whole card: Burning-Tree Emissary casts off RR, GG, or RG" $ do
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    burningTreeEmissary <- S.printingOf s registry "Burning-Tree Emissary"
    let castOff reds greens =
          let (gs, spellId) = S.handOne burningTreeEmissary (mixedLands mountain forest reds greens)
              cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
           in length (GameState.stack cast)
    Spec.assertEqWith s "two Mountains" (castOff 2 0) 1
    Spec.assertEqWith s "two Forests" (castOff 0 2) 1
    Spec.assertEqWith s "one of each" (castOff 1 1) 1
    Spec.assertEqWith s "one land is not enough" (castOff 1 0) 0

  Spec.it s "CR 107.4e a hybrid symbol is ALL of its component colours" $ do
    burningTreeEmissary <- S.printingOf s registry "Burning-Tree Emissary"
    let (oid, gs) = S.addCreature burningTreeEmissary S.alice (Setup.emptyGame S.bothPlayers)
    Spec.assertEqWith
      s
      "red AND green, not one or the other"
      (Projection.colorsOf oid gs)
      (Set.fromList [Color.Red, Color.Green])

twoOrRed :: ManaSymbol.ManaSymbol
twoOrRed = ManaSymbol.MonocoloredHybrid (ManaType.Colored Color.Red)

-- Flame Javelin's printed cost. Restated rather than read off the card so that
-- the payment assertions below say what they mean; CardSpec is what pins this
-- against data/cards/flame-javelin.json.
javelinCost :: ManaCost.ManaCost
javelinCost = ManaCost.MkManaCost [twoOrRed, twoOrRed, twoOrRed]

-- CR 107.4e's other half: "a monocolored hybrid symbol such as {2/B} can be paid
-- with either one black mana or two mana of any type."
--
-- Flame Javelin ({2/R}{2/R}{2/R}) throughout, because the symbol only becomes
-- interesting in bulk: one of them is barely distinguishable from {R}, three of
-- them span {R}{R}{R} to {6}.
monocoloredHybridSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
monocoloredHybridSpec s registry = Spec.describe s "MonocoloredHybrid" $ do
  let -- How many objects the stack holds after alice tries to cast the Javelin
      -- with `gs` already on the battlefield: 1 when the cost was paid, 0 when
      -- CR 601.2h rolled the whole attempt back.
      castsOff javelin gs =
        let (g, spellId) = S.handOne javelin gs
         in length (GameState.stack (snd (Engine.runGamePure S.identityAnswer g (S.cast S.alice spellId))))

  Spec.it s "CR 107.4e one {2/R} takes one Mountain OR two Islands, and one Island is not enough" $ do
    mountain <- S.printingOf s registry "Mountain"
    island <- S.printingOf s registry "Island"
    let cost = ManaCost.MkManaCost [twoOrRed]
    Spec.assertBool s (Mana.canPay S.alice cost (S.landsInPlay mountain 1)) "one Mountain pays it"
    Spec.assertBool s (Mana.canPay S.alice cost (S.landsInPlay island 2)) "two Islands pay it"
    Spec.assertBool s (not (Mana.canPay S.alice cost (S.landsInPlay island 1))) "one Island does not"
    Spec.assertBool s (not (Mana.canPay S.alice cost (S.landsInPlay island 0))) "and nothing does not"

  -- The coloured route, end to end. Three lands for three symbols is the
  -- reading a payment path that charged every symbol one mana would also
  -- get right, so this is the control the cases below discriminate
  -- against -- and the tap count is what says the route was really taken:
  -- six Mountains would still be three taps, because payCost stops as
  -- soon as the cost is payable.
  Spec.it s "CR 107.4e whole card: Flame Javelin casts off three Mountains, {R} per symbol" $ do
    mountain <- S.printingOf s registry "Mountain"
    flameJavelin <- S.printingOf s registry "Flame Javelin"
    let gs = S.landsInPlay mountain 3
    Spec.assertBool s (Mana.canPay S.alice javelinCost gs) "canPay says yes"
    Spec.assertEqWith s "and it casts" (castsOff flameJavelin gs) 1
    let (paid, after) = S.runPureWith S.identityAnswer (S.landsInPlay mountain 6) (Mana.payCost S.alice javelinCost)
    Spec.assertBool s paid "six Mountains pay it too"
    Spec.assertEqWith s "and only three of them are tapped" (S.tappedCount S.alice after) 3
    Spec.assertEqWith s "with nothing left floating" (poolSize S.alice after) 0

  -- The generic route, with no red mana anywhere on the board.
  Spec.it s "CR 107.4e whole card: Flame Javelin casts off six Islands, two generic per symbol" $ do
    island <- S.printingOf s registry "Island"
    flameJavelin <- S.printingOf s registry "Flame Javelin"
    let gs = S.landsInPlay island 6
    Spec.assertBool s (Mana.canPay S.alice javelinCost gs) "canPay says yes"
    Spec.assertEqWith s "and it casts" (castsOff flameJavelin gs) 1

  -- THE discriminating negative. Five Islands is one short of the {6} the
  -- all-generic route needs, and a payment path that charged one mana per
  -- {2/R} would call three of them sufficient, let alone five.
  Spec.it s "CR 107.4e five Islands cannot cast Flame Javelin -- {6} is one mana away" $ do
    island <- S.printingOf s registry "Island"
    flameJavelin <- S.printingOf s registry "Flame Javelin"
    let gs = S.landsInPlay island 5
    Spec.assertBool s (not (Mana.canPay S.alice javelinCost gs)) "canPay says no"
    Spec.assertEqWith s "and it does not cast" (castsOff flameJavelin gs) 0
    Spec.assertBool s (not (Mana.canPay S.alice javelinCost (S.landsInPlay island 3))) "three Islands are nowhere near"

  -- CR 107.4e symbol by symbol, which the card's own ruling spells out:
  -- "you can pay for Flame Javelin by spending {R}{R}{R}, {2}{R}{R},
  -- {4}{R}, or {6}." So the routes are chosen per symbol, and a search
  -- that picked one route for the whole cost would reject both of these.
  Spec.it s "CR 107.4e each symbol picks its own route: {R}{R}{2} and {R}{4}" $ do
    mountain <- S.printingOf s registry "Mountain"
    island <- S.printingOf s registry "Island"
    flameJavelin <- S.printingOf s registry "Flame Javelin"
    let cost = javelinCost
    Spec.assertBool s (Mana.canPay S.alice cost (mixedLands mountain island 2 2)) "two Mountains and two Islands: {R}{R}{2}"
    Spec.assertBool s (Mana.canPay S.alice cost (mixedLands mountain island 1 4)) "one Mountain and four Islands: {R}{4}"
    Spec.assertEqWith s "and that one really casts" (castsOff flameJavelin (mixedLands mountain island 1 4)) 1
    -- One short of {R}{4} and one red short of {R}{R}{2}: four mana with
    -- only one red pays no route at all.
    Spec.assertBool s (not (Mana.canPay S.alice cost (mixedLands mountain island 1 3))) "one Mountain and three Islands: no route"

  -- The gameplay-level proof (design.md section 4): the whole card, cast
  -- and resolved off the all-generic route, doing what it says.
  Spec.it s "CR 107.4e Flame Javelin cast off six Islands resolves for 4 damage" $ do
    island <- S.printingOf s registry "Island"
    flameJavelin <- S.printingOf s registry "Flame Javelin"
    let (g, spellId) = S.handOne flameJavelin (S.landsInPlay island 6)
        cast = snd (Engine.runGamePure S.identityAnswer g (S.cast S.alice spellId))
        resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "stack empty" (length (GameState.stack resolved)) 0
    Spec.assertEqWith s "every Island tapped" (S.tappedCount S.alice resolved) 6
    Spec.assertEqWith s "nothing left floating" (poolSize S.alice resolved) 0
    -- S.identityAnswer takes the least Recipient on offer, which with no
    -- creatures anywhere is alice herself. Who it hits is the answer's
    -- business; that it hits for 4 is the card's.
    Spec.assertEqWith s "4 damage to the chosen target" (S.lifeOf S.alice resolved) (Just 16)

  -- The elision, made visible rather than left implied. Both halves are
  -- payable out of this pool and they leave DIFFERENT pools behind, so
  -- unlike a colour/colour hybrid the choice is observable, and pawl
  -- makes it: it spends the fewest units. CR 601.2b puts that choice with
  -- the player, at announcement (#261).
  Spec.it s "CR 601.2b the engine takes a {2/R}'s one-mana half when both halves are payable (#261)" $
    let red = ManaUnit.MkManaUnit {ManaUnit.manaType = ManaType.Colored Color.Red, ManaUnit.tags = Set.empty}
        colorless = ManaUnit.MkManaUnit {ManaUnit.manaType = ManaType.Colorless, ManaUnit.tags = Set.empty}
     in Spec.assertEqWith
          s
          "the {R} is spent and both {C} remain -- the other half would spend both {C} and leave the {R}"
          (Mana.spend 0 (ManaCost.MkManaCost [twoOrRed]) (Mana.Type.MkMana [red, colorless, colorless]))
          (Just (Mana.Type.MkMana [colorless, colorless], 0))

  -- CR 107.4e's last sentence, as CR 202.2d restates it for the whole
  -- object: a monocolored hybrid's other component is generic mana, which
  -- is no colour, so only the named half counts. Flame Javelin is red
  -- even when six Islands paid for it.
  Spec.it s "CR 107.4e a monocolored hybrid symbol is its coloured half, and only that" $ do
    flameJavelin <- S.printingOf s registry "Flame Javelin"
    let (oid, gs) = S.addCreature flameJavelin S.alice (Setup.emptyGame S.bothPlayers)
    Spec.assertEqWith s "red, not colourless" (Projection.colorsOf oid gs) (Set.singleton Color.Red)

-- Mutagenic Growth's printed cost. Restated rather than read off the card, for
-- the reason javelinCost gives; CardSpec pins it against
-- data/cards/mutagenic-growth.json.
phyrexianCost :: ManaCost.ManaCost
phyrexianCost = ManaCost.MkManaCost [ManaSymbol.Phyrexian Color.Green]

-- Answers Prompt.AnnouncePhyrexianPayment with `way` whenever it is on offer,
-- and defers everything else to S.identityAnswer -- the prefersSource shape. The
-- "whenever it is on offer" is what makes the two elision cases below
-- discriminating: an interpreter asking for the life route on a board that does
-- not offer it must not get it.
announces :: PhyrexianPayment.PhyrexianPayment -> Prompt.Prompt r -> r
announces way p = case p of
  Prompt.AnnouncePhyrexianPayment _ _ _ _ offers ->
    if elem way (NonEmpty.toList offers) then way else NonEmpty.head offers
  _ -> S.identityAnswer p

-- Was CR 118.13a's announcement actually asked for, or did the engine decide?
wasAskedHowToPayPhyrexian :: [Response.Response] -> Bool
wasAskedHowToPayPhyrexian = not . null . phyrexianAnnouncements

-- Every announcement the engine asked for, in the order it asked -- which is CR
-- 601.2b's "for each of those symbols", so the LENGTH is how many of a cost's
-- Phyrexian symbols were a real choice and how many were forced.
phyrexianAnnouncements :: [Response.Response] -> [PhyrexianPayment.PhyrexianPayment]
phyrexianAnnouncements responses =
  let announcement r = case r of
        Response.AnnouncedPhyrexianPayment way -> Just way
        _ -> Nothing
   in Maybe.mapMaybe announcement responses

-- The board issue #361 named: alice controls one untapped Forest and a Goblin Piker for
-- Mutagenic Growth to target, and holds Mutagenic Growth ({G/P}) and Llanowar
-- Elves ({G}). ONE green source and two spells that want it, so which way CR
-- 107.4f's symbol is paid decides whether the Elves can be cast at all -- the
-- most direct observation there is that the choice is not the engine's.
phyrexianBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
phyrexianBoard forest piker growth elves =
  let (_, withPiker) = S.addCreature piker S.alice (S.landsInPlay forest 1)
      (withGrowth, growthId) = S.handOne growth withPiker
      (elvesId, gs) = S.addHandCard elves S.alice withGrowth
   in (growthId, elvesId, gs)

-- Cast `oid` under `answer` and resolve what it put on the stack, returning the
-- transcript of everything the engine asked alongside the final state.
castAndResolve ::
  (forall r. Prompt.Prompt r -> r) ->
  GameState.GameState ->
  ObjectId.ObjectId ->
  ([Response.Response], GameState.GameState)
castAndResolve answer gs oid =
  let ((_, cast), asked) = Replay.record answer gs (S.cast S.alice oid)
   in (asked, snd (S.runPureWith answer cast Stack.resolveTop))

-- alice at `n` life and nothing else on the board.
aliceAt :: Integer -> GameState.GameState
aliceAt n =
  let gs = Setup.emptyGame S.bothPlayers
   in gs {GameState.players = Map.adjust (\p -> p {Player.life = n}) S.alice (GameState.players gs)}

-- CR 107.4f: "A Phyrexian mana symbol represents a cost that can be paid either
-- with one mana of its color or by paying 2 life."
--
-- Mutagenic Growth ({G/P}) throughout -- a plain pump, so every assertion below
-- is about the symbol and nothing else.
--
-- TWO PATHS, and which one a case takes decides who chooses. A case calling
-- Mana.payCost directly pays an UNANNOUNCED cost, where the least-life rule still
-- decides (#373); a case going through Cast.castSpell announces first, under CR
-- 118.13a, and the player decides. The CR 118.13a cases at the end of this group
-- are the second path.
phyrexianSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
phyrexianSpec s registry = Spec.describe s "Phyrexian" $ do
  -- The mana route. The life assertion is what makes this discriminating:
  -- both routes are open here, and paying life as WELL as the mana, or
  -- INSTEAD of it, would each read as "paid" without it.
  --
  -- It also pins what Mana.payCost does with an UNANNOUNCED cost, which is
  -- what this and the four cases after it exercise: they call payCost
  -- directly, so no CR 118.13a announcement has happened and the least-life
  -- rule still decides, which here means none (#373). A cast goes through
  -- Cast.castSpell instead and asks -- see the CR 118.13a cases at the end of
  -- this group.
  Spec.it s "CR 107.4f one {G/P} is paid with one green mana and no life" $ do
    forest <- S.printingOf s registry "Forest"
    let gs = S.landsInPlay forest 1
    Spec.assertBool s (Mana.canPay S.alice phyrexianCost gs) "canPay says yes"
    let (paid, after) = S.runPureWith S.identityAnswer gs (Mana.payCost S.alice phyrexianCost)
    Spec.assertBool s paid "and it really is paid"
    Spec.assertEqWith s "the Forest is tapped" (S.tappedCount S.alice after) 1
    Spec.assertEqWith s "nothing left floating" (poolSize S.alice after) 0
    Spec.assertEqWith s "life untouched" (S.lifeOf S.alice after) (Just 20)

  -- The life route, with a land on the battlefield that cannot help. The tap
  -- count is the discriminator: a payment path that tapped the Mountain
  -- first and then paid life would still leave alice at 18.
  Spec.it s "CR 107.4f one {G/P} is paid by 2 life when no green mana can be made" $ do
    mountain <- S.printingOf s registry "Mountain"
    let gs = S.landsInPlay mountain 1
    Spec.assertBool s (Mana.canPay S.alice phyrexianCost gs) "canPay says yes"
    let (paid, after) = S.runPureWith S.identityAnswer gs (Mana.payCost S.alice phyrexianCost)
    Spec.assertBool s paid "and it really is paid"
    Spec.assertEqWith s "exactly 2 life" (S.lifeOf S.alice after) (Just 18)
    Spec.assertEqWith s "the Mountain is untouched" (S.tappedCount S.alice after) 0
    Spec.assertEqWith s "nothing left floating" (poolSize S.alice after) 0

  -- CR 119.4: "the player may do so only if their life total is greater than
  -- or equal to the amount of the payment." Two is the boundary, and the
  -- payment that takes alice to exactly 0 is legal -- CR 704.5a's loss is a
  -- state-based action afterwards, not a bar on the payment.
  Spec.it s "CR 119.4 a {G/P} is payable at 2 life and unpayable at 1" $ do
    Spec.assertBool s (Mana.canPay S.alice phyrexianCost (aliceAt 2)) "2 life is enough"
    Spec.assertBool s (not (Mana.canPay S.alice phyrexianCost (aliceAt 1))) "1 life is not"
    Spec.assertBool s (not (Mana.canPay S.alice phyrexianCost (aliceAt 0))) "0 life is not"
    let (paid, after) = S.runPureWith S.identityAnswer (aliceAt 2) (Mana.payCost S.alice phyrexianCost)
    Spec.assertBool s paid "paying at 2 really works"
    Spec.assertEqWith s "and takes her to 0" (S.lifeOf S.alice after) (Just 0)
    let (failed, unchanged) = S.runPureWith S.identityAnswer (aliceAt 1) (Mana.payCost S.alice phyrexianCost)
    Spec.assertBool s (not failed) "at 1 the payment fails"
    Spec.assertEqWith s "and CR 601.2h leaves the life total alone" (S.lifeOf S.alice unchanged) (Just 1)

  -- The gameplay-level proof (design.md section 4), mana route: the whole
  -- card, cast off one Forest and resolved. Goblin Piker is 2/1, so +2/+2 is
  -- 4/3.
  Spec.it s "CR 107.4f whole card: Mutagenic Growth casts off one Forest for +2/+2" $ do
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    mutagenicGrowth <- S.printingOf s registry "Mutagenic Growth"
    let (pikerId, withPiker) = S.addCreature piker S.alice (S.landsInPlay forest 1)
        (g, spellId) = S.handOne mutagenicGrowth withPiker
        cast = snd (Engine.runGamePure S.identityAnswer g (S.cast S.alice spellId))
        resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "stack empty" (length (GameState.stack resolved)) 0
    Spec.assertEqWith s "power" (Projection.powerOf pikerId resolved) (Just 4)
    Spec.assertEqWith s "toughness" (Projection.toughnessOf pikerId resolved) (Just 3)
    Spec.assertEqWith s "the Forest paid for it" (S.tappedCount S.alice resolved) 1
    Spec.assertEqWith s "so no life was" (S.lifeOf S.alice resolved) (Just 20)

  -- The same card with NO lands anywhere. Castability has to see the life
  -- route or this never reaches the stack at all.
  Spec.it s "CR 107.4f whole card: Mutagenic Growth casts with no mana at all, for 2 life" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    mutagenicGrowth <- S.printingOf s registry "Mutagenic Growth"
    let (pikerId, withPiker) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
        (g, spellId) = S.handOne mutagenicGrowth withPiker
    Spec.assertBool s (S.castable S.alice spellId g) "castable with an empty battlefield but for the Piker"
    let cast = snd (Engine.runGamePure S.identityAnswer g (S.cast S.alice spellId))
        resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "stack empty" (length (GameState.stack resolved)) 0
    Spec.assertEqWith s "power" (Projection.powerOf pikerId resolved) (Just 4)
    Spec.assertEqWith s "toughness" (Projection.toughnessOf pikerId resolved) (Just 3)
    Spec.assertEqWith s "exactly 2 life paid" (S.lifeOf S.alice resolved) (Just 18)

  -- THE discriminating negative: neither route open. One life short, and no
  -- green mana on the board.
  Spec.it s "CR 119.4 Mutagenic Growth is uncastable at 1 life with no green mana" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    mutagenicGrowth <- S.printingOf s registry "Mutagenic Growth"
    let inHandAt n =
          let (_, withPiker) = S.addCreature piker S.alice (aliceAt n)
           in S.handOne mutagenicGrowth withPiker
        castableAt n = let (g, spellId) = inHandAt n in S.castable S.alice spellId g
        castsAt n =
          let (g, spellId) = inHandAt n
           in length (GameState.stack (snd (Engine.runGamePure S.identityAnswer g (S.cast S.alice spellId))))
    Spec.assertBool s (not (castableAt 1)) "at 1 life it is not castable"
    Spec.assertEqWith s "and it does not cast" (castsAt 1) 0
    Spec.assertBool s (castableAt 2) "at 2 life it is -- the Piker it targets has not moved"
    Spec.assertEqWith s "and it does cast" (castsAt 2) 1

  -- CR 107.4f's FIRST clause, the one a payment-only reading loses:
  -- "Phyrexian mana symbols are colored mana symbols: ... {G/P} is green."
  -- CR 202.2d says the same of the object: "An object with one or more
  -- hybrid mana symbols and/or Phyrexian mana symbols in its mana cost is all
  -- of the colors of those mana symbols, in addition to any other colors the
  -- object might be."
  Spec.it s "CR 107.4f/202.2d a Phyrexian mana symbol is a COLOURED mana symbol" $ do
    mutagenicGrowth <- S.printingOf s registry "Mutagenic Growth"
    let (oid, gs) = S.addCreature mutagenicGrowth S.alice (Setup.emptyGame S.bothPlayers)
    Spec.assertEqWith s "green, not colourless" (Projection.colorsOf oid gs) (Set.singleton Color.Green)

  -- And the colour survives the route that produces no green mana at all --
  -- the reading that would call the card colourless is exactly the one a
  -- life-paid cast tempts.
  Spec.it s "CR 202.2d Mutagenic Growth is green on the stack even when 2 life paid for it" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    mutagenicGrowth <- S.printingOf s registry "Mutagenic Growth"
    let (_, withPiker) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
        (g, spellId) = S.handOne mutagenicGrowth withPiker
        cast = snd (Engine.runGamePure S.identityAnswer g (S.cast S.alice spellId))
    Spec.assertEqWith s "2 life paid, no green mana ever made" (S.lifeOf S.alice cast) (Just 18)
    case GameState.stack cast of
      [sid] -> Spec.assertEqWith s "and the spell is still green" (Projection.colorsOf sid cast) (Set.singleton Color.Green)
      _ -> Spec.assertFailure s "expected exactly one spell on the stack"

  -- Mana.resolutions' SORT, pinned -- the least-life rule has to hold across
  -- symbols and not merely within one, and the per-symbol product alone does
  -- not give that. CR 601.2b's nonhybrid equivalents of {2/R}{G/P} leave the
  -- product in the order 0, 2, 0, 2 life, so unsorted the first PAYABLE entry
  -- on this board is the 2-life one.
  --
  -- A lone Birds of Paradise and two Islands make all four orderings matter:
  -- {R}{G} is impossible (one Birds makes one mana), {R} plus 2 life works,
  -- {G} plus {2} works and costs nothing, and {2} plus 2 life works. The
  -- least is zero, and pawl must find it.
  Spec.it s "CR 107.4f the least-life route is found across symbols, not only within one" $ do
    birds <- S.printingOf s registry "Birds of Paradise"
    island <- S.printingOf s registry "Island"
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    let cost = ManaCost.MkManaCost [twoOrRed, ManaSymbol.Phyrexian Color.Green]
    Spec.assertEqWith
      s
      "the {G} plus {2} route, costing no life"
      (Mana.lifeNeeded S.alice cost (mixedLands island birds 2 1))
      (Just 0)
    Spec.assertBool s (Mana.canPay S.alice cost (mixedLands island birds 2 1)) "and it is payable"
    -- The discriminator: the same cost and the same three permanents, but a
    -- Mountain in the Birds' place makes no green, so every surviving route
    -- costs 2 life and the answer really does depend on the board.
    Spec.assertEqWith
      s
      "with a Mountain instead, 2 life is the cheapest there is"
      (Mana.lifeNeeded S.alice cost (mixedLands island mountain 2 1))
      (Just 2)
    Spec.assertEqWith
      s
      "and a lone {G/P} off nothing at all is 2 as well"
      (Mana.lifeNeeded S.alice phyrexianCost (Setup.emptyGame S.bothPlayers))
      (Just 2)
    Spec.assertEqWith
      s
      "while a lone {G/P} with a Forest is 0"
      (Mana.lifeNeeded S.alice phyrexianCost (S.landsInPlay forest 1))
      (Just 0)

  -- The budget is recomputed as sources are tapped, not fixed when the
  -- payment starts, and a Birds of Paradise is what makes the difference
  -- observable: it COULD make green, so pawl starts with a budget of zero
  -- life and taps it -- and when the player names blue instead, the mana way
  -- is gone and CR 107.4f's 2 life is all that is left. pawl pays it rather
  -- than failing the payment, which is the same MORE PERMISSIVE posture
  -- Mana.payCost's haddock takes towards a mis-tapped colour (#261). Reached
  -- only because this calls payCost directly, with nothing announced (#373).
  Spec.it s "CR 107.4f a Birds tapped for blue still pays a {G/P}, out of life" $ do
    birds <- S.printingOf s registry "Birds of Paradise"
    let (_, gs) = S.addCreature birds S.alice (Setup.emptyGame S.bothPlayers)
        (paidBlue, afterBlue) = S.runPureWith (prefersColor Color.Blue) gs (Mana.payCost S.alice phyrexianCost)
    Spec.assertBool s paidBlue "the cost is still paid"
    Spec.assertEqWith s "by 2 life" (S.lifeOf S.alice afterBlue) (Just 18)
    Spec.assertEqWith s "the Birds was tapped on the way" (S.tappedCount S.alice afterBlue) 1
    Spec.assertEqWith s "and its blue mana is still floating" (poolSize S.alice afterBlue) 1
    -- The control: the same board and the same card, one different answer.
    let (paidGreen, afterGreen) = S.runPureWith (prefersColor Color.Green) gs (Mana.payCost S.alice phyrexianCost)
    Spec.assertBool s paidGreen "green pays it too"
    Spec.assertEqWith s "and costs no life at all" (S.lifeOf S.alice afterGreen) (Just 20)
    Spec.assertEqWith s "with nothing left floating" (poolSize S.alice afterGreen) 0

  -- CR 118.13a: "If the mana cost of a spell ... contains a mana symbol that
  -- can be paid in multiple ways, the choice of how to pay for that symbol is
  -- made as its controller proposes that spell or ability (see rule 601.2b)."
  --
  -- THE proving scenario (#361), and the reason the choice is not the
  -- engine's to make conservatively: a player holding one Forest can cast
  -- Mutagenic Growth AND Llanowar Elves only by announcing 2 life for the
  -- {G/P}. The Elves' castability is the discriminator -- a life total alone
  -- could be produced by paying life on TOP of the mana.
  Spec.it s "CR 118.13a announcing 2 life keeps the Forest, so Llanowar Elves is still castable" $ do
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    growth <- S.printingOf s registry "Mutagenic Growth"
    elves <- S.printingOf s registry "Llanowar Elves"
    let (growthId, elvesId, gs) = phyrexianBoard forest piker growth elves
        (asked, resolved) = castAndResolve (announces PhyrexianPayment.PaysLife) gs growthId
    Spec.assertBool s (wasAskedHowToPayPhyrexian asked) "the engine asked rather than deciding"
    Spec.assertEqWith s "the Growth resolved" (length (GameState.stack resolved)) 0
    Spec.assertEqWith s "exactly 2 life" (S.lifeOf S.alice resolved) (Just 18)
    Spec.assertEqWith s "and the Forest is untapped" (S.tappedCount S.alice resolved) 0
    Spec.assertBool s (S.castable S.alice elvesId resolved) "so the Elves can still be cast"

  -- The control, one answer different on the same board: the mana route
  -- spends the Forest and the Elves are stranded. Both legs are needed --
  -- either alone would pass against an engine that ignored the answer.
  Spec.it s "CR 118.13a announcing coloured mana taps the Forest, and the Elves are stranded" $ do
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    growth <- S.printingOf s registry "Mutagenic Growth"
    elves <- S.printingOf s registry "Llanowar Elves"
    let (growthId, elvesId, gs) = phyrexianBoard forest piker growth elves
        (asked, resolved) = castAndResolve (announces PhyrexianPayment.PaysMana) gs growthId
    Spec.assertBool s (wasAskedHowToPayPhyrexian asked) "the engine asked here too"
    Spec.assertEqWith s "life untouched" (S.lifeOf S.alice resolved) (Just 20)
    Spec.assertEqWith s "the Forest paid for it" (S.tappedCount S.alice resolved) 1
    Spec.assertBool s (not (S.castable S.alice elvesId resolved)) "and the Elves cannot be cast"

  -- The elision, both directions. Where only ONE route is payable there is
  -- nothing to ask, and the interpreter asking for the other route does not
  -- get it -- CR 601.2b's "previously made choices ... may restrict the
  -- player's options" arriving as a board that offers one option.
  Spec.it s "CR 118.13a no green source: the life route is taken and nothing is asked" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    growth <- S.printingOf s registry "Mutagenic Growth"
    elves <- S.printingOf s registry "Llanowar Elves"
    let (_, withPiker) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
        (withGrowth, growthId) = S.handOne growth withPiker
        (_, gs) = S.addHandCard elves S.alice withGrowth
        (asked, resolved) = castAndResolve (announces PhyrexianPayment.PaysMana) gs growthId
    Spec.assertBool s (not (wasAskedHowToPayPhyrexian asked)) "no choice existed, so none was asked"
    Spec.assertEqWith s "the Growth resolved" (length (GameState.stack resolved)) 0
    Spec.assertEqWith s "and 2 life paid for it" (S.lifeOf S.alice resolved) (Just 18)

  -- CR 119.4's floor closing the life route instead: one Forest, one life.
  -- The interpreter asks for life and must not be given it.
  Spec.it s "CR 119.4 at 1 life the life route is not offered, and nothing is asked" $ do
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    growth <- S.printingOf s registry "Mutagenic Growth"
    elves <- S.printingOf s registry "Llanowar Elves"
    let (growthId, _, board) = phyrexianBoard forest piker growth elves
        gs = board {GameState.players = Map.adjust (\p -> p {Player.life = 1}) S.alice (GameState.players board)}
        (asked, resolved) = castAndResolve (announces PhyrexianPayment.PaysLife) gs growthId
    Spec.assertBool s (not (wasAskedHowToPayPhyrexian asked)) "no choice existed, so none was asked"
    Spec.assertEqWith s "the Growth resolved" (length (GameState.stack resolved)) 0
    Spec.assertEqWith s "the Forest paid for it" (S.tappedCount S.alice resolved) 1
    Spec.assertEqWith s "and the life total is untouched" (S.lifeOf S.alice resolved) (Just 1)

-- The single activated ability of a printing that has exactly one -- Moltensteel
-- Dragon's "{R/P}: This creature gets +1/+0 until end of turn." Total because
-- the spec needs a value; a printing with no ability would fail the assertions that
-- follow rather than this lookup.
theAbility :: Printing.Printing -> ActivatedAbility.ActivatedAbility Card.Type.Card
theAbility p = case Face.activatedAbilities (S.combinedFace p) of
  ab : _ -> ab
  [] -> ActivatedAbility.MkActivatedAbility (Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) []) (singleModeAbility [] Map.empty) ActivationTiming.AnyTime

-- `printing` on the battlefield, settled and untapped, on a board of `n`
-- `land`s -- the shape every board below wants and none of Support's helpers
-- spells directly.
withPermanent :: Printing.Printing -> Printing.Printing -> Int -> GameState.GameState
withPermanent land printing n = snd (S.addCreature printing S.alice (S.landsInPlay land n))

-- CR 601.2f: "The total cost is the mana cost or alternative cost (as determined
-- in rule 601.2b), plus all additional costs and cost increases, and minus all
-- cost reductions."
--
-- So CR 118.13a's announcement (CR 601.2b) comes FIRST and the total comes after
-- it -- but the routes the announcement may take are decided by what the TOTAL
-- will cost, not by the printed cost. Getting that backwards is the engine
-- choosing again, one step further on than #361 reached.
--
-- Two directions, and only one of them is merely untidy:
--
--   * a REDUCTION makes the printed cost dearer than the total, so a route the
--     total could pay reads as unpayable. Where that leaves one route standing,
--     the prompt is elided and the engine pays for the player. Sapphire Medallion
--     and Spined Thopter, below.
--   * an INCREASE makes the printed cost cheaper, so a route the total cannot pay
--     is offered. The player answers it and the payment fails, which CR 601.2's
--     own "the game returns to the moment before the casting of that spell was
--     proposed" would also do -- but the answer was never a real option. Thalia
--     and Mutagenic Growth, below.
totalCostSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
totalCostSpec s registry = Spec.describe s "TotalCost" $ do
  -- THE reduction case. Sapphire Medallion is "Blue spells you cast cost {1}
  -- less to cast", Spined Thopter is {2}{U/P}, and two Islands are exactly
  -- the board where the reduction decides the question: the total {1}{U/P}
  -- can be paid with {1}{U} off both Islands, while the printed {2}{U/P}
  -- cannot be paid with mana at all. So the coloured-mana route IS available
  -- and the player must be asked for it.
  Spec.it s "CR 601.2f a reduction opens the coloured-mana route, so the announcement is asked" $ do
    island <- S.printingOf s registry "Island"
    medallion <- S.printingOf s registry "Sapphire Medallion"
    thopter <- S.printingOf s registry "Spined Thopter"
    let (gs, thopterId) = S.handOne thopter (withPermanent island medallion 2)
    Spec.assertBool s (S.castable S.alice thopterId gs) "castable"
    let (asked, resolved) = castAndResolve (announces PhyrexianPayment.PaysMana) gs thopterId
    -- The outcome first, because it is the thing that was wrong: the engine
    -- used to take CR 107.4f's life route here without asking.
    Spec.assertEqWith s "no life was paid" (S.lifeOf S.alice resolved) (Just 20)
    Spec.assertEqWith s "both Islands paid the reduced {1}{U}" (S.tappedCount S.alice resolved) 2
    Spec.assertBool s (wasAskedHowToPayPhyrexian asked) "and the engine asked rather than deciding"
    Spec.assertEqWith s "the Thopter resolved" (length (GameState.stack resolved)) 0

  -- The control, one answer different on the same board: CR 107.4f's life
  -- route leaves an Island up. Both legs are needed -- either alone would
  -- pass against an engine that ignored the answer.
  Spec.it s "CR 601.2f the same board's life route pays 2 and spares an Island" $ do
    island <- S.printingOf s registry "Island"
    medallion <- S.printingOf s registry "Sapphire Medallion"
    thopter <- S.printingOf s registry "Spined Thopter"
    let (gs, thopterId) = S.handOne thopter (withPermanent island medallion 2)
        (asked, resolved) = castAndResolve (announces PhyrexianPayment.PaysLife) gs thopterId
    Spec.assertBool s (wasAskedHowToPayPhyrexian asked) "asked here too"
    Spec.assertEqWith s "the Thopter resolved" (length (GameState.stack resolved)) 0
    Spec.assertEqWith s "one Island paid the reduced {1}" (S.tappedCount S.alice resolved) 1
    Spec.assertEqWith s "and 2 life paid the symbol" (S.lifeOf S.alice resolved) (Just 18)

  -- THE increase case. Thalia is "Noncreature spells cost {1} more to cast",
  -- Mutagenic Growth is an instant, and one Forest is exactly the board where
  -- the increase decides the question: the total {1}{G/P} cannot be paid with
  -- {1}{G} off one Forest, so the coloured-mana route is NOT available and
  -- must not be offered. The interpreter asks for it and does not get it.
  Spec.it s "CR 601.2f an increase closes the coloured-mana route, so nothing is asked" $ do
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    thalia <- S.printingOf s registry "Thalia, Guardian of Thraben"
    growth <- S.printingOf s registry "Mutagenic Growth"
    -- The Piker goes down BEFORE Thalia, because S.identityAnswer's
    -- ChooseTargets takes the lowest object id and both are 2/1 creatures --
    -- so with Thalia first the Growth would pump HER and the assertion below
    -- would be reading the wrong permanent.
    let (_, withPiker) = S.addCreature piker S.alice (S.landsInPlay forest 1)
        (_, withThalia) = S.addCreature thalia S.alice withPiker
        (gs, growthId) = S.handOne growth withThalia
    Spec.assertBool s (S.castable S.alice growthId gs) "castable, by CR 107.4f's life route"
    let (asked, resolved) = castAndResolve (announces PhyrexianPayment.PaysMana) gs growthId
    -- The outcome first again: answering the route the engine used to offer
    -- made the whole cast a no-op, so the Piker went unpumped and no life was
    -- paid at all.
    Spec.assertEqWith s "2 life paid the symbol" (S.lifeOf S.alice resolved) (Just 18)
    Spec.assertEqWith s "the Piker really was pumped" (Projection.powerOf (pikerOn resolved) resolved) (Just 4)
    Spec.assertEqWith s "the Forest paid Thalia's {1}" (S.tappedCount S.alice resolved) 1
    Spec.assertBool s (not (wasAskedHowToPayPhyrexian asked)) "and no route existed, so none was asked"
    Spec.assertEqWith s "the Growth resolved rather than evaporating" (length (GameState.stack resolved)) 0

  -- The increase again, with TWO symbols, which is what makes it a cast lost
  -- rather than a cast made awkwardly: Dismember's total under Thalia is
  -- {2}{B/P}{B/P}, and two Swamps pay that only by CR 107.4f's life route
  -- twice. Measured against the printed {1}{B/P}{B/P} the first symbol looks
  -- like a real choice, and taking its mana route strands the payment.
  Spec.it s "CR 601.2f Dismember under Thalia forces both symbols to life, and the cast survives" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    thalia <- S.printingOf s registry "Thalia, Guardian of Thraben"
    dismember <- S.printingOf s registry "Dismember"
    -- Piker before Thalia, for the reason the case above gives: both are 2/1
    -- creatures and identityAnswer targets the lowest object id.
    let (_, withPiker) = S.addCreature piker S.alice (S.landsInPlay swamp 2)
        (_, withThalia) = S.addCreature thalia S.alice withPiker
        (gs, dismemberId) = S.handOne dismember withThalia
    Spec.assertBool s (S.castable S.alice dismemberId gs) "castable, by two life routes"
    let (asked, resolved) = castAndResolve (announces PhyrexianPayment.PaysMana) gs dismemberId
    Spec.assertEqWith s "4 life paid both symbols" (S.lifeOf S.alice resolved) (Just 16)
    Spec.assertEqWith s "both Swamps paid Thalia's {2}" (S.tappedCount S.alice resolved) 2
    Spec.assertEqWith s "and neither symbol was a choice" (phyrexianAnnouncements asked) []
    Spec.assertEqWith s "Dismember resolved rather than evaporating" (length (GameState.stack resolved)) 0

-- Dismember ({1}{B/P}{B/P}) -- the first card in the pool with more than one
-- Phyrexian mana symbol, and so the first to exercise CR 601.2b's "for each of
-- those symbols" at all. Everything Mutagenic Growth proves about ONE symbol it
-- proves once; what only two symbols can show is the LOOP: one prompt per symbol
-- in printed order, each asked knowing the answers before it, and an earlier
-- answer narrowing a later one's offer -- CR 601.2b's last sentence, "previously
-- made choices ... may restrict the player's options when making these choices."
dismemberSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
dismemberSpec s registry = Spec.describe s "Dismember" $ do
  -- Two Swamps: the first symbol is a real choice, and answering MANA leaves
  -- {1}{B} to pay off two Swamps -- which the second symbol's mana route
  -- would push to three. So the second symbol is forced to life and is not
  -- asked. One prompt, not two, and that count is the assertion.
  Spec.it s "CR 601.2b announcing mana for the first {B/P} forces the second to life" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    dismember <- S.printingOf s registry "Dismember"
    let (gs, dismemberId) = dismemberBoard swamp piker dismember 2
        (asked, resolved) = castAndResolve (announces PhyrexianPayment.PaysMana) gs dismemberId
    Spec.assertEqWith s "one symbol was a choice, the other was forced" (phyrexianAnnouncements asked) [PhyrexianPayment.PaysMana]
    Spec.assertEqWith s "both Swamps paid {1}{B}" (S.tappedCount S.alice resolved) 2
    Spec.assertEqWith s "and 2 life paid the second symbol" (S.lifeOf S.alice resolved) (Just 18)

  -- The same board, the other answer: paying the first symbol with life keeps
  -- both Swamps available, so the SECOND symbol is a real choice too and is
  -- asked. Two prompts, and the answers are 2 life each.
  Spec.it s "CR 601.2b announcing life for the first {B/P} leaves the second a real choice" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    dismember <- S.printingOf s registry "Dismember"
    let (gs, dismemberId) = dismemberBoard swamp piker dismember 2
        (asked, resolved) = castAndResolve (announces PhyrexianPayment.PaysLife) gs dismemberId
    Spec.assertEqWith
      s
      "both symbols were asked, in printed order"
      (phyrexianAnnouncements asked)
      [PhyrexianPayment.PaysLife, PhyrexianPayment.PaysLife]
    Spec.assertEqWith s "one Swamp paid the {1}" (S.tappedCount S.alice resolved) 1
    Spec.assertEqWith s "and 4 life paid both symbols" (S.lifeOf S.alice resolved) (Just 16)

  -- One Swamp: neither symbol can be paid with mana, since the Swamp is
  -- needed for the {1}. Nothing is asked at all, and CR 107.4f's example
  -- arithmetic for two symbols -- 4 life -- is what comes out.
  Spec.it s "CR 107.4f off one Swamp both symbols are forced to life, for 4" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    dismember <- S.printingOf s registry "Dismember"
    let (gs, dismemberId) = dismemberBoard swamp piker dismember 1
        (asked, resolved) = castAndResolve (announces PhyrexianPayment.PaysMana) gs dismemberId
    Spec.assertEqWith s "no choice existed either time" (phyrexianAnnouncements asked) []
    Spec.assertEqWith s "the Swamp paid the {1}" (S.tappedCount S.alice resolved) 1
    Spec.assertEqWith s "and 4 life paid both symbols" (S.lifeOf S.alice resolved) (Just 16)

  -- The gameplay-level proof (design.md section 4): the whole card, cast and
  -- resolved. A Goblin Piker is 2/1, so -5/-5 is -3/-4 and CR 704.5f's
  -- state-based action puts it into its owner's graveyard.
  Spec.it s "CR 107.4f whole card: Dismember kills a Goblin Piker for 4 life" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    dismember <- S.printingOf s registry "Dismember"
    let (gs, dismemberId) = dismemberBoard swamp piker dismember 1
        (_, resolved) = castAndResolve (announces PhyrexianPayment.PaysMana) gs dismemberId
        pikerId = pikerOn resolved
    Spec.assertEqWith s "-5/-5 applied" (Projection.powerOf pikerId resolved) (Just (-3))
    Spec.assertEqWith s "toughness too" (Projection.toughnessOf pikerId resolved) (Just (-4))
    let settled = S.settleSba resolved
    Spec.assertBool s (not (Set.member pikerId (GameState.battlefield settled))) "CR 704.5f buried it"
    Spec.assertEqWith s "and 4 life paid for it" (S.lifeOf S.alice settled) (Just 16)

-- Alice with `n` Swamps, a Goblin Piker for Dismember to target, and Dismember in
-- hand.
dismemberBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Int ->
  (GameState.GameState, ObjectId.ObjectId)
dismemberBoard swamp piker dismember n =
  let (_, withPiker) = S.addCreature piker S.alice (S.landsInPlay swamp n)
   in S.handOne dismember withPiker

-- The one Goblin Piker on the battlefield. The Piker is added before the spell is
-- cast and never moves, so this is a lookup and not a choice; a board with no
-- Piker would fail the assertion that reads it.
pikerOn :: GameState.GameState -> ObjectId.ObjectId
pikerOn gs =
  let isPiker oid = fmap Face.name (Game.faceOf oid gs) == Just (CardName.MkCardName $ Text.pack "Goblin Piker")
   in case filter isPiker (Set.toAscList (GameState.battlefield gs)) of
        oid : _ -> oid
        [] -> ObjectId.MkObjectId 0

-- Moltensteel Dragon ({4}{R/P}{R/P}, with "{R/P}: This creature gets +1/+0 until
-- end of turn") -- the first card in the pool with a Phyrexian mana symbol
-- OUTSIDE a spell's mana cost.
--
-- CR 602.2b: "The remainder of the process for activating an ability is identical
-- to the process for casting a spell listed in rules 601.2b-i", and CR 118.13a
-- names "the activation cost of an activated ability" in its own words -- so the
-- announcement happens at CR 601.2b's position for an activation too. Until this
-- card there was nothing in the pool for Pawl.Engine.Activate's Cost.announce call to do.
moltensteelSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
moltensteelSpec s registry = Spec.describe s "Moltensteel" $ do
  -- The activation cost's symbol IS a choice off a Mountain, and answering
  -- mana taps it. CR 118.13b/c are not what governs this -- the cost is an
  -- activation cost, so CR 118.13a is, and the choice belongs at proposal
  -- rather than at payment (#373 is the other two clauses).
  Spec.it s "CR 118.13a/602.2b an activation cost's {R/P} is asked, and mana taps the Mountain" $ do
    mountain <- S.printingOf s registry "Mountain"
    dragon <- S.printingOf s registry "Moltensteel Dragon"
    let (dragonId, gs) = dragonBoard mountain dragon 1
        (asked, activated) = activateAndResolve (announces PhyrexianPayment.PaysMana) gs dragonId (theAbility dragon)
    Spec.assertEqWith s "one symbol, one prompt" (phyrexianAnnouncements asked) [PhyrexianPayment.PaysMana]
    Spec.assertEqWith s "the Mountain paid it" (S.tappedCount S.alice activated) 1
    Spec.assertEqWith s "no life paid" (S.lifeOf S.alice activated) (Just 20)
    Spec.assertEqWith s "+1/+0" (Projection.powerOf dragonId activated) (Just 5)
    Spec.assertEqWith s "toughness unchanged" (Projection.toughnessOf dragonId activated) (Just 4)

  -- The control, one answer different on the same board: 2 life instead, and
  -- the Mountain is still up for something else.
  Spec.it s "CR 118.13a the same activation's life route spares the Mountain" $ do
    mountain <- S.printingOf s registry "Mountain"
    dragon <- S.printingOf s registry "Moltensteel Dragon"
    let (dragonId, gs) = dragonBoard mountain dragon 1
        (asked, activated) = activateAndResolve (announces PhyrexianPayment.PaysLife) gs dragonId (theAbility dragon)
    Spec.assertEqWith s "asked here too" (phyrexianAnnouncements asked) [PhyrexianPayment.PaysLife]
    Spec.assertEqWith s "the Mountain is untouched" (S.tappedCount S.alice activated) 0
    Spec.assertEqWith s "2 life paid it" (S.lifeOf S.alice activated) (Just 18)
    Spec.assertEqWith s "+1/+0 all the same" (Projection.powerOf dragonId activated) (Just 5)

  -- No red source: CR 107.4f's mana route cannot be completed, so there is
  -- nothing to ask and the life route is taken. The activation still happens,
  -- which is the half a payment-time reading would get right by accident.
  Spec.it s "CR 118.13a with no red source the activation's life route is forced" $ do
    mountain <- S.printingOf s registry "Mountain"
    dragon <- S.printingOf s registry "Moltensteel Dragon"
    let (dragonId, gs) = dragonBoard mountain dragon 0
        (asked, activated) = activateAndResolve (announces PhyrexianPayment.PaysMana) gs dragonId (theAbility dragon)
    Spec.assertEqWith s "no choice existed, so none was asked" (phyrexianAnnouncements asked) []
    Spec.assertEqWith s "2 life paid it" (S.lifeOf S.alice activated) (Just 18)
    Spec.assertEqWith s "+1/+0" (Projection.powerOf dragonId activated) (Just 5)

  -- The gameplay-level proof, and the second board where two symbols in ONE
  -- cost are both real choices: six Mountains pay {4}{R}{R} outright, so both
  -- announcements are asked and neither costs life.
  Spec.it s "CR 107.4f whole card: Moltensteel Dragon casts off six Mountains for no life" $ do
    mountain <- S.printingOf s registry "Mountain"
    dragon <- S.printingOf s registry "Moltensteel Dragon"
    let (gs, dragonId) = S.handOne dragon (S.landsInPlay mountain 6)
        (asked, resolved) = castAndResolve (announces PhyrexianPayment.PaysMana) gs dragonId
    Spec.assertEqWith
      s
      "both symbols were asked"
      (phyrexianAnnouncements asked)
      [PhyrexianPayment.PaysMana, PhyrexianPayment.PaysMana]
    Spec.assertEqWith s "all six Mountains paid {4}{R}{R}" (S.tappedCount S.alice resolved) 6
    Spec.assertEqWith s "and no life did" (S.lifeOf S.alice resolved) (Just 20)
    Spec.assertEqWith s "a 4/4 arrived" (Projection.powerOf (dragonOn resolved) resolved) (Just 4)
    Spec.assertEqWith s "CR 202.2d: red, from the Phyrexian symbols" (Projection.colorsOf (dragonOn resolved) resolved) (Set.singleton Color.Red)

  -- Four Mountains cannot pay {4}{R}{R}, so both symbols are forced to life
  -- and CR 107.4f's arithmetic for two symbols is 4 -- the same card, one
  -- fewer land, and the whole announcement disappears.
  Spec.it s "CR 107.4f whole card: off four Mountains both symbols are forced, for 4 life" $ do
    mountain <- S.printingOf s registry "Mountain"
    dragon <- S.printingOf s registry "Moltensteel Dragon"
    let (gs, dragonId) = S.handOne dragon (S.landsInPlay mountain 4)
        (asked, resolved) = castAndResolve (announces PhyrexianPayment.PaysMana) gs dragonId
    Spec.assertEqWith s "no choice existed either time" (phyrexianAnnouncements asked) []
    Spec.assertEqWith s "all four Mountains paid the {4}" (S.tappedCount S.alice resolved) 4
    Spec.assertEqWith s "and 4 life paid both symbols" (S.lifeOf S.alice resolved) (Just 16)
    Spec.assertEqWith s "a 4/4 arrived all the same" (Projection.powerOf (dragonOn resolved) resolved) (Just 4)

-- Alice with a settled Moltensteel Dragon on the battlefield, `n` Mountains, and
-- priority -- which Activate.activateAbility needs and Setup.emptyGame leaves
-- unset.
dragonBoard :: Printing.Printing -> Printing.Printing -> Int -> (ObjectId.ObjectId, GameState.GameState)
dragonBoard mountain dragon n =
  let (dragonId, gs) = S.addCreature dragon S.alice (S.landsInPlay mountain n)
   in (dragonId, gs {GameState.priority = Just S.alice})

-- The one Moltensteel Dragon on the battlefield -- pikerOn's shape, for the card
-- a cast has just put there under a fresh CR 400.7 id.
dragonOn :: GameState.GameState -> ObjectId.ObjectId
dragonOn gs =
  let isDragon oid = fmap Face.name (Game.faceOf oid gs) == Just (CardName.MkCardName $ Text.pack "Moltensteel Dragon")
   in case filter isDragon (Set.toAscList (GameState.battlefield gs)) of
        oid : _ -> oid
        [] -> ObjectId.MkObjectId 0

-- Activate `ability` on `oid` under `answer` and resolve what it put on the
-- stack, returning the transcript alongside the final state -- castAndResolve for
-- an activation.
activateAndResolve ::
  (forall r. Prompt.Prompt r -> r) ->
  GameState.GameState ->
  ObjectId.ObjectId ->
  ActivatedAbility.ActivatedAbility Card.Type.Card ->
  ([Response.Response], GameState.GameState)
activateAndResolve answer gs oid ability =
  let ((_, activated), asked) = Replay.record answer gs (Activate.activateAbility S.alice oid ability)
   in (asked, snd (S.runPureWith answer activated Stack.resolveTop))
