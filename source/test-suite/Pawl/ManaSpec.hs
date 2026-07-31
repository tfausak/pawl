{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers Pawl.Mana: mana payment and castability. CR 118.13a's announcement lives
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
import qualified Pawl.Activate as Activate
import qualified Pawl.Cast as Cast
import qualified Pawl.Cost as Cost
import qualified Pawl.Engine as Engine
import qualified Pawl.Event as Event
import qualified Pawl.Game as Game
import qualified Pawl.Mana as Mana
import qualified Pawl.Projection as Projection
import qualified Pawl.Registry as Registry
import qualified Pawl.Replay as Replay
import qualified Pawl.Setup as Setup
import qualified Pawl.Stack as Stack
import qualified Pawl.Support as S
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.ActivationTiming as ActivationTiming
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.Effect as Effect
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
import qualified Pawl.Types.Optionality as Optionality
import qualified Pawl.Types.PhyrexianPayment as PhyrexianPayment
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Pool as Pool
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.Registry as Registry.Type
import qualified Pawl.Types.Response as Response
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TargetSpec as TargetSpec
import qualified Pawl.Types.Zone as Zone
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

-- Cast `creature` off `nLands` copies of `land`, then resolve it.
resolvedCreature :: Printing.Printing -> Printing.Printing -> Int -> GameState.GameState
resolvedCreature land creature nLands =
  let (base, oid) = S.handOne creature (S.landsInPlay land nLands)
      afterCast = snd (Engine.runGamePure S.identityAnswer base (Cast.castSpell S.alice oid))
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

castabilityTests :: Registry.Type.Registry -> Tasty.TestTree
castabilityTests registry =
  Tasty.testGroup
    "Castability"
    [ HU.testCase "War Mammoth is cast off four Forests and resolves onto the battlefield" $ do
        forest <- Registry.printing registry "Forest"
        warMammoth <- Registry.printing registry "War Mammoth"
        let gs = resolvedCreature forest warMammoth 4
        HU.assertEqual "stack empty" 0 (length (GameState.stack gs))
        HU.assertEqual "one creature in play" 1 (S.creaturesInPlay S.alice gs)
        HU.assertEqual "lands tapped" 4 (S.tappedCount S.alice gs),
      HU.testCase "Typhoid Rats is cast off one Swamp and resolves onto the battlefield" $ do
        swamp <- Registry.printing registry "Swamp"
        typhoidRats <- Registry.printing registry "Typhoid Rats"
        let gs = resolvedCreature swamp typhoidRats 1
        HU.assertEqual "stack empty" 0 (length (GameState.stack gs))
        HU.assertEqual "one creature in play" 1 (S.creaturesInPlay S.alice gs)
        HU.assertEqual "lands tapped" 1 (S.tappedCount S.alice gs)
    ]

pikerCost :: ManaCost.ManaCost
pikerCost = ManaCost.MkManaCost [ManaSymbol.Generic 1, ManaSymbol.OfType (ManaType.Colored Color.Red)]

poolSize :: PlayerId.PlayerId -> GameState.GameState -> Int
poolSize pid gs = case Mana.poolOf pid gs of
  Mana.Type.MkMana units -> length units

manaTests :: Registry.Type.Registry -> Tasty.TestTree
manaTests registry =
  Tasty.testGroup
    "Mana"
    [ HU.testCase "substituteX replaces each Variable with Generic X, keeping order" $
        let red = ManaSymbol.OfType (ManaType.Colored Color.Red)
            cost = ManaCost.MkManaCost [ManaSymbol.Variable, red]
         in HU.assertEqual
              "X=3 -> {3}{R}"
              (ManaCost.MkManaCost [ManaSymbol.Generic 3, red])
              (Mana.substituteX 3 cost),
      HU.testCase "substituteX 0 leaves a Variable-free cost payable" $
        let red = ManaSymbol.OfType (ManaType.Colored Color.Red)
         in HU.assertEqual
              "floor is {0}{R}"
              (ManaCost.MkManaCost [ManaSymbol.Generic 0, red])
              (Mana.substituteX 0 (ManaCost.MkManaCost [ManaSymbol.Variable, red])),
      HU.testCase "CR 305.6 a Mountain's red mana ability comes from its subtype" $
        HU.assertEqual
          "red"
          (Just (ManaType.Colored Color.Red))
          (Mana.subtypeMana Subtype.Mountain),
      HU.testCase "a Goblin grants no mana ability" $
        HU.assertEqual "none" Nothing (Mana.subtypeMana Subtype.Goblin),
      HU.testCase "CR 305.6 Island taps blue, Plains taps white" $ do
        HU.assertEqual "island" (Just (ManaType.Colored Color.Blue)) (Mana.subtypeMana Subtype.Island)
        HU.assertEqual "plains" (Just (ManaType.Colored Color.White)) (Mana.subtypeMana Subtype.Plains),
      HU.testCase "CR 205.3h: Aura is an enchantment type, so it has no CR 305.6 intrinsic mana" $
        HU.assertEqual "no mana" Nothing (Mana.subtypeMana Subtype.Aura),
      -- CR 305.6 grants its intrinsic ability to "an object with the land card
      -- type and A BASIC LAND TYPE", and CR 205.3i lists which of the land types
      -- those are: "Of that list, Forest, Island, Mountain, Plains, and Swamp are
      -- the basic land types." So a Desert is a land type with no mana of its own
      -- -- the one constructor where this answer and Pawl.Subtype.isLandType's
      -- (asserted in Pawl.ProjectionSpec) come apart, and the reason they are two
      -- functions.
      HU.testCase "CR 305.6 Desert is a land type but not a BASIC one, so it grants no mana" $
        HU.assertEqual "no mana" Nothing (Mana.subtypeMana Subtype.Desert),
      HU.testCase "an empty pool starts empty" $ do
        mountain <- Registry.printing registry "Mountain"
        HU.assertEqual "empty" 0 (poolSize S.alice (S.landsInPlay mountain 2)),
      HU.testCase "tapping a Mountain taps it and adds one red unit" $ do
        mountain <- Registry.printing registry "Mountain"
        let gs = S.landsInPlay mountain 1
        case Game.zoneMembers Zone.Battlefield S.alice gs of
          [] -> HU.assertFailure "fixture should have one Mountain"
          oid : _ -> do
            let after = S.runPure S.identityAnswer gs (Mana.tapForMana oid)
            HU.assertEqual "tapped" 1 (S.tappedCount S.alice after)
            HU.assertEqual
              "pool"
              (Mana.Type.MkMana [ManaUnit.MkManaUnit {ManaUnit.manaType = ManaType.Colored Color.Red}])
              (Mana.poolOf S.alice after),
      HU.testCase "two Mountains can pay {1}{R}" $ do
        mountain <- Registry.printing registry "Mountain"
        HU.assertBool "affordable" (Mana.canPay S.alice pikerCost (S.landsInPlay mountain 2)),
      HU.testCase "one Mountain cannot pay {1}{R}" $ do
        mountain <- Registry.printing registry "Mountain"
        HU.assertBool "unaffordable" (not (Mana.canPay S.alice pikerCost (S.landsInPlay mountain 1))),
      HU.testCase "no Mountains cannot pay {1}{R}" $ do
        mountain <- Registry.printing registry "Mountain"
        HU.assertBool "unaffordable" (not (Mana.canPay S.alice pikerCost (S.landsInPlay mountain 0))),
      -- Three identical Mountains: every candidate is a copy of the same card, so
      -- the choice is genuinely indistinguishable and payCost must NOT ask (#12).
      -- S.identityAnswer would answer anyway; what this pins is the tap count.
      HU.testCase "paying {1}{R} taps exactly two of three Mountains and leaves no float" $ do
        mountain <- Registry.printing registry "Mountain"
        let (paid, after) = S.runPureWith S.identityAnswer (S.landsInPlay mountain 3) (Mana.payCost S.alice pikerCost)
        HU.assertBool "three Mountains should pay {1}{R}" paid
        HU.assertEqual "tapped" 2 (S.tappedCount S.alice after)
        HU.assertEqual "no float" 0 (poolSize S.alice after),
      HU.testCase "CR 500.5 mana pools empty" $ do
        mountain <- Registry.printing registry "Mountain"
        let gs = S.landsInPlay mountain 1
        case Game.zoneMembers Zone.Battlefield S.alice gs of
          [] -> HU.assertFailure "fixture should have one Mountain"
          oid : _ ->
            HU.assertEqual "emptied" 0 (poolSize S.alice (Mana.emptyManaPools (S.runPure S.identityAnswer gs (Mana.tapForMana oid)))),
      HU.testCase "CR 305.6/305.7 an Urborg'd Mountain taps for black too" $ do
        mountain <- Registry.printing registry "Mountain"
        urborg <- Registry.printing registry "Urborg, Tomb of Yawgmoth"
        let base = Setup.emptyGame S.bothPlayers
            (mountainId, g1) = S.addCreature mountain S.alice base
            (_, gs) = S.addCreature urborg S.alice g1
        -- Urborg adds Swamp to all lands, so the Mountain taps for black too.
        HU.assertBool "black available" (ManaType.Colored Color.Black `elem` Mana.manaTypesOf mountainId gs)
        HU.assertBool "red still available" (ManaType.Colored Color.Red `elem` Mana.manaTypesOf mountainId gs),
      HU.testCase "CR 305.6/305.7 a Blood Moon'd Urborg taps for red only" $ do
        urborg <- Registry.printing registry "Urborg, Tomb of Yawgmoth"
        bloodMoon <- Registry.printing registry "Blood Moon"
        let base = Setup.emptyGame S.bothPlayers
            (urborgId, g1) = S.addCreature urborg S.alice base
            (_, gs) = S.addCreature bloodMoon S.alice g1
        HU.assertBool "red available" (ManaType.Colored Color.Red `elem` Mana.manaTypesOf urborgId gs)
        HU.assertBool "black not available (stripped)" (ManaType.Colored Color.Black `notElem` Mana.manaTypesOf urborgId gs),
      -- CR 305.7 takes away the land's PRINTED mana ability and hands back the
      -- one its new basic land type carries: "It loses all abilities generated
      -- from its rules text ... and it gains the appropriate mana ability for
      -- each new basic land type." Reliquary Tower's "{T}: Add {C}" is an
      -- ACTIVATED ability, and it is the pool's sharpest witness that the strip
      -- reaches all of a land's rules text: PlayerEffectSpec already pins the
      -- other half of this same card's text going away under the same Blood Moon.
      HU.testCase "CR 305.7 a Blood Moon'd Reliquary Tower taps for red, not colorless" $ do
        reliquaryTower <- Registry.printing registry "Reliquary Tower"
        bloodMoon <- Registry.printing registry "Blood Moon"
        let base = Setup.emptyGame S.bothPlayers
            (towerId, g1) = S.addCreature reliquaryTower S.alice base
            (_, gs) = S.addCreature bloodMoon S.alice g1
        HU.assertBool "red available (CR 305.6, from the new Mountain type)" (ManaType.Colored Color.Red `elem` Mana.manaTypesOf towerId gs)
        HU.assertBool "colorless gone (the printed {T}: Add {C} was stripped)" (ManaType.Colorless `notElem` Mana.manaTypesOf towerId gs),
      -- The same strip, on a land whose rules text is not a mana ability at all.
      HU.testCase "CR 305.7 a Blood Moon'd Evolving Wilds has no activated ability left" $ do
        evolvingWilds <- Registry.printing registry "Evolving Wilds"
        bloodMoon <- Registry.printing registry "Blood Moon"
        let base = Setup.emptyGame S.bothPlayers
            (wildsId, g1) = S.addCreature evolvingWilds S.alice base
            (_, gs) = S.addCreature bloodMoon S.alice g1
        HU.assertEqual "the fetch ability is gone" [] (Projection.abilitiesOf wildsId gs)
        HU.assertBool "and it taps for red instead" (ManaType.Colored Color.Red `elem` Mana.manaTypesOf wildsId gs),
      -- CR 305.6: the intrinsic mana ability comes with the land TYPE, whether
      -- the type was printed or added at layer 4 -- so an Ashaya-animated
      -- creature taps for green, and Blood Moon (CR 305.7) rewrites that to red
      -- by setting the same subtype it reads.
      HU.testCase "CR 305.6 a creature Ashaya made a Forest land taps for green" $ do
        piker <- Registry.printing registry "Goblin Piker"
        ashaya <- Registry.printing registry "Ashaya, Soul of the Wild"
        let base = Setup.emptyGame S.bothPlayers
            (pikerId, g1) = S.addCreature piker S.alice base
            (_, gs) = S.addCreature ashaya S.alice g1
        HU.assertBool "green available" (ManaType.Colored Color.Green `elem` Mana.manaTypesOf pikerId gs)
        HU.assertBool "and it is a mana source" (pikerId `elem` Mana.manaSources S.alice gs),
      HU.testCase "CR 305.7 Blood Moon turns that same creature-land red" $ do
        piker <- Registry.printing registry "Goblin Piker"
        ashaya <- Registry.printing registry "Ashaya, Soul of the Wild"
        bloodMoon <- Registry.printing registry "Blood Moon"
        let base = Setup.emptyGame S.bothPlayers
            (pikerId, g1) = S.addCreature piker S.alice base
            (_, g2) = S.addCreature bloodMoon S.alice g1
            (_, gs) = S.addCreature ashaya S.alice g2
        HU.assertBool "red available" (ManaType.Colored Color.Red `elem` Mana.manaTypesOf pikerId gs)
        HU.assertBool "green gone" (ManaType.Colored Color.Green `notElem` Mana.manaTypesOf pikerId gs),
      -- Ashaya's reminder text: "(They're still affected by summoning sickness.)"
      -- CR 302.6 gates a CREATURE's {T} ability, and CR 205.1b's "in addition to
      -- their other types" keeps the creature type -- so gaining CR 305.6's mana
      -- ability does not hand a fresh creature a land's exemption. Nothing had to
      -- be built for this; it falls out of Mana.manaSources reading the PROJECTED
      -- card types.
      HU.testCase "CR 302.6 a summoning-sick creature Ashaya animated still cannot tap for mana" $ do
        piker <- Registry.printing registry "Goblin Piker"
        ashaya <- Registry.printing registry "Ashaya, Soul of the Wild"
        let base = Setup.emptyGame S.bothPlayers
            (pikerId, g1) = S.addCreature piker S.alice base
            (_, g2) = S.addCreature ashaya S.alice g1
            sick = g2 {GameState.objects = Map.adjust (\o -> o {Object.sickness = Sickness.Sick}) pikerId (GameState.objects g2)}
        HU.assertBool "it is a land now" (Set.member CardType.Land (Projection.cardTypesOf pikerId sick))
        HU.assertBool "and still a creature" (Projection.isCreatureOf pikerId sick)
        HU.assertBool "so the sick creature is no mana source" (pikerId `notElem` Mana.manaSources S.alice sick),
      HU.testCase "CR 605.1a a {T}: Add {G} ability is a mana ability" $
        let ab =
              ActivatedAbility.MkActivatedAbility
                { ActivatedAbility.cost = Cost.Type.MkCost {Cost.Type.mana = Just (ManaCost.MkManaCost []), Cost.Type.components = []},
                  ActivatedAbility.modal = singleModeAbility [Effect.AddMana (ManaProduction.OfType (ManaType.Colored Color.Green))] Map.empty,
                  ActivatedAbility.timing = ActivationTiming.AnyTime
                }
         in HU.assertBool "mana ability" (Mana.isManaAbility ab),
      HU.testCase "CR 605.1a an ability that targets is NOT a mana ability" $
        let ab =
              ActivatedAbility.MkActivatedAbility
                { ActivatedAbility.cost = Cost.Type.MkCost {Cost.Type.mana = Just (ManaCost.MkManaCost []), Cost.Type.components = []},
                  ActivatedAbility.modal =
                    singleModeAbility
                      [Effect.AddMana (ManaProduction.OfType (ManaType.Colored Color.Green))]
                      (Map.singleton (SlotName.MkSlotName (Text.pack "x")) (TargetSpec.MkTargetSpec Pool.AnyTarget Nothing)),
                  ActivatedAbility.timing = ActivationTiming.AnyTime
                }
         in HU.assertBool "targets -> not mana" (not (Mana.isManaAbility ab)),
      HU.testCase "CR 605.1a a damage ability is NOT a mana ability" $
        let ab =
              ActivatedAbility.MkActivatedAbility
                { ActivatedAbility.cost = Cost.Type.MkCost {Cost.Type.mana = Just (ManaCost.MkManaCost []), Cost.Type.components = []},
                  ActivatedAbility.modal =
                    singleModeAbility
                      [Effect.DealDamage (SlotName.MkSlotName (Text.pack "x")) (Quantity.Literal 1)]
                      (Map.singleton (SlotName.MkSlotName (Text.pack "x")) (TargetSpec.MkTargetSpec Pool.AnyTarget Nothing)),
                  ActivatedAbility.timing = ActivationTiming.AnyTime
                }
         in HU.assertBool "no mana produced -> not mana" (not (Mana.isManaAbility ab)),
      HU.testCase "CR 605 a settled Llanowar Elves is a green mana source" $ do
        llanowarElves <- Registry.printing registry "Llanowar Elves"
        let (elfId, gs) = S.addCreature llanowarElves S.alice (Setup.emptyGame S.bothPlayers)
        HU.assertBool "taps green" (elem (ManaType.Colored Color.Green) (Mana.manaTypesOf elfId gs))
        HU.assertBool "is a mana source" (elem elfId (Mana.manaSources S.alice gs)),
      HU.testCase "CR 302.6 a summoning-sick Llanowar Elves is NOT a mana source" $ do
        llanowarElves <- Registry.printing registry "Llanowar Elves"
        let (elfId, g0) = S.addCreature llanowarElves S.alice (Setup.emptyGame S.bothPlayers)
            sick = g0 {GameState.objects = Map.adjust (\o -> o {Object.sickness = Sickness.Sick}) elfId (GameState.objects g0)}
        HU.assertBool "sick elf excluded" (notElem elfId (Mana.manaSources S.alice sick)),
      -- CR 302.6's other half, and the same trap #198 sprang on attacking: bob's
      -- Elves settled under BOB, so the settle it carries says nothing about
      -- alice. Stealing it does not hand her a mana source this turn.
      HU.testCase "CR 302.6 a stolen Llanowar Elves is not a mana source for the thief" $ do
        llanowarElves <- Registry.printing registry "Llanowar Elves"
        controlMagic <- Registry.printing registry "Control Magic"
        let (elfId, g0) = S.addCreature llanowarElves S.bob (Setup.emptyGame S.bothPlayers)
            settled = S.runPure S.identityAnswer g0 (Engine.settleAll S.bob)
            (aura, withAura) = S.addCreature controlMagic S.alice settled
            stolen = S.attach aura elfId withAura
        HU.assertBool "bob could tap it" (elem elfId (Mana.manaSources S.bob settled))
        HU.assertBool "alice controls it now" (elem elfId (Projection.controls S.alice stolen))
        HU.assertBool "but it is sick for her, so it is not her mana source" (notElem elfId (Mana.manaSources S.alice stolen)),
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
      HU.testCase "CR 702.10c a hasted stolen Llanowar Elves IS a mana source for the thief" $ do
        mountain <- Registry.printing registry "Mountain"
        llanowarElves <- Registry.printing registry "Llanowar Elves"
        actOfTreason <- Registry.printing registry "Act of Treason"
        let base0 = S.landsInPlay mountain 3
            (elfId, base1) = S.addCreature llanowarElves S.bob base0
            base = S.runPure S.identityAnswer base1 (Engine.settleAll S.bob)
            (withSpell, spellId) = S.handOne actOfTreason base
            cast = snd (Engine.runGamePure S.identityAnswer withSpell (Cast.castSpell S.alice spellId))
            resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        HU.assertEqual "alice controls the Elves" (Just S.alice) (Projection.controllerOf elfId resolved)
        HU.assertBool "it has haste" (Projection.hasKeyword Keyword.Haste elfId resolved)
        HU.assertBool "so she may tap it for mana this turn" (elem elfId (Mana.manaSources S.alice resolved)),
      -- CR 601.2g / 602.1: WHICH sources to activate is the player's choice, and
      -- pawl's second invariant is that the engine never makes one. A Forest and a
      -- Llanowar Elves both pay {G}, but they are not interchangeable -- tapping
      -- the Elf spends a creature that could otherwise block -- so the choice must
      -- be asked, and the answer must be honoured (#12).
      HU.testCase "CR 601.2g paying {G} with a Forest AND a Llanowar Elves asks which to tap" $ do
        forest <- Registry.printing registry "Forest"
        llanowarElves <- Registry.printing registry "Llanowar Elves"
        let base0 = S.landsInPlay forest 1
            (elfId, base1) = S.addCreature llanowarElves S.alice base0
            gs = S.runPure S.identityAnswer base1 (Engine.settleAll S.alice)
            green = ManaCost.MkManaCost [ManaSymbol.OfType (ManaType.Colored Color.Green)]
            cost = Cost.Type.MkCost (Just green) []
            tappedElf g = fmap Object.tapped (Game.lookupObject elfId g)
        HU.assertEqual "asked to tap the Elf, it is tapped" (Just TapState.Tapped) (tappedElf (S.runPure (prefersSource elfId) gs (Cost.pay S.alice elfId cost)))
        HU.assertEqual "asked to spare the Elf, it is untapped" (Just TapState.Untapped) (tappedElf (S.runPure (avoidsSource elfId) gs (Cost.pay S.alice elfId cost))),
      -- The other half of the invariant: the elision is exactly "there is only one
      -- candidate, so there is nothing to decide", and nothing broader. Counting
      -- prompts is the direct assertion -- without it, an implementation that
      -- never asks would still pass the test above's first half.
      --
      -- Three Forests DO ask. They are one card, but sameness of card is not
      -- sameness of object (#217), so the engine does not presume to skip it.
      HU.testCase "CR 601.2g one candidate asks nothing; more than one always asks" $ do
        forest <- Registry.printing registry "Forest"
        let green = ManaCost.MkManaCost [ManaSymbol.OfType (ManaType.Colored Color.Green)]
            countingAnswer :: Prompt.Prompt r -> State.State Int r
            countingAnswer p = case p of
              Prompt.ChooseManaSource _ _ candidates -> do
                State.modify' (+ 1)
                pure (NonEmpty.head candidates)
              _ -> pure (S.identityAnswer p)
            promptsFor g = State.execState (Engine.runGame countingAnswer g (Mana.payCost S.alice green)) 0
        HU.assertEqual "a lone Forest: nothing to ask" 0 (promptsFor (S.landsInPlay forest 1))
        HU.assertEqual "three Forests: one real decision" 1 (promptsFor (S.landsInPlay forest 3)),
      -- FILTERED, NOT TRUSTED: an interpreter naming a source that was not offered
      -- must not be honoured. Beyond hygiene, tapForMana is a no-op on an unknown
      -- id, so obeying the answer would leave the state unchanged and loop forever.
      HU.testCase "CR 601.2g an answer outside the offered set is rejected, not obeyed" $ do
        forest <- Registry.printing registry "Forest"
        let green = ManaCost.MkManaCost [ManaSymbol.OfType (ManaType.Colored Color.Green)]
            bogus = ObjectId.MkObjectId 9999
            liar p = case p of
              Prompt.ChooseManaSource {} -> bogus
              _ -> S.identityAnswer p
            gs = S.landsInPlay forest 3
            (paid, after) = S.runPureWith liar gs (Mana.payCost S.alice green)
        HU.assertBool "the cost is still paid" paid
        HU.assertEqual "from a real Forest, not the invented id" 1 (S.tappedCount S.alice after),
      HU.testCase "mana from a controlled permanent goes to its controller, not owner" $ do
        llanowarElves <- Registry.printing registry "Llanowar Elves"
        let (oid, base) = S.addCreature llanowarElves S.bob (Setup.emptyGame S.bothPlayers)
            gs0 = S.giveControl oid S.alice base
            after = S.runPure S.identityAnswer gs0 (Mana.tapForMana oid)
            manaUnitsOf pool = case pool of
              Mana.Type.MkMana units -> units
        HU.assertBool "alice received a mana unit" (not (null (manaUnitsOf (Mana.poolOf S.alice after))))
        HU.assertBool "bob received none" (null (manaUnitsOf (Mana.poolOf S.bob after)))
    ]

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
    let yield = Mana.Type.MkMana [ManaUnit.MkManaUnit {ManaUnit.manaType = ManaType.Colored wanted}]
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
      afterCast = S.runPure answer withSpell (Cast.castSpell S.alice oid)
   in S.runPure answer afterCast Stack.resolveTop

-- The mana Alice's pool holds after tapping `oid` with every prompt answered by
-- `answer` -- the observable that says WHAT a source produced: which type, where
-- it offers several, and how much, where one activation adds more than one.
tappedFor :: (forall r. Prompt.Prompt r -> r) -> ObjectId.ObjectId -> GameState.GameState -> [ManaType.ManaType]
tappedFor answer oid gs = case Mana.poolOf S.alice (S.runPure answer gs (Mana.tapForMana oid)) of
  Mana.Type.MkMana units -> fmap ManaUnit.manaType units

anyColorTests :: Registry.Type.Registry -> Tasty.TestTree
anyColorTests registry =
  Tasty.testGroup
    "Mana of any color"
    [ -- CR 105.4: "If a player is asked to choose a color, they must choose one of
      -- the five colors. 'Multicolored' is not a color. Neither is 'colorless.'"
      -- So AnyColor offers exactly five options, and {C} is not among them.
      HU.testCase "CR 105.4 Birds of Paradise offers the five colors and not colorless" $ do
        birds <- Registry.printing registry "Birds of Paradise"
        let (birdsId, gs) = S.addCreature birds S.alice (Setup.emptyGame S.bothPlayers)
        HU.assertEqual
          "exactly the five colors"
          (fmap ManaType.Colored [Color.White, Color.Blue, Color.Black, Color.Red, Color.Green])
          (Mana.manaTypesOf birdsId gs)
        HU.assertBool "it is a mana source" (elem birdsId (Mana.manaSources S.alice gs)),
      -- The gameplay-level proof (design.md section 4): a real card, cast end to
      -- end off a source that produces no black mana until its controller says so.
      HU.testCase "CR 605.3b Typhoid Rats is cast off a lone Birds of Paradise that taps for black" $ do
        birds <- Registry.printing registry "Birds of Paradise"
        typhoidRats <- Registry.printing registry "Typhoid Rats"
        let resolved = castOffBoard (prefersColor Color.Black) [birds] typhoidRats
        HU.assertEqual "stack empty" 0 (length (GameState.stack resolved))
        HU.assertEqual "the Birds and the Rats" 2 (S.creaturesInPlay S.alice resolved)
        HU.assertEqual "the Birds is tapped" 1 (S.tappedCount S.alice resolved),
      -- The discriminating half: identical board, identical spell, one different
      -- answer. If the engine picked the colour itself this would pass too.
      HU.testCase "the color is the player's: a Birds tapped for green does not pay {B}" $ do
        birds <- Registry.printing registry "Birds of Paradise"
        typhoidRats <- Registry.printing registry "Typhoid Rats"
        let resolved = castOffBoard (prefersColor Color.Green) [birds] typhoidRats
        HU.assertEqual "the Rats never resolved" 1 (S.creaturesInPlay S.alice resolved)
        -- CR 601.2h: partial payments are not allowed, so the failed payment is
        -- rolled back whole and the Birds is left untapped.
        HU.assertEqual "payment rolled back" 0 (S.tappedCount S.alice resolved),
      -- CR 118.3 exactness. A greedy walk fails this one: it taps the Forest for
      -- {G}, then takes the Birds' FIRST colour (white) and reports {G}{B}
      -- unaffordable. Only a matching over what each source COULD produce gets it
      -- right.
      HU.testCase "CR 118.3 a Forest and a Birds of Paradise can pay {G}{B}" $ do
        forest <- Registry.printing registry "Forest"
        birds <- Registry.printing registry "Birds of Paradise"
        let (_, g1) = S.addCreature forest S.alice (Setup.emptyGame S.bothPlayers)
            (_, gs) = S.addCreature birds S.alice g1
            cost = ManaCost.MkManaCost [ManaSymbol.OfType (ManaType.Colored Color.Green), ManaSymbol.OfType (ManaType.Colored Color.Black)]
        HU.assertBool "affordable" (Mana.canPay S.alice cost gs),
      HU.testCase "CR 118.3 two Birds of Paradise can pay {B}{B}, one cannot" $ do
        birds <- Registry.printing registry "Birds of Paradise"
        let (_, one) = S.addCreature birds S.alice (Setup.emptyGame S.bothPlayers)
            (_, two) = S.addCreature birds S.alice one
            black = ManaSymbol.OfType (ManaType.Colored Color.Black)
            cost = ManaCost.MkManaCost [black, black]
        HU.assertBool "two suffice" (Mana.canPay S.alice cost two)
        HU.assertBool "one does not" (not (Mana.canPay S.alice cost one)),
      -- The OTHER way to get this wrong, which Hall's condition also rules out:
      -- checking each symbol independently ("is there a source that could make
      -- white?") passes both {W} symbols, because the same Birds answers each
      -- one. Only weighing the whole demand set against the supplies that could
      -- serve it catches that one source cannot make two mana. Two demands, three
      -- sources, plenty of mana, still unpayable.
      HU.testCase "CR 118.3 a Birds and two Forests cannot pay {W}{W}" $ do
        birds <- Registry.printing registry "Birds of Paradise"
        forest <- Registry.printing registry "Forest"
        let (_, g1) = S.addCreature birds S.alice (Setup.emptyGame S.bothPlayers)
            (_, g2) = S.addCreature forest S.alice g1
            (_, gs) = S.addCreature forest S.alice g2
            white = ManaSymbol.OfType (ManaType.Colored Color.White)
        HU.assertBool "only one white source" (not (Mana.canPay S.alice (ManaCost.MkManaCost [white, white]) gs))
        HU.assertBool "but one {W} plus {2} is fine" (Mana.canPay S.alice (ManaCost.MkManaCost [white, ManaSymbol.Generic 2]) gs),
      -- Not only "any color": a source with two BASIC LAND TYPES has been a real
      -- choice in this pool since Urborg landed, and tapForMana was silently
      -- taking the first. Both directions, so the answer is proven to decide.
      HU.testCase "CR 305.6/305.7 an Urborg'd Mountain's controller chooses red or black" $ do
        mountain <- Registry.printing registry "Mountain"
        urborg <- Registry.printing registry "Urborg, Tomb of Yawgmoth"
        let base = Setup.emptyGame S.bothPlayers
            (mountainId, g1) = S.addCreature mountain S.alice base
            (_, gs) = S.addCreature urborg S.alice g1
        HU.assertEqual "choosing black" [ManaType.Colored Color.Black] (tappedFor (prefersColor Color.Black) mountainId gs)
        HU.assertEqual "choosing red" [ManaType.Colored Color.Red] (tappedFor (prefersColor Color.Red) mountainId gs),
      -- The elision side of the invariant: where the rules leave nothing to ask,
      -- do not ask. A Forest offers one yield, so no ChooseManaYield is raised.
      HU.testCase "CR 605 a single-yield source is not asked what to produce" $ do
        forest <- Registry.printing registry "Forest"
        birds <- Registry.printing registry "Birds of Paradise"
        let countingAnswer :: Prompt.Prompt r -> State.State Int r
            countingAnswer p = case p of
              Prompt.ChooseManaYield {} -> do
                State.modify (+ 1)
                pure (S.identityAnswer p)
              _ -> pure (S.identityAnswer p)
            asks printing =
              let (oid, gs) = S.addCreature printing S.alice (Setup.emptyGame S.bothPlayers)
               in State.execState (Engine.runGame countingAnswer gs (Mana.tapForMana oid)) 0
        HU.assertEqual "a Forest: nothing to ask" 0 (asks forest)
        HU.assertEqual "a Birds of Paradise: one real decision" 1 (asks birds)
    ]

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
upwellingTests :: Registry.Type.Registry -> Tasty.TestTree
upwellingTests registry =
  Tasty.testGroup
    "Upwelling"
    [ -- The control. Same board, same float, no Upwelling: both pools go.
      HU.testCase "CR 500.5 without Upwelling both players lose their unspent mana" $ do
        forest <- Registry.printing registry "Forest"
        let (_, floated) = floatedPools [] forest
            ended = Mana.emptyManaPools floated
        HU.assertEqual "alice floated one" 1 (poolSize S.alice floated)
        HU.assertEqual "bob floated one" 1 (poolSize S.bob floated)
        HU.assertEqual "alice lost it" 0 (poolSize S.alice ended)
        HU.assertEqual "bob lost it" 0 (poolSize S.bob ended),
      HU.testCase "CR 613.11 Upwelling keeps its controller's unspent mana" $ do
        forest <- Registry.printing registry "Forest"
        upwelling <- Registry.printing registry "Upwelling"
        let (_, floated) = floatedPools [upwelling] forest
        HU.assertEqual "alice kept it" 1 (poolSize S.alice (Mana.emptyManaPools floated)),
      -- The discriminating half of the scope. Alice controls the Upwelling and
      -- bob keeps his mana anyway -- that is what PlayerScope.EachPlayer means,
      -- and a You-scoped implementation passes the test above and fails this one.
      HU.testCase "CR 613.11 Upwelling is symmetric: an opponent's unspent mana is kept too" $ do
        forest <- Registry.printing registry "Forest"
        upwelling <- Registry.printing registry "Upwelling"
        let (_, floated) = floatedPools [upwelling] forest
        HU.assertEqual "bob kept it, though alice controls the Upwelling" 1 (poolSize S.bob (Mana.emptyManaPools floated)),
      -- CR 604.2: a static ability's continuous effect is active only while its
      -- permanent "remains on the battlefield and has the ability". The effect is
      -- read LIVE at the moment the pools empty, so destroying the Upwelling in
      -- the same step restores the emptying with nothing to unwind.
      HU.testCase "CR 604.2 destroying Upwelling restores the emptying in the same step" $ do
        forest <- Registry.printing registry "Forest"
        upwelling <- Registry.printing registry "Upwelling"
        let (extras, floated) = floatedPools [upwelling] forest
        case extras of
          [] -> HU.assertFailure "fixture should have an Upwelling on the battlefield"
          oid : _ -> do
            let gone = S.runPure S.identityAnswer floated (Event.destroy Regenerability.Regenerable [oid])
            HU.assertEqual "kept while it stands" 1 (poolSize S.alice (Mana.emptyManaPools floated))
            HU.assertEqual "alice loses it once it is gone" 0 (poolSize S.alice (Mana.emptyManaPools gone))
            HU.assertEqual "and so does bob" 0 (poolSize S.bob (Mana.emptyManaPools gone)),
      -- The gameplay-level proof (design.md section 4), end to end through
      -- Engine.runStep. Alice taps a Birds of Paradise for BLUE toward a green
      -- spell. CR 105.4 makes that colour HER choice, and blue cannot pay {G}, so
      -- the Forest is tapped as well and the {U} is genuinely unspent when the
      -- precombat main phase ends -- CR 106.4's "unspent mana", reached by
      -- playing rather than by writing a pool into a fixture. Upwelling keeps it,
      -- and it pays for an Unsummon in the upkeep step that follows.
      HU.testCase "CR 500.5 whole card: mana Upwelling keeps across a step boundary pays for Unsummon" $ do
        forest <- Registry.printing registry "Forest"
        birds <- Registry.printing registry "Birds of Paradise"
        upwelling <- Registry.printing registry "Upwelling"
        llanowarElves <- Registry.printing registry "Llanowar Elves"
        unsummon <- Registry.printing registry "Unsummon"
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
            cast = S.runPure floatBlue board (Cast.castSpell S.alice elvesId)
            afterStep = S.runPure S.identityAnswer cast Engine.runStep
        HU.assertEqual "the Elves are cast off the Forest, floating the Birds' blue" 1 (poolSize S.alice cast)
        HU.assertEqual "both sources tapped" 2 (S.tappedCount S.alice afterStep)
        HU.assertEqual "the float survived the end of the precombat main phase" 1 (poolSize S.alice afterStep)
        HU.assertEqual "Unsummon is still in hand" [unsummonId] (Game.zoneMembers Zone.Hand S.alice afterStep)
        let spent = S.runPure S.identityAnswer afterStep (Cast.castSpell S.alice unsummonId)
        HU.assertEqual "the retained {U} paid for it" 0 (poolSize S.alice spent)
        HU.assertEqual "and nothing new was tapped" 2 (S.tappedCount S.alice spent)
        HU.assertEqual "Unsummon is on the stack" 1 (length (GameState.stack spent))
    ]

-- CR 605.3b: one activation of one mana ability, adding TWO mana. Sol Ring ({1}
-- Artifact, "{T}: Add {C}{C}") is the pool's first source whose yield is not one
-- unit, and it is what separates "the types this source could produce" from "the
-- mana this source produces when it is tapped" (#238).
solRingTests :: Registry.Type.Registry -> Tasty.TestTree
solRingTests registry =
  Tasty.testGroup
    "Sol Ring"
    [ -- The unit fact. A mode holding two AddMana effects is ONE activation
      -- yielding two mana, not a choice between two singles.
      HU.testCase "CR 605 tapping Sol Ring adds two colorless mana, not one" $ do
        solRing <- Registry.printing registry "Sol Ring"
        let (solRingId, gs) = S.addCreature solRing S.alice (Setup.emptyGame S.bothPlayers)
        HU.assertEqual
          "two units of {C}"
          [ManaType.Colorless, ManaType.Colorless]
          (tappedFor S.identityAnswer solRingId gs),
      -- The payability half, which reads the same yield through a different door:
      -- CR 118.3 counts an untapped source as the mana it could make, so one Sol
      -- Ring is two supplies and pays {2} by itself.
      HU.testCase "CR 118.3 a lone Sol Ring pays {2} by itself" $ do
        solRing <- Registry.printing registry "Sol Ring"
        let (_, gs) = S.addCreature solRing S.alice (Setup.emptyGame S.bothPlayers)
        HU.assertBool "{2} is affordable" (Mana.canPay S.alice (ManaCost.MkManaCost [ManaSymbol.Generic 2]) gs)
        HU.assertBool "{3} is not" (not (Mana.canPay S.alice (ManaCost.MkManaCost [ManaSymbol.Generic 3]) gs)),
      -- Both supplies a Sol Ring contributes are COLORLESS, so they swell the
      -- generic count and serve no typed demand. Discriminating against a supply
      -- model that merely counted a source twice without keeping its types: that
      -- one passes the first assertion and fails the second.
      HU.testCase "CR 118.3 a Sol Ring and a Mountain pay {2}{R}, but not {R}{R}" $ do
        solRing <- Registry.printing registry "Sol Ring"
        mountain <- Registry.printing registry "Mountain"
        let (_, g1) = S.addCreature solRing S.alice (Setup.emptyGame S.bothPlayers)
            (_, gs) = S.addCreature mountain S.alice g1
            red = ManaSymbol.OfType (ManaType.Colored Color.Red)
        HU.assertBool "{2}{R} is affordable" (Mana.canPay S.alice (ManaCost.MkManaCost [ManaSymbol.Generic 2, red]) gs)
        HU.assertBool "{R}{R} is not" (not (Mana.canPay S.alice (ManaCost.MkManaCost [red, red]) gs)),
      -- The gameplay-level proof (design.md section 4): a real spell cast end to
      -- end off a single permanent, which no one-mana-per-source engine can do.
      HU.testCase "CR 601.2g Sapphire Medallion is cast off a lone Sol Ring" $ do
        solRing <- Registry.printing registry "Sol Ring"
        sapphireMedallion <- Registry.printing registry "Sapphire Medallion"
        let resolved = castOffBoard S.identityAnswer [solRing] sapphireMedallion
        HU.assertEqual "stack empty" 0 (length (GameState.stack resolved))
        HU.assertEqual "the Medallion resolved" 1 (S.countOnBattlefieldByName (Text.pack "Sapphire Medallion") S.alice resolved)
        HU.assertEqual "the Sol Ring is tapped" 1 (S.tappedCount S.alice resolved)
        HU.assertEqual "and both mana were spent" 0 (poolSize S.alice resolved),
      -- The elision side of the invariant: Sol Ring offers exactly one yield, so
      -- there is nothing to ask -- and NOT because its two mana are the same
      -- type, which would be the engine choosing. "CR 605 a single-yield source
      -- is not asked what to produce" above is the counterpart that keeps a real
      -- choice asked.
      HU.testCase "CR 605 Sol Ring is not asked what to produce" $ do
        solRing <- Registry.printing registry "Sol Ring"
        let countingAnswer :: Prompt.Prompt r -> State.State Int r
            countingAnswer p = case p of
              Prompt.ChooseManaYield {} -> do
                State.modify' (+ 1)
                pure (S.identityAnswer p)
              _ -> pure (S.identityAnswer p)
            (solRingId, gs) = S.addCreature solRing S.alice (Setup.emptyGame S.bothPlayers)
        HU.assertEqual "nothing to ask" 0 (State.execState (Engine.runGame countingAnswer gs (Mana.tapForMana solRingId)) 0)
    ]

tests :: Registry.Type.Registry -> Tasty.TestTree
tests registry =
  Tasty.testGroup
    "Mana"
    [ manaTests registry,
      castabilityTests registry,
      anyColorTests registry,
      solRingTests registry,
      hybridTests registry,
      monocoloredHybridTests registry,
      phyrexianTests registry,
      totalCostTests registry,
      dismemberTests registry,
      moltensteelTests registry,
      upwellingTests registry
    ]

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
hybridTests :: Registry.Type.Registry -> Tasty.TestTree
hybridTests registry =
  Tasty.testGroup
    "Hybrid"
    [ HU.testCase "CR 107.4e one {R/G} is payable from either half, and from neither otherwise" $ do
        mountain <- Registry.printing registry "Mountain"
        forest <- Registry.printing registry "Forest"
        island <- Registry.printing registry "Island"
        let cost = ManaCost.MkManaCost [redGreen]
        HU.assertBool "a Mountain pays it" (Mana.canPay S.alice cost (S.landsInPlay mountain 1))
        HU.assertBool "a Forest pays it" (Mana.canPay S.alice cost (mixedLands mountain forest 0 1))
        HU.assertBool "an Island does not" (not (Mana.canPay S.alice cost (S.landsInPlay island 1)))
        HU.assertBool "and nothing does not" (not (Mana.canPay S.alice cost (S.landsInPlay mountain 0))),
      -- THE case a greedy left-to-right match gets wrong, and the reason
      -- Mana.spendDemands searches instead of folding. One Mountain and one
      -- Forest pay {R/G}{R} only if the hybrid takes the GREEN; handing it the
      -- red first strands the {R} with a Forest still untapped.
      HU.testCase "CR 107.4e {R/G}{R} off one Mountain and one Forest: the hybrid must take the GREEN" $ do
        mountain <- Registry.printing registry "Mountain"
        forest <- Registry.printing registry "Forest"
        let cost = ManaCost.MkManaCost [redGreen, redSymbol]
            gs = mixedLands mountain forest 1 1
        HU.assertBool "canPay says yes" (Mana.canPay S.alice cost gs)
        let (paid, after) = S.runPureWith S.identityAnswer gs (Mana.payCost S.alice cost)
        HU.assertBool "and it really is paid" paid
        HU.assertEqual "both lands tapped" 2 (S.tappedCount S.alice after)
        HU.assertEqual "nothing left floating" 0 (poolSize S.alice after),
      -- The twin: the same cost with no red anywhere is unpayable, so the case
      -- above is not "hybrids always succeed".
      HU.testCase "CR 107.4e {R/G}{R} off two Forests is unpayable -- the {R} has no source" $ do
        mountain <- Registry.printing registry "Mountain"
        forest <- Registry.printing registry "Forest"
        let cost = ManaCost.MkManaCost [redGreen, redSymbol]
            gs = mixedLands mountain forest 0 2
        HU.assertBool "canPay says no" (not (Mana.canPay S.alice cost gs))
        HU.assertBool "and paying fails" (not (fst (S.runPureWith S.identityAnswer gs (Mana.payCost S.alice cost))))
        HU.assertEqual "two {R/G} alone WOULD be payable from them" True (Mana.canPay S.alice (ManaCost.MkManaCost [redGreen, redGreen]) gs),
      HU.testCase "CR 107.4e whole card: Burning-Tree Emissary casts off RR, GG, or RG" $ do
        mountain <- Registry.printing registry "Mountain"
        forest <- Registry.printing registry "Forest"
        burningTreeEmissary <- Registry.printing registry "Burning-Tree Emissary"
        let castOff reds greens =
              let (gs, spellId) = S.handOne burningTreeEmissary (mixedLands mountain forest reds greens)
                  cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
               in length (GameState.stack cast)
        HU.assertEqual "two Mountains" 1 (castOff 2 0)
        HU.assertEqual "two Forests" 1 (castOff 0 2)
        HU.assertEqual "one of each" 1 (castOff 1 1)
        HU.assertEqual "one land is not enough" 0 (castOff 1 0),
      HU.testCase "CR 107.4e a hybrid symbol is ALL of its component colours" $ do
        burningTreeEmissary <- Registry.printing registry "Burning-Tree Emissary"
        let (oid, gs) = S.addCreature burningTreeEmissary S.alice (Setup.emptyGame S.bothPlayers)
        HU.assertEqual
          "red AND green, not one or the other"
          (Set.fromList [Color.Red, Color.Green])
          (Projection.colorsOf oid gs)
    ]

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
monocoloredHybridTests :: Registry.Type.Registry -> Tasty.TestTree
monocoloredHybridTests registry =
  let -- How many objects the stack holds after alice tries to cast the Javelin
      -- with `gs` already on the battlefield: 1 when the cost was paid, 0 when
      -- CR 601.2h rolled the whole attempt back.
      castsOff javelin gs =
        let (g, spellId) = S.handOne javelin gs
         in length (GameState.stack (snd (Engine.runGamePure S.identityAnswer g (Cast.castSpell S.alice spellId))))
   in Tasty.testGroup
        "MonocoloredHybrid"
        [ HU.testCase "CR 107.4e one {2/R} takes one Mountain OR two Islands, and one Island is not enough" $ do
            mountain <- Registry.printing registry "Mountain"
            island <- Registry.printing registry "Island"
            let cost = ManaCost.MkManaCost [twoOrRed]
            HU.assertBool "one Mountain pays it" (Mana.canPay S.alice cost (S.landsInPlay mountain 1))
            HU.assertBool "two Islands pay it" (Mana.canPay S.alice cost (S.landsInPlay island 2))
            HU.assertBool "one Island does not" (not (Mana.canPay S.alice cost (S.landsInPlay island 1)))
            HU.assertBool "and nothing does not" (not (Mana.canPay S.alice cost (S.landsInPlay island 0))),
          -- The coloured route, end to end. Three lands for three symbols is the
          -- reading a payment path that charged every symbol one mana would also
          -- get right, so this is the control the cases below discriminate
          -- against -- and the tap count is what says the route was really taken:
          -- six Mountains would still be three taps, because payCost stops as
          -- soon as the cost is payable.
          HU.testCase "CR 107.4e whole card: Flame Javelin casts off three Mountains, {R} per symbol" $ do
            mountain <- Registry.printing registry "Mountain"
            flameJavelin <- Registry.printing registry "Flame Javelin"
            let gs = S.landsInPlay mountain 3
            HU.assertBool "canPay says yes" (Mana.canPay S.alice javelinCost gs)
            HU.assertEqual "and it casts" 1 (castsOff flameJavelin gs)
            let (paid, after) = S.runPureWith S.identityAnswer (S.landsInPlay mountain 6) (Mana.payCost S.alice javelinCost)
            HU.assertBool "six Mountains pay it too" paid
            HU.assertEqual "and only three of them are tapped" 3 (S.tappedCount S.alice after)
            HU.assertEqual "with nothing left floating" 0 (poolSize S.alice after),
          -- The generic route, with no red mana anywhere on the board.
          HU.testCase "CR 107.4e whole card: Flame Javelin casts off six Islands, two generic per symbol" $ do
            island <- Registry.printing registry "Island"
            flameJavelin <- Registry.printing registry "Flame Javelin"
            let gs = S.landsInPlay island 6
            HU.assertBool "canPay says yes" (Mana.canPay S.alice javelinCost gs)
            HU.assertEqual "and it casts" 1 (castsOff flameJavelin gs),
          -- THE discriminating negative. Five Islands is one short of the {6} the
          -- all-generic route needs, and a payment path that charged one mana per
          -- {2/R} would call three of them sufficient, let alone five.
          HU.testCase "CR 107.4e five Islands cannot cast Flame Javelin -- {6} is one mana away" $ do
            island <- Registry.printing registry "Island"
            flameJavelin <- Registry.printing registry "Flame Javelin"
            let gs = S.landsInPlay island 5
            HU.assertBool "canPay says no" (not (Mana.canPay S.alice javelinCost gs))
            HU.assertEqual "and it does not cast" 0 (castsOff flameJavelin gs)
            HU.assertBool "three Islands are nowhere near" (not (Mana.canPay S.alice javelinCost (S.landsInPlay island 3))),
          -- CR 107.4e symbol by symbol, which the card's own ruling spells out:
          -- "you can pay for Flame Javelin by spending {R}{R}{R}, {2}{R}{R},
          -- {4}{R}, or {6}." So the routes are chosen per symbol, and a search
          -- that picked one route for the whole cost would reject both of these.
          HU.testCase "CR 107.4e each symbol picks its own route: {R}{R}{2} and {R}{4}" $ do
            mountain <- Registry.printing registry "Mountain"
            island <- Registry.printing registry "Island"
            flameJavelin <- Registry.printing registry "Flame Javelin"
            let cost = javelinCost
            HU.assertBool "two Mountains and two Islands: {R}{R}{2}" (Mana.canPay S.alice cost (mixedLands mountain island 2 2))
            HU.assertBool "one Mountain and four Islands: {R}{4}" (Mana.canPay S.alice cost (mixedLands mountain island 1 4))
            HU.assertEqual "and that one really casts" 1 (castsOff flameJavelin (mixedLands mountain island 1 4))
            -- One short of {R}{4} and one red short of {R}{R}{2}: four mana with
            -- only one red pays no route at all.
            HU.assertBool "one Mountain and three Islands: no route" (not (Mana.canPay S.alice cost (mixedLands mountain island 1 3))),
          -- The gameplay-level proof (design.md section 4): the whole card, cast
          -- and resolved off the all-generic route, doing what it says.
          HU.testCase "CR 107.4e Flame Javelin cast off six Islands resolves for 4 damage" $ do
            island <- Registry.printing registry "Island"
            flameJavelin <- Registry.printing registry "Flame Javelin"
            let (g, spellId) = S.handOne flameJavelin (S.landsInPlay island 6)
                cast = snd (Engine.runGamePure S.identityAnswer g (Cast.castSpell S.alice spellId))
                resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
            HU.assertEqual "stack empty" 0 (length (GameState.stack resolved))
            HU.assertEqual "every Island tapped" 6 (S.tappedCount S.alice resolved)
            HU.assertEqual "nothing left floating" 0 (poolSize S.alice resolved)
            -- S.identityAnswer takes the least Recipient on offer, which with no
            -- creatures anywhere is alice herself. Who it hits is the answer's
            -- business; that it hits for 4 is the card's.
            HU.assertEqual "4 damage to the chosen target" (Just 16) (S.lifeOf S.alice resolved),
          -- The elision, made visible rather than left implied. Both halves are
          -- payable out of this pool and they leave DIFFERENT pools behind, so
          -- unlike a colour/colour hybrid the choice is observable, and pawl
          -- makes it: it spends the fewest units. CR 601.2b puts that choice with
          -- the player, at announcement (#261).
          HU.testCase "CR 601.2b the engine takes a {2/R}'s one-mana half when both halves are payable (#261)" $
            let red = ManaUnit.MkManaUnit {ManaUnit.manaType = ManaType.Colored Color.Red}
                colorless = ManaUnit.MkManaUnit {ManaUnit.manaType = ManaType.Colorless}
             in HU.assertEqual
                  "the {R} is spent and both {C} remain -- the other half would spend both {C} and leave the {R}"
                  (Just (Mana.Type.MkMana [colorless, colorless], 0))
                  (Mana.spend 0 (ManaCost.MkManaCost [twoOrRed]) (Mana.Type.MkMana [red, colorless, colorless])),
          -- CR 107.4e's last sentence, as CR 202.2d restates it for the whole
          -- object: a monocolored hybrid's other component is generic mana, which
          -- is no colour, so only the named half counts. Flame Javelin is red
          -- even when six Islands paid for it.
          HU.testCase "CR 107.4e a monocolored hybrid symbol is its coloured half, and only that" $ do
            flameJavelin <- Registry.printing registry "Flame Javelin"
            let (oid, gs) = S.addCreature flameJavelin S.alice (Setup.emptyGame S.bothPlayers)
            HU.assertEqual "red, not colourless" (Set.singleton Color.Red) (Projection.colorsOf oid gs)
        ]

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
  let ((_, cast), asked) = Replay.record answer gs (Cast.castSpell S.alice oid)
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
phyrexianTests :: Registry.Type.Registry -> Tasty.TestTree
phyrexianTests registry =
  Tasty.testGroup
    "Phyrexian"
    [ -- The mana route. The life assertion is what makes this discriminating:
      -- both routes are open here, and paying life as WELL as the mana, or
      -- INSTEAD of it, would each read as "paid" without it.
      --
      -- It also pins what Mana.payCost does with an UNANNOUNCED cost, which is
      -- what this and the four cases after it exercise: they call payCost
      -- directly, so no CR 118.13a announcement has happened and the least-life
      -- rule still decides, which here means none (#373). A cast goes through
      -- Cast.castSpell instead and asks -- see the CR 118.13a cases at the end of
      -- this group.
      HU.testCase "CR 107.4f one {G/P} is paid with one green mana and no life" $ do
        forest <- Registry.printing registry "Forest"
        let gs = S.landsInPlay forest 1
        HU.assertBool "canPay says yes" (Mana.canPay S.alice phyrexianCost gs)
        let (paid, after) = S.runPureWith S.identityAnswer gs (Mana.payCost S.alice phyrexianCost)
        HU.assertBool "and it really is paid" paid
        HU.assertEqual "the Forest is tapped" 1 (S.tappedCount S.alice after)
        HU.assertEqual "nothing left floating" 0 (poolSize S.alice after)
        HU.assertEqual "life untouched" (Just 20) (S.lifeOf S.alice after),
      -- The life route, with a land on the battlefield that cannot help. The tap
      -- count is the discriminator: a payment path that tapped the Mountain
      -- first and then paid life would still leave alice at 18.
      HU.testCase "CR 107.4f one {G/P} is paid by 2 life when no green mana can be made" $ do
        mountain <- Registry.printing registry "Mountain"
        let gs = S.landsInPlay mountain 1
        HU.assertBool "canPay says yes" (Mana.canPay S.alice phyrexianCost gs)
        let (paid, after) = S.runPureWith S.identityAnswer gs (Mana.payCost S.alice phyrexianCost)
        HU.assertBool "and it really is paid" paid
        HU.assertEqual "exactly 2 life" (Just 18) (S.lifeOf S.alice after)
        HU.assertEqual "the Mountain is untouched" 0 (S.tappedCount S.alice after)
        HU.assertEqual "nothing left floating" 0 (poolSize S.alice after),
      -- CR 119.4: "the player may do so only if their life total is greater than
      -- or equal to the amount of the payment." Two is the boundary, and the
      -- payment that takes alice to exactly 0 is legal -- CR 704.5a's loss is a
      -- state-based action afterwards, not a bar on the payment.
      HU.testCase "CR 119.4 a {G/P} is payable at 2 life and unpayable at 1" $ do
        HU.assertBool "2 life is enough" (Mana.canPay S.alice phyrexianCost (aliceAt 2))
        HU.assertBool "1 life is not" (not (Mana.canPay S.alice phyrexianCost (aliceAt 1)))
        HU.assertBool "0 life is not" (not (Mana.canPay S.alice phyrexianCost (aliceAt 0)))
        let (paid, after) = S.runPureWith S.identityAnswer (aliceAt 2) (Mana.payCost S.alice phyrexianCost)
        HU.assertBool "paying at 2 really works" paid
        HU.assertEqual "and takes her to 0" (Just 0) (S.lifeOf S.alice after)
        let (failed, unchanged) = S.runPureWith S.identityAnswer (aliceAt 1) (Mana.payCost S.alice phyrexianCost)
        HU.assertBool "at 1 the payment fails" (not failed)
        HU.assertEqual "and CR 601.2h leaves the life total alone" (Just 1) (S.lifeOf S.alice unchanged),
      -- The gameplay-level proof (design.md section 4), mana route: the whole
      -- card, cast off one Forest and resolved. Goblin Piker is 2/1, so +2/+2 is
      -- 4/3.
      HU.testCase "CR 107.4f whole card: Mutagenic Growth casts off one Forest for +2/+2" $ do
        forest <- Registry.printing registry "Forest"
        piker <- Registry.printing registry "Goblin Piker"
        mutagenicGrowth <- Registry.printing registry "Mutagenic Growth"
        let (pikerId, withPiker) = S.addCreature piker S.alice (S.landsInPlay forest 1)
            (g, spellId) = S.handOne mutagenicGrowth withPiker
            cast = snd (Engine.runGamePure S.identityAnswer g (Cast.castSpell S.alice spellId))
            resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        HU.assertEqual "stack empty" 0 (length (GameState.stack resolved))
        HU.assertEqual "power" (Just 4) (Projection.powerOf pikerId resolved)
        HU.assertEqual "toughness" (Just 3) (Projection.toughnessOf pikerId resolved)
        HU.assertEqual "the Forest paid for it" 1 (S.tappedCount S.alice resolved)
        HU.assertEqual "so no life was" (Just 20) (S.lifeOf S.alice resolved),
      -- The same card with NO lands anywhere. Castability has to see the life
      -- route or this never reaches the stack at all.
      HU.testCase "CR 107.4f whole card: Mutagenic Growth casts with no mana at all, for 2 life" $ do
        piker <- Registry.printing registry "Goblin Piker"
        mutagenicGrowth <- Registry.printing registry "Mutagenic Growth"
        let (pikerId, withPiker) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
            (g, spellId) = S.handOne mutagenicGrowth withPiker
        HU.assertBool "castable with an empty battlefield but for the Piker" (Cast.castable S.alice spellId g)
        let cast = snd (Engine.runGamePure S.identityAnswer g (Cast.castSpell S.alice spellId))
            resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        HU.assertEqual "stack empty" 0 (length (GameState.stack resolved))
        HU.assertEqual "power" (Just 4) (Projection.powerOf pikerId resolved)
        HU.assertEqual "toughness" (Just 3) (Projection.toughnessOf pikerId resolved)
        HU.assertEqual "exactly 2 life paid" (Just 18) (S.lifeOf S.alice resolved),
      -- THE discriminating negative: neither route open. One life short, and no
      -- green mana on the board.
      HU.testCase "CR 119.4 Mutagenic Growth is uncastable at 1 life with no green mana" $ do
        piker <- Registry.printing registry "Goblin Piker"
        mutagenicGrowth <- Registry.printing registry "Mutagenic Growth"
        let inHandAt n =
              let (_, withPiker) = S.addCreature piker S.alice (aliceAt n)
               in S.handOne mutagenicGrowth withPiker
            castableAt n = let (g, spellId) = inHandAt n in Cast.castable S.alice spellId g
            castsAt n =
              let (g, spellId) = inHandAt n
               in length (GameState.stack (snd (Engine.runGamePure S.identityAnswer g (Cast.castSpell S.alice spellId))))
        HU.assertBool "at 1 life it is not castable" (not (castableAt 1))
        HU.assertEqual "and it does not cast" 0 (castsAt 1)
        HU.assertBool "at 2 life it is -- the Piker it targets has not moved" (castableAt 2)
        HU.assertEqual "and it does cast" 1 (castsAt 2),
      -- CR 107.4f's FIRST clause, the one a payment-only reading loses:
      -- "Phyrexian mana symbols are colored mana symbols: ... {G/P} is green."
      -- CR 202.2d says the same of the object: "An object with one or more
      -- hybrid mana symbols and/or Phyrexian mana symbols in its mana cost is all
      -- of the colors of those mana symbols, in addition to any other colors the
      -- object might be."
      HU.testCase "CR 107.4f/202.2d a Phyrexian mana symbol is a COLOURED mana symbol" $ do
        mutagenicGrowth <- Registry.printing registry "Mutagenic Growth"
        let (oid, gs) = S.addCreature mutagenicGrowth S.alice (Setup.emptyGame S.bothPlayers)
        HU.assertEqual "green, not colourless" (Set.singleton Color.Green) (Projection.colorsOf oid gs),
      -- And the colour survives the route that produces no green mana at all --
      -- the reading that would call the card colourless is exactly the one a
      -- life-paid cast tempts.
      HU.testCase "CR 202.2d Mutagenic Growth is green on the stack even when 2 life paid for it" $ do
        piker <- Registry.printing registry "Goblin Piker"
        mutagenicGrowth <- Registry.printing registry "Mutagenic Growth"
        let (_, withPiker) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
            (g, spellId) = S.handOne mutagenicGrowth withPiker
            cast = snd (Engine.runGamePure S.identityAnswer g (Cast.castSpell S.alice spellId))
        HU.assertEqual "2 life paid, no green mana ever made" (Just 18) (S.lifeOf S.alice cast)
        case GameState.stack cast of
          [sid] -> HU.assertEqual "and the spell is still green" (Set.singleton Color.Green) (Projection.colorsOf sid cast)
          _ -> HU.assertFailure "expected exactly one spell on the stack",
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
      HU.testCase "CR 107.4f the least-life route is found across symbols, not only within one" $ do
        birds <- Registry.printing registry "Birds of Paradise"
        island <- Registry.printing registry "Island"
        mountain <- Registry.printing registry "Mountain"
        forest <- Registry.printing registry "Forest"
        let cost = ManaCost.MkManaCost [twoOrRed, ManaSymbol.Phyrexian Color.Green]
        HU.assertEqual
          "the {G} plus {2} route, costing no life"
          (Just 0)
          (Mana.lifeNeeded S.alice cost (mixedLands island birds 2 1))
        HU.assertBool "and it is payable" (Mana.canPay S.alice cost (mixedLands island birds 2 1))
        -- The discriminator: the same cost and the same three permanents, but a
        -- Mountain in the Birds' place makes no green, so every surviving route
        -- costs 2 life and the answer really does depend on the board.
        HU.assertEqual
          "with a Mountain instead, 2 life is the cheapest there is"
          (Just 2)
          (Mana.lifeNeeded S.alice cost (mixedLands island mountain 2 1))
        HU.assertEqual
          "and a lone {G/P} off nothing at all is 2 as well"
          (Just 2)
          (Mana.lifeNeeded S.alice phyrexianCost (Setup.emptyGame S.bothPlayers))
        HU.assertEqual
          "while a lone {G/P} with a Forest is 0"
          (Just 0)
          (Mana.lifeNeeded S.alice phyrexianCost (S.landsInPlay forest 1)),
      -- The budget is recomputed as sources are tapped, not fixed when the
      -- payment starts, and a Birds of Paradise is what makes the difference
      -- observable: it COULD make green, so pawl starts with a budget of zero
      -- life and taps it -- and when the player names blue instead, the mana way
      -- is gone and CR 107.4f's 2 life is all that is left. pawl pays it rather
      -- than failing the payment, which is the same MORE PERMISSIVE posture
      -- Mana.payCost's haddock takes towards a mis-tapped colour (#261). Reached
      -- only because this calls payCost directly, with nothing announced (#373).
      HU.testCase "CR 107.4f a Birds tapped for blue still pays a {G/P}, out of life" $ do
        birds <- Registry.printing registry "Birds of Paradise"
        let (_, gs) = S.addCreature birds S.alice (Setup.emptyGame S.bothPlayers)
            (paidBlue, afterBlue) = S.runPureWith (prefersColor Color.Blue) gs (Mana.payCost S.alice phyrexianCost)
        HU.assertBool "the cost is still paid" paidBlue
        HU.assertEqual "by 2 life" (Just 18) (S.lifeOf S.alice afterBlue)
        HU.assertEqual "the Birds was tapped on the way" 1 (S.tappedCount S.alice afterBlue)
        HU.assertEqual "and its blue mana is still floating" 1 (poolSize S.alice afterBlue)
        -- The control: the same board and the same card, one different answer.
        let (paidGreen, afterGreen) = S.runPureWith (prefersColor Color.Green) gs (Mana.payCost S.alice phyrexianCost)
        HU.assertBool "green pays it too" paidGreen
        HU.assertEqual "and costs no life at all" (Just 20) (S.lifeOf S.alice afterGreen)
        HU.assertEqual "with nothing left floating" 0 (poolSize S.alice afterGreen),
      -- CR 118.13a: "If the mana cost of a spell ... contains a mana symbol that
      -- can be paid in multiple ways, the choice of how to pay for that symbol is
      -- made as its controller proposes that spell or ability (see rule 601.2b)."
      --
      -- THE proving scenario (#361), and the reason the choice is not the
      -- engine's to make conservatively: a player holding one Forest can cast
      -- Mutagenic Growth AND Llanowar Elves only by announcing 2 life for the
      -- {G/P}. The Elves' castability is the discriminator -- a life total alone
      -- could be produced by paying life on TOP of the mana.
      HU.testCase "CR 118.13a announcing 2 life keeps the Forest, so Llanowar Elves is still castable" $ do
        forest <- Registry.printing registry "Forest"
        piker <- Registry.printing registry "Goblin Piker"
        growth <- Registry.printing registry "Mutagenic Growth"
        elves <- Registry.printing registry "Llanowar Elves"
        let (growthId, elvesId, gs) = phyrexianBoard forest piker growth elves
            (asked, resolved) = castAndResolve (announces PhyrexianPayment.PaysLife) gs growthId
        HU.assertBool "the engine asked rather than deciding" (wasAskedHowToPayPhyrexian asked)
        HU.assertEqual "the Growth resolved" 0 (length (GameState.stack resolved))
        HU.assertEqual "exactly 2 life" (Just 18) (S.lifeOf S.alice resolved)
        HU.assertEqual "and the Forest is untapped" 0 (S.tappedCount S.alice resolved)
        HU.assertBool "so the Elves can still be cast" (Cast.castable S.alice elvesId resolved),
      -- The control, one answer different on the same board: the mana route
      -- spends the Forest and the Elves are stranded. Both legs are needed --
      -- either alone would pass against an engine that ignored the answer.
      HU.testCase "CR 118.13a announcing coloured mana taps the Forest, and the Elves are stranded" $ do
        forest <- Registry.printing registry "Forest"
        piker <- Registry.printing registry "Goblin Piker"
        growth <- Registry.printing registry "Mutagenic Growth"
        elves <- Registry.printing registry "Llanowar Elves"
        let (growthId, elvesId, gs) = phyrexianBoard forest piker growth elves
            (asked, resolved) = castAndResolve (announces PhyrexianPayment.PaysMana) gs growthId
        HU.assertBool "the engine asked here too" (wasAskedHowToPayPhyrexian asked)
        HU.assertEqual "life untouched" (Just 20) (S.lifeOf S.alice resolved)
        HU.assertEqual "the Forest paid for it" 1 (S.tappedCount S.alice resolved)
        HU.assertBool "and the Elves cannot be cast" (not (Cast.castable S.alice elvesId resolved)),
      -- The elision, both directions. Where only ONE route is payable there is
      -- nothing to ask, and the interpreter asking for the other route does not
      -- get it -- CR 601.2b's "previously made choices ... may restrict the
      -- player's options" arriving as a board that offers one option.
      HU.testCase "CR 118.13a no green source: the life route is taken and nothing is asked" $ do
        piker <- Registry.printing registry "Goblin Piker"
        growth <- Registry.printing registry "Mutagenic Growth"
        elves <- Registry.printing registry "Llanowar Elves"
        let (_, withPiker) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
            (withGrowth, growthId) = S.handOne growth withPiker
            (_, gs) = S.addHandCard elves S.alice withGrowth
            (asked, resolved) = castAndResolve (announces PhyrexianPayment.PaysMana) gs growthId
        HU.assertBool "no choice existed, so none was asked" (not (wasAskedHowToPayPhyrexian asked))
        HU.assertEqual "the Growth resolved" 0 (length (GameState.stack resolved))
        HU.assertEqual "and 2 life paid for it" (Just 18) (S.lifeOf S.alice resolved),
      -- CR 119.4's floor closing the life route instead: one Forest, one life.
      -- The interpreter asks for life and must not be given it.
      HU.testCase "CR 119.4 at 1 life the life route is not offered, and nothing is asked" $ do
        forest <- Registry.printing registry "Forest"
        piker <- Registry.printing registry "Goblin Piker"
        growth <- Registry.printing registry "Mutagenic Growth"
        elves <- Registry.printing registry "Llanowar Elves"
        let (growthId, _, board) = phyrexianBoard forest piker growth elves
            gs = board {GameState.players = Map.adjust (\p -> p {Player.life = 1}) S.alice (GameState.players board)}
            (asked, resolved) = castAndResolve (announces PhyrexianPayment.PaysLife) gs growthId
        HU.assertBool "no choice existed, so none was asked" (not (wasAskedHowToPayPhyrexian asked))
        HU.assertEqual "the Growth resolved" 0 (length (GameState.stack resolved))
        HU.assertEqual "the Forest paid for it" 1 (S.tappedCount S.alice resolved)
        HU.assertEqual "and the life total is untouched" (Just 1) (S.lifeOf S.alice resolved)
    ]

-- The single activated ability of a printing that has exactly one -- Moltensteel
-- Dragon's "{R/P}: This creature gets +1/+0 until end of turn." Total because
-- HUnit needs a value; a printing with no ability would fail the assertions that
-- follow rather than this lookup.
theAbility :: Printing.Printing -> ActivatedAbility.ActivatedAbility Card.Type.Card
theAbility p = case Card.Type.activatedAbilities (Printing.card p) of
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
totalCostTests :: Registry.Type.Registry -> Tasty.TestTree
totalCostTests registry =
  Tasty.testGroup
    "TotalCost"
    [ -- THE reduction case. Sapphire Medallion is "Blue spells you cast cost {1}
      -- less to cast", Spined Thopter is {2}{U/P}, and two Islands are exactly
      -- the board where the reduction decides the question: the total {1}{U/P}
      -- can be paid with {1}{U} off both Islands, while the printed {2}{U/P}
      -- cannot be paid with mana at all. So the coloured-mana route IS available
      -- and the player must be asked for it.
      HU.testCase "CR 601.2f a reduction opens the coloured-mana route, so the announcement is asked" $ do
        island <- Registry.printing registry "Island"
        medallion <- Registry.printing registry "Sapphire Medallion"
        thopter <- Registry.printing registry "Spined Thopter"
        let (gs, thopterId) = S.handOne thopter (withPermanent island medallion 2)
        HU.assertBool "castable" (Cast.castable S.alice thopterId gs)
        let (asked, resolved) = castAndResolve (announces PhyrexianPayment.PaysMana) gs thopterId
        -- The outcome first, because it is the thing that was wrong: the engine
        -- used to take CR 107.4f's life route here without asking.
        HU.assertEqual "no life was paid" (Just 20) (S.lifeOf S.alice resolved)
        HU.assertEqual "both Islands paid the reduced {1}{U}" 2 (S.tappedCount S.alice resolved)
        HU.assertBool "and the engine asked rather than deciding" (wasAskedHowToPayPhyrexian asked)
        HU.assertEqual "the Thopter resolved" 0 (length (GameState.stack resolved)),
      -- The control, one answer different on the same board: CR 107.4f's life
      -- route leaves an Island up. Both legs are needed -- either alone would
      -- pass against an engine that ignored the answer.
      HU.testCase "CR 601.2f the same board's life route pays 2 and spares an Island" $ do
        island <- Registry.printing registry "Island"
        medallion <- Registry.printing registry "Sapphire Medallion"
        thopter <- Registry.printing registry "Spined Thopter"
        let (gs, thopterId) = S.handOne thopter (withPermanent island medallion 2)
            (asked, resolved) = castAndResolve (announces PhyrexianPayment.PaysLife) gs thopterId
        HU.assertBool "asked here too" (wasAskedHowToPayPhyrexian asked)
        HU.assertEqual "the Thopter resolved" 0 (length (GameState.stack resolved))
        HU.assertEqual "one Island paid the reduced {1}" 1 (S.tappedCount S.alice resolved)
        HU.assertEqual "and 2 life paid the symbol" (Just 18) (S.lifeOf S.alice resolved),
      -- THE increase case. Thalia is "Noncreature spells cost {1} more to cast",
      -- Mutagenic Growth is an instant, and one Forest is exactly the board where
      -- the increase decides the question: the total {1}{G/P} cannot be paid with
      -- {1}{G} off one Forest, so the coloured-mana route is NOT available and
      -- must not be offered. The interpreter asks for it and does not get it.
      HU.testCase "CR 601.2f an increase closes the coloured-mana route, so nothing is asked" $ do
        forest <- Registry.printing registry "Forest"
        piker <- Registry.printing registry "Goblin Piker"
        thalia <- Registry.printing registry "Thalia, Guardian of Thraben"
        growth <- Registry.printing registry "Mutagenic Growth"
        -- The Piker goes down BEFORE Thalia, because S.identityAnswer's
        -- ChooseTargets takes the lowest object id and both are 2/1 creatures --
        -- so with Thalia first the Growth would pump HER and the assertion below
        -- would be reading the wrong permanent.
        let (_, withPiker) = S.addCreature piker S.alice (S.landsInPlay forest 1)
            (_, withThalia) = S.addCreature thalia S.alice withPiker
            (gs, growthId) = S.handOne growth withThalia
        HU.assertBool "castable, by CR 107.4f's life route" (Cast.castable S.alice growthId gs)
        let (asked, resolved) = castAndResolve (announces PhyrexianPayment.PaysMana) gs growthId
        -- The outcome first again: answering the route the engine used to offer
        -- made the whole cast a no-op, so the Piker went unpumped and no life was
        -- paid at all.
        HU.assertEqual "2 life paid the symbol" (Just 18) (S.lifeOf S.alice resolved)
        HU.assertEqual "the Piker really was pumped" (Just 4) (Projection.powerOf (pikerOn resolved) resolved)
        HU.assertEqual "the Forest paid Thalia's {1}" 1 (S.tappedCount S.alice resolved)
        HU.assertBool "and no route existed, so none was asked" (not (wasAskedHowToPayPhyrexian asked))
        HU.assertEqual "the Growth resolved rather than evaporating" 0 (length (GameState.stack resolved)),
      -- The increase again, with TWO symbols, which is what makes it a cast lost
      -- rather than a cast made awkwardly: Dismember's total under Thalia is
      -- {2}{B/P}{B/P}, and two Swamps pay that only by CR 107.4f's life route
      -- twice. Measured against the printed {1}{B/P}{B/P} the first symbol looks
      -- like a real choice, and taking its mana route strands the payment.
      HU.testCase "CR 601.2f Dismember under Thalia forces both symbols to life, and the cast survives" $ do
        swamp <- Registry.printing registry "Swamp"
        piker <- Registry.printing registry "Goblin Piker"
        thalia <- Registry.printing registry "Thalia, Guardian of Thraben"
        dismember <- Registry.printing registry "Dismember"
        -- Piker before Thalia, for the reason the case above gives: both are 2/1
        -- creatures and identityAnswer targets the lowest object id.
        let (_, withPiker) = S.addCreature piker S.alice (S.landsInPlay swamp 2)
            (_, withThalia) = S.addCreature thalia S.alice withPiker
            (gs, dismemberId) = S.handOne dismember withThalia
        HU.assertBool "castable, by two life routes" (Cast.castable S.alice dismemberId gs)
        let (asked, resolved) = castAndResolve (announces PhyrexianPayment.PaysMana) gs dismemberId
        HU.assertEqual "4 life paid both symbols" (Just 16) (S.lifeOf S.alice resolved)
        HU.assertEqual "both Swamps paid Thalia's {2}" 2 (S.tappedCount S.alice resolved)
        HU.assertEqual "and neither symbol was a choice" [] (phyrexianAnnouncements asked)
        HU.assertEqual "Dismember resolved rather than evaporating" 0 (length (GameState.stack resolved))
    ]

-- Dismember ({1}{B/P}{B/P}) -- the first card in the pool with more than one
-- Phyrexian mana symbol, and so the first to exercise CR 601.2b's "for each of
-- those symbols" at all. Everything Mutagenic Growth proves about ONE symbol it
-- proves once; what only two symbols can show is the LOOP: one prompt per symbol
-- in printed order, each asked knowing the answers before it, and an earlier
-- answer narrowing a later one's offer -- CR 601.2b's last sentence, "previously
-- made choices ... may restrict the player's options when making these choices."
dismemberTests :: Registry.Type.Registry -> Tasty.TestTree
dismemberTests registry =
  Tasty.testGroup
    "Dismember"
    [ -- Two Swamps: the first symbol is a real choice, and answering MANA leaves
      -- {1}{B} to pay off two Swamps -- which the second symbol's mana route
      -- would push to three. So the second symbol is forced to life and is not
      -- asked. One prompt, not two, and that count is the assertion.
      HU.testCase "CR 601.2b announcing mana for the first {B/P} forces the second to life" $ do
        swamp <- Registry.printing registry "Swamp"
        piker <- Registry.printing registry "Goblin Piker"
        dismember <- Registry.printing registry "Dismember"
        let (gs, dismemberId) = dismemberBoard swamp piker dismember 2
            (asked, resolved) = castAndResolve (announces PhyrexianPayment.PaysMana) gs dismemberId
        HU.assertEqual "one symbol was a choice, the other was forced" [PhyrexianPayment.PaysMana] (phyrexianAnnouncements asked)
        HU.assertEqual "both Swamps paid {1}{B}" 2 (S.tappedCount S.alice resolved)
        HU.assertEqual "and 2 life paid the second symbol" (Just 18) (S.lifeOf S.alice resolved),
      -- The same board, the other answer: paying the first symbol with life keeps
      -- both Swamps available, so the SECOND symbol is a real choice too and is
      -- asked. Two prompts, and the answers are 2 life each.
      HU.testCase "CR 601.2b announcing life for the first {B/P} leaves the second a real choice" $ do
        swamp <- Registry.printing registry "Swamp"
        piker <- Registry.printing registry "Goblin Piker"
        dismember <- Registry.printing registry "Dismember"
        let (gs, dismemberId) = dismemberBoard swamp piker dismember 2
            (asked, resolved) = castAndResolve (announces PhyrexianPayment.PaysLife) gs dismemberId
        HU.assertEqual
          "both symbols were asked, in printed order"
          [PhyrexianPayment.PaysLife, PhyrexianPayment.PaysLife]
          (phyrexianAnnouncements asked)
        HU.assertEqual "one Swamp paid the {1}" 1 (S.tappedCount S.alice resolved)
        HU.assertEqual "and 4 life paid both symbols" (Just 16) (S.lifeOf S.alice resolved),
      -- One Swamp: neither symbol can be paid with mana, since the Swamp is
      -- needed for the {1}. Nothing is asked at all, and CR 107.4f's example
      -- arithmetic for two symbols -- 4 life -- is what comes out.
      HU.testCase "CR 107.4f off one Swamp both symbols are forced to life, for 4" $ do
        swamp <- Registry.printing registry "Swamp"
        piker <- Registry.printing registry "Goblin Piker"
        dismember <- Registry.printing registry "Dismember"
        let (gs, dismemberId) = dismemberBoard swamp piker dismember 1
            (asked, resolved) = castAndResolve (announces PhyrexianPayment.PaysMana) gs dismemberId
        HU.assertEqual "no choice existed either time" [] (phyrexianAnnouncements asked)
        HU.assertEqual "the Swamp paid the {1}" 1 (S.tappedCount S.alice resolved)
        HU.assertEqual "and 4 life paid both symbols" (Just 16) (S.lifeOf S.alice resolved),
      -- The gameplay-level proof (design.md section 4): the whole card, cast and
      -- resolved. A Goblin Piker is 2/1, so -5/-5 is -3/-4 and CR 704.5f's
      -- state-based action puts it into its owner's graveyard.
      HU.testCase "CR 107.4f whole card: Dismember kills a Goblin Piker for 4 life" $ do
        swamp <- Registry.printing registry "Swamp"
        piker <- Registry.printing registry "Goblin Piker"
        dismember <- Registry.printing registry "Dismember"
        let (gs, dismemberId) = dismemberBoard swamp piker dismember 1
            (_, resolved) = castAndResolve (announces PhyrexianPayment.PaysMana) gs dismemberId
            pikerId = pikerOn resolved
        HU.assertEqual "-5/-5 applied" (Just (-3)) (Projection.powerOf pikerId resolved)
        HU.assertEqual "toughness too" (Just (-4)) (Projection.toughnessOf pikerId resolved)
        let settled = S.settleSba resolved
        HU.assertBool "CR 704.5f buried it" (not (Set.member pikerId (GameState.battlefield settled)))
        HU.assertEqual "and 4 life paid for it" (Just 16) (S.lifeOf S.alice settled)
    ]

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
  let isPiker oid = fmap Card.Type.name (Game.cardOf oid gs) == Just (Text.pack "Goblin Piker")
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
-- card there was nothing in the pool for Pawl.Activate's Cost.announce call to do.
moltensteelTests :: Registry.Type.Registry -> Tasty.TestTree
moltensteelTests registry =
  Tasty.testGroup
    "Moltensteel"
    [ -- The activation cost's symbol IS a choice off a Mountain, and answering
      -- mana taps it. CR 118.13b/c are not what governs this -- the cost is an
      -- activation cost, so CR 118.13a is, and the choice belongs at proposal
      -- rather than at payment (#373 is the other two clauses).
      HU.testCase "CR 118.13a/602.2b an activation cost's {R/P} is asked, and mana taps the Mountain" $ do
        mountain <- Registry.printing registry "Mountain"
        dragon <- Registry.printing registry "Moltensteel Dragon"
        let (dragonId, gs) = dragonBoard mountain dragon 1
            (asked, activated) = activateAndResolve (announces PhyrexianPayment.PaysMana) gs dragonId (theAbility dragon)
        HU.assertEqual "one symbol, one prompt" [PhyrexianPayment.PaysMana] (phyrexianAnnouncements asked)
        HU.assertEqual "the Mountain paid it" 1 (S.tappedCount S.alice activated)
        HU.assertEqual "no life paid" (Just 20) (S.lifeOf S.alice activated)
        HU.assertEqual "+1/+0" (Just 5) (Projection.powerOf dragonId activated)
        HU.assertEqual "toughness unchanged" (Just 4) (Projection.toughnessOf dragonId activated),
      -- The control, one answer different on the same board: 2 life instead, and
      -- the Mountain is still up for something else.
      HU.testCase "CR 118.13a the same activation's life route spares the Mountain" $ do
        mountain <- Registry.printing registry "Mountain"
        dragon <- Registry.printing registry "Moltensteel Dragon"
        let (dragonId, gs) = dragonBoard mountain dragon 1
            (asked, activated) = activateAndResolve (announces PhyrexianPayment.PaysLife) gs dragonId (theAbility dragon)
        HU.assertEqual "asked here too" [PhyrexianPayment.PaysLife] (phyrexianAnnouncements asked)
        HU.assertEqual "the Mountain is untouched" 0 (S.tappedCount S.alice activated)
        HU.assertEqual "2 life paid it" (Just 18) (S.lifeOf S.alice activated)
        HU.assertEqual "+1/+0 all the same" (Just 5) (Projection.powerOf dragonId activated),
      -- No red source: CR 107.4f's mana route cannot be completed, so there is
      -- nothing to ask and the life route is taken. The activation still happens,
      -- which is the half a payment-time reading would get right by accident.
      HU.testCase "CR 118.13a with no red source the activation's life route is forced" $ do
        mountain <- Registry.printing registry "Mountain"
        dragon <- Registry.printing registry "Moltensteel Dragon"
        let (dragonId, gs) = dragonBoard mountain dragon 0
            (asked, activated) = activateAndResolve (announces PhyrexianPayment.PaysMana) gs dragonId (theAbility dragon)
        HU.assertEqual "no choice existed, so none was asked" [] (phyrexianAnnouncements asked)
        HU.assertEqual "2 life paid it" (Just 18) (S.lifeOf S.alice activated)
        HU.assertEqual "+1/+0" (Just 5) (Projection.powerOf dragonId activated),
      -- The gameplay-level proof, and the second board where two symbols in ONE
      -- cost are both real choices: six Mountains pay {4}{R}{R} outright, so both
      -- announcements are asked and neither costs life.
      HU.testCase "CR 107.4f whole card: Moltensteel Dragon casts off six Mountains for no life" $ do
        mountain <- Registry.printing registry "Mountain"
        dragon <- Registry.printing registry "Moltensteel Dragon"
        let (gs, dragonId) = S.handOne dragon (S.landsInPlay mountain 6)
            (asked, resolved) = castAndResolve (announces PhyrexianPayment.PaysMana) gs dragonId
        HU.assertEqual
          "both symbols were asked"
          [PhyrexianPayment.PaysMana, PhyrexianPayment.PaysMana]
          (phyrexianAnnouncements asked)
        HU.assertEqual "all six Mountains paid {4}{R}{R}" 6 (S.tappedCount S.alice resolved)
        HU.assertEqual "and no life did" (Just 20) (S.lifeOf S.alice resolved)
        HU.assertEqual "a 4/4 arrived" (Just 4) (Projection.powerOf (dragonOn resolved) resolved)
        HU.assertEqual "CR 202.2d: red, from the Phyrexian symbols" (Set.singleton Color.Red) (Projection.colorsOf (dragonOn resolved) resolved),
      -- Four Mountains cannot pay {4}{R}{R}, so both symbols are forced to life
      -- and CR 107.4f's arithmetic for two symbols is 4 -- the same card, one
      -- fewer land, and the whole announcement disappears.
      HU.testCase "CR 107.4f whole card: off four Mountains both symbols are forced, for 4 life" $ do
        mountain <- Registry.printing registry "Mountain"
        dragon <- Registry.printing registry "Moltensteel Dragon"
        let (gs, dragonId) = S.handOne dragon (S.landsInPlay mountain 4)
            (asked, resolved) = castAndResolve (announces PhyrexianPayment.PaysMana) gs dragonId
        HU.assertEqual "no choice existed either time" [] (phyrexianAnnouncements asked)
        HU.assertEqual "all four Mountains paid the {4}" 4 (S.tappedCount S.alice resolved)
        HU.assertEqual "and 4 life paid both symbols" (Just 16) (S.lifeOf S.alice resolved)
        HU.assertEqual "a 4/4 arrived all the same" (Just 4) (Projection.powerOf (dragonOn resolved) resolved)
    ]

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
  let isDragon oid = fmap Card.Type.name (Game.cardOf oid gs) == Just (Text.pack "Moltensteel Dragon")
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
