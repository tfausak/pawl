{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers Pawl.Mana: mana payment and castability.
module Pawl.ManaSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Pawl.Cast as Cast
import qualified Pawl.Cost as Cost
import qualified Pawl.Engine as Engine
import qualified Pawl.Game as Game
import qualified Pawl.Mana as Mana
import qualified Pawl.Projection as Projection
import qualified Pawl.Registry as Registry
import qualified Pawl.Setup as Setup
import qualified Pawl.Stack as Stack
import qualified Pawl.Support as S
import qualified Pawl.Type.ActivatedAbility as ActivatedAbility
import qualified Pawl.Type.ActivationTiming as ActivationTiming
import qualified Pawl.Type.Color as Color
import qualified Pawl.Type.Cost as Cost.Type
import qualified Pawl.Type.Effect as Effect
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Keyword as Keyword
import qualified Pawl.Type.Mana as Mana.Type
import qualified Pawl.Type.ManaCost as ManaCost
import qualified Pawl.Type.ManaProduction as ManaProduction
import qualified Pawl.Type.ManaSymbol as ManaSymbol
import qualified Pawl.Type.ManaType as ManaType
import qualified Pawl.Type.ManaUnit as ManaUnit
import qualified Pawl.Type.Modal as Modal
import qualified Pawl.Type.Mode as Mode
import qualified Pawl.Type.ModeSelection as ModeSelection
import qualified Pawl.Type.Object as Object
import qualified Pawl.Type.ObjectId as ObjectId
import qualified Pawl.Type.PlayerId as PlayerId
import qualified Pawl.Type.Pool as Pool
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Prompt as Prompt
import qualified Pawl.Type.Quantity as Quantity
import qualified Pawl.Type.Registry as Registry.Type
import qualified Pawl.Type.Sickness as Sickness
import qualified Pawl.Type.SlotName as SlotName
import qualified Pawl.Type.Subtype as Subtype
import qualified Pawl.Type.TapState as TapState
import qualified Pawl.Type.TargetSpec as TargetSpec
import qualified Pawl.Type.Zone as Zone
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
  Modal.MkModal (Seq.singleton (Mode.MkMode (Seq.fromList effects) specs)) (ModeSelection.ChooseExactly 1)

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

-- Answers Prompt.ChooseManaType with `wanted` whenever it is on offer, and
-- defers everything else to S.identityAnswer. The ChooseManaType sibling of
-- prefersSource: a pair of tests differing only in this colour proves the
-- ANSWER decides what is produced, rather than the order Mana.manaTypesOf
-- happens to return.
prefersColor :: Color.Color -> Prompt.Prompt r -> r
prefersColor wanted p = case p of
  Prompt.ChooseManaType _ _ _ candidates ->
    if elem (ManaType.Colored wanted) (NonEmpty.toList candidates)
      then ManaType.Colored wanted
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
-- `answer` -- the observable that says which type a multi-type source produced.
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
      -- do not ask. A Forest produces one type, so no ChooseManaType is raised.
      HU.testCase "CR 605 a single-type source is not asked which type to produce" $ do
        forest <- Registry.printing registry "Forest"
        birds <- Registry.printing registry "Birds of Paradise"
        let countingAnswer :: Prompt.Prompt r -> State.State Int r
            countingAnswer p = case p of
              Prompt.ChooseManaType {} -> do
                State.modify (+ 1)
                pure (S.identityAnswer p)
              _ -> pure (S.identityAnswer p)
            asks printing =
              let (oid, gs) = S.addCreature printing S.alice (Setup.emptyGame S.bothPlayers)
               in State.execState (Engine.runGame countingAnswer gs (Mana.tapForMana oid)) 0
        HU.assertEqual "a Forest: nothing to ask" 0 (asks forest)
        HU.assertEqual "a Birds of Paradise: one real decision" 1 (asks birds)
    ]

tests :: Registry.Type.Registry -> Tasty.TestTree
tests registry = Tasty.testGroup "Mana" [manaTests registry, castabilityTests registry, anyColorTests registry]
