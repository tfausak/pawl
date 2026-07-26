-- Covers Pawl.Mana: mana payment and castability.
module Pawl.ManaSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Pawl.Cast as Cast
import qualified Pawl.Engine as Engine
import qualified Pawl.Game as Game
import qualified Pawl.Mana as Mana
import qualified Pawl.Projection as Projection
import qualified Pawl.Registry as Registry
import qualified Pawl.Setup as Setup
import qualified Pawl.Stack as Stack
import qualified Pawl.Support as S
import qualified Pawl.Type.ActivatedAbility as ActivatedAbility
import qualified Pawl.Type.Color as Color
import qualified Pawl.Type.Cost as Cost.Type
import qualified Pawl.Type.Effect as Effect
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Mana as Mana.Type
import qualified Pawl.Type.ManaCost as ManaCost
import qualified Pawl.Type.ManaSymbol as ManaSymbol
import qualified Pawl.Type.ManaType as ManaType
import qualified Pawl.Type.ManaUnit as ManaUnit
import qualified Pawl.Type.Modal as Modal
import qualified Pawl.Type.Mode as Mode
import qualified Pawl.Type.ModeSelection as ModeSelection
import qualified Pawl.Type.Object as Object
import qualified Pawl.Type.PlayerId as PlayerId
import qualified Pawl.Type.Pool as Pool
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Quantity as Quantity
import qualified Pawl.Type.Registry as Registry.Type
import qualified Pawl.Type.Sickness as Sickness
import qualified Pawl.Type.SlotName as SlotName
import qualified Pawl.Type.Subtype as Subtype
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
            let after = Mana.tapForMana oid gs
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
      HU.testCase "paying {1}{R} taps exactly two of three Mountains and leaves no float" $ do
        mountain <- Registry.printing registry "Mountain"
        case Mana.payCost S.alice pikerCost (S.landsInPlay mountain 3) of
          Nothing -> HU.assertFailure "three Mountains should pay {1}{R}"
          Just after -> do
            HU.assertEqual "tapped" 2 (S.tappedCount S.alice after)
            HU.assertEqual "no float" 0 (poolSize S.alice after),
      HU.testCase "CR 500.4 mana pools empty" $ do
        mountain <- Registry.printing registry "Mountain"
        let gs = S.landsInPlay mountain 1
        case Game.zoneMembers Zone.Battlefield S.alice gs of
          [] -> HU.assertFailure "fixture should have one Mountain"
          oid : _ ->
            HU.assertEqual "emptied" 0 (poolSize S.alice (Mana.emptyManaPools (Mana.tapForMana oid gs))),
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
                  ActivatedAbility.modal = singleModeAbility [Effect.AddMana (ManaType.Colored Color.Green)] Map.empty
                }
         in HU.assertBool "mana ability" (Mana.isManaAbility ab),
      HU.testCase "CR 605.1a an ability that targets is NOT a mana ability" $
        let ab =
              ActivatedAbility.MkActivatedAbility
                { ActivatedAbility.cost = Cost.Type.MkCost {Cost.Type.mana = Just (ManaCost.MkManaCost []), Cost.Type.components = []},
                  ActivatedAbility.modal =
                    singleModeAbility
                      [Effect.AddMana (ManaType.Colored Color.Green)]
                      (Map.singleton (SlotName.MkSlotName (Text.pack "x")) (TargetSpec.MkTargetSpec Pool.AnyTarget Nothing))
                }
         in HU.assertBool "targets -> not mana" (not (Mana.isManaAbility ab)),
      HU.testCase "CR 605.1a a damage ability is NOT a mana ability" $
        let ab =
              ActivatedAbility.MkActivatedAbility
                { ActivatedAbility.cost = Cost.Type.MkCost {Cost.Type.mana = Just (ManaCost.MkManaCost []), Cost.Type.components = []},
                  ActivatedAbility.modal =
                    singleModeAbility
                      [Effect.DealDamage (SlotName.MkSlotName (Text.pack "x")) (Quantity.Literal 1)]
                      (Map.singleton (SlotName.MkSlotName (Text.pack "x")) (TargetSpec.MkTargetSpec Pool.AnyTarget Nothing))
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
      HU.testCase "mana from a controlled permanent goes to its controller, not owner" $ do
        llanowarElves <- Registry.printing registry "Llanowar Elves"
        let (oid, base) = S.addCreature llanowarElves S.bob (Setup.emptyGame S.bothPlayers)
            gs0 = S.giveControl oid S.alice base
            after = Mana.tapForMana oid gs0
            manaUnitsOf pool = case pool of
              Mana.Type.MkMana units -> units
        HU.assertBool "alice received a mana unit" (not (null (manaUnitsOf (Mana.poolOf S.alice after))))
        HU.assertBool "bob received none" (null (manaUnitsOf (Mana.poolOf S.bob after)))
    ]

tests :: Registry.Type.Registry -> Tasty.TestTree
tests registry = Tasty.testGroup "Mana" [manaTests registry, castabilityTests registry]
