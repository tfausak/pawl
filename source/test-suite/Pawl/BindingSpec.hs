-- Covers: Pawl.Type.Binding, Pawl.Binding
module Pawl.BindingSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Binding as Binding
import qualified Pawl.Support as S
import qualified Pawl.Type.ModeIndex as ModeIndex
import qualified Pawl.Type.ProjectedCharacteristics as PC
import qualified Pawl.Type.Recipient as Recipient
import qualified Pawl.Type.SlotName as SlotName
import qualified Pawl.Type.Subtype as Subtype
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

sampleSnapshot :: PC.ProjectedCharacteristics
sampleSnapshot =
  PC.MkProjectedCharacteristics
    { PC.keywords = Set.empty,
      PC.colors = Set.empty,
      PC.power = Just 2,
      PC.toughness = Just 1,
      PC.characteristicPT = Nothing,
      PC.cardTypes = Set.empty,
      PC.subtypes = Set.empty,
      PC.rulesTextActive = True,
      PC.activatedAbilities = [],
      PC.replacementEffects = [],
      PC.triggeredAbilities = []
    }

tests :: Tasty.TestTree
tests =
  Tasty.testGroup
    "Pawl.Binding"
    [ HU.testCase "fromChoices merges a shared slot's target and subtypes" $
        let slot = SlotName.MkSlotName (Text.pack "target")
            r = Recipient.ToPlayer S.alice
            pair = (Subtype.Mountain, Subtype.Island)
            m = Binding.fromChoices (Map.singleton slot r) (Map.singleton slot pair) Nothing Set.empty
         in do
              HU.assertEqual "target projected" (Map.singleton slot r) (Binding.targetsOf m)
              HU.assertEqual "subtypes projected" (Map.singleton slot pair) (Binding.subtypesOf m),
      HU.testCase "fromChoices stores X under the reserved slot" $
        let m = Binding.fromChoices Map.empty Map.empty (Just 3) Set.empty
         in HU.assertEqual "amount readable" (Just 3) (Binding.amountOf Binding.variableX m),
      HU.testCase "amountOf is Nothing for an absent slot" $
        HU.assertEqual "no amount" Nothing (Binding.amountOf Binding.variableX Map.empty),
      HU.testCase "modesOf round-trips a stamped set of chosen modes" $
        let chosen = Set.fromList [ModeIndex.MkModeIndex 0, ModeIndex.MkModeIndex 2]
            m = Binding.fromChoices Map.empty Map.empty Nothing chosen
         in HU.assertEqual "modes readable" chosen (Binding.modesOf m),
      HU.testCase "modesOf is empty for an absent slot" $
        HU.assertEqual "no modes" Set.empty (Binding.modesOf Map.empty),
      HU.testCase "setCopy then copyOf round-trips the snapshot" $
        HU.assertEqual "copy snapshot" (Just sampleSnapshot) (Binding.copyOf (Binding.setCopy sampleSnapshot Map.empty)),
      HU.testCase "no copy binding means copyOf is Nothing" $
        HU.assertEqual "absent" Nothing (Binding.copyOf Map.empty),
      HU.testCase "markPending sets pendingCopy" $
        HU.assertBool "marked" (Binding.pendingCopy (Binding.markPending Map.empty)),
      HU.testCase "clearPending removes the marker" $
        HU.assertBool "cleared" (not (Binding.pendingCopy (Binding.clearPending (Binding.markPending Map.empty)))),
      HU.testCase "empty bindings are not pending" $
        HU.assertBool "empty" (not (Binding.pendingCopy Map.empty))
    ]
