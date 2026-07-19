-- Covers: Pawl.Type.Binding, Pawl.Binding
module Pawl.BindingSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Pawl.Binding as Binding
import qualified Pawl.Support as S
import qualified Pawl.Type.Recipient as Recipient
import qualified Pawl.Type.SlotName as SlotName
import qualified Pawl.Type.Subtype as Subtype
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

tests :: Tasty.TestTree
tests =
  Tasty.testGroup
    "Pawl.Binding"
    [ HU.testCase "fromChoices merges a shared slot's target and subtypes" $
        let slot = SlotName.MkSlotName (Text.pack "target")
            r = Recipient.ToPlayer S.alice
            pair = (Subtype.Mountain, Subtype.Island)
            m = Binding.fromChoices (Map.singleton slot r) (Map.singleton slot pair) Nothing
         in do
              HU.assertEqual "target projected" (Map.singleton slot r) (Binding.targetsOf m)
              HU.assertEqual "subtypes projected" (Map.singleton slot pair) (Binding.subtypesOf m),
      HU.testCase "fromChoices stores X under the reserved slot" $
        let m = Binding.fromChoices Map.empty Map.empty (Just 3)
         in HU.assertEqual "amount readable" (Just 3) (Binding.amountOf Binding.variableX m),
      HU.testCase "amountOf is Nothing for an absent slot" $
        HU.assertEqual "no amount" Nothing (Binding.amountOf Binding.variableX Map.empty)
    ]
