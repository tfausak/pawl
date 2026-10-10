module Pawl.Codec.CostChangeSpec where

import qualified Pawl.Codec.CostChange as CostChange
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AppliedReduction as AppliedReduction
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.CostAddition as CostAddition
import qualified Pawl.Types.CostAmount as CostAmount
import qualified Pawl.Types.CostChange as CostChange.Type
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.CostScale as CostScale
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.Sacrifice as Sacrifice

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CostChange" $ do
  Spec.it s "Increase" $
    Common.assertCodec
      s
      CostChange.codec
      (CostChange.Type.Increase 2)
      " {\"type\":\"Increase\",\"value\":2} "
  -- The rules' defaults, no floor and CR 118.7b-d's spill, write no key.
  Spec.it s "Reduce, Sapphire Medallion's" $
    Common.assertCodec
      s
      CostChange.codec
      (CostChange.Type.Reduce (AppliedReduction.MkAppliedReduction (ManaCost.MkManaCost [ManaSymbol.Generic 1]) 0 False))
      " {\"type\":\"Reduce\",\"value\":{\"amount\":[{\"type\":\"Generic\",\"value\":1}]}} "
  Spec.it s "Reduce, Edgewalker's coloured mana only" $
    Common.assertCodec
      s
      CostChange.codec
      (CostChange.Type.Reduce (AppliedReduction.MkAppliedReduction (ManaCost.MkManaCost [ManaSymbol.OfType (ManaType.Colored Color.White)]) 0 True))
      " {\"type\":\"Reduce\",\"value\":{\"amount\":[{\"type\":\"OfType\",\"value\":{\"type\":\"Colored\",\"value\":{\"type\":\"White\"}}}],\"coloredOnly\":true}} "
  Spec.it s "Reduce, Heartstone's floor" $
    Common.assertCodec
      s
      CostChange.codec
      (CostChange.Type.Reduce (AppliedReduction.MkAppliedReduction (ManaCost.MkManaCost [ManaSymbol.Generic 1]) 1 False))
      " {\"type\":\"Reduce\",\"value\":{\"amount\":[{\"type\":\"Generic\",\"value\":1}],\"atLeast\":1}} "
  -- A scale of Once writes no key.
  Spec.it s "Add, Brutal Suppression's" $
    Common.assertCodec
      s
      CostChange.codec
      (CostChange.Type.Add (CostAddition.MkCostAddition [CostComponent.Sacrifice (Sacrifice.MkSacrifice (CostAmount.Fixed 1) (Filter.HasCardType CardType.Land))] CostScale.Once))
      " {\"type\":\"Add\",\"value\":{\"components\":[{\"type\":\"Sacrifice\",\"value\":{\"count\":{\"type\":\"Fixed\",\"value\":1},\"whichPermanents\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Land\"}}}}]}} "
  Spec.it s "Add, Drought's scale" $
    Common.assertCodec
      s
      CostChange.codec
      (CostChange.Type.Add (CostAddition.MkCostAddition [] (CostScale.PerColoredSymbol Color.Black)))
      " {\"type\":\"Add\",\"value\":{\"components\":[],\"scale\":{\"type\":\"PerColoredSymbol\",\"value\":{\"type\":\"Black\"}}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s CostChange.codec
