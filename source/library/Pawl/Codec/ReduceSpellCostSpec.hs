module Pawl.Codec.ReduceSpellCostSpec where

import qualified Pawl.Codec.ReduceSpellCost as ReduceSpellCost
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.ReduceSpellCost as ReduceSpellCost

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ReduceSpellCost" $ do
  -- CR 118.7, as Sapphire Medallion reduces a blue spell by {1}.
  Spec.it s "MkReduceSpellCost" $
    Common.assertCodec
      s
      ReduceSpellCost.codec
      ( ReduceSpellCost.MkReduceSpellCost
          { ReduceSpellCost.whichSpells = Filter.HasColor Color.Blue,
            ReduceSpellCost.reduction = ManaCost.MkManaCost [ManaSymbol.Generic 1]
          }
      )
      " {\"whichSpells\":{\"type\":\"HasColor\",\"value\":{\"type\":\"Blue\"}},\"reduction\":[{\"type\":\"Generic\",\"value\":1}]} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ReduceSpellCost.codec
