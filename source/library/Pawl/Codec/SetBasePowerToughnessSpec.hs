module Pawl.Codec.SetBasePowerToughnessSpec where

import qualified Pawl.Codec.SetBasePowerToughness as SetBasePowerToughness
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.SetBasePowerToughness as SetBasePowerToughness

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.SetBasePowerToughness" $ do
  -- CR 613.1g, layer 7b. Asymmetric on purpose: a swapped codec would round-trip an equal pair.
  Spec.it s "MkSetBasePowerToughness" $
    Common.assertCodec
      s
      SetBasePowerToughness.codec
      ( SetBasePowerToughness.MkSetBasePowerToughness
          { SetBasePowerToughness.power = Quantity.Literal 1,
            SetBasePowerToughness.toughness = Quantity.Literal 2
          }
      )
      " {\"power\":{\"type\":\"Literal\",\"value\":1},\"toughness\":{\"type\":\"Literal\",\"value\":2}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s SetBasePowerToughness.codec
