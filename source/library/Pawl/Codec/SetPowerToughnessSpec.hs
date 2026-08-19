module Pawl.Codec.SetPowerToughnessSpec where

import qualified Pawl.Codec.SetPowerToughness as SetPowerToughness
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.SetPowerToughness as SetPowerToughness

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.SetPowerToughness" $ do
  -- CR 707.9b. The fixture is deliberately NOT square: Quicksilver Gargantuan's
  -- own 7/7 would round-trip a codec that swapped the two boxes.
  Spec.it s "MkSetPowerToughness, an asymmetric body" $
    Common.assertCodec
      s
      SetPowerToughness.codec
      (SetPowerToughness.MkSetPowerToughness {SetPowerToughness.power = 3, SetPowerToughness.toughness = 4})
      " {\"power\":3,\"toughness\":4} "
  Spec.it s "has a schema" $ Common.assertHasSchema s SetPowerToughness.codec
