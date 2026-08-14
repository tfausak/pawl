{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.ModifyPowerToughnessSpec where

import qualified Pawl.Codec.ModifyPowerToughness as ModifyPowerToughness
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ModifyPowerToughness as ModifyPowerToughness
import qualified Pawl.Types.Quantity as Quantity

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ModifyPowerToughness" $ do
  -- CR 613.1g, layer 7c. Asymmetric on purpose: a swapped codec would round-trip an equal pair.
  Spec.it s "MkModifyPowerToughness" $
    Common.assertCodec
      s
      ModifyPowerToughness.codec
      ( ModifyPowerToughness.MkModifyPowerToughness
          { ModifyPowerToughness.power = Quantity.Literal 1,
            ModifyPowerToughness.toughness = Quantity.Literal 2
          }
      )
      """ {"power":{"type":"Literal","value":1},"toughness":{"type":"Literal","value":2}} """
  Spec.it s "has a schema" $ Common.assertHasSchema s ModifyPowerToughness.codec
