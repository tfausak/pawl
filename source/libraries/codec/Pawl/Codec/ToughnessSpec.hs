module Pawl.Codec.ToughnessSpec where

import qualified Pawl.Codec.Toughness as Toughness
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.Toughness as Toughness

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Toughness" $ do
  Spec.it s "MkToughness delegates to Quantity" $
    Common.assertCodec
      s
      Toughness.codec
      (Toughness.MkToughness (Quantity.Literal 2))
      " {\"type\":\"Literal\",\"value\":2} "
  Spec.it s "has a schema" $ Common.assertHasSchema s Toughness.codec
