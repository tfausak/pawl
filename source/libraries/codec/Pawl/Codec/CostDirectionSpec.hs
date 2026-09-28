module Pawl.Codec.CostDirectionSpec where

import qualified Pawl.Codec.CostDirection as CostDirection
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CostDirection as CostDirection

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CostDirection" $ do
  Spec.it s "Less" $
    Common.assertCodec
      s
      CostDirection.codec
      CostDirection.Less
      " {\"type\":\"Less\"} "
  Spec.it s "More" $
    Common.assertCodec
      s
      CostDirection.codec
      CostDirection.More
      " {\"type\":\"More\"} "
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s CostDirection.codec
  Spec.it s "has a schema" $ Common.assertHasSchema s CostDirection.codec
