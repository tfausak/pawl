module Pawl.Codec.PowerSpec where

import qualified Pawl.Codec.Power as Power
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Power as Power
import qualified Pawl.Types.Quantity as Quantity

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Power" $ do
  Spec.it s "MkPower delegates to Quantity" $
    Common.assertCodec
      s
      Power.codec
      (Power.MkPower (Quantity.Literal 2))
      " {\"type\":\"Literal\",\"value\":2} "
  Spec.it s "has a schema" $ Common.assertHasSchema s Power.codec
