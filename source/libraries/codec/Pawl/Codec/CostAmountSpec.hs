module Pawl.Codec.CostAmountSpec where

import qualified Pawl.Codec.CostAmount as CostAmount
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CostAmount as CostAmount

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CostAmount" $ do
  Spec.it s "Fixed" $
    Common.assertCodec
      s
      CostAmount.codec
      (CostAmount.Fixed 3)
      " {\"type\":\"Fixed\",\"value\":3} "
  Spec.it s "AnnouncedX" $
    Common.assertCodec
      s
      CostAmount.codec
      CostAmount.AnnouncedX
      " {\"type\":\"AnnouncedX\"} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s CostAmount.codec
