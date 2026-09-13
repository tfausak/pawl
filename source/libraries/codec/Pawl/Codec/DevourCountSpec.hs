module Pawl.Codec.DevourCountSpec where

import qualified Pawl.Codec.DevourCount as DevourCount
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.DevourCount as DevourCount

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.DevourCount" $ do
  -- CR 702.82a: Thunder-Thrash Elder's devour 3.
  Spec.it s "Fixed carries its N" $
    Common.assertCodec s DevourCount.codec (DevourCount.Fixed 3) " {\"type\":\"Fixed\",\"value\":3} "
  -- CR 702.82b: Thromok the Insatiable's devour X.
  Spec.it s "Devoured carries none" $
    Common.assertCodec s DevourCount.codec DevourCount.Devoured " {\"type\":\"Devoured\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s DevourCount.codec
