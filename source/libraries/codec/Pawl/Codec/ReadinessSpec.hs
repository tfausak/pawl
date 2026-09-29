module Pawl.Codec.ReadinessSpec where

import qualified Pawl.Codec.Readiness as Readiness
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Readiness as Readiness.Type

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Readiness" $ do
  Spec.it s "ready is true" $
    Common.assertCodec s Readiness.codec Readiness.Type.Ready " true "
  Spec.it s "sick is false" $
    Common.assertCodec s Readiness.codec Readiness.Type.Sick " false "
  Spec.it s "has a schema" $
    Common.assertHasSchema s Readiness.codec
