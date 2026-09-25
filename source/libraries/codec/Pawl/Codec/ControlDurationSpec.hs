module Pawl.Codec.ControlDurationSpec where

import qualified Pawl.Codec.ControlDuration as ControlDuration
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ControlDuration as ControlDuration
import qualified Pawl.Types.ObjectId as ObjectId

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ControlDuration" $ do
  Spec.it s "UntilTurnEnds" $
    Common.assertCodec
      s
      ControlDuration.codec
      ControlDuration.UntilTurnEnds
      " {\"type\":\"UntilTurnEnds\"} "
  Spec.it s "UntilResolutionEnds" $
    Common.assertCodec
      s
      ControlDuration.codec
      ControlDuration.UntilResolutionEnds
      " {\"type\":\"UntilResolutionEnds\"} "
  Spec.it s "WhileResolving" $
    Common.assertCodec
      s
      ControlDuration.codec
      (ControlDuration.WhileResolving (ObjectId.MkObjectId 7))
      " {\"type\":\"WhileResolving\",\"value\":7} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ControlDuration.codec
