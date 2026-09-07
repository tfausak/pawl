module Pawl.Codec.ControlDurationSpec where

import qualified Pawl.Codec.ControlDuration as ControlDuration
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ControlDuration as ControlDuration

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
  -- Exhaustive where the literals above are representative: Arm.enum derives the
  -- arm list from the type, so this is what would catch a constructor the
  -- derivation missed or two that encode alike.
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s ControlDuration.codec
  Spec.it s "has a schema" $ Common.assertHasSchema s ControlDuration.codec
