module Pawl.Codec.ActivatorSpec where

import qualified Pawl.Codec.Activator as Activator
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Activator as Activator

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Activator" $ do
  Spec.it s "Controller" $
    Common.assertCodec
      s
      Activator.codec
      Activator.Controller
      " {\"type\":\"Controller\"} "
  Spec.it s "AnyPlayer" $
    Common.assertCodec
      s
      Activator.codec
      Activator.AnyPlayer
      " {\"type\":\"AnyPlayer\"} "
  -- Exhaustive where the literals above are representative: Arm.enum derives the
  -- arm list from the type, so this is what would catch a constructor the
  -- derivation missed or two that encode alike.
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s Activator.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s Activator.codec
