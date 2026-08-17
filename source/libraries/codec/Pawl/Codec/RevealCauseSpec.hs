module Pawl.Codec.RevealCauseSpec where

import qualified Pawl.Codec.RevealCause as RevealCause
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.RevealCause as RevealCause

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.RevealCause" $ do
  Spec.it s "Ordinary" $
    Common.assertCodec
      s
      RevealCause.codec
      RevealCause.Ordinary
      " {\"type\":\"Ordinary\"} "
  Spec.it s "ForMiracle" $
    Common.assertCodec
      s
      RevealCause.codec
      RevealCause.ForMiracle
      " {\"type\":\"ForMiracle\"} "
  -- Exhaustive where the literals above are representative: Arm.enum derives
  -- the arm list from the type, so this is what would catch a constructor the
  -- derivation missed or two that encode alike.
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s RevealCause.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s RevealCause.codec
