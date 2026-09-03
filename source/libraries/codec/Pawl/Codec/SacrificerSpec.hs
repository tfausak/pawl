module Pawl.Codec.SacrificerSpec where

import qualified Pawl.Codec.Sacrificer as Sacrificer
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Sacrificer as Sacrificer

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Sacrificer" $ do
  Spec.it s "EffectController" $
    Common.assertCodec
      s
      Sacrificer.codec
      Sacrificer.EffectController
      " {\"type\":\"EffectController\"} "
  Spec.it s "PermanentController" $
    Common.assertCodec
      s
      Sacrificer.codec
      Sacrificer.PermanentController
      " {\"type\":\"PermanentController\"} "
  -- Exhaustive where the two literals above are representative, Counterability's
  -- reason: Arm.enum derives the arm list from the type.
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s Sacrificer.codec
  Spec.it s "has a schema" $ Common.assertHasSchema s Sacrificer.codec
