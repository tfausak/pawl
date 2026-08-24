module Pawl.Codec.ManaRiderEffectSpec where

import qualified Pawl.Codec.ManaRiderEffect as ManaRiderEffect
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ManaRiderEffect as ManaRiderEffect

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ManaRiderEffect" $ do
  Spec.it s "CantBeCountered" $
    Common.assertCodec
      s
      ManaRiderEffect.codec
      ManaRiderEffect.CantBeCountered
      " {\"type\":\"CantBeCountered\"} "
  -- Arm.enum derives the arm list from the type, so this is what would catch a
  -- second payload that encoded like the first.
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s ManaRiderEffect.codec
  Spec.it s "has a schema" $ Common.assertHasSchema s ManaRiderEffect.codec
