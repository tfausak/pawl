module Pawl.Codec.DamageDirectionSpec where

import qualified Pawl.Codec.DamageDirection as DamageDirection
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.DamageDirection as DamageDirection

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.DamageDirection" $ do
  Spec.it s "DealtTo" $
    Common.assertCodec
      s
      DamageDirection.codec
      DamageDirection.DealtTo
      " {\"type\":\"DealtTo\"} "
  Spec.it s "DealtBy" $
    Common.assertCodec
      s
      DamageDirection.codec
      DamageDirection.DealtBy
      " {\"type\":\"DealtBy\"} "
  -- Pawl.Codec.DamageKindSpec's reason: Arm.enum derives the arm list from the
  -- type, so this is what would catch a constructor the derivation missed or two
  -- that encode alike.
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s DamageDirection.codec
  Spec.it s "has a schema" $ Common.assertHasSchema s DamageDirection.codec
