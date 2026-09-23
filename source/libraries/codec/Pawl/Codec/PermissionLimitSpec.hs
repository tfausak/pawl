module Pawl.Codec.PermissionLimitSpec where

import qualified Pawl.Codec.PermissionLimit as PermissionLimit
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.PermissionLimit as PermissionLimit

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.PermissionLimit" $ do
  Spec.it s "Unlimited" $
    Common.assertCodec
      s
      PermissionLimit.codec
      PermissionLimit.Unlimited
      " {\"type\":\"Unlimited\"} "
  Spec.it s "OnceEachTurn" $
    Common.assertCodec
      s
      PermissionLimit.codec
      PermissionLimit.OnceEachTurn
      " {\"type\":\"OnceEachTurn\"} "
  Spec.it s "OnceEachOfYourTurns" $
    Common.assertCodec
      s
      PermissionLimit.codec
      PermissionLimit.OnceEachOfYourTurns
      " {\"type\":\"OnceEachOfYourTurns\"} "
  -- Exhaustive where the literals above are representative: Arm.enum derives the
  -- arm list from the type, so this is what would catch a constructor the
  -- derivation missed or two that encode alike.
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s PermissionLimit.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s PermissionLimit.codec
