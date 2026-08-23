module Pawl.Codec.PlayPermissionOriginSpec where

import qualified Pawl.Codec.PlayPermissionOrigin as PlayPermissionOrigin
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.PlayPermissionOrigin as PlayPermissionOrigin

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.PlayPermissionOrigin" $ do
  -- CR 715.3d, whose last clause is the only thing that asks which rule granted
  -- the permission.
  Spec.it s "Adventure" $
    Common.assertCodec
      s
      PlayPermissionOrigin.codec
      PlayPermissionOrigin.Adventure
      " {\"type\":\"Adventure\"} "
  -- CR 601.3, via Effect.GrantPlayFromExile: no Adventure exclusion rides here.
  Spec.it s "Granted" $
    Common.assertCodec
      s
      PlayPermissionOrigin.codec
      PlayPermissionOrigin.Granted
      " {\"type\":\"Granted\"} "
  -- Exhaustive where the literals above are representative: Arm.enum derives
  -- the arm list from the type, so this is what would catch a constructor the
  -- derivation missed or two that encode alike.
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s PlayPermissionOrigin.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s PlayPermissionOrigin.codec
