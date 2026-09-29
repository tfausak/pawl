module Pawl.Codec.PermissionPoolSpec where

import qualified Pawl.Codec.PermissionPool as PermissionPool
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.PermissionPool as PermissionPool

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.PermissionPool" $ do
  Spec.it s "CardsExiledWithSource" $
    Common.assertCodec
      s
      PermissionPool.codec
      PermissionPool.CardsExiledWithSource
      " {\"type\":\"CardsExiledWithSource\"} "
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s PermissionPool.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s PermissionPool.codec
