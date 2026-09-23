module Pawl.Codec.PermissionVerbSpec where

import qualified Pawl.Codec.PermissionVerb as PermissionVerb
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.PermissionVerb as PermissionVerb

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.PermissionVerb" $ do
  Spec.it s "Cast" $
    Common.assertCodec
      s
      PermissionVerb.codec
      PermissionVerb.Cast
      " {\"type\":\"Cast\"} "
  Spec.it s "Play" $
    Common.assertCodec
      s
      PermissionVerb.codec
      PermissionVerb.Play
      " {\"type\":\"Play\"} "
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s PermissionVerb.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s PermissionVerb.codec
