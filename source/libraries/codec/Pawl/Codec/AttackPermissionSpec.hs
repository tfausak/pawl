module Pawl.Codec.AttackPermissionSpec where

import qualified Pawl.Codec.AttackPermission as AttackPermission
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.AttackPermission as AttackPermission
import qualified Pawl.Types.Filter as Filter

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.AttackPermission" $ do
  -- Prison Barricade's kicked grant (CR 702.3b): "This creature".
  Spec.it s "MkAttackPermission" $
    Common.assertCodec
      s
      AttackPermission.codec
      (AttackPermission.MkAttackPermission (Affected.Matching Filter.IsSource))
      " {\"affected\":{\"type\":\"Matching\",\"value\":{\"type\":\"IsSource\"}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s AttackPermission.codec
