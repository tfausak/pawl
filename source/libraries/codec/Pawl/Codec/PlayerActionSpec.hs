module Pawl.Codec.PlayerActionSpec where

import qualified Pawl.Codec.PlayerAction as PlayerAction
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.PlayerAction as PlayerAction

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.PlayerAction" $ do
  Spec.it s "Scry" $
    Common.assertCodec
      s
      PlayerAction.codec
      PlayerAction.Scry
      " {\"type\":\"Scry\"} "
  -- Pawl.Codec.DamageKindSpec's reason: Arm.enum derives the arm list from the
  -- type, so this is what would catch a constructor the derivation missed or two
  -- that encode alike.
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s PlayerAction.codec
  Spec.it s "has a schema" $ Common.assertHasSchema s PlayerAction.codec
