module Pawl.Codec.PermanentActionSpec where

import qualified Pawl.Codec.PermanentAction as PermanentAction
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.PermanentAction as PermanentAction

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.PermanentAction" $ do
  Spec.it s "Evolve" $
    Common.assertCodec
      s
      PermanentAction.codec
      PermanentAction.Evolve
      " {\"type\":\"Evolve\"} "
  -- Pawl.Codec.DamageKindSpec's reason: Arm.enum derives the arm list from the
  -- type, so this is what would catch a constructor the derivation missed or two
  -- that encode alike.
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s PermanentAction.codec
  Spec.it s "has a schema" $ Common.assertHasSchema s PermanentAction.codec
