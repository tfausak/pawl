module Pawl.Codec.MoveDurationSpec where

import qualified Pawl.Codec.MoveDuration as MoveDuration
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.MoveDuration as MoveDuration

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.MoveDuration" $ do
  Spec.it s "UntilSourceLeavesTheBattlefield" $
    Common.assertCodec
      s
      MoveDuration.codec
      MoveDuration.UntilSourceLeavesTheBattlefield
      " {\"type\":\"UntilSourceLeavesTheBattlefield\"} "
  -- Exhaustive where the literal above is representative: Arm.enum derives the
  -- arm list from the type, so this is what would catch a constructor the
  -- derivation missed or two that encode alike.
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s MoveDuration.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s MoveDuration.codec
