module Pawl.Codec.TriggerLimitSpec where

import qualified Pawl.Codec.TriggerLimit as TriggerLimit
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.TriggerLimit as TriggerLimit

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.TriggerLimit" $ do
  Spec.it s "Unlimited" $
    Common.assertCodec
      s
      TriggerLimit.codec
      TriggerLimit.Unlimited
      " {\"type\":\"Unlimited\"} "
  Spec.it s "OncePerTurn" $
    Common.assertCodec
      s
      TriggerLimit.codec
      TriggerLimit.OncePerTurn
      " {\"type\":\"OncePerTurn\"} "
  -- Exhaustive where the literals above are representative: Arm.enum derives
  -- the arm list from the type, so this is what would catch a constructor the
  -- derivation missed or two that encode alike.
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s TriggerLimit.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s TriggerLimit.codec
