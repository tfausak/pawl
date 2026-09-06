module Pawl.Codec.LoyaltyKindSpec where

import qualified Pawl.Codec.LoyaltyKind as LoyaltyKind
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.LoyaltyKind as LoyaltyKind

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.LoyaltyKind" $ do
  Spec.it s "LoyaltyAbility" $
    Common.assertCodec
      s
      LoyaltyKind.codec
      LoyaltyKind.LoyaltyAbility
      " {\"type\":\"LoyaltyAbility\"} "

  Spec.it s "NonLoyaltyAbility" $
    Common.assertCodec
      s
      LoyaltyKind.codec
      LoyaltyKind.NonLoyaltyAbility
      " {\"type\":\"NonLoyaltyAbility\"} "

  -- Exhaustive where the two above are literal, Pawl.Codec.AbilityKindSpec's
  -- posture: Arm.enum derives its arms from the type.
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s LoyaltyKind.codec
  Spec.it s "has a schema" $ Common.assertHasSchema s LoyaltyKind.codec
