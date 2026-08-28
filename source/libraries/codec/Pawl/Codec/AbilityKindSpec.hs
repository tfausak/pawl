module Pawl.Codec.AbilityKindSpec where

import qualified Pawl.Codec.AbilityKind as AbilityKind
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AbilityKind as AbilityKind

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.AbilityKind" $ do
  Spec.it s "ManaAbility" $
    Common.assertCodec
      s
      AbilityKind.codec
      AbilityKind.ManaAbility
      " {\"type\":\"ManaAbility\"} "

  Spec.it s "NonManaAbility" $
    Common.assertCodec
      s
      AbilityKind.codec
      AbilityKind.NonManaAbility
      " {\"type\":\"NonManaAbility\"} "

  -- Exhaustive where the two above are literal, Pawl.Codec.KeywordFamilySpec's
  -- posture: Arm.enum derives its arms from the type.
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s AbilityKind.codec
  Spec.it s "has a schema" $ Common.assertHasSchema s AbilityKind.codec
