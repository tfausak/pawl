module Pawl.Codec.MorphVariantSpec where

import qualified Pawl.Codec.MorphVariant as MorphVariant
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.MorphVariant as MorphVariant

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.MorphVariant" $ do
  Spec.it s "Plain (CR 702.37a)" $
    Common.assertCodec
      s
      MorphVariant.codec
      MorphVariant.Plain
      " {\"type\":\"Plain\"} "
  Spec.it s "Mega (CR 702.37b)" $
    Common.assertCodec
      s
      MorphVariant.codec
      MorphVariant.Mega
      " {\"type\":\"Mega\"} "
  -- Exhaustive where the literals above are representative: Arm.enum derives
  -- the arm list from the type, so this is what would catch a constructor the
  -- derivation missed or two that encode alike.
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s MorphVariant.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s MorphVariant.codec
