module Pawl.Codec.MeasureSpec where

import qualified Pawl.Codec.Measure as Measure
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Measure as Measure

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Measure" $ do
  Spec.it s "ManaValue" $
    Common.assertCodec
      s
      Measure.codec
      Measure.ManaValue
      " {\"type\":\"ManaValue\"} "
  -- Pawl.Codec.DamageKindSpec's reason: Arm.enum derives the arm list from the
  -- type, so this is what would catch a constructor the derivation missed or two
  -- that encode alike.
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s Measure.codec
  Spec.it s "has a schema" $ Common.assertHasSchema s Measure.codec
