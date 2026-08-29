module Pawl.Codec.ConjureDestinationSpec where

import qualified Pawl.Codec.ConjureDestination as ConjureDestination
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ConjureDestination as ConjureDestination

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ConjureDestination" $ do
  Spec.it s "Hand" $
    Common.assertCodec
      s
      ConjureDestination.codec
      ConjureDestination.Hand
      " {\"type\":\"Hand\"} "
  -- Exhaustive where the literal above is representative: Arm.enum derives the
  -- arm list from the type, so this is what would catch a constructor the
  -- derivation missed or two that encode alike.
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s ConjureDestination.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s ConjureDestination.codec
