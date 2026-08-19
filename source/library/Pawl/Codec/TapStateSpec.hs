module Pawl.Codec.TapStateSpec where

import qualified Pawl.Codec.TapState as TapState
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.TapState as TapState

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.TapState" $ do
  Spec.it s "Untapped" $
    Common.assertCodec
      s
      TapState.codec
      TapState.Untapped
      " {\"type\":\"Untapped\"} "
  Spec.it s "Tapped" $
    Common.assertCodec
      s
      TapState.codec
      TapState.Tapped
      " {\"type\":\"Tapped\"} "
  -- Exhaustive where the literals above are representative: Arm.enum derives
  -- the arm list from the type, so this is what would catch a constructor the
  -- derivation missed or two that encode alike.
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s TapState.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s TapState.codec
