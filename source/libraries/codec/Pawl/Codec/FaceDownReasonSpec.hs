module Pawl.Codec.FaceDownReasonSpec where

import qualified Pawl.Codec.FaceDownReason as FaceDownReason
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.FaceDownReason as FaceDownReason

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.FaceDownReason" $ do
  Spec.it s "Morphed" $
    Common.assertCodec
      s
      FaceDownReason.codec
      FaceDownReason.Morphed
      " {\"type\":\"Morphed\"} "
  Spec.it s "Disguised" $
    Common.assertCodec
      s
      FaceDownReason.codec
      FaceDownReason.Disguised
      " {\"type\":\"Disguised\"} "
  Spec.it s "Manifested" $
    Common.assertCodec
      s
      FaceDownReason.codec
      FaceDownReason.Manifested
      " {\"type\":\"Manifested\"} "
  Spec.it s "TurnedFaceDown" $
    Common.assertCodec
      s
      FaceDownReason.codec
      FaceDownReason.TurnedFaceDown
      " {\"type\":\"TurnedFaceDown\"} "
  Spec.it s "EnteredFaceDown" $
    Common.assertCodec
      s
      FaceDownReason.codec
      FaceDownReason.EnteredFaceDown
      " {\"type\":\"EnteredFaceDown\"} "
  -- Exhaustive where the literals above are representative: Arm.enum derives
  -- the arm list from the type, so this is what would catch a constructor the
  -- derivation missed or two that encode alike.
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s FaceDownReason.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s FaceDownReason.codec
