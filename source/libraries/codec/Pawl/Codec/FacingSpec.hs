module Pawl.Codec.FacingSpec where

import qualified Pawl.Codec.Facing as Facing
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.FaceDownReason as FaceDownReason
import qualified Pawl.Types.Facing as Facing

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Facing" $ do
  Spec.it s "FaceUp" $
    Common.assertCodec s Facing.codec Facing.FaceUp " {\"type\":\"FaceUp\"} "
  -- Through Facing.faceDown, the constructor every producer that lists nothing
  -- uses, so the arm is exercised the way the engine writes it.
  Spec.it s "FaceDown" $
    Common.assertCodec
      s
      Facing.codec
      (Facing.faceDown FaceDownReason.Manifested)
      " {\"type\":\"FaceDown\",\"value\":{\"reason\":{\"type\":\"Manifested\"},\"listed\":{}}} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s Facing.codec
