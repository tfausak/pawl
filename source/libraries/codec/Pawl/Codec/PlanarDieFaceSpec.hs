module Pawl.Codec.PlanarDieFaceSpec where

import qualified Pawl.Codec.PlanarDieFace as PlanarDieFace
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.PlanarDieFace as PlanarDieFace

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.PlanarDieFace" $ do
  Spec.it s "Chaos" $
    Common.assertCodec
      s
      PlanarDieFace.codec
      PlanarDieFace.Chaos
      " {\"type\":\"Chaos\"} "
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s PlanarDieFace.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s PlanarDieFace.codec
