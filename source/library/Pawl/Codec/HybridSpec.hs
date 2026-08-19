module Pawl.Codec.HybridSpec where

import qualified Pawl.Codec.Hybrid as Hybrid
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Hybrid as Hybrid
import qualified Pawl.Types.ManaType as ManaType

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Hybrid" $ do
  -- CR 107.4e. BOTH keys are a ManaType, so the fixture names them differently
  -- on purpose: only an asymmetric case catches a codec that swapped the halves.
  Spec.it s "MkHybrid, both keys" $
    Common.assertCodec
      s
      Hybrid.codec
      (Hybrid.MkHybrid {Hybrid.left = ManaType.Colored Color.White, Hybrid.right = ManaType.Colored Color.Blue})
      " {\"left\":{\"type\":\"Colored\",\"value\":{\"type\":\"White\"}},\"right\":{\"type\":\"Colored\",\"value\":{\"type\":\"Blue\"}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s Hybrid.codec
