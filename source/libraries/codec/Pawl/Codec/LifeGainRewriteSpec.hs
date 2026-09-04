module Pawl.Codec.LifeGainRewriteSpec where

import qualified Pawl.Codec.LifeGainRewrite as LifeGainRewrite
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.LifeGainRewrite as LifeGainRewrite
import qualified Pawl.Types.Scaling as Scaling

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.LifeGainRewrite" $ do
  -- Boon Reflection's "you gain twice that much life instead".
  Spec.it s "Scaled" $
    Common.assertCodec
      s
      LifeGainRewrite.codec
      (LifeGainRewrite.Scaled (Scaling.Multiply 2))
      " {\"type\":\"Scaled\",\"value\":{\"type\":\"Multiply\",\"value\":2}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s LifeGainRewrite.codec
