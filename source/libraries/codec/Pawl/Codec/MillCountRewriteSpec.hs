module Pawl.Codec.MillCountRewriteSpec where

import qualified Pawl.Codec.MillCountRewrite as MillCountRewrite
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.MillCountRewrite as MillCountRewrite
import qualified Pawl.Types.Scaling as Scaling

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.MillCountRewrite" $ do
  -- Bruvac the Grandiloquent's "they mill twice that many cards instead".
  Spec.it s "Scaled" $
    Common.assertCodec
      s
      MillCountRewrite.codec
      (MillCountRewrite.Scaled (Scaling.Multiply 2))
      " {\"type\":\"Scaled\",\"value\":{\"type\":\"Multiply\",\"value\":2}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s MillCountRewrite.codec
