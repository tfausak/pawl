module Pawl.Codec.DrawCountRewriteSpec where

import qualified Pawl.Codec.DrawCountRewrite as DrawCountRewrite
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.DrawCountRewrite as DrawCountRewrite

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.DrawCountRewrite" $ do
  -- CR 121.2a: Alms Collector.
  Spec.it s "EachDrawOne" $
    Common.assertCodec
      s
      DrawCountRewrite.codec
      DrawCountRewrite.EachDrawOne
      " {\"type\":\"EachDrawOne\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s DrawCountRewrite.codec
