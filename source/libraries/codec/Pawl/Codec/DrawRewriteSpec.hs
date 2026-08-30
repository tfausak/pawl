module Pawl.Codec.DrawRewriteSpec where

import qualified Pawl.Codec.DrawRewrite as DrawRewrite
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.DrawRewrite as DrawRewrite

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.DrawRewrite" $ do
  -- CR 614.1a: Words of Worship.
  Spec.it s "GainLife" $
    Common.assertCodec
      s
      DrawRewrite.codec
      (DrawRewrite.GainLife 5)
      " {\"type\":\"GainLife\",\"value\":5} "
  Spec.it s "has a schema" $ Common.assertHasSchema s DrawRewrite.codec
