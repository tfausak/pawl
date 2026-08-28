module Pawl.Codec.LifeLossRewriteSpec where

import qualified Pawl.Codec.LifeLossRewrite as LifeLossRewrite
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.LifeLossRewrite as LifeLossRewrite

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.LifeLossRewrite" $ do
  -- Worship's "reduces it to 1 instead".
  Spec.it s "LeaveAtLeast" $
    Common.assertCodec
      s
      LifeLossRewrite.codec
      (LifeLossRewrite.LeaveAtLeast 1)
      " {\"type\":\"LeaveAtLeast\",\"value\":1} "
  Spec.it s "has a schema" $ Common.assertHasSchema s LifeLossRewrite.codec
