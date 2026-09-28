module Pawl.Codec.ScryRewriteSpec where

import qualified Pawl.Codec.ScryRewrite as ScryRewrite
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ScryRewrite as ScryRewrite

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ScryRewrite" $ do
  -- CR 701.22a: Kenessos, Priest of Thassa.
  Spec.it s "PlusOne" $
    Common.assertCodec
      s
      ScryRewrite.codec
      ScryRewrite.PlusOne
      " {\"type\":\"PlusOne\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ScryRewrite.codec
