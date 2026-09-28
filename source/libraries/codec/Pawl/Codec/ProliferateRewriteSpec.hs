module Pawl.Codec.ProliferateRewriteSpec where

import qualified Pawl.Codec.ProliferateRewrite as ProliferateRewrite
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ProliferateRewrite as ProliferateRewrite

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ProliferateRewrite" $ do
  -- CR 701.34a: Tekuthal, Inquiry Dominus.
  Spec.it s "Doubled" $
    Common.assertCodec
      s
      ProliferateRewrite.codec
      ProliferateRewrite.Doubled
      " {\"type\":\"Doubled\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ProliferateRewrite.codec
