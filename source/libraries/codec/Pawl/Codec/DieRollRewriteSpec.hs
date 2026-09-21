module Pawl.Codec.DieRollRewriteSpec where

import qualified Pawl.Codec.DieRollRewrite as DieRollRewrite
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.DieRollRewrite as DieRollRewrite

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.DieRollRewrite" $ do
  -- CR 706.6: Pixie Guide.
  Spec.it s "ExtraIgnoringLowest" $
    Common.assertCodec
      s
      DieRollRewrite.codec
      DieRollRewrite.ExtraIgnoringLowest
      " {\"type\":\"ExtraIgnoringLowest\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s DieRollRewrite.codec
