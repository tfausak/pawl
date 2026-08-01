module Pawl.Codec.DamageRewriteSpec where

import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.DamageRewrite as DamageRewrite
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.DamageRewrite as DamageRewrite

spec :: (Monad m) => Spec.Spec m n -> n ()
spec s =
  Spec.describe s "Pawl.Codec.DamageRewrite" . Spec.it s "PreventAll" $
    Common.assertJsonCodec s DamageRewrite.toJson DamageRewrite.fromJson DamageRewrite.PreventAll "{\"type\":\"PreventAll\"}"
