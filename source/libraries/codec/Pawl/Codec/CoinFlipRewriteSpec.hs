module Pawl.Codec.CoinFlipRewriteSpec where

import qualified Pawl.Codec.CoinFlipRewrite as CoinFlipRewrite
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CoinFlipRewrite as CoinFlipRewrite

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CoinFlipRewrite" $ do
  -- CR 705.1: Krark's Thumb.
  Spec.it s "Doubled" $
    Common.assertCodec
      s
      CoinFlipRewrite.codec
      CoinFlipRewrite.Doubled
      " {\"type\":\"Doubled\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s CoinFlipRewrite.codec
