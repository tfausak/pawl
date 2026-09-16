module Pawl.Codec.CoinFlipRSpec where

import qualified Pawl.Codec.CoinFlipR as CoinFlipR
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CoinFlipR as CoinFlipR
import qualified Pawl.Types.CoinFlipRewrite as CoinFlipRewrite
import qualified Pawl.Types.ControllerRelation as ControllerRelation

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CoinFlipR" $ do
  -- CR 705.1: Krark's Thumb.
  Spec.it s "MkCoinFlipR" $
    Common.assertCodec
      s
      CoinFlipR.codec
      (CoinFlipR.MkCoinFlipR ControllerRelation.Yours CoinFlipRewrite.Doubled)
      " {\"whose\":{\"type\":\"Yours\"},\"rewrite\":{\"type\":\"Doubled\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s CoinFlipR.codec
