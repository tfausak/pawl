{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.CoinFlipR where

import qualified Pawl.Codec.CoinFlipRewrite as CoinFlipRewrite
import qualified Pawl.Codec.ControllerRelation as ControllerRelation
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.CoinFlipR as CoinFlipR

codec :: Codec.Codec CoinFlipR.CoinFlipR
codec = Fields.object $ do
  whose <- Fields.required "whose" ControllerRelation.codec CoinFlipR.whose
  rewrite <- Fields.required "rewrite" CoinFlipRewrite.codec CoinFlipR.rewrite
  pure
    CoinFlipR.MkCoinFlipR
      { CoinFlipR.whose = whose,
        CoinFlipR.rewrite = rewrite
      }
