{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.DiscardCauseSpec where

import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.DiscardCause as DiscardCause
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.DiscardCause as DiscardCause

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.DiscardCause" $ do
  Spec.it s "Ordinary" $
    Common.assertJsonCodec
      s
      DiscardCause.toJson
      DiscardCause.fromJson
      DiscardCause.Ordinary
      """ {"type":"Ordinary"} """
  Spec.it s "ToPayCyclingCost" $
    Common.assertJsonCodec
      s
      DiscardCause.toJson
      DiscardCause.fromJson
      DiscardCause.ToPayCyclingCost
      """ {"type":"ToPayCyclingCost"} """
