{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.DiscardCauseSpec where

import qualified Pawl.Codec.DiscardCause as DiscardCause
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.DiscardCause as DiscardCause

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.DiscardCause" $ do
  Spec.it s "Ordinary" $
    Common.assertCodec
      s
      DiscardCause.codec
      DiscardCause.Ordinary
      """ {"type":"Ordinary"} """
  Spec.it s "ToPayCyclingCost" $
    Common.assertCodec
      s
      DiscardCause.codec
      DiscardCause.ToPayCyclingCost
      """ {"type":"ToPayCyclingCost"} """
  Spec.it s "has a schema" $
    Common.assertHasSchema s DiscardCause.codec
