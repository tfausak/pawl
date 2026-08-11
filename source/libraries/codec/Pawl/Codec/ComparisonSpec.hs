{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.ComparisonSpec where

import qualified Pawl.Codec.Comparison as Comparison
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Comparison as Comparison

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Comparison" $ do
  Spec.it s "Exactly" $
    Common.assertJsonCodec
      s
      Comparison.toJson
      Comparison.fromJson
      Comparison.Exactly
      """ {"type":"Exactly"} """
  Spec.it s "AtLeast" $
    Common.assertJsonCodec
      s
      Comparison.toJson
      Comparison.fromJson
      Comparison.AtLeast
      """ {"type":"AtLeast"} """
  Spec.it s "AtMost" $
    Common.assertJsonCodec
      s
      Comparison.toJson
      Comparison.fromJson
      Comparison.AtMost
      """ {"type":"AtMost"} """
