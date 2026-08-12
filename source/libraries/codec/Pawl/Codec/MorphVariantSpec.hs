{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.MorphVariantSpec where

import qualified Pawl.Codec.MorphVariant as MorphVariant
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.MorphVariant as MorphVariant

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.MorphVariant" $ do
  Spec.it s "Plain (CR 702.37a)" $
    Common.assertJsonCodec
      s
      MorphVariant.toJson
      MorphVariant.fromJson
      MorphVariant.Plain
      """ {"type":"Plain"} """
  Spec.it s "Mega (CR 702.37b)" $
    Common.assertJsonCodec
      s
      MorphVariant.toJson
      MorphVariant.fromJson
      MorphVariant.Mega
      """ {"type":"Mega"} """
