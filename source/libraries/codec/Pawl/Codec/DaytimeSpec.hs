{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.DaytimeSpec where

import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Daytime as Daytime
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Daytime as Daytime

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Daytime" $ do
  Spec.it s "Day" $
    Common.assertJsonCodec
      s
      Daytime.toJson
      Daytime.fromJson
      Daytime.Day
      """ {"type":"Day"} """
  Spec.it s "Night" $
    Common.assertJsonCodec
      s
      Daytime.toJson
      Daytime.fromJson
      Daytime.Night
      """ {"type":"Night"} """
