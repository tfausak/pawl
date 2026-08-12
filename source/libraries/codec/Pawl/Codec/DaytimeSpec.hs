{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.DaytimeSpec where

import qualified Pawl.Codec.Daytime as Daytime
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Daytime as Daytime

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Daytime" $ do
  Spec.it s "Day" $
    Common.assertCodec
      s
      Daytime.codec
      Daytime.Day
      """ {"type":"Day"} """
  Spec.it s "Night" $
    Common.assertCodec
      s
      Daytime.codec
      Daytime.Night
      """ {"type":"Night"} """
  Spec.it s "has a schema" $
    Common.assertHasSchema s Daytime.codec
