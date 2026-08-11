{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.PowerSpec where

import qualified Pawl.Codec.Power as Power
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Power as Power
import qualified Pawl.Types.Quantity as Quantity

spec :: (Monad m) => Spec.Spec m n -> n ()
spec s =
  Spec.describe s "Pawl.Codec.Power" . Spec.it s "MkPower delegates to Quantity" $
    Common.assertJsonCodec
      s
      Power.toJson
      Power.fromJson
      (Power.MkPower (Quantity.Literal 2))
      """ {"type":"Literal","value":2} """
