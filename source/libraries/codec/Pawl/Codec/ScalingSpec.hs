{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.ScalingSpec where

import qualified Pawl.Codec.Scaling as Scaling
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Scaling as Scaling

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Scaling" $ do
  Spec.it s "Multiply" $
    Common.assertCodec
      s
      Scaling.codec
      (Scaling.Multiply 2)
      """ {"type":"Multiply","value":2} """
  Spec.it s "AddMore" $
    Common.assertCodec
      s
      Scaling.codec
      (Scaling.AddMore 1)
      """ {"type":"AddMore","value":1} """
  Spec.it s "Halve" $
    Common.assertCodec
      s
      Scaling.codec
      Scaling.Halve
      """ {"type":"Halve"} """
  Spec.it s "has a schema" $
    Common.assertHasSchema s Scaling.codec
