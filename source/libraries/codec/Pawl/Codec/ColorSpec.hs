{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.ColorSpec where

import qualified Pawl.Codec.Color as Color
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Color as Color

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Color" $ do
  Spec.it s "White" $
    Common.assertCodec
      s
      Color.codec
      Color.White
      """ {"type":"White"} """
  Spec.it s "Blue" $
    Common.assertCodec
      s
      Color.codec
      Color.Blue
      """ {"type":"Blue"} """
  Spec.it s "Black" $
    Common.assertCodec
      s
      Color.codec
      Color.Black
      """ {"type":"Black"} """
  Spec.it s "Red" $
    Common.assertCodec
      s
      Color.codec
      Color.Red
      """ {"type":"Red"} """
  Spec.it s "Green" $
    Common.assertCodec
      s
      Color.codec
      Color.Green
      """ {"type":"Green"} """
  Spec.it s "unknown tag fails" $
    Spec.assertBool s (either (const True) (const False) (Codec.decode Color.codec (Value.object []))) "left"
  Spec.it s "has a schema" $
    Common.assertHasSchema s Color.codec
