{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.ColorSpec where

import qualified Pawl.Codec.Color as Color
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Color as Color

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Color" $ do
  Spec.it s "White" $
    Common.assertJsonCodec
      s
      Color.toJson
      Color.fromJson
      Color.White
      """ {"type":"White"} """
  Spec.it s "Blue" $
    Common.assertJsonCodec
      s
      Color.toJson
      Color.fromJson
      Color.Blue
      """ {"type":"Blue"} """
  Spec.it s "Black" $
    Common.assertJsonCodec
      s
      Color.toJson
      Color.fromJson
      Color.Black
      """ {"type":"Black"} """
  Spec.it s "Red" $
    Common.assertJsonCodec
      s
      Color.toJson
      Color.fromJson
      Color.Red
      """ {"type":"Red"} """
  Spec.it s "Green" $
    Common.assertJsonCodec
      s
      Color.toJson
      Color.fromJson
      Color.Green
      """ {"type":"Green"} """
  Spec.it s "unknown tag fails" $
    Spec.assertBool s (either (const True) (const False) (Color.fromJson (Common.object []))) "left"
