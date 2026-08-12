{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.LayoutSpec where

import qualified Data.Either as Either
import qualified Data.Text as Text
import qualified Pawl.Codec.Layout as Layout
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Layout as Layout

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Layout" $ do
  Spec.it s "Normal" $
    Common.assertJsonCodec
      s
      Layout.toJson
      Layout.fromJson
      Layout.Normal
      """ {"type":"Normal"} """
  -- CR 709.1.
  Spec.it s "Split" $
    Common.assertJsonCodec
      s
      Layout.toJson
      Layout.fromJson
      Layout.Split
      """ {"type":"Split"} """
  -- CR 709.5.
  Spec.it s "Room" $
    Common.assertJsonCodec
      s
      Layout.toJson
      Layout.fromJson
      Layout.Room
      """ {"type":"Room"} """
  -- CR 715.1.
  Spec.it s "Adventure" $
    Common.assertJsonCodec
      s
      Layout.toJson
      Layout.fromJson
      Layout.Adventure
      """ {"type":"Adventure"} """
  -- CR 712.2.
  Spec.it s "Transforming" $
    Common.assertJsonCodec
      s
      Layout.toJson
      Layout.fromJson
      Layout.Transforming
      """ {"type":"Transforming"} """
  -- CR 712.3.
  Spec.it s "ModalDoubleFaced" $
    Common.assertJsonCodec
      s
      Layout.toJson
      Layout.fromJson
      Layout.ModalDoubleFaced
      """ {"type":"ModalDoubleFaced"} """
  -- CR 709-722 names a dozen more layouts, most of which have not landed. A
  -- file naming one must fail loudly rather than fall back to Normal, which
  -- would silently play a flip card (CR 710) as its unflipped half.
  Spec.it s "a layout that has not landed is rejected" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack """ {"type":"Flip"} """) >>= Layout.fromJson))
      "expected an unknown layout tag to fail to decode"
