{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.LayoutSpec where

import qualified Data.Either as Either
import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Layout as Layout
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
  -- CR 709-722 names a dozen more layouts, most of which have not landed. A
  -- file naming one must fail loudly rather than fall back to Normal, which
  -- would silently play a flip card (CR 710) as its unflipped half.
  Spec.it s "a layout that has not landed is rejected" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack """ {"type":"Flip"} """) >>= Layout.fromJson))
      "expected an unknown layout tag to fail to decode"
