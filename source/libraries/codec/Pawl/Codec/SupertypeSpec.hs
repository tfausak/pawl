{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.SupertypeSpec where

import qualified Pawl.Codec.Supertype as Supertype
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Supertype as Supertype

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Supertype" $ do
  Spec.it s "Basic" $
    Common.assertJsonCodec
      s
      Supertype.toJson
      Supertype.fromJson
      Supertype.Basic
      """ {"type":"Basic"} """

  Spec.it s "Legendary" $
    Common.assertJsonCodec
      s
      Supertype.toJson
      Supertype.fromJson
      Supertype.Legendary
      """ {"type":"Legendary"} """

  Spec.it s "Ongoing" $
    Common.assertJsonCodec
      s
      Supertype.toJson
      Supertype.fromJson
      Supertype.Ongoing
      """ {"type":"Ongoing"} """

  Spec.it s "Snow" $
    Common.assertJsonCodec
      s
      Supertype.toJson
      Supertype.fromJson
      Supertype.Snow
      """ {"type":"Snow"} """

  Spec.it s "World" $
    Common.assertJsonCodec
      s
      Supertype.toJson
      Supertype.fromJson
      Supertype.World
      """ {"type":"World"} """
