{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.SubtypeFamilySpec where

import qualified Pawl.Codec.SubtypeFamily as SubtypeFamily
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.SubtypeFamily as SubtypeFamily

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.SubtypeFamily" $ do
  Spec.it s "BasicLandType" $
    Common.assertJsonCodec
      s
      SubtypeFamily.toJson
      SubtypeFamily.fromJson
      SubtypeFamily.BasicLandType
      """ {"type":"BasicLandType"} """
  Spec.it s "CreatureType" $
    Common.assertJsonCodec
      s
      SubtypeFamily.toJson
      SubtypeFamily.fromJson
      SubtypeFamily.CreatureType
      """ {"type":"CreatureType"} """
