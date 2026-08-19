{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.FaceDownCharacteristics where

import qualified Pawl.Codec.Power as Power
import qualified Pawl.Codec.Toughness as Toughness
import qualified Pawl.Codec.TypeLine as TypeLine
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.FaceDownCharacteristics as FaceDownCharacteristics

-- | Every key is DEFAULTED to CR 708.2a's value, so a card lists only what its
-- own text lists: "a 2\/2 Cyberman artifact creature" writes a type line and
-- leaves the power and toughness out, and an effect that lists nothing at all
-- writes no object.
codec :: Codec.Codec FaceDownCharacteristics.FaceDownCharacteristics
codec = Fields.object $ do
  typeLine <- Fields.defaulted "typeLine" (FaceDownCharacteristics.typeLine FaceDownCharacteristics.defaultValue) TypeLine.codec FaceDownCharacteristics.typeLine
  power <- Fields.defaulted "power" (FaceDownCharacteristics.power FaceDownCharacteristics.defaultValue) (Common.maybe Power.codec) FaceDownCharacteristics.power
  toughness <- Fields.defaulted "toughness" (FaceDownCharacteristics.toughness FaceDownCharacteristics.defaultValue) (Common.maybe Toughness.codec) FaceDownCharacteristics.toughness
  pure
    FaceDownCharacteristics.MkFaceDownCharacteristics
      { FaceDownCharacteristics.typeLine = typeLine,
        FaceDownCharacteristics.power = power,
        FaceDownCharacteristics.toughness = toughness
      }
