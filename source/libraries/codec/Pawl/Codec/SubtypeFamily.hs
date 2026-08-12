module Pawl.Codec.SubtypeFamily where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.SubtypeFamily as SubtypeFamily

codec :: Codec.Codec SubtypeFamily.SubtypeFamily
codec =
  Arm.tagged
    encode
    [ Arm.nullary "BasicLandType" SubtypeFamily.BasicLandType,
      Arm.nullary "CreatureType" SubtypeFamily.CreatureType
    ]
  where
    encode f = Common.nullary $ case f of
      SubtypeFamily.BasicLandType -> "BasicLandType"
      SubtypeFamily.CreatureType -> "CreatureType"
