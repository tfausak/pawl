module Pawl.Codec.Designation where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Designation as Designation

codec :: Codec.Codec Designation.Designation
codec =
  Arm.tagged
    encode
    [ Arm.nullary "Renowned" Designation.Renowned,
      Arm.nullary "Monstrous" Designation.Monstrous,
      Arm.nullary "Suspected" Designation.Suspected
    ]
  where
    encode d = Common.nullary $ case d of
      Designation.Renowned -> "Renowned"
      Designation.Monstrous -> "Monstrous"
      Designation.Suspected -> "Suspected"
