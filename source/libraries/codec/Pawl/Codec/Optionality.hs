module Pawl.Codec.Optionality where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Optionality as Optionality

codec :: Codec.Codec Optionality.Optionality
codec =
  Arm.tagged
    encode
    [ Arm.nullary "Mandatory" Optionality.Mandatory,
      Arm.nullary "Optional" Optionality.Optional
    ]
  where
    encode o = Common.nullary $ case o of
      Optionality.Mandatory -> "Mandatory"
      Optionality.Optional -> "Optional"
