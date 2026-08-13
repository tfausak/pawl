module Pawl.Codec.RevealCause where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.RevealCause as RevealCause

codec :: Codec.Codec RevealCause.RevealCause
codec =
  Arm.tagged
    encode
    [ Arm.nullary "Ordinary" RevealCause.Ordinary,
      Arm.nullary "ForMiracle" RevealCause.ForMiracle
    ]
  where
    encode c = Common.nullary $ case c of
      RevealCause.Ordinary -> "Ordinary"
      RevealCause.ForMiracle -> "ForMiracle"
