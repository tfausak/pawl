module Pawl.Codec.PlayerCounterKind where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind

codec :: Codec.Codec PlayerCounterKind.PlayerCounterKind
codec =
  Arm.tagged
    encode
    [ Arm.nullary "Energy" PlayerCounterKind.Energy,
      Arm.nullary "Poison" PlayerCounterKind.Poison,
      Arm.nullary "Rad" PlayerCounterKind.Rad,
      Arm.nullary "Experience" PlayerCounterKind.Experience
    ]
  where
    encode k = Common.nullary $ case k of
      PlayerCounterKind.Energy -> "Energy"
      PlayerCounterKind.Poison -> "Poison"
      PlayerCounterKind.Rad -> "Rad"
      PlayerCounterKind.Experience -> "Experience"
