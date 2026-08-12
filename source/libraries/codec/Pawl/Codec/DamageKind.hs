module Pawl.Codec.DamageKind where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.DamageKind as DamageKind

codec :: Codec.Codec DamageKind.DamageKind
codec =
  Arm.tagged
    encode
    [ Arm.nullary "Combat" DamageKind.Combat,
      Arm.nullary "Noncombat" DamageKind.Noncombat
    ]
  where
    encode k = Common.nullary $ case k of
      DamageKind.Combat -> "Combat"
      DamageKind.Noncombat -> "Noncombat"
