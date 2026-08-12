module Pawl.Codec.PlayerScope where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.PlayerScope as PlayerScope

codec :: Codec.Codec PlayerScope.PlayerScope
codec =
  Arm.tagged
    encode
    [ Arm.nullary "You" PlayerScope.You,
      Arm.nullary "Opponents" PlayerScope.Opponents,
      Arm.nullary "EachPlayer" PlayerScope.EachPlayer,
      Arm.nullary "ControllingMostPermanents" PlayerScope.ControllingMostPermanents
    ]
  where
    encode s = Common.nullary $ case s of
      PlayerScope.You -> "You"
      PlayerScope.Opponents -> "Opponents"
      PlayerScope.EachPlayer -> "EachPlayer"
      PlayerScope.ControllingMostPermanents -> "ControllingMostPermanents"
