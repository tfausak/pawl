module Pawl.Codec.PlayerRelation where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.PlayerRelation as PlayerRelation

codec :: Codec.Codec PlayerRelation.PlayerRelation
codec =
  Arm.tagged
    encode
    [ Arm.nullary "You" PlayerRelation.You,
      Arm.nullary "Opponent" PlayerRelation.Opponent
    ]
  where
    encode r = Common.nullary $ case r of
      PlayerRelation.You -> "You"
      PlayerRelation.Opponent -> "Opponent"
