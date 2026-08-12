module Pawl.Codec.ControllerRelation where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.ControllerRelation as ControllerRelation

codec :: Codec.Codec ControllerRelation.ControllerRelation
codec =
  Arm.tagged
    encode
    [ Arm.nullary "Yours" ControllerRelation.Yours,
      Arm.nullary "Anyones" ControllerRelation.Anyones,
      Arm.nullary "Opponents" ControllerRelation.Opponents
    ]
  where
    encode r = Common.nullary $ case r of
      ControllerRelation.Yours -> "Yours"
      ControllerRelation.Anyones -> "Anyones"
      ControllerRelation.Opponents -> "Opponents"
