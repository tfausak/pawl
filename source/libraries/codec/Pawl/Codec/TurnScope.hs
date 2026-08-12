module Pawl.Codec.TurnScope where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.TurnScope as TurnScope

codec :: Codec.Codec TurnScope.TurnScope
codec =
  Arm.tagged
    encode
    [ Arm.nullary "EachTurn" TurnScope.EachTurn,
      Arm.nullary "ControllersTurn" TurnScope.ControllersTurn,
      Arm.nullary "OpponentsTurn" TurnScope.OpponentsTurn
    ]
  where
    encode s = Common.nullary $ case s of
      TurnScope.EachTurn -> "EachTurn"
      TurnScope.ControllersTurn -> "ControllersTurn"
      TurnScope.OpponentsTurn -> "OpponentsTurn"
