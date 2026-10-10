module Pawl.Codec.PlayerAction where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.PlayerAction as PlayerAction

codec :: Codec.Codec PlayerAction.PlayerAction
codec = Arm.enum
