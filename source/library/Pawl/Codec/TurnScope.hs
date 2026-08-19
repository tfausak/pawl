module Pawl.Codec.TurnScope where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.TurnScope as TurnScope

codec :: Codec.Codec TurnScope.TurnScope
codec = Arm.enum
