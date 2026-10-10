module Pawl.Codec.PermanentAction where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.PermanentAction as PermanentAction

codec :: Codec.Codec PermanentAction.PermanentAction
codec = Arm.enum
