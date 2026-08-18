module Pawl.Codec.ExcessDestination where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.ExcessDestination as ExcessDestination

codec :: Codec.Codec ExcessDestination.ExcessDestination
codec = Arm.enum
