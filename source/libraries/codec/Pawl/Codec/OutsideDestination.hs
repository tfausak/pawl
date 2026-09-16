module Pawl.Codec.OutsideDestination where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.OutsideDestination as OutsideDestination

codec :: Codec.Codec OutsideDestination.OutsideDestination
codec = Arm.enum
