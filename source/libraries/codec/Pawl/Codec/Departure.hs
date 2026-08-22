module Pawl.Codec.Departure where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.Departure as Departure

codec :: Codec.Codec Departure.Departure
codec = Arm.enum
