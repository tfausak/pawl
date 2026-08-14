module Pawl.Codec.Daytime where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.Daytime as Daytime

codec :: Codec.Codec Daytime.Daytime
codec = Arm.enum
