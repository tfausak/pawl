module Pawl.Codec.TriggerLimit where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.TriggerLimit as TriggerLimit

codec :: Codec.Codec TriggerLimit.TriggerLimit
codec = Arm.enum
