module Pawl.Codec.TriggerFrequency where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.TriggerFrequency as TriggerFrequency

codec :: Codec.Codec TriggerFrequency.TriggerFrequency
codec = Arm.enum
