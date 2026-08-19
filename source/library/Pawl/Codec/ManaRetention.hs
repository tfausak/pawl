module Pawl.Codec.ManaRetention where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.ManaRetention as ManaRetention

codec :: Codec.Codec ManaRetention.ManaRetention
codec = Arm.enum
