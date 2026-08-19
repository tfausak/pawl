module Pawl.Codec.Counterability where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.Counterability as Counterability

codec :: Codec.Codec Counterability.Counterability
codec = Arm.enum
