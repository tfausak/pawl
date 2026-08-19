module Pawl.Codec.Regenerability where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.Regenerability as Regenerability

codec :: Codec.Codec Regenerability.Regenerability
codec = Arm.enum
