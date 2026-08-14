module Pawl.Codec.Onset where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.Onset as Onset

codec :: Codec.Codec Onset.Onset
codec = Arm.enum
