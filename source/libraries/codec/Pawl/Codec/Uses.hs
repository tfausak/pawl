module Pawl.Codec.Uses where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.Uses as Uses

codec :: Codec.Codec Uses.Uses
codec = Arm.enum
