module Pawl.Codec.PlayerScope where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.PlayerScope as PlayerScope

codec :: Codec.Codec PlayerScope.PlayerScope
codec = Arm.enum
