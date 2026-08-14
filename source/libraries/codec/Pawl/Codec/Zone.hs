module Pawl.Codec.Zone where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.Zone as Zone

codec :: Codec.Codec Zone.Zone
codec = Arm.enum
