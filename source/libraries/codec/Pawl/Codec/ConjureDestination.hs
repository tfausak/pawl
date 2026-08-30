module Pawl.Codec.ConjureDestination where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.ConjureDestination as ConjureDestination

codec :: Codec.Codec ConjureDestination.ConjureDestination
codec = Arm.enum
