module Pawl.Codec.SearchDestination where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.SearchDestination as SearchDestination

codec :: Codec.Codec SearchDestination.SearchDestination
codec = Arm.enum
