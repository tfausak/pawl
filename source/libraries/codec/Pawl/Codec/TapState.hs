module Pawl.Codec.TapState where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.TapState as TapState

codec :: Codec.Codec TapState.TapState
codec = Arm.enum
