module Pawl.Codec.Measure where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.Measure as Measure

codec :: Codec.Codec Measure.Measure
codec = Arm.enum
