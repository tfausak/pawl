module Pawl.Codec.CostDirection where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.CostDirection as CostDirection

codec :: Codec.Codec CostDirection.CostDirection
codec = Arm.enum
