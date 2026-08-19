module Pawl.Codec.AttackCostScope where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.AttackCostScope as AttackCostScope

codec :: Codec.Codec AttackCostScope.AttackCostScope
codec = Arm.enum
