module Pawl.Codec.CombatStep where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.CombatStep as CombatStep

codec :: Codec.Codec CombatStep.CombatStep
codec = Arm.enum
