module Pawl.Codec.EquipTarget where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.EquipTarget as EquipTarget

codec :: Codec.Codec EquipTarget.EquipTarget
codec = Arm.enum
