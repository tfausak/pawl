module Pawl.Codec.RollModifier where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.RollModifier as RollModifier

codec :: Codec.Codec RollModifier.RollModifier
codec = Arm.enum
