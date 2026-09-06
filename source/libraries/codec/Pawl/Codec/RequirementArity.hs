module Pawl.Codec.RequirementArity where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.RequirementArity as RequirementArity

codec :: Codec.Codec RequirementArity.RequirementArity
codec = Arm.enum
