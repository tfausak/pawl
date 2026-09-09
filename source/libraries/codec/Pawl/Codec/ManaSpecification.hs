module Pawl.Codec.ManaSpecification where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.ManaSpecification as ManaSpecification

codec :: Codec.Codec ManaSpecification.ManaSpecification
codec = Arm.enum
