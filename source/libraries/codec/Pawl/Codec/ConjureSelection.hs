module Pawl.Codec.ConjureSelection where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.ConjureSelection as ConjureSelection

codec :: Codec.Codec ConjureSelection.ConjureSelection
codec = Arm.enum
