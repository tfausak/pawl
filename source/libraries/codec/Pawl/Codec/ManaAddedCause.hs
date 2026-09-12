module Pawl.Codec.ManaAddedCause where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.ManaAddedCause as ManaAddedCause

codec :: Codec.Codec ManaAddedCause.ManaAddedCause
codec = Arm.enum
