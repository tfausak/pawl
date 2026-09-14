module Pawl.Codec.CastRepetition where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.CastRepetition as CastRepetition

codec :: Codec.Codec CastRepetition.CastRepetition
codec = Arm.enum
