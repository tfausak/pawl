module Pawl.Codec.ReplacementOrigin where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.ReplacementOrigin as ReplacementOrigin

codec :: Codec.Codec ReplacementOrigin.ReplacementOrigin
codec = Arm.enum
