module Pawl.Codec.Prohibition where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.Prohibition as Prohibition

codec :: Codec.Codec Prohibition.Prohibition
codec = Arm.enum
