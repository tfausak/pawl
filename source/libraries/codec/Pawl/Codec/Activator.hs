module Pawl.Codec.Activator where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.Activator as Activator

codec :: Codec.Codec Activator.Activator
codec = Arm.enum
