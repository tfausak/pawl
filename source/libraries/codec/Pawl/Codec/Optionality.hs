module Pawl.Codec.Optionality where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.Optionality as Optionality

codec :: Codec.Codec Optionality.Optionality
codec = Arm.enum
