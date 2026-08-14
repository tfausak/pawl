module Pawl.Codec.MorphVariant where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.MorphVariant as MorphVariant

codec :: Codec.Codec MorphVariant.MorphVariant
codec = Arm.enum
