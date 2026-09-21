module Pawl.Codec.DieRollRewrite where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.DieRollRewrite as DieRollRewrite

codec :: Codec.Codec DieRollRewrite.DieRollRewrite
codec = Arm.enum
