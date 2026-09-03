module Pawl.Codec.DrawCountRewrite where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.DrawCountRewrite as DrawCountRewrite

codec :: Codec.Codec DrawCountRewrite.DrawCountRewrite
codec = Arm.enum
