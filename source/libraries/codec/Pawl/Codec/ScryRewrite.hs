module Pawl.Codec.ScryRewrite where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.ScryRewrite as ScryRewrite

codec :: Codec.Codec ScryRewrite.ScryRewrite
codec = Arm.enum
