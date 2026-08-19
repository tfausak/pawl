module Pawl.Codec.DestructionRewrite where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.DestructionRewrite as DestructionRewrite

codec :: Codec.Codec DestructionRewrite.DestructionRewrite
codec = Arm.enum
