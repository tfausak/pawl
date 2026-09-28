module Pawl.Codec.ProliferateRewrite where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.ProliferateRewrite as ProliferateRewrite

codec :: Codec.Codec ProliferateRewrite.ProliferateRewrite
codec = Arm.enum
