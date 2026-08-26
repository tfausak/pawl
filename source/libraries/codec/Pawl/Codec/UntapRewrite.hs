module Pawl.Codec.UntapRewrite where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.UntapRewrite as UntapRewrite

codec :: Codec.Codec UntapRewrite.UntapRewrite
codec = Arm.enum
