module Pawl.Codec.PayBranch where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.PayBranch as PayBranch

codec :: Codec.Codec PayBranch.PayBranch
codec = Arm.enum
