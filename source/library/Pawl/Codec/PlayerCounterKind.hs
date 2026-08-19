module Pawl.Codec.PlayerCounterKind where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind

codec :: Codec.Codec PlayerCounterKind.PlayerCounterKind
codec = Arm.enum
