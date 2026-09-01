module Pawl.Codec.AttackOption where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.AttackOption as AttackOption

codec :: Codec.Codec AttackOption.AttackOption
codec = Arm.enum
