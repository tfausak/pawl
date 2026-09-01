module Pawl.Codec.AttackTargetKind where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.AttackTargetKind as AttackTargetKind

codec :: Codec.Codec AttackTargetKind.AttackTargetKind
codec = Arm.enum
