module Pawl.Codec.LoopMembers where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.LoopMembers as LoopMembers

codec :: Codec.Codec LoopMembers.LoopMembers
codec = Arm.enum
