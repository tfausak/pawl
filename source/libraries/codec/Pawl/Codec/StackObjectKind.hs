module Pawl.Codec.StackObjectKind where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.StackObjectKind as StackObjectKind

codec :: Codec.Codec StackObjectKind.StackObjectKind
codec = Arm.enum
