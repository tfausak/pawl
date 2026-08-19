module Pawl.Codec.ManaSpending where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.ManaSpending as ManaSpending

codec :: Codec.Codec ManaSpending.ManaSpending
codec = Arm.enum
