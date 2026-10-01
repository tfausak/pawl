module Pawl.Codec.CostKeyword where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.CostKeyword as CostKeyword

codec :: Codec.Codec CostKeyword.CostKeyword
codec = Arm.enum
