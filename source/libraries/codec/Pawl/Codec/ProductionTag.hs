module Pawl.Codec.ProductionTag where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.ProductionTag as ProductionTag

codec :: Codec.Codec ProductionTag.ProductionTag
codec = Arm.enum
