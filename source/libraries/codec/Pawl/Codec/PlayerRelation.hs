module Pawl.Codec.PlayerRelation where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.PlayerRelation as PlayerRelation

codec :: Codec.Codec PlayerRelation.PlayerRelation
codec = Arm.enum
