module Pawl.Codec.ControllerRelation where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.ControllerRelation as ControllerRelation

codec :: Codec.Codec ControllerRelation.ControllerRelation
codec = Arm.enum
