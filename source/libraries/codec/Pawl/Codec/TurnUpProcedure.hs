module Pawl.Codec.TurnUpProcedure where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.TurnUpProcedure as TurnUpProcedure

codec :: Codec.Codec TurnUpProcedure.TurnUpProcedure
codec = Arm.enum
