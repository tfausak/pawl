module Pawl.Codec.EndTurnSignal where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.EndTurnSignal as EndTurnSignal

codec :: Codec.Codec EndTurnSignal.EndTurnSignal
codec = Arm.enum
