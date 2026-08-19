module Pawl.Codec.DiscardCause where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.DiscardCause as DiscardCause

codec :: Codec.Codec DiscardCause.DiscardCause
codec = Arm.enum
