module Pawl.Codec.ZonePair where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.ZonePair as ZonePair

codec :: Codec.Codec ZonePair.ZonePair
codec = Arm.enum
