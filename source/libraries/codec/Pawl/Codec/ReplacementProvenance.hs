module Pawl.Codec.ReplacementProvenance where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.ReplacementProvenance as ReplacementProvenance

codec :: Codec.Codec ReplacementProvenance.ReplacementProvenance
codec = Arm.enum
