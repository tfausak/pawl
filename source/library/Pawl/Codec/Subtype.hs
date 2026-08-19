module Pawl.Codec.Subtype where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.Subtype as Subtype

codec :: Codec.Codec Subtype.Subtype
codec = Arm.enum
