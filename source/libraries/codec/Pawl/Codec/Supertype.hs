module Pawl.Codec.Supertype where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.Supertype as Supertype

codec :: Codec.Codec Supertype.Supertype
codec = Arm.enum
