module Pawl.Codec.RecipientKind where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.RecipientKind as RecipientKind

codec :: Codec.Codec RecipientKind.RecipientKind
codec = Arm.enum
