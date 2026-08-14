module Pawl.Codec.CardType where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.CardType as CardType

codec :: Codec.Codec CardType.CardType
codec = Arm.enum
