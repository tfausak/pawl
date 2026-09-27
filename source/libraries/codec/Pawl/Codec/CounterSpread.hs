module Pawl.Codec.CounterSpread where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.CounterSpread as CounterSpread

-- | Nullary tags, Pawl.Codec.AbilityKind's shape and for its reason.
codec :: Codec.Codec CounterSpread.CounterSpread
codec = Arm.enum
