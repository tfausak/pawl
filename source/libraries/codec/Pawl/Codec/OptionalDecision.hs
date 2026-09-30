module Pawl.Codec.OptionalDecision where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.OptionalDecision as OptionalDecision

-- | Nullary tags, derived from the type by Arm.enum.
codec :: Codec.Codec OptionalDecision.OptionalDecision
codec = Arm.enum
