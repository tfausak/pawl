module Pawl.Codec.PaymentDecision where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.PaymentDecision as PaymentDecision

-- | Nullary tags, derived from the type by Arm.enum.
codec :: Codec.Codec PaymentDecision.PaymentDecision
codec = Arm.enum
