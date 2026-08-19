module Pawl.Codec.Loyalty where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Loyalty as Loyalty

-- | Tagged like every other sum, so that CR 107.3's printed X is a wire value
-- rather than a sentinel number the decoder would have to reserve.
codec :: Codec.Codec Loyalty.Loyalty
codec =
  Arm.tagged
    [ Arm.payload "Literal" Common.natural Loyalty.Literal (\x -> case x of Loyalty.Literal y -> Just y; _ -> Nothing),
      Arm.nullary "Variable" Loyalty.Variable
    ]
