module Pawl.Codec.Scaling where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Scaling as Scaling

codec :: Codec.Codec Scaling.Scaling
codec =
  Arm.tagged
    [ Arm.payload "Multiply" Common.natural Scaling.Multiply (\x -> case x of Scaling.Multiply y -> Just y; _ -> Nothing),
      Arm.payload "AddMore" Common.natural Scaling.AddMore (\x -> case x of Scaling.AddMore y -> Just y; _ -> Nothing),
      Arm.nullary "Halve" Scaling.Halve
    ]
