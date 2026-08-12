module Pawl.Codec.Scaling where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Scaling as Scaling

codec :: Codec.Codec Scaling.Scaling
codec =
  Arm.tagged
    encode
    [ Arm.payload "Multiply" Common.natural Scaling.Multiply,
      Arm.payload "AddMore" Common.natural Scaling.AddMore,
      Arm.nullary "Halve" Scaling.Halve
    ]
  where
    encode s = case s of
      Scaling.Multiply n -> Common.tagged "Multiply" . Just $ Codec.encode Common.natural n
      Scaling.AddMore n -> Common.tagged "AddMore" . Just $ Codec.encode Common.natural n
      Scaling.Halve -> Common.nullary "Halve"
