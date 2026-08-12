module Pawl.Codec.ReplacementOrigin where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.ReplacementOrigin as ReplacementOrigin

codec :: Codec.Codec ReplacementOrigin.ReplacementOrigin
codec =
  Arm.tagged
    encode
    [ Arm.nullary "SelfReplacement" ReplacementOrigin.SelfReplacement,
      Arm.nullary "Other" ReplacementOrigin.Other
    ]
  where
    encode o = Common.nullary $ case o of
      ReplacementOrigin.SelfReplacement -> "SelfReplacement"
      ReplacementOrigin.Other -> "Other"
