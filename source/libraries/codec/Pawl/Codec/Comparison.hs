module Pawl.Codec.Comparison where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Comparison as Comparison

codec :: Codec.Codec Comparison.Comparison
codec =
  Arm.tagged
    encode
    [ Arm.nullary "Exactly" Comparison.Exactly,
      Arm.nullary "AtLeast" Comparison.AtLeast,
      Arm.nullary "AtMost" Comparison.AtMost
    ]
  where
    encode c = Common.nullary $ case c of
      Comparison.Exactly -> "Exactly"
      Comparison.AtLeast -> "AtLeast"
      Comparison.AtMost -> "AtMost"
