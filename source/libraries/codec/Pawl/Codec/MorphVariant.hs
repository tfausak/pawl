module Pawl.Codec.MorphVariant where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.MorphVariant as MorphVariant

codec :: Codec.Codec MorphVariant.MorphVariant
codec =
  Arm.tagged
    encode
    [ Arm.nullary "Plain" MorphVariant.Plain,
      Arm.nullary "Mega" MorphVariant.Mega
    ]
  where
    encode v = Common.nullary $ case v of
      MorphVariant.Plain -> "Plain"
      MorphVariant.Mega -> "Mega"
