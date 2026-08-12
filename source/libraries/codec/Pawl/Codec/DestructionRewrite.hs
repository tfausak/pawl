module Pawl.Codec.DestructionRewrite where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.DestructionRewrite as DestructionRewrite

codec :: Codec.Codec DestructionRewrite.DestructionRewrite
codec =
  Arm.tagged
    encode
    [ Arm.nullary "Regenerate" DestructionRewrite.Regenerate,
      Arm.nullary "RemoveShieldCounter" DestructionRewrite.RemoveShieldCounter
    ]
  where
    encode r = Common.nullary $ case r of
      DestructionRewrite.Regenerate -> "Regenerate"
      DestructionRewrite.RemoveShieldCounter -> "RemoveShieldCounter"
