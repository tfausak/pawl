module Pawl.Codec.Uses where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Uses as Uses

codec :: Codec.Codec Uses.Uses
codec =
  Arm.tagged
    encode
    [ Arm.nullary "Unlimited" Uses.Unlimited,
      Arm.nullary "Once" Uses.Once
    ]
  where
    encode u = Common.nullary $ case u of
      Uses.Unlimited -> "Unlimited"
      Uses.Once -> "Once"
