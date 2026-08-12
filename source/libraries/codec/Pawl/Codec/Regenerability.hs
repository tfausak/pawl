module Pawl.Codec.Regenerability where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Regenerability as Regenerability

codec :: Codec.Codec Regenerability.Regenerability
codec =
  Arm.tagged
    encode
    [ Arm.nullary "Regenerable" Regenerability.Regenerable,
      Arm.nullary "CantBeRegenerated" Regenerability.CantBeRegenerated
    ]
  where
    encode r = Common.nullary $ case r of
      Regenerability.Regenerable -> "Regenerable"
      Regenerability.CantBeRegenerated -> "CantBeRegenerated"
