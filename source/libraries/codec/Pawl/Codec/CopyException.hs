module Pawl.Codec.CopyException where

import qualified Pawl.Codec.SetPowerToughness as SetPowerToughness
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.CopyException as CopyException

codec :: Codec.Codec CopyException.CopyException
codec =
  Arm.tagged
    encode
    [ Arm.payload "SetPowerToughness" SetPowerToughness.codec CopyException.SetPowerToughness
    ]
  where
    encode e = case e of
      CopyException.SetPowerToughness x ->
        Common.tagged "SetPowerToughness" . Just $ Codec.encode SetPowerToughness.codec x
