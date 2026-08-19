module Pawl.Codec.CopyException where

import qualified Pawl.Codec.SetPowerToughness as SetPowerToughness
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.CopyException as CopyException

codec :: Codec.Codec CopyException.CopyException
codec =
  Arm.tagged
    [ Arm.payload "SetPowerToughness" SetPowerToughness.codec CopyException.SetPowerToughness (\(CopyException.SetPowerToughness y) -> Just y)
    ]
