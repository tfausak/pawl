module Pawl.Codec.CopyException where

import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.SetPowerToughness as SetPowerToughness
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.CopyException as CopyException

codec :: Codec.Codec CopyException.CopyException
codec =
  Arm.tagged
    [ Arm.payload "SetPowerToughness" SetPowerToughness.codec CopyException.SetPowerToughness $ \x -> case x of
        CopyException.SetPowerToughness y -> Just y
        _ -> Nothing,
      Arm.payload "GainKeywords" (Common.set Keyword.codec) CopyException.GainKeywords $ \x -> case x of
        CopyException.GainKeywords y -> Just y
        _ -> Nothing
    ]
