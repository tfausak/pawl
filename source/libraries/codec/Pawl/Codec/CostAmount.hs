module Pawl.Codec.CostAmount where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.CostAmount as CostAmount

codec :: Codec.Codec CostAmount.CostAmount
codec =
  Arm.tagged
    tagOf
    [ Arm.payload "Fixed" Common.natural CostAmount.Fixed (\x -> case x of CostAmount.Fixed y -> Just y; _ -> Nothing),
      Arm.nullary "AnnouncedX" CostAmount.AnnouncedX
    ]

tagOf :: CostAmount.CostAmount -> String
tagOf x = case x of
  CostAmount.Fixed {} -> "Fixed"
  CostAmount.AnnouncedX {} -> "AnnouncedX"
