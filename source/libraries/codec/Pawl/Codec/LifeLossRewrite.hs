module Pawl.Codec.LifeLossRewrite where

import qualified Pawl.Codec.Scaling as Scaling
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.LifeLossRewrite as LifeLossRewrite

codec :: Codec.Codec LifeLossRewrite.LifeLossRewrite
codec =
  Arm.tagged
    tagOf
    [ Arm.payload "LeaveAtLeast" Common.natural LifeLossRewrite.LeaveAtLeast (\x -> case x of LifeLossRewrite.LeaveAtLeast y -> Just y; _ -> Nothing),
      Arm.payload "Scaled" Scaling.codec LifeLossRewrite.Scaled (\x -> case x of LifeLossRewrite.Scaled y -> Just y; _ -> Nothing),
      Arm.nullary "ExileFromTopOfYourLibrary" LifeLossRewrite.ExileFromTopOfYourLibrary,
      Arm.nullary "GainInstead" LifeLossRewrite.GainInstead
    ]

tagOf :: LifeLossRewrite.LifeLossRewrite -> String
tagOf x = case x of
  LifeLossRewrite.LeaveAtLeast {} -> "LeaveAtLeast"
  LifeLossRewrite.Scaled {} -> "Scaled"
  LifeLossRewrite.ExileFromTopOfYourLibrary {} -> "ExileFromTopOfYourLibrary"
  LifeLossRewrite.GainInstead {} -> "GainInstead"
