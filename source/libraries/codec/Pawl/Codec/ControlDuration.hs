module Pawl.Codec.ControlDuration where

import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.ControlDuration as ControlDuration

codec :: Codec.Codec ControlDuration.ControlDuration
codec =
  Arm.tagged
    tagOf
    [ Arm.nullary "UntilTurnEnds" ControlDuration.UntilTurnEnds,
      Arm.nullary "UntilResolutionEnds" ControlDuration.UntilResolutionEnds,
      Arm.payload "WhileResolving" ObjectId.codec ControlDuration.WhileResolving (\x -> case x of ControlDuration.WhileResolving y -> Just y; _ -> Nothing)
    ]

tagOf :: ControlDuration.ControlDuration -> String
tagOf x = case x of
  ControlDuration.UntilTurnEnds -> "UntilTurnEnds"
  ControlDuration.UntilResolutionEnds -> "UntilResolutionEnds"
  ControlDuration.WhileResolving {} -> "WhileResolving"
