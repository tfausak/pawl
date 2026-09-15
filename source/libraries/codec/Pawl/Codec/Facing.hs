module Pawl.Codec.Facing where

import qualified Pawl.Codec.FaceDownState as FaceDownState
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.Facing as Facing

codec :: Codec.Codec Facing.Facing
codec =
  Arm.tagged
    tagOf
    [ Arm.nullary "FaceUp" Facing.FaceUp,
      Arm.payload "FaceDown" FaceDownState.codec Facing.FaceDown (\x -> case x of Facing.FaceDown y -> Just y; _ -> Nothing)
    ]

tagOf :: Facing.Facing -> String
tagOf x = case x of
  Facing.FaceUp {} -> "FaceUp"
  Facing.FaceDown {} -> "FaceDown"
