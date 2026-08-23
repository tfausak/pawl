module Pawl.Codec.Result where

import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.Result as Result

codec :: Codec.Codec Result.Result
codec =
  Arm.tagged
    [ Arm.payload "Won" PlayerId.codec Result.Won (\x -> case x of Result.Won y -> Just y; _ -> Nothing),
      Arm.nullary "Drawn" Result.Drawn
    ]
