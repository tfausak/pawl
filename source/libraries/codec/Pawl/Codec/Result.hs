module Pawl.Codec.Result where

import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.Codec.TeamId as TeamId
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.Result as Result

codec :: Codec.Codec Result.Result
codec =
  Arm.tagged
    tagOf
    [ Arm.payload "Won" PlayerId.codec Result.Won (\x -> case x of Result.Won y -> Just y; _ -> Nothing),
      Arm.payload "TeamWon" TeamId.codec Result.TeamWon (\x -> case x of Result.TeamWon y -> Just y; _ -> Nothing),
      Arm.nullary "Drawn" Result.Drawn
    ]

tagOf :: Result.Result -> String
tagOf x = case x of
  Result.Won {} -> "Won"
  Result.TeamWon {} -> "TeamWon"
  Result.Drawn {} -> "Drawn"
