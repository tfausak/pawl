module Pawl.Codec.ReturnEnding where

import qualified Pawl.Codec.MonarchWatch as MonarchWatch
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.ReturnEnding as ReturnEnding

codec :: Codec.Codec ReturnEnding.ReturnEnding
codec =
  Arm.tagged
    tagOf
    [ Arm.payload "SourceLeaves" ObjectId.codec ReturnEnding.SourceLeaves (\x -> case x of ReturnEnding.SourceLeaves y -> Just y; _ -> Nothing),
      Arm.payload "OpponentCrowned" MonarchWatch.codec ReturnEnding.OpponentCrowned (\x -> case x of ReturnEnding.OpponentCrowned y -> Just y; _ -> Nothing)
    ]

tagOf :: ReturnEnding.ReturnEnding -> String
tagOf x = case x of
  ReturnEnding.SourceLeaves {} -> "SourceLeaves"
  ReturnEnding.OpponentCrowned {} -> "OpponentCrowned"
