module Pawl.Codec.OutsideCard where

import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.PrintingId as PrintingId
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.OutsideCard as OutsideCard

codec :: Codec.Codec OutsideCard.OutsideCard
codec =
  Arm.tagged
    [ Arm.payload "InPool" PrintingId.codec OutsideCard.InPool (\x -> case x of OutsideCard.InPool y -> Just y; _ -> Nothing),
      Arm.payload "InAnotherGame" ObjectId.codec OutsideCard.InAnotherGame (\x -> case x of OutsideCard.InAnotherGame y -> Just y; _ -> Nothing)
    ]
