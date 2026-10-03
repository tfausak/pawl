module Pawl.Codec.OutsideCard where

import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.PrintingId as PrintingId
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.OutsideCard as OutsideCard

-- | Reached through Pawl.Codec.Player's `companion` key.
codec :: Codec.Codec OutsideCard.OutsideCard
codec =
  Arm.tagged
    tagOf
    [ Arm.payload "InPool" PrintingId.codec OutsideCard.InPool (\x -> case x of OutsideCard.InPool y -> Just y; _ -> Nothing),
      Arm.payload "InAnotherGame" ObjectId.codec OutsideCard.InAnotherGame (\x -> case x of OutsideCard.InAnotherGame y -> Just y; _ -> Nothing)
    ]

tagOf :: OutsideCard.OutsideCard -> String
tagOf x = case x of
  OutsideCard.InPool {} -> "InPool"
  OutsideCard.InAnotherGame {} -> "InAnotherGame"
