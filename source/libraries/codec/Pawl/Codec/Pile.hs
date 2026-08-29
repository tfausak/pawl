module Pawl.Codec.Pile where

import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.Codec.Timestamp as Timestamp
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.Pile as Pile

codec :: Codec.Codec Pile.Pile
codec =
  Arm.tagged
    [ Arm.payload "OfForetold" Timestamp.codec Pile.OfForetold (\x -> case x of Pile.OfForetold y -> Just y; _ -> Nothing),
      Arm.payload "OfFaceDown" PlayerId.codec Pile.OfFaceDown (\x -> case x of Pile.OfFaceDown y -> Just y; _ -> Nothing)
    ]
