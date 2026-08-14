module Pawl.Codec.Recipient where

import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.Recipient as Recipient

-- | The wire format is unchanged by the conversion to a bundle; what it adds is
-- the schema.
codec :: Codec.Codec Recipient.Recipient
codec =
  Arm.tagged
    [ Arm.payload "ToCreature" ObjectId.codec Recipient.ToCreature (\x -> case x of Recipient.ToCreature y -> Just y; _ -> Nothing),
      Arm.payload "ToPlaneswalker" ObjectId.codec Recipient.ToPlaneswalker (\x -> case x of Recipient.ToPlaneswalker y -> Just y; _ -> Nothing),
      Arm.payload "ToBattle" ObjectId.codec Recipient.ToBattle (\x -> case x of Recipient.ToBattle y -> Just y; _ -> Nothing),
      Arm.payload "ToPlayer" PlayerId.codec Recipient.ToPlayer (\x -> case x of Recipient.ToPlayer y -> Just y; _ -> Nothing),
      Arm.payload "ToObject" ObjectId.codec Recipient.ToObject (\x -> case x of Recipient.ToObject y -> Just y; _ -> Nothing)
    ]
