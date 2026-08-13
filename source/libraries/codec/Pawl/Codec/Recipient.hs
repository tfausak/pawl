module Pawl.Codec.Recipient where

import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Recipient as Recipient

-- | The wire format is unchanged by the conversion to a bundle; what it adds is
-- the schema.
codec :: Codec.Codec Recipient.Recipient
codec =
  Arm.tagged
    encode
    [ Arm.payload "ToCreature" ObjectId.codec Recipient.ToCreature,
      Arm.payload "ToPlaneswalker" ObjectId.codec Recipient.ToPlaneswalker,
      Arm.payload "ToBattle" ObjectId.codec Recipient.ToBattle,
      Arm.payload "ToPlayer" PlayerId.codec Recipient.ToPlayer,
      Arm.payload "ToObject" ObjectId.codec Recipient.ToObject
    ]
  where
    encode r = case r of
      Recipient.ToCreature oid -> Common.tagged "ToCreature" . Just $ Codec.encode ObjectId.codec oid
      Recipient.ToPlaneswalker oid -> Common.tagged "ToPlaneswalker" . Just $ Codec.encode ObjectId.codec oid
      Recipient.ToBattle oid -> Common.tagged "ToBattle" . Just $ Codec.encode ObjectId.codec oid
      Recipient.ToPlayer pid -> Common.tagged "ToPlayer" . Just $ Codec.encode PlayerId.codec pid
      Recipient.ToObject oid -> Common.tagged "ToObject" . Just $ Codec.encode ObjectId.codec oid
