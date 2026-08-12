module Pawl.Codec.Recipient where

import qualified Data.Text as Text
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Recipient as Recipient

toJson :: Recipient.Recipient -> Value.Value
toJson r = case r of
  Recipient.ToCreature oid -> Common.tagged "ToCreature" . Just $ Codec.encode ObjectId.codec oid
  Recipient.ToPlaneswalker oid -> Common.tagged "ToPlaneswalker" . Just $ Codec.encode ObjectId.codec oid
  Recipient.ToBattle oid -> Common.tagged "ToBattle" . Just $ Codec.encode ObjectId.codec oid
  Recipient.ToPlayer pid -> Common.tagged "ToPlayer" . Just $ Codec.encode PlayerId.codec pid
  Recipient.ToObject oid -> Common.tagged "ToObject" . Just $ Codec.encode ObjectId.codec oid

fromJson :: Value.Value -> Either Text.Text Recipient.Recipient
fromJson value = do
  (t, mv) <- Common.asTagged value
  case (t, mv) of
    ("ToCreature", Just v) -> Recipient.ToCreature <$> Codec.decode ObjectId.codec v
    ("ToPlaneswalker", Just v) -> Recipient.ToPlaneswalker <$> Codec.decode ObjectId.codec v
    ("ToBattle", Just v) -> Recipient.ToBattle <$> Codec.decode ObjectId.codec v
    ("ToPlayer", Just v) -> Recipient.ToPlayer <$> Codec.decode PlayerId.codec v
    ("ToObject", Just v) -> Recipient.ToObject <$> Codec.decode ObjectId.codec v
    _ -> Left . Text.pack $ "unknown Recipient: " <> t
