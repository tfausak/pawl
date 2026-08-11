module Pawl.Codec.Recipient where

import qualified Data.Text as Text
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Recipient as Recipient

toJson :: Recipient.Recipient -> Value.Value
toJson r = case r of
  Recipient.ToCreature oid -> Common.tagged "ToCreature" . Just $ ObjectId.toJson oid
  Recipient.ToPlaneswalker oid -> Common.tagged "ToPlaneswalker" . Just $ ObjectId.toJson oid
  Recipient.ToBattle oid -> Common.tagged "ToBattle" . Just $ ObjectId.toJson oid
  Recipient.ToPlayer pid -> Common.tagged "ToPlayer" . Just $ PlayerId.toJson pid
  Recipient.ToObject oid -> Common.tagged "ToObject" . Just $ ObjectId.toJson oid

fromJson :: Value.Value -> Either Text.Text Recipient.Recipient
fromJson value = do
  (t, mv) <- Common.asTagged value
  case (t, mv) of
    ("ToCreature", Just v) -> Recipient.ToCreature <$> ObjectId.fromJson v
    ("ToPlaneswalker", Just v) -> Recipient.ToPlaneswalker <$> ObjectId.fromJson v
    ("ToBattle", Just v) -> Recipient.ToBattle <$> ObjectId.fromJson v
    ("ToPlayer", Just v) -> Recipient.ToPlayer <$> PlayerId.fromJson v
    ("ToObject", Just v) -> Recipient.ToObject <$> ObjectId.fromJson v
    _ -> Left . Text.pack $ "unknown Recipient: " <> t
