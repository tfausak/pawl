-- | The @Recipient ⇆ Json@ codec (#481).
module Pawl.Codec.Recipient where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Json as Json
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.PlayerId as PlayerId
import Pawl.Json.Value (Value)
import qualified Pawl.Types.Recipient as Recipient

recipientToJson :: Recipient.Recipient -> Value
recipientToJson r = case r of
  Recipient.ToCreature oid -> Json.tagged (Text.pack "ToCreature") (Just (ObjectId.toJson oid))
  Recipient.ToPlaneswalker oid -> Json.tagged (Text.pack "ToPlaneswalker") (Just (ObjectId.toJson oid))
  Recipient.ToPlayer pid -> Json.tagged (Text.pack "ToPlayer") (Just (PlayerId.toJson pid))
  Recipient.ToObject oid -> Json.tagged (Text.pack "ToObject") (Just (ObjectId.toJson oid))

jsonToRecipient :: Value -> Either Text Recipient.Recipient
jsonToRecipient value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("ToCreature", Just v) -> Recipient.ToCreature <$> ObjectId.fromJson v
    ("ToPlaneswalker", Just v) -> Recipient.ToPlaneswalker <$> ObjectId.fromJson v
    ("ToPlayer", Just v) -> Recipient.ToPlayer <$> PlayerId.fromJson v
    ("ToObject", Just v) -> Recipient.ToObject <$> ObjectId.fromJson v
    _ -> Left (Text.pack "unknown Recipient: " <> t)
