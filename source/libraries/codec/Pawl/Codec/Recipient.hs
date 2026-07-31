-- | The @Recipient ⇆ Json@ codec (#481).
module Pawl.Codec.Recipient where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Json as Json
import Pawl.Codec.ObjectId (jsonToObjectId, objectIdToJson)
import Pawl.Codec.PlayerId (jsonToPlayerId, playerIdToJson)
import Pawl.Json.Value (Value)
import qualified Pawl.Types.Recipient as Recipient

recipientToJson :: Recipient.Recipient -> Value
recipientToJson r = case r of
  Recipient.ToCreature oid -> Json.tagged (Text.pack "ToCreature") (Just (objectIdToJson oid))
  Recipient.ToPlayer pid -> Json.tagged (Text.pack "ToPlayer") (Just (playerIdToJson pid))
  Recipient.ToObject oid -> Json.tagged (Text.pack "ToObject") (Just (objectIdToJson oid))

jsonToRecipient :: Value -> Either Text Recipient.Recipient
jsonToRecipient value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("ToCreature", Just v) -> Recipient.ToCreature <$> jsonToObjectId v
    ("ToPlayer", Just v) -> Recipient.ToPlayer <$> jsonToPlayerId v
    ("ToObject", Just v) -> Recipient.ToObject <$> jsonToObjectId v
    _ -> Left (Text.pack "unknown Recipient: " <> t)
