-- | The @Affected ⇆ Json@ codec, split out of the former
-- Pawl.Codec.All (#481).
module Pawl.Codec.Affected where

import Data.Text (Text)
import qualified Data.Text as Text
import Pawl.Codec.Filter (filterToJson, jsonToFilter)
import qualified Pawl.Codec.Json as Json
import Pawl.Codec.ObjectId (jsonToObjectId, objectIdToJson)
import Pawl.Json.Value (Value)
import qualified Pawl.Types.Affected as Affected

affectedToJson :: Affected.Affected -> Value
affectedToJson a = case a of
  Affected.TheseObjects ids -> Json.tagged (Text.pack "TheseObjects") (Just (Json.setTo objectIdToJson ids))
  Affected.Matching f -> Json.tagged (Text.pack "Matching") (Just (filterToJson f))
  Affected.Attached -> Json.tagged (Text.pack "Attached") Nothing
  Affected.AttachedPlayerControls f -> Json.tagged (Text.pack "AttachedPlayerControls") (Just (filterToJson f))

jsonToAffected :: Value -> Either Text Affected.Affected
jsonToAffected value = do
  (t, mv) <- Json.tag value
  case Text.unpack t of
    "TheseObjects" -> Json.withValue mv (fmap Affected.TheseObjects . Json.setFrom jsonToObjectId)
    "Matching" -> Json.withValue mv (fmap Affected.Matching . jsonToFilter)
    "Attached" -> pure Affected.Attached
    "AttachedPlayerControls" -> Json.withValue mv (fmap Affected.AttachedPlayerControls . jsonToFilter)
    _ -> Left (Text.pack "unknown Affected: " <> t)
