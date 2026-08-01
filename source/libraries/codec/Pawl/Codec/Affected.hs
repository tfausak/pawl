-- | The @Affected ⇆ Json@ codec (#481).
module Pawl.Codec.Affected where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Json as Json
import qualified Pawl.Codec.ObjectId as ObjectId
import Pawl.Json.Value (Value)
import qualified Pawl.Types.Affected as Affected

affectedToJson :: Affected.Affected -> Value
affectedToJson a = case a of
  Affected.TheseObjects ids -> Json.tagged (Text.pack "TheseObjects") (Just (Json.setTo ObjectId.toJson ids))
  Affected.Matching f -> Json.tagged (Text.pack "Matching") (Just (Filter.toJson f))
  Affected.Attached -> Json.tagged (Text.pack "Attached") Nothing
  Affected.AttachedPlayerControls f -> Json.tagged (Text.pack "AttachedPlayerControls") (Just (Filter.toJson f))

jsonToAffected :: Value -> Either Text Affected.Affected
jsonToAffected value = do
  (t, mv) <- Json.tag value
  case Text.unpack t of
    "TheseObjects" -> Json.withValue mv (fmap Affected.TheseObjects . Json.setFrom ObjectId.fromJson)
    "Matching" -> Json.withValue mv (fmap Affected.Matching . Filter.fromJson)
    "Attached" -> pure Affected.Attached
    "AttachedPlayerControls" -> Json.withValue mv (fmap Affected.AttachedPlayerControls . Filter.fromJson)
    _ -> Left (Text.pack "unknown Affected: " <> t)
