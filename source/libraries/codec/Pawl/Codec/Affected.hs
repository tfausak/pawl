module Pawl.Codec.Affected where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.Affected as Affected

toJson :: Affected.Affected -> Value.Value
toJson a = case a of
  Affected.TheseObjects ids -> Common.tagged "TheseObjects" . Just $ Common.encodeSet ObjectId.toJson ids
  Affected.Matching f -> Common.tagged "Matching" . Just $ Filter.toJson Keyword.toJson f
  Affected.Attached -> Common.tagged "Attached" Nothing
  Affected.AttachedPlayerControls f -> Common.tagged "AttachedPlayerControls" . Just $ Filter.toJson Keyword.toJson f

fromJson :: Value.Value -> Either Text.Text Affected.Affected
fromJson value = do
  (t, mv) <- Common.asTagged value
  case t of
    "TheseObjects" -> Common.withValue mv (fmap Affected.TheseObjects . Common.decodeSet ObjectId.fromJson)
    "Matching" -> Common.withValue mv (fmap Affected.Matching . Filter.fromJson Keyword.fromJson)
    "Attached" -> pure Affected.Attached
    "AttachedPlayerControls" -> Common.withValue mv (fmap Affected.AttachedPlayerControls . Filter.fromJson Keyword.fromJson)
    _ -> Left . Text.pack $ "unknown Affected: " <> t
