module Pawl.Codec.Affected where

import qualified Data.Text as Text
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Affected as Affected

toJson :: Affected.Affected -> Value.Value
toJson a = case a of
  Affected.TheseObjects ids -> Common.tagged "TheseObjects" . Just $ Common.encodeSet ObjectId.toJson ids
  Affected.Matching f -> Common.tagged "Matching" . Just $ Codec.encode (Filter.codec Keyword.codec) f
  Affected.MatchingAnywhere f -> Common.tagged "MatchingAnywhere" . Just $ Codec.encode (Filter.codec Keyword.codec) f
  Affected.Attached -> Common.tagged "Attached" Nothing
  Affected.AttachedPlayerControls f -> Common.tagged "AttachedPlayerControls" . Just $ Codec.encode (Filter.codec Keyword.codec) f

fromJson :: Value.Value -> Either Text.Text Affected.Affected
fromJson value = do
  (t, mv) <- Common.asTagged value
  case t of
    "TheseObjects" -> Common.withValue mv (fmap Affected.TheseObjects . Common.decodeSet ObjectId.fromJson)
    "Matching" -> Common.withValue mv (fmap Affected.Matching . Codec.decode (Filter.codec Keyword.codec))
    "MatchingAnywhere" -> Common.withValue mv (fmap Affected.MatchingAnywhere . Codec.decode (Filter.codec Keyword.codec))
    "Attached" -> pure Affected.Attached
    "AttachedPlayerControls" -> Common.withValue mv (fmap Affected.AttachedPlayerControls . Codec.decode (Filter.codec Keyword.codec))
    _ -> Left . Text.pack $ "unknown Affected: " <> t
