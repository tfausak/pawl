module Pawl.Codec.SpecialAction where

import qualified Data.Text as Text
import qualified Pawl.Codec.Cost as Cost
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.SpecialAction as SpecialAction

toJson :: SpecialAction.SpecialAction -> Value.Value
toJson a = case a of
  SpecialAction.DiscardThisAnyTime -> Common.nullary "DiscardThisAnyTime"
  SpecialAction.IgnoreThisUntilEndOfTurn c -> Common.tagged "IgnoreThisUntilEndOfTurn" . Just $ Codec.encode (Cost.codec Keyword.codec) c

fromJson :: Value.Value -> Either Text.Text SpecialAction.SpecialAction
fromJson value = do
  (t, mv) <- Common.asTagged value
  case (t, mv) of
    ("DiscardThisAnyTime", _) -> Right SpecialAction.DiscardThisAnyTime
    ("IgnoreThisUntilEndOfTurn", Just v) -> SpecialAction.IgnoreThisUntilEndOfTurn <$> Codec.decode (Cost.codec Keyword.codec) v
    _ -> Left . Text.pack $ "unknown SpecialAction: " <> t
