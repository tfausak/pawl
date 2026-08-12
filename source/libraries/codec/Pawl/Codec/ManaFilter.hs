module Pawl.Codec.ManaFilter where

import qualified Data.Text as Text
import qualified Pawl.Codec.ManaType as ManaType
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.ManaFilter as ManaFilter

toJson :: ManaFilter.ManaFilter -> Value.Value
toJson f = case f of
  ManaFilter.Any -> Common.nullary "Any"
  ManaFilter.OfType mt -> Common.tagged "OfType" . Just $ ManaType.toJson mt

fromJson :: Value.Value -> Either Text.Text ManaFilter.ManaFilter
fromJson value = do
  (t, mv) <- Common.asTagged value
  case (t, mv) of
    ("Any", _) -> Right ManaFilter.Any
    ("OfType", Just v) -> ManaFilter.OfType <$> ManaType.fromJson v
    _ -> Left . Text.pack $ "unknown ManaFilter: " <> t
