module Pawl.Codec.EventShape where

import qualified Data.Text as Text
import qualified Pawl.Codec.Zone as Zone
import qualified Pawl.Json.Array as Array
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.EventShape as EventShape

toJson :: EventShape.EventShape -> Value.Value
toJson s = case s of
  EventShape.MovedBetween from to -> Common.tagged "MovedBetween" . Just . Common.array $ [Zone.toJson from, Zone.toJson to]
  EventShape.SpellCast -> Common.tagged "SpellCast" Nothing

fromJson :: Value.Value -> Either Text.Text EventShape.EventShape
fromJson value = do
  (t, mv) <- Common.asTagged value
  case (t, mv) of
    ("MovedBetween", Just (Value.Array (Array.MkArray [f, u]))) -> EventShape.MovedBetween <$> Zone.fromJson f <*> Zone.fromJson u
    ("SpellCast", Nothing) -> Right EventShape.SpellCast
    _ -> Left . Text.pack $ "unknown EventShape: " <> t
