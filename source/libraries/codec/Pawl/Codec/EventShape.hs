module Pawl.Codec.EventShape where

import qualified Data.Text as Text
import qualified Pawl.Codec.Zone as Zone
import qualified Pawl.Json.Array as Array
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.EventShape as EventShape

toJson :: EventShape.EventShape -> Value.Value
toJson s = case s of
  EventShape.MovedBetween from to -> Common.tagged "MovedBetween" . Just . Value.array $ [Codec.encode Zone.codec from, Codec.encode Zone.codec to]
  EventShape.SpellCast -> Common.tagged "SpellCast" Nothing

fromJson :: Value.Value -> Either Text.Text EventShape.EventShape
fromJson value = do
  (t, mv) <- Common.asTagged value
  case (t, mv) of
    ("MovedBetween", Just (Value.Array (Array.MkArray [f, u]))) -> EventShape.MovedBetween <$> Codec.decode Zone.codec f <*> Codec.decode Zone.codec u
    ("SpellCast", Nothing) -> Right EventShape.SpellCast
    _ -> Left . Text.pack $ "unknown EventShape: " <> t
