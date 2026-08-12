module Pawl.Codec.ObjectRef where

import qualified Data.Text as Text
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.ObjectRef as ObjectRef

-- An ObjectRef is told apart by JSON TYPE rather than by a tag, the shape
-- Effect.Create's optional EntryRiders also uses: a slot name is a string, a
-- Filter is an object and the player sweep is an array, so no two can be
-- confused. The array names itself rather than being empty or null, so a card
-- file still reads as words.
toJson :: ObjectRef.ObjectRef -> Value.Value
toJson r = case r of
  ObjectRef.InSlot n -> SlotName.toJson n
  ObjectRef.EachMatching f -> Filter.toJson Keyword.toJson f
  ObjectRef.EachPlayer -> Value.array [Value.text eachPlayer]

fromJson :: Value.Value -> Either Text.Text ObjectRef.ObjectRef
fromJson value = case value of
  Value.Object _ -> ObjectRef.EachMatching <$> Filter.fromJson Keyword.fromJson value
  Value.Array _ -> do
    values <- Common.asArray value
    case values of
      [only] -> do
        word <- Common.asText only
        if word == eachPlayer
          then Right ObjectRef.EachPlayer
          else Left . Text.pack $ "unknown ObjectRef sweep " <> show word
      _ -> Left . Text.pack $ "ObjectRef array expects exactly one word"
  _ -> ObjectRef.InSlot <$> SlotName.fromJson value

-- The one word the array arm carries, written once so the encoder and the
-- decoder cannot drift.
eachPlayer :: Text.Text
eachPlayer = Text.pack "EachPlayer"
