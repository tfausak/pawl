module Pawl.Codec.ObjectRef where

import qualified Data.Text as Text
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.ObjectRef as ObjectRef

-- An ObjectRef is told apart by JSON TYPE rather than by a tag, the shape
-- Effect.Create's optional EntryRiders also uses: a slot name is a string, a
-- Filter is an object and the two arms naming no slot and no filter are arrays,
-- so no two can be confused. An array LEADS WITH ITS OWN WORD rather than being
-- empty or null, so a card file still reads as words, and the word is what tells
-- those two apart -- one carries a payload after it, the other does not.
toJson :: ObjectRef.ObjectRef -> Value.Value
toJson r = case r of
  ObjectRef.InSlot n -> Codec.encode SlotName.codec n
  ObjectRef.EachMatching f -> Codec.encode (Filter.codec Keyword.codec) f
  ObjectRef.EachPlayer -> Value.array [Value.text eachPlayer]
  ObjectRef.TopOfLibrary p -> Value.array [Value.text topOfLibrary, PlayerRef.toJson p]

fromJson :: Value.Value -> Either Text.Text ObjectRef.ObjectRef
fromJson value = case value of
  Value.Object _ -> ObjectRef.EachMatching <$> Codec.decode (Filter.codec Keyword.codec) value
  Value.Array _ -> do
    values <- Common.asArray value
    case values of
      [only] -> do
        word <- Common.asText only
        if word == eachPlayer
          then Right ObjectRef.EachPlayer
          else Left . Text.pack $ "unknown ObjectRef sweep " <> show word
      [first, payload] -> do
        word <- Common.asText first
        if word == topOfLibrary
          then ObjectRef.TopOfLibrary <$> PlayerRef.fromJson payload
          else Left . Text.pack $ "unknown ObjectRef position " <> show word
      _ -> Left . Text.pack $ "ObjectRef array expects a word, and a payload only where the word takes one"
  _ -> ObjectRef.InSlot <$> Codec.decode SlotName.codec value

-- The words the array arms lead with, written once each so the encoder and the
-- decoder cannot drift.
eachPlayer :: Text.Text
eachPlayer = Text.pack "EachPlayer"

topOfLibrary :: Text.Text
topOfLibrary = Text.pack "TopOfLibrary"
