module Pawl.Codec.ObjectRef where

import qualified Data.Text as Text
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.ObjectRef as ObjectRef

-- An ObjectRef is told apart by JSON TYPE rather than by a tag, the shape
-- Effect.Create's optional EntryRiders already uses: a slot name is a string
-- (SlotName.toJson) and a Filter is an object, so the two can never be confused.
-- Untagged on purpose -- `"target"` is what an object-affecting effect has always
-- written, and it goes on meaning the one slot it always meant.
toJson :: ObjectRef.ObjectRef -> Value.Value
toJson r = case r of
  ObjectRef.InSlot n -> SlotName.toJson n
  ObjectRef.EachMatching f -> Filter.toJson Keyword.toJson f

fromJson :: Value.Value -> Either Text.Text ObjectRef.ObjectRef
fromJson value = case value of
  Value.Object _ -> ObjectRef.EachMatching <$> Filter.fromJson Keyword.fromJson value
  _ -> ObjectRef.InSlot <$> SlotName.fromJson value
