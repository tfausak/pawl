-- | The @ObjectRef ⇆ Json@ codec (#481).
module Pawl.Codec.ObjectRef where

import Data.Text (Text)
import Pawl.Codec.Filter (filterToJson, jsonToFilter)
import qualified Pawl.Codec.SlotName as SlotName
import Pawl.Json.Value (Value (Object))
import qualified Pawl.Types.ObjectRef as ObjectRef

-- An ObjectRef is told apart by JSON TYPE rather than by a tag, the shape
-- Effect.Create's optional EntryRiders already uses: a slot name is a string
-- (SlotName.toJson) and a Filter is an object, so the two can never be confused.
-- Untagged on purpose -- `"target"` is what an object-affecting effect has always
-- written, and it goes on meaning the one slot it always meant.
objectRefToJson :: ObjectRef.ObjectRef -> Value
objectRefToJson r = case r of
  ObjectRef.InSlot n -> SlotName.toJson n
  ObjectRef.EachMatching f -> filterToJson f

jsonToObjectRef :: Value -> Either Text ObjectRef.ObjectRef
jsonToObjectRef value = case value of
  Object _ -> ObjectRef.EachMatching <$> jsonToFilter value
  _ -> ObjectRef.InSlot <$> SlotName.fromJson value
