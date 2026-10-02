module Pawl.Codec.Reply where

import qualified Data.Text as Text
import qualified Pawl.Json.Pair as Pair
import qualified Pawl.Json.String as String
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonSchema.Schema as Schema
import qualified Pawl.Types.Reply as Reply

-- | Any JSON, its shape checked only when a prompt reads it.
codec :: Codec.Codec Reply.Reply
codec = Common.scalar (Schema.fromPairs []) toValue fromValue

toValue :: Reply.Reply -> Value.Value
toValue reply = case reply of
  Reply.Null -> Value.null
  Reply.Boolean b -> Value.boolean b
  Reply.Number n -> Value.integer n
  Reply.Text t -> Value.text t
  Reply.Array xs -> Value.array (fmap toValue xs)
  Reply.Object kvs -> Value.object (fmap (\(k, v) -> Value.pair (Text.unpack k) (toValue v)) kvs)

fromValue :: Value.Value -> Either Text.Text Reply.Reply
fromValue value = case value of
  Value.Null _ -> Right Reply.Null
  Value.Boolean _ -> fmap Reply.Boolean (Common.asBoolean value)
  Value.Number _ -> fmap Reply.Number (Common.asInteger value)
  Value.String s -> Right (Reply.Text (String.unwrap s))
  Value.Array _ -> fmap Reply.Array (Common.asArray value >>= traverse fromValue)
  Value.Object _ -> do
    pairs <- Common.asObject value
    fmap Reply.Object (traverse (\p -> fmap ((,) (String.unwrap (Pair.name p))) (fromValue (Pair.value p))) pairs)
