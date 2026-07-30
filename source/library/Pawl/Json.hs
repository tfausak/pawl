-- | Construction, normalization, and extraction helpers over the @json@
-- sublibrary's 'Value', plus the small tagged-object convention the codec (§2 of
-- the M3.5 spec) builds on. Encoding and decoding themselves live in
-- 'Pawl.Json.Value'; this module only adapts them to the codec's @Either Text@
-- error channel.
--
-- 'jObject' and 'asObject' trade in assoc lists, which is the shape the codec
-- wants: it writes fields in a readable order rather than an alphabetical one,
-- and reads them back by name. That order is incidental -- JSON objects are
-- unordered, nothing checks the bytes of a card file, and 'sortKeys' exists to
-- compare two values regardless of it.
module Pawl.Json where

import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Decimal as Decimal
import qualified Pawl.Extra.Builder as Builder
import qualified Pawl.Json.Array as Array
import qualified Pawl.Json.Boolean as Boolean
import qualified Pawl.Json.Null as Null
import qualified Pawl.Json.Number as Number
import qualified Pawl.Json.Object as Object
import qualified Pawl.Json.Pair as Pair
import qualified Pawl.Json.String as String
import Pawl.Json.Value (Value)
import qualified Pawl.Json.Value as Value
import qualified Text.Parsec as Parsec

-- Construction helpers -------------------------------------------------------

jNull :: Value
jNull = Value.Null (Null.MkNull ())

jInt :: Integer -> Value
jInt n = Value.Number (Number.MkNumber (Decimal.mkDecimal n 0))

jText :: Text -> Value
jText = Value.String . String.MkString

jBool :: Bool -> Value
jBool = Value.Boolean . Boolean.MkBoolean

jArray :: [Value] -> Value
jArray = Value.Array . Array.MkArray

jObject :: [(Text, Value)] -> Value
jObject = Value.Object . Object.MkObject . fmap (\(k, v) -> Pair.MkPair (String.MkString k) v)

tagged :: Text -> Maybe Value -> Value
tagged t mv =
  let base = [(Text.pack "type", jText t)]
   in jObject $ case mv of
        Nothing -> base
        Just v -> base <> [(Text.pack "value", v)]

-- Normalization --------------------------------------------------------------

-- | Recursively sorts every object's keys, so that two values differing only in
-- key order compare equal. JSON objects are unordered, so this is the right
-- notion of equality for comparing a parsed file against a re-encoded one.
--
-- Arrays are deliberately left alone: JSON arrays /are/ ordered, and the codec
-- relies on that -- a name-keyed map is rendered as a sorted array of entries
-- precisely so the order is deterministic.
--
-- Duplicate keys are not merged. 'List.sortOn' is stable and the extraction
-- helpers take the first match, so the two agree.
sortKeys :: Value -> Value
sortKeys value = case value of
  Value.Array a -> Value.Array (Array.MkArray (fmap sortKeys (Array.unwrap a)))
  Value.Object o ->
    Value.Object
      . Object.MkObject
      . List.sortOn (String.unwrap . Pair.name)
      . fmap (\p -> Pair.MkPair (Pair.name p) (sortKeys (Pair.value p)))
      $ Object.unwrap o
  _ -> value

-- Rendering ------------------------------------------------------------------

render :: Value -> Text
render = Text.pack . Builder.toString . Value.encode

-- Extraction helpers ---------------------------------------------------------

asObject :: Value -> Either Text [(Text, Value)]
asObject value = case value of
  Value.Object o -> Right (fmap (\p -> (String.unwrap (Pair.name p), Pair.value p)) (Object.unwrap o))
  _ -> Left (Text.pack "expected object")

asArray :: Value -> Either Text [Value]
asArray value = case value of
  Value.Array a -> Right (Array.unwrap a)
  _ -> Left (Text.pack "expected array")

asText :: Value -> Either Text Text
asText value = case value of
  Value.String s -> Right (String.unwrap s)
  _ -> Left (Text.pack "expected string")

asInteger :: Value -> Either Text Integer
asInteger value = case value of
  Value.Number n ->
    let d = Number.unwrap n
        e = Decimal.exponent d
     in if e >= 0
          then Right (Decimal.mantissa d * (10 ^ e))
          else Left (Text.pack "expected integer, got fraction")
  _ -> Left (Text.pack "expected number")

field :: Text -> [(Text, Value)] -> Either Text Value
field k ps = case lookup k ps of
  Just v -> Right v
  Nothing -> Left (Text.pack "missing field: " <> k)

optField :: Text -> [(Text, Value)] -> Maybe Value
optField = lookup

tag :: Value -> Either Text (Text, Maybe Value)
tag value = do
  ps <- asObject value
  t <- field (Text.pack "type") ps >>= asText
  pure (t, optField (Text.pack "value") ps)

-- Parsing --------------------------------------------------------------------

-- | 'Value.decode' already consumes the blanks around a document, so this only
-- has to pin the end of input and adapt the error to the codec's channel.
parse :: Text -> Either Text Value
parse input = case Parsec.parse (Value.decode <* Parsec.eof) "" input of
  Left err -> Left (Text.pack (show err))
  Right value -> Right value
